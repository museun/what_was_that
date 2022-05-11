#![cfg_attr(
    any(debug_assertions, test),
    allow(dead_code, unused_variables, unreachable_code,)
)]

use std::{borrow::Cow, collections::HashMap};

mod spotify {
    use crate::IterExt as _;
    use anyhow::Context as _;
    use rspotify::{
        auth_code::AuthCodeSpotify,
        clients::oauth::OAuthClient,
        model::{CurrentlyPlayingContext, CurrentlyPlayingType, FullTrack, PlayableItem, TrackId},
        Credentials, OAuth,
    };
    use std::{collections::VecDeque, time::Duration};

    pub struct Queue<T> {
        inner: VecDeque<T>,
        max: usize,
    }

    impl<T> Queue<T> {
        pub fn new(max: usize) -> Self {
            assert!(max > 0, "queue must have atleast a single element");
            Self {
                inner: VecDeque::with_capacity(max),
                max,
            }
        }

        pub fn push(&mut self, item: T) {
            while self.inner.len() >= self.max {
                self.inner.pop_back();
            }

            self.inner.push_back(item);
        }

        pub fn has(&self, item: &T) -> bool
        where
            T: PartialEq,
        {
            self.inner.back().filter(|&d| d == item).is_some()
        }

        pub fn newest(&self) -> Option<&T> {
            self.inner.back()
        }

        pub fn oldest(&self) -> Option<&T> {
            self.inner.front()
        }

        pub fn iter(&self) -> impl Iterator<Item = &T> + '_ {
            self.inner.iter().rev()
        }
    }

    pub struct Spotify<C> {
        client: C,
        seen: Queue<TrackId>,
    }

    impl Spotify<AuthCodeSpotify> {
        pub async fn connect() -> anyhow::Result<Self> {
            let credentials =
                Credentials::from_env().with_context(|| "cannot get rspotify credentials")?;

            let oauth = OAuth::from_env(rspotify::scopes!(
                "user-read-playback-state",
                "user-read-currently-playing"
            ))
            .with_context(|| "cannot get rspotify oauth pref")?;

            let config = rspotify::Config {
                token_cached: true,
                token_refreshing: true,
                ..rspotify::Config::default()
            };

            let mut auth = AuthCodeSpotify::with_config(credentials, oauth, config);
            let url = auth.get_authorize_url(false)?;
            auth.prompt_for_token(&url).await?;

            Ok(auth).map(Self::new)
        }
    }

    impl<C> Spotify<C>
    where
        C: OAuthClient,
    {
        pub fn new(client: C) -> Self {
            Self {
                client,
                seen: Queue::new(10),
            }
        }

        pub async fn get_song(&mut self) -> Option<Song> {
            let current = self
                .client
                .current_playing(None, <Option<Option<_>>>::None)
                .await
                .ok()
                .flatten()
                .filter(
                    |&CurrentlyPlayingContext {
                         is_playing,
                         currently_playing_type: ty,
                         ..
                     }| {
                        is_playing && matches!(ty, CurrentlyPlayingType::Track)
                    },
                )?;

            let FullTrack {
                id,
                href,
                name,
                artists,
                duration,
                ..
            } = match current.item {
                Some(PlayableItem::Track(track)) => track,
                _ => return None,
            };

            let id = id?;
            if self.seen.has(&id) {
                return None;
            }
            self.seen.push(id.clone());

            let song = Song {
                id,
                href: href?,
                name,
                artists: artists.iter().map(|a| &*a.name).join(", "),
                duration,
                progress: current.progress?,
            };

            Some(song)
        }
    }

    #[derive(Debug, Clone)]
    pub struct Song {
        pub id: TrackId,
        pub href: String,
        pub name: String,
        pub artists: String,
        pub duration: Duration,
        pub progress: Duration,
    }

    impl Song {
        #[allow(dead_code)]
        const fn end_of_song(&self) -> Option<Duration> {
            self.duration.checked_sub(self.progress)
        }
    }

    impl std::fmt::Display for Song {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{} -> {} - {}", self.href, self.artists, self.name)
        }
    }
}

trait IterExt: Iterator + Sized {
    fn join(self, s: &str) -> String;
}

impl<'a, I, T> IterExt for I
where
    I: Iterator<Item = T>,
    T: std::ops::Deref<Target = str> + 'a,
{
    fn join(self, s: &str) -> String {
        self.fold(String::new(), |mut a, c| {
            if !a.is_empty() {
                a.push_str(s);
            }
            a.push_str(&c);
            a
        })
    }
}

mod twitch {
    use std::{borrow::Cow, collections::HashMap, str::FromStr};

    use anyhow::Context;
    use tokio::{
        io::{AsyncBufReadExt, AsyncWriteExt, BufReader},
        net::tcp::{OwnedReadHalf, OwnedWriteHalf},
    };

    pub const DEFAULT_CAPS: [&str; 3] = [
        "CAP REQ :twitch.tv/membership",
        "CAP REQ :twitch.tv/tags",
        "CAP REQ :twitch.tv/commands",
    ];

    #[derive(Debug, Clone)]
    pub struct Registration<'a, const N: usize> {
        pub oauth: Cow<'a, str>,
        pub caps: [&'static str; N],
        pub channel: Cow<'a, str>,
    }

    pub struct Bot {
        read: BufReader<OwnedReadHalf>,
        write: OwnedWriteHalf,
    }

    impl Bot {
        pub async fn read_message(&mut self) -> anyhow::Result<Message> {
            let mut buf = String::with_capacity(1024);
            let n = self.read.read_line(&mut buf).await?;
            let data = &buf[..n];
            log::trace!(target: "irc", "{}", data.escape_debug());
            Ok(Message::parse(data))
        }

        pub async fn send(
            &mut self,
            channel: &str,
            data: impl std::fmt::Display + Send,
        ) -> anyhow::Result<()> {
            let msg = format!("PRIVMSG {channel} :{data}\r\n");
            log::trace!("> {}", msg.escape_debug());
            self.raw(msg).await?;
            Ok(())
        }

        pub async fn raw(&mut self, input: impl AsRef<[u8]> + Send + Sync) -> anyhow::Result<()> {
            self.write.write_all(input.as_ref()).await?;
            self.write.write_all(b"\r\n").await?;
            self.write.flush().await?;
            Ok(())
        }

        pub async fn connect<const N: usize>(reg: Registration<'_, N>) -> anyhow::Result<Self> {
            // TODO heartbeat

            let Registration {
                oauth,
                channel,
                caps,
            } = reg;

            let stream = tokio::net::TcpStream::connect("irc.chat.twitch.tv:6667").await?;
            let (read, mut write) = stream.into_split();
            let mut read = BufReader::new(read);

            for cap in caps {
                write.write_all(cap.as_bytes()).await?;
                write.write_all(b"\r\n").await?;
            }

            write
                .write_all(format!("PASS {}\r\nNICK {}\r\n", oauth, "shaken_bot").as_bytes())
                .await?;

            let mut buf = String::with_capacity(1024);
            loop {
                let n = read.read_line(&mut buf).await?;
                let data = &buf[..n];

                let msg = Message::parse(data);
                match msg.command {
                    Command::Error { message } => anyhow::bail!("error! {}", message),
                    Command::GlobalUserState => break,
                    Command::Ping { token } => {
                        write
                            .write_all(format!("PONG {}\r\n", token).as_bytes())
                            .await?
                    }
                    _ => {}
                }
                buf.clear();
            }

            write
                .write_all(format!("JOIN {}\r\n", channel).as_bytes())
                .await?;

            Ok(Self { read, write })
        }
    }

    #[derive(Debug)]
    pub enum Command {
        GlobalUserState,
        Ping { token: Box<str> },
        Privmsg { channel: Box<str>, data: Box<str> },
        Error { message: Box<str> },
        Unknown { tail: Box<str> },
    }

    impl Command {
        fn parse(input: &str) -> Self {
            input
                .split_once(" :")
                .and_then(|(args, data)| {
                    let mut iter = args.split_whitespace();
                    let cmd = match iter.next()? {
                        "PING" => Self::Ping { token: data.into() },
                        "PRIVMSG" => Self::Privmsg {
                            data: data.into(),
                            channel: iter.next().expect("channel").into(),
                        },
                        "ERROR" => Self::Error {
                            message: data.into(),
                        },
                        _ => return None,
                    };
                    Some(cmd)
                })
                .unwrap_or_else(|| match input.trim() {
                    "GLOBALUSERSTATE" => Self::GlobalUserState,
                    data => Self::Unknown { tail: data.into() },
                })
        }
    }

    #[derive(Debug)]
    pub struct Message {
        pub raw: Box<str>,
        pub tags: Option<Tags>,
        pub prefix: Option<Prefix>,
        pub command: Command,
    }

    impl Message {
        fn parse(input: &str) -> Self {
            let raw = input;
            let mut input = input.trim();
            let input = &mut input;

            Self {
                raw: raw.into(),
                tags: Tags::parse(input),
                prefix: Prefix::parse(input),
                command: Command::parse(input),
            }
        }
    }

    #[derive(Debug)]
    pub struct Tags(HashMap<Box<str>, Box<str>>);

    // TODO what is this supposed to be used for?
    #[allow(dead_code)]
    impl Tags {
        pub fn get<K>(&self, key: &K) -> Option<&str>
        where
            Box<str>: std::borrow::Borrow<K>,
            K: std::hash::Hash + Eq,
        {
            self.0.get(key).map(|s| &**s)
        }

        pub fn get_parsed<K, T>(&self, key: &K) -> anyhow::Result<T>
        where
            Box<str>: std::borrow::Borrow<K>,
            K: std::hash::Hash + Eq + std::fmt::Debug,
            T: FromStr,
            T::Err: Into<anyhow::Error>,
        {
            self.get(key)
                .with_context(|| anyhow::anyhow!("cannot find '{:?}'", key))?
                .parse()
                .map_err(Into::into)
        }

        pub fn get_bool<K>(&self, key: &K) -> bool
        where
            Box<str>: std::borrow::Borrow<K>,
            K: std::hash::Hash + Eq + std::fmt::Debug,
        {
            self.get(key)
                .filter(|&s| matches!(s, "1" | "true" | "TRUE"))
                .is_some()
        }
    }

    impl Tags {
        fn parse(input: &mut &str) -> Option<Self> {
            if !input.starts_with('@') {
                return None;
            }
            let (head, tail) = input[1..].split_once(' ')?;
            *input = tail;

            Some(Self(
                head.split_terminator(';')
                    .flat_map(|s| s.split_once('='))
                    .map(|(l, r)| (l.into(), r.into()))
                    .collect(),
            ))
        }
    }

    #[derive(Debug)]
    pub enum Prefix {
        User { name: Box<str> },
        Server { host: Box<str> },
    }

    impl Prefix {
        pub const fn name(&self) -> &str {
            match self {
                Self::User { name } | Self::Server { host: name } => &*name,
            }
        }

        fn parse(input: &mut &str) -> Option<Self> {
            if !input.starts_with(':') {
                return None;
            }

            let (head, tail) = input[1..].split_once(' ')?;
            *input = tail;

            Some(
                head.split_once('!')
                    .map(|(head, _)| Self::User { name: head.into() })
                    .unwrap_or_else(|| Self::Server { host: head.into() }),
            )
        }
    }
}

// TODO this could borrow from the queue (and be covariant to 'static)
type SpotifyCallback = fn(&spotify::Queue<spotify::Song>) -> Reply<'static>;

#[derive(Default)]
struct Commands {
    map: HashMap<Box<str>, SpotifyCallback>,
}

impl Commands {
    fn with(
        mut self,
        key: &str,
        func: fn(&spotify::Queue<spotify::Song>) -> Reply<'static>,
    ) -> Self {
        self.map.insert(key.into(), func);
        self
    }

    fn dispatch<'a>(
        &'a self,
        input: &str,
        queue: &'a spotify::Queue<spotify::Song>,
    ) -> Option<Reply<'static>> {
        self.map.get(input).map(|func| func(queue))
    }
}

enum Reply<'a> {
    Single(Cow<'a, str>),
    Many(Cow<'a, [Cow<'a, str>]>),
    #[allow(dead_code)]
    None,
}

impl<'b, 'a: 'b> Reply<'a> {
    fn iter(&'b self) -> impl Iterator<Item = &'b str> + 'b {
        match self {
            Self::Single(s) => std::slice::from_ref(s),
            Self::Many(n) => &**n,
            Self::None => &[],
        }
        .iter()
        .map(std::ops::Deref::deref)
    }
}

trait FutureExt
where
    Self: std::future::Future + Sized + Send + Sync,
    Self::Output: Send + Sync,
{
    fn race<F>(self, other: F) -> Or<Self, F>
    where
        F: std::future::Future + Send + Sync,
        F::Output: Send + Sync;
}

impl<T> FutureExt for T
where
    T: std::future::Future + Sized + Send + Sync,
    T::Output: Send + Sync,
{
    fn race<F>(self, other: F) -> Or<Self, F>
    where
        F: std::future::Future + Send + Sync,
        F::Output: Send + Sync,
    {
        let left = self;
        Or {
            left,
            right: other,
            biased: false,
        }
    }
}

pin_project_lite::pin_project! {
    struct Or<L, R> {
        #[pin] left: L,
        #[pin] right: R,
        biased: bool
    }
}

impl<L, R> std::future::Future for Or<L, R>
where
    L: std::future::Future + Send + Sync,
    R: std::future::Future + Send + Sync,

    L::Output: Send + Sync,
    R::Output: Send + Sync,
{
    type Output = Either<L::Output, R::Output>;

    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        use {
            std::task::Poll,
            Either::{Left, Right},
        };

        let this = self.project();

        macro_rules! poll {
            ($expr:ident => $map:expr) => {
                if let Poll::Ready(t) = this.$expr.poll(cx).map($map) {
                    return Poll::Ready(t);
                }
            };
        }

        if *this.biased {
            poll!(left => Left);
            poll!(right => Right);
            return Poll::Pending;
        }

        if fastrand::bool() {
            poll!(left => Left);
            poll!(right => Right);
        } else {
            poll!(right => Right);
            poll!(left => Left);
        }

        Poll::Pending
    }
}

enum Either<L, R> {
    Left(L),
    Right(R),
}

fn current(queue: &spotify::Queue<spotify::Song>) -> Reply<'static> {
    Reply::Single(match queue.newest() {
        Some(s) => s.to_string().into(),
        None => Cow::Borrowed("I don't know"),
    })
}

fn previous(queue: &spotify::Queue<spotify::Song>) -> Reply<'static> {
    Reply::Single(match queue.oldest() {
        Some(s) => s.to_string().into(),
        None => Cow::Borrowed("no previous song"),
    })
}

fn recent(queue: &spotify::Queue<spotify::Song>) -> Reply<'static> {
    const MAX: usize = 10;

    Reply::Many(
        queue
            .iter()
            .enumerate()
            .map(|(i, s)| {
                format!(
                    "{}: {}",
                    match i {
                        0 => Cow::Borrowed("current"),
                        1 => Cow::Borrowed("previous"),
                        n => Cow::Owned(format!("previous (-{})", n)),
                    },
                    s
                )
            })
            .map(Into::into)
            .take(MAX)
            .collect::<Cow<'_, [Cow<'_, str>]>>(),
    )
}

struct AnnoyingBot {
    channel: Box<str>,
    bot: twitch::Bot,
    commands: Commands,
    queue: spotify::Queue<spotify::Song>,
}

impl AnnoyingBot {
    fn new(bot: twitch::Bot, channel: impl Into<Box<str>>, commands: Commands) -> Self {
        Self {
            bot,
            channel: channel.into(),
            commands,
            queue: spotify::Queue::new(10),
        }
    }

    fn push_song(&mut self, song: spotify::Song) {
        self.queue.push(song)
    }

    async fn read_message(&mut self) -> anyhow::Result<twitch::Message> {
        self.bot.read_message().await
    }

    async fn handle_privmsg(&mut self, msg: &twitch::Message) -> anyhow::Result<()> {
        let (data, channel) = match &msg.command {
            twitch::Command::Privmsg { data, channel } => (data, channel),
            _ => return Ok(()),
        };

        log::debug!(
            "[{}] {}: {}",
            channel,
            msg.prefix.as_ref().unwrap().name(),
            data
        );

        for el in self
            .commands
            .dispatch(&*data, &self.queue)
            .iter()
            .flat_map(Reply::iter)
        {
            self.reply(el).await?;
        }

        Ok(())
    }

    async fn reply(&mut self, data: impl std::fmt::Display + Send) -> anyhow::Result<()> {
        self.bot.send(&*self.channel, data).await
    }
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    simple_env_load::load_env_from(&[".dev.env", ".env"]);
    alto_logger::init_term_logger()?;

    let (tx, mut rx) = tokio::sync::mpsc::channel(1);

    let (quit_tx, mut quit_rx) = tokio::sync::oneshot::channel::<()>();

    let fut = async move {
        let mut spotify = spotify::Spotify::connect().await.unwrap();
        let mut interval = tokio::time::interval(std::time::Duration::from_secs(10));

        loop {
            // TODO look up to see if I'm streaming, if I'm not, don't do this

            tokio::pin! {
                let song = spotify.get_song();
                let quit = &mut quit_rx;
                let tick = interval.tick();
            }

            tokio::select! {
                Some(song) = song => {
                    log::trace!(target: "spotify", "got song: {}", song);
                    if tx.send(song).await.is_err() {
                        break;
                    }
                }
                _ = quit => break,
                _ = tick => {}
            }
        }
    };

    tokio::task::spawn(fut);

    let channel = std::env::var("SHAKEN_TWITCH_CHANNEL").unwrap();

    let oauth = std::env::var("SHAKEN_TWITCH_OAUTH_TOKEN").unwrap();
    let bot = twitch::Bot::connect(twitch::Registration {
        oauth: Cow::Owned(oauth),
        channel: Cow::Borrowed(&*channel),
        caps: twitch::DEFAULT_CAPS,
    })
    .await?;

    let commands = Commands::default()
        .with("!song", current)
        .with("!previous", previous)
        .with("!recent", recent);

    let mut annoying = AnnoyingBot::new(bot, channel, commands);

    loop {
        let recv = rx.recv();
        let msg = annoying.read_message();

        match msg.race(recv).await {
            Either::Left(Ok(msg)) => annoying.handle_privmsg(&msg).await?,

            Either::Right(Some(song)) => {
                annoying.reply(&song).await?;
                annoying.push_song(song);
            }
            _ => continue,
        }
    }
}
