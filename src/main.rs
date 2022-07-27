use std::{borrow::Cow, collections::HashMap};

use twitch::ReadState;

mod spotify;
mod twitch;
mod util;

// TODO this could borrow from the queue (and be covariant to 'static)
type SpotifyCallback = fn(&spotify::Queue<spotify::Song>) -> Reply<'static>;

struct Entry<'a> {
    key: &'a str,
    val: SpotifyCallback,
    map: HashMap<Box<str>, SpotifyCallback>,
}

impl<'a> std::fmt::Debug for Entry<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Entry")
            .field("key", &self.key)
            .field("val", &(self.val as *const SpotifyCallback))
            .field("map", &self.map.keys().map(|s| &**s))
            .finish()
    }
}

impl<'a> Entry<'a> {
    fn alias(mut self, key: &'a str) -> Self {
        let key = std::mem::replace(&mut self.key, key);
        self.map.insert(Box::from(key), self.val);
        self
    }

    fn with(mut self, key: &'a str, func: SpotifyCallback) -> Self {
        let key = std::mem::replace(&mut self.key, key);
        let func = std::mem::replace(&mut self.val, func);
        self.map.insert(Box::from(key), func);
        self
    }

    fn finish(mut self) -> Commands {
        self.map.insert(Box::from(self.key), self.val);
        Commands { map: self.map }
    }
}

#[derive(Default)]
struct Commands {
    map: HashMap<Box<str>, SpotifyCallback>,
}

impl std::fmt::Debug for Commands {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut map = f.debug_map();
        for (k, v) in &self.map {
            map.entry(&k, &format_args!("{:p}", (*v) as *const SpotifyCallback));
        }

        map.finish()
    }
}

impl Commands {
    fn with(self, key: &str, func: SpotifyCallback) -> Entry<'_> {
        Entry {
            key,
            val: func,
            map: self.map,
        }
    }

    fn dispatch<'a>(
        &'a self,
        input: &str,
        queue: &'a spotify::Queue<spotify::Song>,
    ) -> Option<Reply<'static>> {
        self.map
            .get(input)
            .map(|s| {
                log::debug!(
                    "got command: {} -> {:p}",
                    input,
                    s as *const SpotifyCallback
                );
                s
            })
            .map(|func| func(queue))
    }
}

#[derive(Debug)]
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

fn current(queue: &spotify::Queue<spotify::Song>) -> Reply<'static> {
    Reply::Single(match queue.newest() {
        Some(s) => s.to_string().into(),
        None => Cow::Borrowed("I don't know"),
    })
}

fn previous(queue: &spotify::Queue<spotify::Song>) -> Reply<'static> {
    Reply::Single(match queue.previous() {
        Some(s) => s.to_string().into(),
        None => Cow::Borrowed("no previous song"),
    })
}

fn recent(queue: &spotify::Queue<spotify::Song>) -> Reply<'static> {
    fn format(index: usize, size: impl std::fmt::Display) -> String {
        format!(
            "{}: {}",
            match index {
                0 => Cow::Borrowed("current"),
                1 => Cow::Borrowed("previous"),
                n => Cow::Owned(format!("previous -{}", n - 1)),
            },
            size
        )
    }

    let seq = queue
        .iter()
        .enumerate()
        .map(|(i, s)| format(i, s))
        .map(Into::into)
        .take(AnnoyingBot::MAX)
        .collect();

    Reply::Many(seq)
}

struct AnnoyingBot {
    channel: Box<str>,
    conn: twitch::Connection,
    commands: Commands,
    queue: spotify::Queue<spotify::Song>,
    should_spam: bool,
}

impl AnnoyingBot {
    const MAX: usize = 5;

    fn new(
        conn: twitch::Connection,
        channel: impl Into<Box<str>>,
        commands: Commands,
        should_spam: bool,
    ) -> Self {
        Self {
            conn,
            channel: channel.into(),
            commands,
            queue: spotify::Queue::new(Self::MAX),
            should_spam,
        }
    }

    fn push_song(&mut self, song: spotify::Song) {
        self.queue.push(song)
    }

    fn read_message(&mut self) -> anyhow::Result<ReadState<twitch::Message>> {
        self.conn.read_message()
    }

    fn handle_message(&mut self, msg: &twitch::Message) -> anyhow::Result<()> {
        let (data, channel) = match &msg.command {
            twitch::Command::Privmsg { data, channel } => (data, channel),
            twitch::Command::Ping { token } => {
                log::debug!("got a PING with {}", token);
                self.conn.raw(format!("PONG :{}", token))?;
                return Ok(());
            }
            _ => return Ok(()),
        };

        log::debug!(
            "[{}] {}: {}",
            channel,
            msg.prefix.as_ref().unwrap().name(),
            data
        );

        let max = (self.should_spam).then_some(Self::MAX).unwrap_or(1);
        for resp in self
            .commands
            .dispatch(data, &self.queue)
            .iter()
            .flat_map(Reply::iter)
            .take(max)
        {
            log::debug!("replying: {}", resp);
            self.reply(resp)?;
        }

        Ok(())
    }

    fn reply(&mut self, data: impl std::fmt::Display + Send) -> anyhow::Result<()> {
        self.conn.send(&self.channel, data)
    }
}

fn main() -> anyhow::Result<()> {
    simple_env_load::load_env_from(&[".dev.env", ".env"]);
    alto_logger::init_term_logger()?;

    let (tx, rx) = flume::bounded(1);

    let mut spotify = spotify::Spotify::connect()?;
    let func = move || {
        let interval = std::time::Duration::from_secs(10);
        loop {
            // TODO look up to see if I'm streaming. if I'm not, don't do this
            if let Some(song) = spotify.get_song() {
                if tx.send(song).is_err() {
                    break;
                }
            }
            std::thread::sleep(interval);
        }
    };

    std::thread::spawn(func);

    let channel = std::env::var("SHAKEN_TWITCH_CHANNEL").unwrap();
    let oauth = std::env::var("SHAKEN_TWITCH_OAUTH_TOKEN").unwrap();
    let nick = std::env::var("SHAKEN_TWITCH_NAME").unwrap();
    let should_spam = std::env::var("SHAKEN_TWITCH_SHOULD_SPAM")
        .map(|s| s.to_ascii_lowercase())
        .map(|s| matches!(&*s, "1" | "yes" | "true" | "absolutely"))
        .unwrap_or_default();

    let bot = twitch::Connection::connect(twitch::Registration {
        oauth: Cow::Owned(oauth),
        nick: Cow::Owned(nick),
        channel: Cow::Borrowed(&*channel),
        caps: twitch::DEFAULT_CAPS,
    })?;

    let commands = Commands::default()
        .with("!song", current)
        .alias("!current")
        .with("!previous", previous)
        .with("!recent", recent)
        .finish();

    let mut annoying = AnnoyingBot::new(bot, channel, commands, should_spam);

    loop {
        if let Ok(song) = rx.try_recv() {
            log::trace!("song: {:?}", song);
            if should_spam {
                annoying.reply(&song)?;
            }
            annoying.push_song(song);
        }

        match annoying.read_message()? {
            ReadState::Complete(msg) => annoying.handle_message(&msg)?,
            ReadState::Eof => break,
            ReadState::Incomplete => {
                fn yield_now() {
                    // TODO really yield here
                    std::thread::sleep(std::time::Duration::from_millis(1))
                }
                // XXX: spin_hint isn't doing anything
                yield_now()
            }
        }
    }

    Ok(())
}
