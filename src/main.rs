use std::{borrow::Cow, collections::HashMap};

mod spotify;
mod twitch;
mod util;

use util::{
    Either::{Left, Right},
    FutureExt as _,
};

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
    let (_quit_tx, mut quit_rx) = tokio::sync::oneshot::channel::<()>();

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
            Left(Ok(msg)) => annoying.handle_privmsg(&msg).await?,

            Right(Some(song)) => {
                annoying.reply(&song).await?;
                annoying.push_song(song);
            }
            _ => continue,
        }
    }
}
