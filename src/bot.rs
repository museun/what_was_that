use crate::{
    commands::Commands,
    reply::Reply,
    spotify,
    twitch::{self, ReadState},
};

pub struct AnnoyingBot {
    channel: Box<str>,
    conn: twitch::Connection,
    commands: Commands,
    queue: spotify::Queue<spotify::Song>,
    should_spam: bool,
}

impl AnnoyingBot {
    pub const MAX: usize = 5;

    pub fn new(
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

    pub fn push_song(&mut self, song: spotify::Song) {
        self.queue.push(song)
    }

    pub fn read_message(&mut self) -> anyhow::Result<ReadState<twitch::Message>> {
        self.conn.read_message()
    }

    pub fn handle_message(&mut self, msg: &twitch::Message) -> anyhow::Result<()> {
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

    pub fn reply(&mut self, data: impl std::fmt::Display + Send) -> anyhow::Result<()> {
        self.conn.send(&self.channel, data)
    }
}
