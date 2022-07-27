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

pub struct Connection {
    read: BufReader<OwnedReadHalf>,
    write: OwnedWriteHalf,
}

impl Connection {
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
            Self::User { name } | Self::Server { host: name } => name,
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