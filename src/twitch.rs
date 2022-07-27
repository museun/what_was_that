use std::{
    borrow::Cow,
    io::{BufRead as _, BufReader, Write as _},
    net::TcpStream,
};

pub const DEFAULT_CAPS: [&str; 3] = [
    "CAP REQ :twitch.tv/membership",
    "CAP REQ :twitch.tv/tags",
    "CAP REQ :twitch.tv/commands",
];

#[derive(Debug, Clone)]
pub struct Registration<'a, const N: usize> {
    pub oauth: Cow<'a, str>,
    pub nick: Cow<'a, str>,
    pub caps: [&'static str; N],
    pub channel: Cow<'a, str>,
}

pub struct Connection {
    read: BufReader<TcpStream>,
    write: TcpStream,
}

pub enum ReadState<T> {
    Eof,
    Incomplete,
    Complete(T),
}

impl Connection {
    pub fn read_message(&mut self) -> anyhow::Result<ReadState<Message>> {
        use std::io::ErrorKind::*;
        let mut buf = String::with_capacity(1024);
        let n = match self.read.read_line(&mut buf) {
            Ok(0) => return Ok(ReadState::Eof),
            Ok(n) => n,
            Err(e) if e.kind() == WouldBlock => return Ok(ReadState::Incomplete),
            Err(e) if e.kind() == Interrupted => return Ok(ReadState::Incomplete),
            Err(err) => anyhow::bail!(err),
        };
        let data = &buf[..n];
        log::trace!(target: "irc", "{}", data.escape_debug());
        Ok(ReadState::Complete(Message::parse(data)))
    }

    pub fn send(
        &mut self,
        channel: &str,
        data: impl std::fmt::Display + Send,
    ) -> anyhow::Result<()> {
        let msg = format!("PRIVMSG {channel} :{data}\r\n");
        log::trace!("> {}", msg.escape_debug());
        self.raw(msg)?;
        Ok(())
    }

    pub fn raw(&mut self, input: impl AsRef<[u8]> + Send + Sync) -> anyhow::Result<()> {
        self.write.write_all(input.as_ref())?;
        self.write.write_all(b"\r\n")?;
        self.write.flush()?;
        Ok(())
    }

    pub fn connect<const N: usize>(reg: Registration<'_, N>) -> anyhow::Result<Self> {
        // TODO heartbeat

        let Registration {
            oauth,
            nick,
            channel,
            caps,
        } = reg;

        let mut write = TcpStream::connect("irc.chat.twitch.tv:6667")?;
        let mut read = write.try_clone().map(BufReader::new)?;

        for cap in caps {
            write.write_all(cap.as_bytes())?;
            write.write_all(b"\r\n")?;
        }

        let msg = format!("PASS {}\r\nNICK {}\r\n", oauth, nick);

        write.write_all(msg.as_bytes())?;

        log::info!("waiting for ready");
        let mut buf = String::with_capacity(1024);
        loop {
            let n = read.read_line(&mut buf)?;
            let data = &buf[..n];

            let msg = Message::parse(data);
            match msg.command {
                Command::Error { message } => anyhow::bail!("error! {}", message),
                Command::GlobalUserState => {
                    log::info!("connected");
                    break;
                }
                Command::Ping { token } => {
                    let msg = format!("PONG {}\r\n", token);
                    write.write_all(msg.as_bytes())?
                }
                _ => {}
            }
            buf.clear();
        }

        read.get_mut().set_nonblocking(true)?;

        log::info!("joining: {}", channel);
        let msg = format!("JOIN {}\r\n", channel);
        write.write_all(msg.as_bytes())?;

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
    pub prefix: Option<Prefix>,
    pub command: Command,
}

impl Message {
    fn parse(input: &str) -> Self {
        let raw = input;
        let mut input = input.trim();
        let input = &mut input;

        // skip the tags
        if input.starts_with('@') {
            let (_tags, tail) = input.split_once(' ').expect("valid input");
            *input = tail
        }

        Self {
            raw: raw.into(),
            prefix: Prefix::parse(input),
            command: Command::parse(input),
        }
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
