use std::{
    borrow::Cow,
    collections::{HashSet, VecDeque},
};

use anyhow::Context as _;
use rspotify::clients::OAuthClient;
use serenity::{
    async_trait,
    client::{Client, Context, EventHandler},
    framework::standard::{
        macros::{command, group},
        CommandResult, StandardFramework,
    },
    model::channel::Message,
};

#[group]
#[commands(current, previous, history)]
struct General;

#[command]
async fn current(ctx: &Context, msg: &Message) -> CommandResult {
    log::info!("got a current command");
    msg.reply(ctx, "current").await?;
    Ok(())
}

#[command]
async fn previous(ctx: &Context, msg: &Message) -> CommandResult {
    log::info!("got a previous command");
    msg.reply(ctx, "previous").await?;
    Ok(())
}

#[command]
async fn history(ctx: &Context, msg: &Message) -> CommandResult {
    log::info!("got a history command");
    msg.reply(ctx, "history").await?;
    Ok(())
}

struct Handler;

#[async_trait]
impl EventHandler for Handler {}

mod spotify {
    use std::{collections::VecDeque, sync::Arc, time::Duration};

    use anyhow::Context as _;
    use rspotify::{
        auth_code::AuthCodeSpotify,
        clients::oauth::OAuthClient,
        model::{CurrentlyPlayingType, FullTrack, PlayableItem, TrackId},
        Credentials, OAuth,
    };

    async fn connect_to_spotify() -> anyhow::Result<AuthCodeSpotify> {
        simple_env_load::load_env_from(&[".dev.env", ".env"]);

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

        Ok(auth)
    }

    struct Queue<T> {
        inner: VecDeque<T>,
        max: usize,
    }

    impl<T> Queue<T> {
        fn new(max: usize) -> Self {
            assert!(max > 0, "queue must have atleast a single element");
            Self {
                inner: VecDeque::with_capacity(max),
                max,
            }
        }

        fn push(&mut self, item: T) {
            while self.inner.len() >= self.max {
                self.inner.pop_back();
            }

            self.inner.push_back(item);
        }

        fn has(&self, item: &T) -> bool
        where
            T: PartialEq,
        {
            self.inner.back().filter(|&d| d == item).is_some()
        }
    }

    pub struct Spotify<C> {
        client: C,
        seen: Queue<TrackId>,
    }

    impl Spotify<AuthCodeSpotify> {
        pub async fn connect() -> anyhow::Result<Self> {
            connect_to_spotify().await.map(Self::new)
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

        async fn get_song(&mut self) -> anyhow::Result<Option<Song>> {
            let current = self
                .client
                .current_playing(None, <Option<Option<_>>>::None)
                .await?
                .with_context(|| "no song found")?;

            if !current.is_playing
                || !matches!(current.currently_playing_type, CurrentlyPlayingType::Track)
            {
                return Ok(None);
            }

            let FullTrack {
                id,
                href,
                name,
                artists,
                duration,
                ..
            } = match current.item {
                Some(PlayableItem::Track(track)) => track,
                _ => return Ok(None),
            };

            let id = id.with_context(|| "no id found")?;
            if self.seen.has(&id) {
                return Ok(None);
            }
            self.seen.push(id.clone());

            let song = Song {
                id,
                href: href.with_context(|| "no href found")?,
                name,
                artists: artists.iter().map(|a| &*a.name).join(", "),
                duration,
                progress: current.progress.with_context(|| "song is not playing")?,
            };

            Ok(Some(song))
        }
    }

    #[derive(Clone, Default)]
    struct SharedSongs {
        songs: Arc<tokio::sync::Mutex<Songs>>,
    }

    impl SharedSongs {
        async fn current(&self) -> Option<Stats> {
            self.songs.lock().await.current()
        }
    }

    #[derive(Default, Debug)]
    struct Songs {
        current: Option<Stats>,
        previous: Vec<Stats>,
    }

    #[derive(Clone, Debug)]
    struct Stats {
        song: Song,
        played: usize,
    }

    impl Songs {
        fn current(&self) -> Option<Stats> {
            self.current.clone()
        }

        fn all(&self) -> impl Iterator<Item = &Stats> {
            self.current
                .as_ref()
                .into_iter()
                .chain(self.previous.iter())
        }
    }

    #[derive(Debug, Clone)]
    struct Song {
        id: TrackId,
        href: String,
        name: String,
        artists: String,
        duration: Duration,
        progress: Duration,
    }

    impl Song {
        fn end_of_song(&self) -> Option<Duration> {
            self.duration.checked_sub(self.progress)
        }
    }

    impl std::fmt::Display for Song {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{} -> {} - {}", self.href, self.artists, self.name)
        }
    }

    trait IterExt: Iterator + Sized {
        fn join(self, s: &str) -> String;
    }

    impl<'a, I> IterExt for I
    where
        I: Iterator<Item = &'a str>,
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

    #[tokio::test]
    async fn foo() {
        let mut spotify = Spotify::connect().await.unwrap();
        while let Ok(song) = spotify.get_song().await {
            if let Some(song) = song {
                eprintln!("{}", song)
            }
            tokio::time::sleep(std::time::Duration::from_secs(10)).await;
        }
    }
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    alto_logger::init_alt_term_logger()?;
    simple_env_load::load_env_from(&[".dev.env", ".env"]);

    // let token = std::env::var("DISCORD_TOKEN").expect("token");
    // let framework = StandardFramework::new()
    //     .configure(|c| c.prefix("!"))
    //     .group(&GENERAL_GROUP);

    // log::trace!("connecting..");
    // let mut client = Client::builder(token)
    //     .event_handler(Handler)
    //     .framework(framework)
    //     .await?;

    // log::trace!("starting the bot");
    // client.start().await?;
    // log::debug!("done running");
    Ok(())
}
