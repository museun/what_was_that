use crate::util::IterExt as _;

use anyhow::Context as _;
use rspotify::{
    auth_code::AuthCodeSpotify,
    clients::oauth::OAuthClient,
    model::{CurrentlyPlayingContext, CurrentlyPlayingType, FullTrack, Id, PlayableItem, TrackId},
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
            self.inner.pop_front();
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

    pub fn previous(&self) -> Option<&T> {
        self.inner.iter().rev().nth(1)
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
    pub fn connect() -> anyhow::Result<Self> {
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
        auth.prompt_for_token(&url)?;

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

    pub fn try_produce_songs(mut self) -> impl Fn() -> Option<Song>
    where
        C: Send + Sync + 'static,
    {
        let (tx, rx) = flume::bounded(1);

        let func = move || {
            let interval = std::time::Duration::from_secs(10);
            loop {
                // TODO look up to see if I'm streaming. if I'm not, don't do this
                if let Some(song) = self.get_song() {
                    if tx.send(song).is_err() {
                        break;
                    }
                }
                std::thread::sleep(interval);
            }
        };

        let _handle = std::thread::spawn(func);

        move || rx.try_recv().ok()
    }

    pub fn get_song(&mut self) -> Option<Song> {
        let current = self
            .client
            .current_playing(None, <Option<Option<_>>>::None)
            .ok()
            .flatten()
            .filter(
                |&CurrentlyPlayingContext {
                     is_playing,
                     currently_playing_type: ty,
                     ..
                 }| is_playing && matches!(ty, CurrentlyPlayingType::Track),
            )?;

        let FullTrack {
            id,
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
        write!(f, "{} -> {} - {}", self.id.url(), self.artists, self.name)
    }
}
