use std::collections::HashMap;

use crate::{spotify, Reply};

// TODO this could borrow from the queue (and be covariant to 'static)
pub type SpotifyCallback = fn(&spotify::Queue<spotify::Song>) -> Reply<'static>;

pub struct Entry<'a> {
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
    pub fn alias(mut self, key: &'a str) -> Self {
        let key = std::mem::replace(&mut self.key, key);
        self.map.insert(Box::from(key), self.val);
        self
    }

    pub fn with(mut self, key: &'a str, func: SpotifyCallback) -> Self {
        let key = std::mem::replace(&mut self.key, key);
        let func = std::mem::replace(&mut self.val, func);
        self.map.insert(Box::from(key), func);
        self
    }

    pub fn finish(mut self) -> Commands {
        self.map.insert(Box::from(self.key), self.val);
        Commands { map: self.map }
    }
}

#[derive(Default)]
pub struct Commands {
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
    pub fn with(self, key: &str, func: SpotifyCallback) -> Entry<'_> {
        Entry {
            key,
            val: func,
            map: self.map,
        }
    }

    pub fn dispatch<'a>(
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
