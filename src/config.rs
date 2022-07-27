use anyhow::Context as _;
use std::borrow::Cow;

use crate::twitch::{self, Registration};

pub struct Config {
    pub channel: String,
    pub oauth: String,
    pub nick: String,
    pub should_spam: bool,
}

impl Config {
    pub fn load_from_env() -> anyhow::Result<Self> {
        const SHAKEN_TWITCH_CHANNEL: &str = "SHAKEN_TWITCH_CHANNEL";
        const SHAKEN_TWITCH_OAUTH_TOKEN: &str = "SHAKEN_TWITCH_OAUTH_TOKEN";
        const SHAKEN_TWITCH_NAME: &str = "SHAKEN_TWITCH_NAME";
        const SHAKEN_TWITCH_SHOULD_SPAM: &str = "SHAKEN_TWITCH_SHOULD_SPAM";

        fn env_get(key: &str) -> anyhow::Result<String> {
            std::env::var(key).with_context(|| anyhow::anyhow!("env var `{}` must be set", key))
        }

        let is_true = |s: String| matches!(&*s, "1" | "yes" | "true" | "absolutely");

        Ok(Self {
            channel: env_get(SHAKEN_TWITCH_CHANNEL)?,
            oauth: env_get(SHAKEN_TWITCH_OAUTH_TOKEN)?,
            nick: env_get(SHAKEN_TWITCH_NAME)?,
            should_spam: env_get(SHAKEN_TWITCH_SHOULD_SPAM)
                .map(|s| s.to_ascii_lowercase())
                .map(is_true)
                .unwrap_or_default(),
        })
    }
}

impl<'a> From<&'a Config> for Registration<'a, { twitch::DEFAULT_CAPS.len() }> {
    fn from(this: &'a Config) -> Self {
        Self {
            oauth: Cow::Borrowed(&*this.oauth),
            nick: Cow::Borrowed(&*this.nick),
            channel: Cow::Borrowed(&*this.channel),
            caps: twitch::DEFAULT_CAPS,
        }
    }
}
