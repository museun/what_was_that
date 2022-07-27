use std::borrow::Cow;

use crate::{bot::AnnoyingBot, reply::Reply, spotify};

pub fn current(queue: &spotify::Queue<spotify::Song>) -> Reply<'static> {
    Reply::Single(match queue.newest() {
        Some(s) => s.to_string().into(),
        None => Cow::Borrowed("I don't know"),
    })
}

pub fn previous(queue: &spotify::Queue<spotify::Song>) -> Reply<'static> {
    Reply::Single(match queue.previous() {
        Some(s) => s.to_string().into(),
        None => Cow::Borrowed("no previous song"),
    })
}

pub fn recent(queue: &spotify::Queue<spotify::Song>) -> Reply<'static> {
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
