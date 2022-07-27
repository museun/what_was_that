mod bot;
use crate::bot::AnnoyingBot;

mod commands;
use commands::Commands;

mod callbacks;

mod config;
use config::Config;

mod spotify;
use spotify::Song;

mod twitch;
use twitch::ReadState;

mod util;

mod reply;
use reply::Reply;

fn try_get_song_loop() -> anyhow::Result<impl Fn() -> Option<Song>> {
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

    let _handle = std::thread::spawn(func);

    Ok(move || rx.try_recv().ok())
}

fn main() -> anyhow::Result<()> {
    simple_env_load::load_env_from(&[".dev.env", ".env"]);
    alto_logger::init_term_logger()?;

    let config = Config::load_from_env()?;

    let reg = (&config).into();
    let bot = twitch::Connection::connect(reg)?;

    let commands = Commands::default()
        .with("!song", callbacks::current)
        .alias("!current")
        .with("!previous", callbacks::previous)
        .with("!recent", callbacks::recent)
        .finish();

    let mut annoying = AnnoyingBot::new(bot, config.channel, commands, config.should_spam);

    let next_song = try_get_song_loop()?;
    loop {
        if let Some(song) = next_song() {
            log::trace!("song: {:?}", song);
            if config.should_spam {
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
