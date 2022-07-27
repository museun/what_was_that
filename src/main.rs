mod bot;
use crate::bot::AnnoyingBot;

mod commands;
use commands::Commands;

mod callbacks;

mod config;
use config::Config;

mod spotify;

mod twitch;
use twitch::ReadState;

mod util;

mod reply;
use reply::Reply;

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
    let next_song = spotify::Spotify::connect()?.try_produce_songs();

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
