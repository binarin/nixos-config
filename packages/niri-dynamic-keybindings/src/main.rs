use anyhow::Result;
use clap::Parser;
use log::{error, info, debug, LevelFilter};
use niri_ipc::socket::Socket;
use niri_ipc::state::{EventStreamState, EventStreamStatePart};
use niri_ipc::{Event, Request, Response, Window};
use pretty_env_logger::formatted_timed_builder;
use std::fs::File;
use std::io::Write;
use std::path::Path;

#[derive(Debug, Parser)]
#[command(version)]
struct Args {
    #[arg(short, long)]
    target_file: String,
}

#[derive(Debug)]
struct BindWriter {
    path: Box<Path>,
    is_emacs: Option<bool>,
}

impl BindWriter {
    fn update_binds(&mut self, focused_window: &Window) -> Result<()> {
        let new_is_emacs = Some(match focused_window {
            Window {
                is_focused: true,
                app_id: Some(app_id),
                ..
            } => app_id.to_lowercase() == "emacs",
            _ => false,
        });

        if self.is_emacs == new_is_emacs {
            return Ok(());
        }
        self.is_emacs = new_is_emacs;

        let mut file = File::create(&self.path)?;

        if self.is_emacs == Some(true) {
            debug!("is emacs, cleaning file");
            file.write("".as_bytes())?;
        } else {
            debug!("not emacs, binding");
            file.write_all(
                r#"
                  binds {
                    Ctrl+Backslash { switch-layout "next"; }
                  }
                "#
                .as_bytes(),
            )?;
        }
        Ok(())
    }
}

fn main() -> Result<()> {
    formatted_timed_builder()
        .filter(None, LevelFilter::Info)
        .init();

    let args = Args::parse();

    info!("Starting, target {}", args.target_file);

    let mut writer = BindWriter {
        path: Path::new(&args.target_file).into(),
        is_emacs: None,
    };

    let mut state: EventStreamState = Default::default();

    let mut socket = Socket::connect()?;
    let reply = socket.send(Request::EventStream)?;
    if matches!(reply, Ok(Response::Handled)) {
        let mut read_event = socket.read_events();
        while let Ok(event) = read_event() {
            state.apply(event.clone());

            let id = match event {
                Event::WindowOpenedOrChanged {
                    window: Window { id, .. },
                } => id,
                Event::WindowFocusChanged { id: Some(id) } => id,
                _ => continue,
            };

            match state.windows.windows.get(&id) {
                Some(
                    w @ Window {
                        is_focused: true, ..
                    },
                ) => match writer.update_binds(&w) {
                    Err(e) => error!("{e}"),
                    Ok(_) => (),
                },
                _ => continue,
            }
        }
    }
    Ok(())
}
