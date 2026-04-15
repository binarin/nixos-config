use anyhow::{Context, Result};
use inotify::{Inotify, WatchMask};
use serde::Serialize;
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};

const FILE_NAME: &str = "org-mode-clock.txt";
const DEFAULT_MAX_LEN: usize = 30;

#[derive(Serialize)]
struct WaybarOutput {
    text: String,
    class: String,
}

fn watch_dir() -> PathBuf {
    let base = std::env::var("XDG_RUNTIME_DIR").unwrap_or_else(|_| "/tmp".to_string());
    PathBuf::from(base).join("org-mode")
}

fn truncate(s: &str, max_len: usize) -> String {
    if s.chars().count() <= max_len {
        s.to_string()
    } else {
        let mut t: String = s.chars().take(max_len).collect();
        t.push('…');
        t
    }
}

fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
}

fn read_clock_state(dir: &Path, max_len: usize) -> WaybarOutput {
    let path = dir.join(FILE_NAME);
    match fs::read_to_string(&path) {
        Ok(contents) => {
            let trimmed = contents.trim_end_matches('\n');
            if trimmed.is_empty() {
                WaybarOutput {
                    text: String::new(),
                    class: "clocked-out".to_string(),
                }
            } else {
                WaybarOutput {
                    text: html_escape(&truncate(trimmed, max_len)),
                    class: "clocked-in".to_string(),
                }
            }
        }
        Err(_) => WaybarOutput {
            text: String::new(),
            class: "clocked-out".to_string(),
        },
    }
}

fn emit(output: &WaybarOutput) -> Result<String> {
    let json = serde_json::to_string(output)?;
    let mut stdout = io::stdout().lock();
    writeln!(stdout, "{}", json)?;
    stdout.flush()?;
    Ok(json)
}

fn main() -> Result<()> {
    let max_len = std::env::args()
        .nth(1)
        .map(|s| s.parse::<usize>().expect("max length must be a number"))
        .unwrap_or(DEFAULT_MAX_LEN);

    let dir = watch_dir();
    fs::create_dir_all(&dir).context("failed to create watch directory")?;

    // Emit initial state
    let mut last_output = emit(&read_clock_state(&dir, max_len))?;

    // Set up inotify on the directory
    let mut inotify = Inotify::init().context("failed to init inotify")?;
    inotify
        .watches()
        .add(
            &dir,
            WatchMask::CREATE
                | WatchMask::MODIFY
                | WatchMask::DELETE
                | WatchMask::MOVED_TO
                | WatchMask::MOVED_FROM,
        )
        .context("failed to add inotify watch")?;

    let mut buffer = [0; 1024];
    loop {
        let events = inotify
            .read_events_blocking(&mut buffer)
            .context("failed to read inotify events")?;

        // Consume all events, then check state once
        let mut relevant = false;
        for event in events {
            if let Some(name) = event.name {
                if name.to_string_lossy() == FILE_NAME {
                    relevant = true;
                }
            }
        }

        if !relevant {
            continue;
        }

        let state = read_clock_state(&dir, max_len);
        let json = serde_json::to_string(&state)?;
        if json != last_output {
            emit(&state)?;
            last_output = json;
        }
    }
}
