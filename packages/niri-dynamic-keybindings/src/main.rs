use anyhow::Result;
use niri_ipc::{Request, Response};
use niri_ipc::socket::Socket;

fn main() -> Result<()> {
    let mut socket = Socket::connect()?;
    let reply = socket.send(Request::EventStream)?;
    if matches!(reply, Ok(Response::Handled))  {
        let mut read_event = socket.read_events();
        while let Ok(event) = read_event() {
            println!("Recieved eveent: {event:?}");
        }
    }
    Ok(())
} 
