use std::env;
#[cfg(not(debug_assertions))]
use std::panic;

fn main() {
    #[cfg(not(debug_assertions))]
    {
        let prefix = if let Some(name) = env::args_os().next() {
            format!("{}: ", name.to_string_lossy())
        } else {
            String::new()
        };
        panic::set_hook(Box::new(move |info| {
            if let Some(s) = info.payload().downcast_ref::<&str>() {
                eprintln!("{prefix}{s}");
            } else if let Some(string) = info.payload().downcast_ref::<String>() {
                eprintln!("{prefix}{string}");
            } else {
                eprintln!("{prefix}unknown error occured");
            }
        }));
    }
    match env::args_os().nth(1) {
        Some(s) if s == "run" => qol::run("main.qol"),
        _ => qol::compile("main.qol"),
    }
}
