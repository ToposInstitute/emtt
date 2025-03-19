mod batch;
mod core;
mod cron;
mod ctx;
mod elab;
mod env;
mod eval;
mod pprint;
mod symtable;
mod toplevel;

use clap::Parser;
use std::{fs, time::SystemTime};

#[derive(Parser)]
struct Args {
    #[arg(short, long)]
    file: Option<String>,

    #[arg(short, long)]
    bench: bool,

    #[arg(short, long)]
    repl: bool,
}

fn main() {
    let args = Args::parse();

    let start_time = SystemTime::now();

    match &args.file {
        Some(filename) => {
            let input = fs::read_to_string(filename).unwrap();
            batch::check(&input);
        }
        None => {
            if args.repl {
                panic!("repl not implemented yet")
            }
        }
    }

    if args.bench {
        println!(
            "finished in {}ms",
            start_time.elapsed().unwrap().as_millis()
        )
    }
}
