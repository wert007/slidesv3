#![feature(round_char_boundary)]

use std::path::PathBuf;
use std::sync::atomic::{self, AtomicUsize};

use clap::Parser;
use colorized::*;
use futures::{stream, StreamExt};
use tokio::process::Command;
use tokio::{fs, join};

#[derive(Debug, Parser)]
struct Args {
    #[arg(short, long)]
    record: bool,
    #[arg(short, long)]
    filter: Option<String>,
    #[arg(short, long)]
    directory: String,
}

#[tokio::main]
async fn main() {
    let args = Args::parse();
    let total_files = glob::glob(&format!("{}/**/*.sld", args.directory))
        .unwrap()
        .count();
    let input_files = glob::glob(&format!("{}/**/*.sld", args.directory)).unwrap();
    let files_done = AtomicUsize::new(0);
    stream::iter(input_files)
        .for_each_concurrent(100, |file| async {
            let file = file.unwrap();
            if let Some(filter) = &args.filter {
                if !file.to_string_lossy().contains(filter) {
                    return;
                }
            }
            let output = Command::new("cargo")
                .arg("run")
                .arg("--quiet")
                .arg("--package")
                .arg("reaktor")
                .arg(&file)
                .output()
                .await
                .unwrap();
            let _errors_occured = if args.record {
                let (t1, t2) = join!(
                    fs::write(file.with_extension("out"), output.stdout),
                    fs::write(file.with_extension("err"), output.stderr),
                );
                t1.unwrap();
                t2.unwrap();
                false
            } else {
                let (t1, t2) = join!(
                    ensure_equal(file.with_extension("out"), output.stdout,),
                    ensure_equal(file.with_extension("err"), output.stderr,)
                );
                t1 || t2
            };
            files_done.fetch_add(1, atomic::Ordering::Relaxed);
            println!(
                "Completed {:3}/{total_files}: {}",
                files_done.load(atomic::Ordering::Relaxed),
                file.file_name().unwrap().to_string_lossy()
            );
        })
        .await;
}

enum Error {
    FileNotFound(PathBuf, std::io::Error),
    FileLengthDifferent(Vec<u8>, Vec<u8>),
    FileByteDifferentAt(usize, Vec<u8>, Vec<u8>),
}

/// Returns true, if an error occured.
async fn ensure_equal(file: PathBuf, output: Vec<u8>) -> bool {
    let file = match fs::read(&file).await {
        Ok(it) => it,
        Err(err) => {
            report_error(Error::FileNotFound(file, err));
            return true;
        }
    };
    if file.len() != output.len() {
        report_error(Error::FileLengthDifferent(file, output));
        return true;
    }
    for (index, (f, o)) in file.iter().copied().zip(output.iter().copied()).enumerate() {
        if f != o {
            report_error(Error::FileByteDifferentAt(index, file, output));
            return true;
        }
    }
    false
}

fn report_error(err: Error) {
    colorize_print("ERROR: ", Colors::RedFg);
    match err {
        Error::FileNotFound(file, err) => {
            println!(
                "Failed to find file '{}', original error was:",
                colorize_this(file.to_string_lossy(), Colors::CyanFg)
            );
            println!("{err}");
        }
        Error::FileLengthDifferent(file, output) => {
            let file = String::from_utf8_lossy(&file);
            let output = String::from_utf8_lossy(&output);
            println!(
                "Different lengths. File was {} bytes long and the output {} bytes.",
                file.len(),
                output.len()
            );
            println!("File read  '{file}'");
            println!("Output was '{output}'");
        }
        Error::FileByteDifferentAt(index, file, output) => {
            println!("Different byte at {index}.");
            let file = String::from_utf8_lossy(&file);
            let output = String::from_utf8_lossy(&output);
            print!("File read  ");
            print_difference_highlighted(&file, index, Colors::BrightCyanFg);
            println!();
            print!("Output was ");
            print_difference_highlighted(&output, index, Colors::BrightCyanFg);
            println!();
        }
    }
}

fn print_difference_highlighted(text: &str, highlighted: usize, highlight_color: Colors) {
    print!("{}", &text[..highlighted]);
    let end = text.ceil_char_boundary(highlighted + 1);
    colorize_print(&text[highlighted..end], highlight_color);
    print!("{}", &text[end..]);
}
