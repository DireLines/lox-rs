use std::env::args;
use std::error::Error;
use std::io::{BufRead, Write};
use treewalk::run;
type Result<T> = std::result::Result<T, Box<dyn Error>>;

fn main() -> Result<()> {
    let args: Vec<String> = args().collect();
    match &args[1..] {
        &[] => run_prompt()?,
        [file] => run_file(file)?,
        _ => {
            println!("Usage: lox [script]");
            return Ok(());
        }
    }
    Ok(())
}

fn run_file(path: &str) -> Result<()> {
    //get all bytes of file
    let s = std::fs::read_to_string(path)?;
    //run on the string
    run(&s);
    Ok(())
}

fn run_prompt() -> Result<()> {
    let mut stdin = std::io::stdin().lock().lines();
    loop {
        print!("> ");
        std::io::stdout().flush()?;
        let Some(line) = stdin.next() else {
            return Ok(());
        };
        let line = line?;
        if line == "exit" {
            break;
        }
        run(&line);
    }
    Ok(())
}
