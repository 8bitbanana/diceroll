pub mod roll;
use std::io::{self, Write};

#[allow(unused)]
fn read_loop() {
    loop {
        print!("Make a roll > ");
        let mut input = String::new();
        io::stdout().flush().unwrap();
        io::stdin().read_line(&mut input).unwrap();
        input = input.trim_end().to_owned();

        let result = roll::run_stubbed(&input);

        match result {
            Ok(x) => println!("{}", x),
            Err(e) => println!("{}", e)
        }
    }
}

fn main() {
    read_loop();
}