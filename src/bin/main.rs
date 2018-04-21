extern crate magician;
use magician::magicparser::{parse_css, parse_html};
use std::env;
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let args: Vec<String> = env::args().collect();
    let flag = &args[1];
    if flag != "--css" && flag != "--html" {
        println!("Usage: magician --html|--css <file>");
        std::process::exit(1);
    }
    let filename = &args[2];
    let mut f = File::open(filename).expect(&format!("file {:?} not found", filename));
    let mut input = String::new();
    f.read_to_string(&mut input)
        .expect(&format!("error reading file {:?}", filename));
    match flag.as_ref() {
        "--html" => {
            let html = parse_html(&input);
            println!("{:#?}", html);
        }
        "--css" => {
            let css = parse_css(&input);
            println!("{:#?}", css);
        }
        _ => unreachable!(),
    }
}
