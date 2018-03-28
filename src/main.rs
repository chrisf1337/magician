extern crate magicparser;

use magicparser::htmlparser::HtmlParser;
use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn main() {
    let test_dir = Path::new(&env::var("CARGO_MANIFEST_DIR").unwrap()).join("tests");
    let mut f = File::open(test_dir.join("simple.html")).expect("file not found");
    let mut input = String::new();
    f.read_to_string(&mut input).expect("read");
    let res = HtmlParser::parse(&input);
    println!("{:?}", res);
}
