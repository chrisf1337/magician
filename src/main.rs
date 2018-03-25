extern crate httpparser;

use httpparser::lexer;

fn main() {
    lexer::lex(&String::from("input"));
}
