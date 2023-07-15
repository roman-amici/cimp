use parser::Parser;
use scanner::scan;

mod ast;
mod parser;
mod scanner;
mod token;

fn main() {
    let tokens = scan("x * y + z", "files");

    let mut parser = Parser::new(tokens);

    let tree = parser.parse_expression().unwrap();
}
