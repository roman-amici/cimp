use parser::Parser;
use resolver::Resolver;
use scanner::scan;
use tree_walker::TreeWalker;
use type_checker::TypeChecker;

mod ast;
mod parser;
mod resolver;
mod scanner;
mod token;
mod tree_walker;
mod type_checker;

fn main() {
    let tokens = scan("{ let x = 3; x; }", "files");

    let mut parser = Parser::new(tokens);

    let mut tree = parser.parse_statement().unwrap();
    let mut resolver = Resolver::new();

    resolver.resolve_statement(&mut tree).unwrap();
    let mut type_checker = TypeChecker::new();

    type_checker.check_statement(&mut tree).unwrap();

    let mut tree_walker = TreeWalker::new();
    tree_walker.eval_statement(&mut tree);
}
