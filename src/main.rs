use scanner::scan;

mod scanner;
mod token;

fn main() {
    let tokens = scan(
        "x==3 + fn else topoi // skljdfklsdaf sklfsjkdfk\n != [] sdlkdfs1kjsdflk 0.123 1234",
        "files",
    );

    for token in tokens {
        println!(
            "{:?} - {:?}({})",
            token.token_type, token.literal, token.line
        )
    }
}
