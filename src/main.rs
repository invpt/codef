mod char_reader;
mod errors;
mod parser;
mod strings;
mod tokenizer;
mod reifier;
mod lowerer;
mod optimizers;
mod backends;

// Current plan: Parser (done) -> Reifier+Typeck (done) -> TAC+CFG+SSA (done) -> opts (none so far) -> RISC-V (in progress - regalloc)

fn main() {
    let path = std::env::args().nth(1).unwrap();
    let strings = strings::Strings::new();
    let toks = tokenizer::Tokens::of(
        char_reader::IoCharReader::<256, _>::new(std::fs::File::open(path).unwrap()),
        &strings,
    );
    let errs = errors::ErrorStream::new();
    let (strings, tree) = parser::parse(toks, &errs).unwrap();
    //println!("{:#?}", tree);
    let reified = reifier::reify(strings, &tree).unwrap();
    //println!("\n\n\n\nREIFIED:\n{reified:#?}");
    let cfg = lowerer::lower(&reified);
    dbg!(cfg);
}
