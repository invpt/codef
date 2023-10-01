//use crate::parser::utils::ast_size;

mod char_reader;
mod errors;
mod parser;
mod string_storage;
mod tokenizer;
mod reifier;

// Current plan: Parser (done) -> Reifier (in progress) -> Typeck -> CFG -> ??
// Could typeck be done before parser?
// Well, there is some reification that needs to be done with variants.
// I think typeck isn't so much a different pass as it is a function that modifies the RST a bit

fn main() {
    let path = std::env::args().nth(1).unwrap();
    let storage = string_storage::StringStorage::new();
    let toks = tokenizer::Tokens::of(
        char_reader::IoCharReader::<256, _>::new(std::fs::File::open(path).unwrap()),
        &storage,
    );
    let errs = errors::ErrorStream::new();
    let tree = parser::parse(toks, &errs).unwrap();
    println!("{:#?}", tree);
    let reified = reifier::reify(&storage, &tree).unwrap();
    println!("\n\n\n\nREIFIED:\n{:#?}", reified);
    //println!(
     //   "AST size: {}KiB (Expr {} bytes)",
     //   ast_size(&tree) / 1024,
     //   std::mem::size_of::<parser::Expr>()
   // )
}
