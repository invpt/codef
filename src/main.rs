//use crate::parser::utils::ast_size;

mod char_reader;
mod errors;
mod parser;
mod string_storage;
mod tokenizer;
mod reifier;
mod typechecker;

// Current plan: Parser (done) -> Reifier (done?) -> Typeck (in progress) -> CFG -> ??
// Could typeck be done before parser?
// Well, there is some reification that needs to be done with variants.
// I think typeck isn't so much a different pass as it is a function that modifies the RST a bit

fn main() {
    let path = std::env::args().nth(1).unwrap();
    let storage = string_storage::StringStorage::new();
    let mut interner = string_storage::StringInterner::new(&storage);
    let toks = tokenizer::Tokens::of(
        char_reader::IoCharReader::<256, _>::new(std::fs::File::open(path).unwrap()),
        &mut interner,
    );
    let errs = errors::ErrorStream::new();
    let (interner, tree) = parser::parse(toks, &errs).unwrap();
    println!("{:#?}", tree);
    let mut reified = reifier::reify(interner, &tree).unwrap();
    println!("\n\n\n\nREIFIED:\n{reified:#?}");
    typechecker::typecheck(&mut reified).unwrap();
    println!("\n\n\n\nTYPECHECKING SUCCESS:\n{reified:#?}");
    //println!(
     //   "AST size: {}KiB (Expr {} bytes)",
     //   ast_size(&tree) / 1024,
     //   std::mem::size_of::<parser::Expr>()
   // )
}
