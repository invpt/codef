mod rir;

pub use rir::*;
use crate::parser;

/*
    Goals:
    - Lower assignments that use destructuring 
    - Lower identifiers to indices
    - Reslove EVERYTHING to where it actually goes to
        - e.g. list.push(2), where is list stored, how do we get push?
        - Maybe we even go ahead and inline push? How's it represented?
    - Figure out how func. arg. destructuring shall work
        - We just need a good, deterministic algorithm for figuring out
          how arguments will be represented in registers, I guess. Maybe
          this should be delayed until platform-specific code kicks in
          and a most-optimal choice can be made. Actually, the most optimal
          choice really depends on how the function's body consumes it.
          But that means that a function type doesn't specify its calling
          convention, which is a big no-no. For now, I should probably
          stick to splitting up just one hierarchy level of tuples. But what
          if a function really wants to take in a tuple to pass it some-
          where? This breaks the principle of being able to create anonymous
          structs penalty-free. So, let's say we want functions to be able
          to specifiy how they want their arguments to be. How could that
          be possible? Well, we could do what every C-based language does,
          and make function arguments into a special case. This technically
          restricts you to a sort of flat hierarchy, but it gives you real
          choice.
    - Typeck / removal of type annotations at some point
*/

pub fn lower(expr: parser::Expr) -> Expr {
    todo!()
}
