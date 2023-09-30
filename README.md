# codef

A programming language with predictable runtime code generation.

`codef` is a *[multi-stage programming language](https://en.wikipedia.org/wiki/Multi-stage_programming)*. Languages like this allow programmers to write code that can be specialized at runtime. In `codef`'s case, the intermediate representation of all code that must be runtime-specialized is bundled with compiled programs, and part of the compiler is also bundled to compile this IR when it is ready to be specialized. A straightforward use-case of a multi-stage programming language is to turn a simple interpreter into a simple compiler:

```scala
// The dollar sign means "do the specialization with these arguments when the function is called"
def compile(val expr :: Expr) $=> (val arg :: Int) => eval(expr, arg);

def main() {
    val src = readline();
    val expr = parse(src);
    val calc = compile(expr);

    // Now we can evaluate this function a bunch without paying the interpretation
    // penalty every time!
    val total = 0;
    for var i = 0; i < 1_000_000_000; set i = i + 1 {
        set total = total + calc(i);
    }

    print("Total from zero to a billion: " + total);
}

// Simple AST, parser, and tree-walking interpreter for single-arg functions.
type Expr /* ... */;
def parse(val src :: String) -> Expr { /* ... */ }
def eval(val expr :: Expr, val arg :: Int) -> Int {
    case expr = Expr\Add(val a, val b) {
        eval(a) + eval(b)
    } else expr = Expr\Mul(val a, val b) {
        eval(a) * eval(b)
    } else expr = Expr\Neg(val a) {
        -eval(a)
    }  else expr = Expr\Const(val i) {
        i
    } else expr = Expr\Arg {
        arg
    }
}
```
