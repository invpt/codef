# Radi

This repository holds an experimental programming language. At the moment, all that is here is
an incomplete tokenizer and parser.

Next goals:
- Complete the tokenizer and parser
- Implement compilation to WebAssembly
- ...
- Implement compilation to native code - perhaps via LLVM
- ...

## Syntax inspiration

Radi's syntax is insipred by a number of languages, the most apparent of which being Scala.
Personally, I have never used Scala, but it turns out that I quite like its syntax.

## Wild ideas

I've got a wild idea to build a language that lets you write functions and then tell the language
to specialize them with a specific set of the function's arguments specified. Theoretically, this
specialization could happen at both runtime and compile-time. When a function is specialized, it
would be recompiled but taking into account the fact that the arguments have been made constant
(and what constant values they have been given).
Said in a few words, this is "partial function application as JIT compilation."

In the limit, this feature could make it very easy to make a performant implementation of another
language: just write an interpreter for it and then specialize the function with the code specified
but not the external state. As described, this exact mark is probably impossible to achieve.
However, it's still a very interesting idea, and so I think it's worth exploring.
