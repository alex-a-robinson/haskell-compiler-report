# COMS22201 LE Compiler Assignment

[Instructions](https://www.cs.bris.ac.uk/Teaching/Resources/COMS22201)

## Overview

0. Cata/Fix
    0.1 Explain what the hell it is
1. Give an overview of parsing options
    1.1 Parser and lexer
    1.2 Combinators vs other parser methods
2. Implement monadic parser from bottom up -> show that parsec is developed version of this
3. Show how BNF translates to parser Combinators
    3.1. Explain BNF
    3.2. Simple example with date language converting to combinators
    3.3.
4. Write a simple compiler for a language
    4.1. Use parsec for parsing
    4.2. use LLVM for compilation to low code


## Resources

### Haskell compilers

**Compilers:**

- [Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours)
- [How to build a monadic interpreter in one day](https://wiki.haskell.org/wikiupload/c/c6/ICMI45-paper-en.pdf)
- [Writing an Interpreter with BNFC and Haskell](http://gregorulm.com/writing-an-interpreter-with-bnfc-and-haskell/)
- [A Tiny Compiler For A Typed Higher Order Language](http://jozefg.bitbucket.org/posts/2015-03-24-pcf.html)
- [LET'S BUILD A COMPILER (IN HASKELL)](http://alephnullplex.github.io/cradle/)
- [DIY: Make Your Own Programming Language](http://blog.ppelgren.se/2015-01-03/DIY-Make-Your-Own-Programming-language/)
- [Simple Monadic Parser in Haskell](http://michal.muskala.eu/2015/09/23/simple-monadic-parser-in-haskell.html)
- [Monadic Parsing: A Case Study](http://berniepope.id.au/docs/monad_parse.pdf)
- [language-lua - Lua 5.3 lexer, parser and pretty-printer](https://github.com/osa1/language-lua)

**Parsec:**

- [An introduction to parsing text in Haskell with Parsec](http://unbui.lt/#!/post/haskell-parsec-basics)
- [An Introduction to the Parsec Library](https://kunigami.blog/2014/01/21/an-introduction-to-the-parsec-library/)
- [Real world Haskell: Chapter 16\. Using Parsec](http://book.realworldhaskell.org/read/using-parsec.html)
- [Is there any trick about translating BNF to Parsec program?](http://stackoverflow.com/questions/28828586/is-there-any-trick-about-translating-bnf-to-parsec-program)
- [Parsing a simple imperative language](https://wiki.haskell.org/Parsing_a_simple_imperative_language)

**Parser Combinators:**

- [The fundamental limitations of parser combinators... and how to fix them.](http://lambda-the-ultimate.org/node/4160)
- [Monadic Parsing in Haskell](http://www.cs.uwyo.edu/~jlc/courses/3015/parser_pearl.pdf)
- **Good one to start with** [Monadic Parsers: Implementing a micro Parsec](http://olenhad.me/articles/monadic-parsers/)

### Misc

- [Haskell blog](http://www.well-typed.com/blog/)
- Could talk about non-determanistic parsing
