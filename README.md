# colang - the CO language compiler

CO is a language designed for use in programming olympiads and contests
like IOI and ACM ICPC. It features a simple syntax, opinionated
algorithm-oriented standard library, C++ tier performance,
and many small bits that make solving tasks easier and more fun.

colang can either run code directly (in the so-called _interpreter mode_)
or compile it into C source code that can be submitted to every judge system
that accepts C or C++.

## Status

colang can already do most of the basic imperative language-like stuff
(except `for` loops, you have to use `while` for now), but the syntax
is a bit clunky in some places and the built-in primitive types are
limited (no short and wide `intXX`, no `float` or `double`). There is
also no standard library yet: some more advanced language features
that are needed as a basis for a really good standard library are not
yet there (traits and function templates).

There is no language documentation yet, the easiest way to get a rough
idea about what CO looks like is to through the test samples under
`colang-cli/tests/samples/features`.

## Building

If you still want to try it out right now, grab the source, make sure
you have [Rust](https://www.rust-lang.org/) installed, and run
`cargo build` in the root directory. This should produce a `colang-cli`
binary under `target/debug` that can be used to run the compiler.

## Compiling and running programs

Once you have your `colang-cli` built, you can use it to both run the
programs directly (interpreter mode) and compile them into C.

As an example, take the following program that computes the sum of two
numbers (not very exciting, yes):

```
fun main() {
    var x: int, y: int;
    read x, y;
    writeln x + y;
}
```

Save it as `sum.co`, and try running `/path/to/your/colang-cli run sum.co`.
Give it two numbers on standard input, and you should see their sum!

Now try running `/path/to/your/colang-cli compile sum.co`. You should see
a file named `sum.c` created next to your `sum.co`. You can try compiling
this file with `gcc` or `clang`, running the binary, and see that it works
the same as in interpreter mode!