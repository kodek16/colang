# Next version

## Features

- Added a new C/C++ target backend for the compiler. Running `colang` with
  `--compile` flag will produce a `.cpp` file for the program that can be
  then compiled by a C++ compiler and executed. C/C++ target allows to
  achieve native performance with CO programs while keeping the same
  semantics as the interpreter _for safe code_.
- Added `readln` form of the `read` statement. `readln` can only be used
  with `string` targets, and it consumes a whole line from the standard
  input, unlike `read` that only consumes a single word. Newline character
  is not appended to the read line. Mixing `read` and `readln` may cause
  unexpected behavior, as a call to `read` marks the current input line as
  "not clean", and a subsequent call to `readln` drop the rest of the words
  and return the next "fully clean" line.