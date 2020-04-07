# Next version

## Features

- Added `new` expression: `new T` allocates a value of type `T` on the
  heap (so it effectively lives forever), and returns a pointer to it.
- Added the notion of a standard library. CO standard library is
  defined in a single `std.co` file that is shipped with the compiler
  and automatically compiled alongside the user program.
- Removed the internal `int::abs` method, and instead added an equivalent
  function to the standard library: `abs(int) -> int`.
- Added `char` type, which represents a single byte that is normally
  treated as an ASCII character.
- Added `char` literals: `'a'`, `'0'`, and `'\n'` are all valid
  literals of type `char`. Literals (after unescaping) must contain
  exactly one ASCII character.
- Added `string` type, which represents an array of `char`. There
  is a one-to-one mapping between `string` and `[char]`, but strings
  carry additional meaning of UTF-8 text that can be read and printed.
  That said, UTF-8-validness is not enforced, so strings can contain
  invalid sequences.
- Extended `read` statements to work with strings: `read s` consumes
  the next word (whitespace-delimited) from stdin and stores it
  literal in the `s` variable.