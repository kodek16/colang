# Next version

## Features

- Added `new` expression: `new T` allocates a value of type `T` on the
  heap (so it effectively lives forever), and returns a pointer to it.
- Added the notion of the standard library. CO standard library is
  defined in a single `std.co` file that is shipped with the compiler
  and automatically compiled alongside with the user program.
- Removed the internal `int::abs` method, and instead added an equivalent
  function to the standard library: `abs(int) -> int`.