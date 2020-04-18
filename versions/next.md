# Next version

## Features

- Functions and types can be referred to from any point in the program,
  even if their definition was not yet encountered.
- As a consequence, indirect recursion is now possible: function
  `A` may call function `B` which in turn calls function `A`. The
  same is possible for types (through fields).
- Added logical operators: binary `&&` and `||`, and unary `!`.
  They behave as usual, short-circuiting when possible.
- Added `is` expression that compares pointers: `x is y`, where `x` and `y`
  are pointers to the same type, is true iff `x` and `y` point to the same
  location in memory.
  
## Bugfixes

- `new T` in template methods now gets instantiated correctly.
