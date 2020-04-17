# Next version

## Features

- Functions and types can be referred to from any point in the program,
  even if their definition was not yet encountered.
- As a consequence, indirect recursion is now possible: function
  `A` may call function `B` which in turn calls function `A`. The
  same is possible for types (through fields).