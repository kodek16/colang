# Next version

## Features

- User can define their own `struct` types. Structs are pass-by-value
  product types: they are made up of several fields of different types,
  and every field is copied when a struct is copied.
- Struct fields can be defined using the following syntax:
  ```
  struct Point {
    x: int;
    y: int;
  }
  ```
- Struct fields can be accessed using dot-notation: if `p` is a `Point`,
  `p.x` and `p.y` are its respective fields.