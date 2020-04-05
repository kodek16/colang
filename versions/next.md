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
- Structs can have methods defined alongside fields. Method syntax is
  mostly similar to usual function definitions, except that the first
  parameter must be either `self` or `&self`.
- Inside method bodies, `self` is a valid expression that refers to
  the method call receiver.
- For field access and method calls, if the receiver object is a
  pointer, it is now automatically dereferenced. Pointers have neither
  fields, nor methods, so this allows for a convenience syntax:
  `(*self).x` can now be written as `self.x`.