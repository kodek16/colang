# Next version

## Features

- Types now may have methods. A method is a function defined for a
  specific type that accepts an instance of this type as the first
  parameter and requires a special call syntax.
  - Methods can be called like this: `x.foo(5)`, where `foo` is a
    method defined for type `T` of x with signature `T, int -> ...`.
  - First parameter for all methods is called `self`, and its type
    is either `T`, or `&T`.
- Added internal method `abs()` for type `int`.