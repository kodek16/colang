# Next version

## Features

- Added an internal `assert` function, which accepts a single
  `bool` parameter, and exits the program with an error if it is
  false.
- Added limited type inference. The expression `[]` is in general
  ambiguous: the compiler cannot infer its type without any context.
  With the new logic, compiler can now infer the array type in
  some contexts, for example:
  - Variable initializer: `var x: [int] = []` is not ambiguous.
  - Function argument: `f([])` is not ambiguous, type is inferred
    from signature.
  - Function return value: `[]` as the final expression in a function
    is not ambiguous.
