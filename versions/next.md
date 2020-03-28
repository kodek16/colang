# Next version

## Features

- Added loop support with `while` statement.
- Added assignment statement: `a = b`. Note that assignment is
  not an expression.
- Added basic function support, only without parameters for now.
  - The definition syntax is `fun foo(): int { ... }`
  - If the function returns a value, its body must be an expression.
  - The syntax for calling a function is `foo()`.
  - Recursive calls are allowed and supported.
- The program code now must be defined in a `main` function.
