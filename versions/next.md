# Next version

## Features

- Added more context in error messages: auxiliary source code locations
  are now highlighted, for example for name conflicts the previous definition
  is shown, for argument type mismatch - the parameter definition, etc.
- Added call stack traces for runtime errors: when an error occurs, the
  function call stack that led to it is dumped to console.
- Added division and modulo - `/` and `%` operators for type `int`.
  Division by zero causes a runtime error.
- Added `return` statement that allows to return a value from a function
  early. For now, `return` statement is not treated specially by the type
  checker, so it cannot be the last statement in a non-void function.
  - `return` statement can appear without a value in functions with no
    return type (a.k.a. returning `void`).