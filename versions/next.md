# Next version

## Features

- Added line comments beginning with `//`.
- Added arrays.
  - Arrays are allocated on the heap, and in general are similar
    to Java arrays.
  - Arrays are pass-by-reference, like in Java and Python,
    that is, after doing `var v1 = [2]; var v2 = v1`, `v1` and `v2`
    share data, and modifications to one of them will be reflected
    in the other.
  - Array type names look like this: `[int]` is an array of `int`.
  - Arrays are indexed from zero, indexing syntax is the usual `v[i]`.
  - Out-of-bounds checks are optional for backends to support, the
    intention is for interpreter to support them, and C compiler
    to not support them.
- Added special syntax for initializing arbitrary-size arrays
  filled with some value: `[true; 64]` means "an array of 64
  elements equal to `true`"
- Lvalue/rvalue split is introduced. Every expression has a value
  category, that is, one of "lvalue" and "rvalue". Lvalues are
  assignable, while rvalues are not.
  - Currently all expressions are rvalue, except direct variable
    reference, and indexing of lvalue arrays.
  - Assignment and `read` statements now accept any lvalue.

## Bugfixes

- Scoping and deallocation in blocks now works correctly even if
  the final expression in block uses block local variables.