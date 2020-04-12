# Next version

## Features

- Users can now define their own type templates: types parametrized
  by other types.
- A template type definition looks like a usual struct definition,
  but type parameters are included in angle brackets:
  `struct Foo<T> { ... }` defines a type template named `Foo` that
  has one type parameter of type `T`.
- Type templates can have fields of types that depend on type
  parameters: `Foo` can have a field of type `[T]` or `&T`, or
  some other type template.
- Type templates can use type parameters in their method signatures
  and bodies.
- Type templates can be _instantiated_ by providing concrete type
  arguments in place of type parameters: `Foo<int>` is a concrete
  type: an instantiation of type template `Foo` with `T` substituted
  for `int`.
- Unlike C++, type templates are fully checked. What this means is
  that instantiating a type template cannot produce any errors 
  coming from the type template itself. As long as type arguments
  conform to type parameters, it is guaranteed that the instantiation
  will succeed.
