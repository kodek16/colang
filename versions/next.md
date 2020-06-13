# Next version

## Features

- Add trait support. Traits define a common interface for a set of types and can
  be used as _bounds_ for type parameters in templates. For example:
  ```
  // A trait for types that can be converted into `int`.
  trait IntoInt {
    fun into(self): int
  }
  
  // `Foo` claims to implement `IntoInt`, so it has to provide implementations
  // for all trait methods.
  struct Foo : IntoInt {
    x: int;
  
    // An implementation for `into` is provided.
    fun into(self): int {
      self.x
    }
  }
  
  // `Printer` can be instantiated for any type argument that implements
  // `IntoInt`.
  struct Printer<T: IntoInt> {
    fun print(self, t: T) {
      // Since we know that `T` implements `IntoInt`, we can call `into`.
      writeln t.into();
    }
  }
  
  fun main {
    var p: Printer<Foo>;
    var f: Foo;
    f.x = 42;
    p.print(f);  // will print "42"
  }
  ```
  
## Breaking changes

- Semicolons are no longer allowed after field definitions.

## Other changes

- Remove `void` type. What was previously called "void-expressions" are now usual
  statements. All expressions now evaluate to a non-void value. Functions can be
  now said to be either "void" or "non-void" depending on whether they return a
  value (have a return type) or not.
