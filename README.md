# getter-methods

[![ci](https://github.com/lukesneeringer/getter-methods/actions/workflows/ci.yaml/badge.svg)](https://github.com/lukesneeringer/getter-methods/actions/workflows/ci.yaml)
[![docs](https://img.shields.io/badge/docs-release-blue)](https://docs.rs/getter-methods/)
![license](https://img.shields.io/badge/license-MIT-blue)

This is `getter-methods`, a derive macro that will generate an impl with accessor methods for each
field on the struct.

Using `getter-methods` is straightforward: simply derive it:

```rs
use getter_methods::GetterMethods;

#[derive(GetterMethods)]
struct Foo {
  bar: String,
  baz: i64,
}

let foo = Foo { bar: "bacon".into(), baz: 42 };
assert_eq!(foo.bar(), "bacon");
assert_eq!(foo.baz(), 42);
```

For more, see [the documentation](https://docs.rs/getter-methods).
