# getters

This is `getters`, a derive macro that will generate an impl with accessor methods for each field
on the struct.

Using `getters` is straightforward: simply derive it:

```rs
use getters::Getters;

#[derive(Getters)]
struct Foo {
  bar: String,
  baz: i64,
}

let foo = Foo { bar: "bacon".into(), baz: 42 };
assert_eq!(foo.bar(), "bacon");
assert_eq!(foo.baz(), 42);
```

For more, see [the documentation](https://docs.rs/getters).
