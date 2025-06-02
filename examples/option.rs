#![allow(clippy::disallowed_names)]

use getter_methods::Getters;

#[derive(Debug, Getters)]
struct Foo<T> {
  bar: Option<T>,
}

fn main() {
  let foo = Foo { bar: Some(42i64) };
  assert_eq!(foo.bar(), Some(&42));
}
