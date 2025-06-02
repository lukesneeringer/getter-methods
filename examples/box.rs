#![allow(clippy::disallowed_names)]

use getter_methods::Getters;

#[derive(Debug, Getters)]
struct Foo {
  bar: Box<i64>,
}

fn main() {
  let foo = Foo { bar: Box::new(42) };
  assert_eq!(foo.bar(), &42);
}
