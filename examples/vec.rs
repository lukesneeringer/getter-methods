#![allow(clippy::disallowed_names)]

use getter_methods::Getters;

#[derive(Debug, Getters)]
struct Foo {
  bar: Vec<i64>,
}

fn main() {
  let foo = Foo { bar: vec![17, 42] };
  assert_eq!(foo.bar(), &[17, 42]);
}
