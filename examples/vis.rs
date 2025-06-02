#![allow(clippy::disallowed_names)]

use getter_methods::Getters;

#[derive(Debug, Getters)]
#[getters(vis = pub(crate))]
pub(crate) struct Foo {
  bar: i64,
}

fn main() {
  let foo = Foo { bar: 17 };
  assert_eq!(foo.bar(), 17);
}
