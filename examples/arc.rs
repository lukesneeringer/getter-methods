#![allow(clippy::disallowed_names)]

use std::sync::Arc;

use getter_methods::Getters;

#[derive(Debug, Getters)]
struct Foo {
  bar: Arc<i64>,
}

fn main() {
  let foo = Foo { bar: Arc::new(42) };
  let rc2 = foo.bar();
  assert_eq!(foo.bar(), rc2);
}
