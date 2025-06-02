#![allow(clippy::disallowed_names)]

use std::rc::Rc;

use getter_methods::Getters;

#[derive(Debug, Getters)]
struct Foo {
  bar: Rc<i64>,
}

fn main() {
  let foo = Foo { bar: Rc::new(42) };
  let rc2 = foo.bar();
  assert_eq!(foo.bar(), rc2);
}
