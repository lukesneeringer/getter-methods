#![allow(clippy::disallowed_names)]

use getter_methods::Getters;

#[derive(Debug, Getters)]
struct Foo {
  bar: String,
}

fn main() {
  let foo = Foo { bar: "The quick brown fox jumped over the lazy dogs.".to_string() };
  assert_eq!(foo.bar(), "The quick brown fox jumped over the lazy dogs.");
}
