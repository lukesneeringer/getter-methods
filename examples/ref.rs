#![allow(clippy::disallowed_names)]

use getter_methods::Getters;

#[derive(Debug, Getters)]
struct Foo<'a> {
  ref_: &'a str,
}

fn main() {
  let foo = Foo { ref_: QUICK };
  assert_eq!(foo.ref_(), QUICK);
}

static QUICK: &str = "The quick brown fox jumped over the lazy dogs.";
