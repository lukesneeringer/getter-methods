#![allow(clippy::disallowed_names)]

use getter_methods::Getters;

trait FooGetters {
  fn bar(&self) -> &str;
  fn baz(&self) -> i64;
  fn bacon(&self) -> &[u8];
}

#[derive(Debug, Getters)]
#[getters(trait_impl = FooGetters)]
struct Foo {
  bar: String,
  baz: i64,
  bacon: Vec<u8>,
}

trait SpamGetters<T> {
  fn eggs(&self) -> Option<&T>;
}

#[derive(Debug, Getters)]
#[getters(trait_impl = SpamGetters<T>)]
struct Spam<T> {
  eggs: Option<T>,
}

fn main() {
  let foo = Foo { bar: "The quick brown fox".to_string(), baz: 42, bacon: vec![1, 2, 3] };
  assert_eq!(foo.bar(), "The quick brown fox");
  assert_eq!(foo.baz(), 42);
  assert_eq!(foo.bacon(), &[1, 2, 3]);

  let spam = Spam { eggs: Some(17i64) };
  assert_eq!(spam.eggs(), Some(&17));
}
