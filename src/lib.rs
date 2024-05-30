//! `getter_methods` is a derive macro that will implement accessor methods for each field on the
//! struct.
//!
//! Using `getter_methods` is straightforward: simply derive it:
//!
//! ```
//! use getter_methods::GetterMethods;
//!
//! #[derive(GetterMethods)]
//! struct Foo {
//!   bar: String,
//!   baz: i64,
//! }
//!
//! # fn main() {
//! let foo = Foo { bar: "bacon".into(), baz: 42 };
//! assert_eq!(foo.bar(), "bacon");
//! assert_eq!(foo.baz(), 42);
//! # }
//! ```
//!
//! ## Return types
//!
//! Accessors will get a convenient return type depending on the type of the field on the struct:
//!
//! Struct Field                 | Accessor Return Type
//! ---------------------------- | --------------------
//! [`String`]                   | [`&str`][`str`]
//! Primitive `T` (e.g. [`i64`]) | `T`
//! Any other `T`                | `&T`
//!
//! ## Returning Copies
//!
//! If you want a non-primitive `T` that implements `Copy` to return itself rather than a
//! reference, annotate it with `#[getter_methods(copy)]`:
//!
//! ```
//! #[derive(GetterMethods)]
//! struct Foo {
//!   #[getter_methods(copy)]
//!   bar: Option<i64>,
//! }
//! ```
//!
//! ## Skipping fields
//!
//! If you don't want a certain field to have an accessor method, annotate it:
//!
//! ```compile_fail
//! use getter_methods::GetterMethods;
//!
//! #[derive(GetterMethods)]
//! struct Foo {
//!   bar: String,
//!   #[getter_methods(skip)]
//!   baz: i64,
//! }
//!
//! # fn main() {
//! let foo = Foo { bar: "bacon".into(), baz: 42 }
//! assert_eq!(foo.bar(), "bacon");
//! assert_eq!(foo.baz(), 42);  // Compile error: There is no `foo.baz()`.
//! # }
//! ```
//!
//! ## Documentation
//!
//! Any docstrings used on the fields are copied to the accessor method.

use proc_macro::TokenStream as TokenStream1;
use proc_macro2::TokenStream;
use quote::quote;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;

/// Derive accessor or "getter" methods for each field on the struct.
///
/// The output types are determined based on the input types, and follow the following rules:
///
/// 1. Primitives (e.g. [`i64`]) return a copy of themselves.
/// 2. [`String`] fields return [`&str`][`str`].
/// 3. Fields of any other type `T` return `&T`.
///
/// Note: You can use `#[getter_methods(copy)] to override rule 3 and make other types that
/// implement [`Copy`] also return copies; this can be done either on the struct or on individual
/// fields.
#[proc_macro_derive(GetterMethods, attributes(doc, getter_methods))]
pub fn derive_getter_methods(input: TokenStream1) -> TokenStream1 {
  getters(input.into()).unwrap_or_else(|e| e.into_compile_error()).into()
}

fn getters(input: TokenStream) -> syn::Result<TokenStream> {
  let mut getters: Vec<TokenStream> = vec![];

  // Parse the tokens as a struct.
  let struct_ = syn::parse2::<syn::ItemStruct>(input)?;

  // Look for attributes that may modify behavior.
  let copy_all = match struct_.attrs.iter().find(|a| a.path().is_ident("getter_methods")) {
    Some(attr) => {
      let args =
        attr.parse_args_with(Punctuated::<syn::Meta, syn::Token![,]>::parse_terminated)?;
      args[0].path().is_ident("copy")
    },
    None => false,
  };

  // Iterate over each field and create an accessor method.
  'field: for field in struct_.fields {
    // Sanity check: Do we need to do anything unusual?
    let mut copy = copy_all;
    if let Some(getters_attr) = field.attrs.iter().find(|a| a.path().is_ident("getter_methods")) {
      let nested =
        getters_attr.parse_args_with(Punctuated::<syn::Meta, syn::Token![,]>::parse_terminated)?;
      for m in nested {
        // Do we need to skip?
        if m.path().is_ident("skip") {
          continue 'field;
        }
        if m.path().is_ident("copy") {
          copy = true;
        }
      }
    }

    // Preserve documentation from the field to the method.
    let doc = {
      let mut answer = String::new();
      for doc_attr in field.attrs.iter().filter(|d| d.path().is_ident("doc")) {
        if let syn::Meta::NameValue(nv) = &doc_attr.meta {
          if let syn::Expr::Lit(l) = &nv.value {
            if let syn::Lit::Str(s) = &l.lit {
              if !answer.is_empty() {
                answer += "\n";
              }
              answer += s.value().as_str();
            }
          }
        }
      }
      answer
    };

    // Render the appropriate accessor method.
    let span = field.span();
    let field_ident =
      &field.ident.ok_or_else(|| syn::Error::new(span, "Fields must be named."))?;
    let field_type = &field.ty;
    let (return_type, getter_impl) = match field_type {
      syn::Type::Path(p) => {
        let ident = &p.path.segments.last().unwrap().ident;
        match ident.to_string().as_str() {
          "String" => (quote! { &str }, quote! { self.#field_ident.as_str() }),
          "i8" | "i16" | "i32" | "i64" | "i128" | "isize" | "u8" | "u16" | "u32" | "u64"
          | "u128" | "usize" | "f32" | "f64" | "char" =>
            (quote! { #field_type }, quote! { self.#field_ident }),
          _ => match copy {
            true => (quote! { #field_type }, quote! { self.#field_ident }),
            false => (quote! { &#field_type }, quote! { &self.#field_ident }),
          },
        }
      },
      _ => (quote! { &#field_type }, quote! { &self.#field_ident }),
    };
    let const_ = match return_type.to_string() == "& str" {
      true => quote! {},
      false => quote! { const },
    };
    getters.push(quote! {
      #[doc = #doc]
      #[inline]
      pub #const_ fn #field_ident(&self) -> #return_type {
        #getter_impl
      }
    });
  }

  // Write the final output with the accessor implementation.
  let ident = &struct_.ident;
  let generics = &struct_.generics;
  Ok(quote! {
    impl #generics #ident #generics {
      #(#getters)*
    }
  })
}
