//! `getter_methods` is a derive macro that will implement accessor methods for each field on the
//! struct.
//!
//! Using `getter_methods` is straightforward: simply derive it:
//!
//! ```
//! use getter_methods::Getters;
//!
//! #[derive(Getters)]
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
//! Primitive `T` (e.g. [`i64`]) | `T`
//! [`String`]                   | [`&str`][`str`]
//! [`Vec<T>`][Vec]              | `&[T]`
//! [`Box<T>`][Box]              | `&T`
//! [`Option<T>`][Option]        | `Option<&T>`
//! [`Rc<T>`][std::rc::Rc]       | `Rc<T>`
//! [`Arc<T>`][std::sync::Arc]   | `Arc<T>`
//! Any `&'a T`                  | `&'a T`
//! Any `*T`                     | `*T`
//! Any other `T`                | `&T`
//!
//! ## Returning Copies
//!
//! If you want a non-primitive `T` that implements `Copy` to return itself rather than a
//! reference, annotate it with `#[getters(copy)]`:
//!
//! ```
//! use getter_methods::Getters;
//!
//! #[derive(Getters)]
//! struct Foo {
//!   #[getters(copy)]
//!   bar: Option<i64>,
//! }
//! # fn main() {}
//! ```
//!
//! ## Skipping fields
//!
//! If you don't want a certain field to have an accessor method, annotate it:
//!
//! ```compile_fail
//! use getter_methods::Getters;
//!
//! #[derive(Getters)]
//! struct Foo {
//!   bar: String,
//!   #[getters(skip)]
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
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::ToTokens;
use quote::quote;
use syn::Visibility;
use syn::spanned::Spanned;

/// Derive accessor or "getter" methods for each field on the struct.
///
/// The output types are determined based on the input types, and follow the following rules:
///
/// 1. Primitives (e.g. [`i64`]) return a copy of themselves.
/// 2. [`String`] fields return [`&str`][`str`].
/// 3. [`Vec<T>`][Vec] fields return `&[T]`
/// 4. [`Box<T>`][Box] fields return `&T`.
/// 5. [`Option<T>`][Option] fields return `Option<&T>`.
/// 6. [`Rc<T>`][std::rc::Rc] and [`Arc<T>`][std::sync::Arc] return a clone of themselves.
/// 7. References and pointers return copies of themselves.
/// 8. Fields of any other type `T` return `&T`.
///
/// Note: You can use `#[getters(copy)] to override rule 6 and make other types that implement
/// [`Copy`] also return copies; this can be done either on the struct or on individual fields.
#[proc_macro_derive(Getters, attributes(getters))]
pub fn derive_getter_methods(input: TokenStream1) -> TokenStream1 {
  getters(input.into()).unwrap_or_else(|e| e.into_compile_error()).into()
}

/// Get the ident of a meta as a &str. Used for clean `match` statements.
macro_rules! ident_str {
  ($meta:ident ) => {
    $meta.path.get_ident().map(|i| i.to_string()).unwrap_or_default().as_str()
  };
}

/// Create a `syn` Error.
macro_rules! error {
 ($msg:literal $(,$e:expr)*) => {
  syn::Error::new(proc_macro2::Span::call_site(), format!($msg $(,$e)*))
 }
}

/// Parse out the generic of a generic type, e.g. `T` from `Vec<T>`.
macro_rules! generic {
  ($segment:ident) => {{
    let syn::PathArguments::AngleBracketed(angle_generic) = &$segment.arguments else {
      return Err(error!("Unparseable type: {}", $segment.to_token_stream().to_string()));
    };
    angle_generic.args.clone()
  }};
}

fn getters(input: TokenStream) -> syn::Result<TokenStream> {
  let mut getters: Vec<TokenStream> = vec![];

  // Parse the tokens as a struct.
  let struct_ = syn::parse2::<syn::ItemStruct>(input)?;

  // Look for attributes that may modify behavior.
  let struct_opts = {
    let mut struct_opts = StructOptions::default();
    if let Some(attr) = struct_.attrs.iter().find(|a| a.path().is_ident("getters")) {
      attr.parse_nested_meta(|meta| {
        match ident_str!(meta) {
          "copy" => struct_opts.copy = true,
          "vis" => struct_opts.vis = meta.value()?.parse::<Visibility>()?,
          other => Err(error!("Invalid option: {}", other))?,
        };
        Ok(())
      })?;
    };
    struct_opts
  };

  // Iterate over each field and create an accessor method.
  for field in struct_.fields {
    // Do we need to do anything unusual?
    let field_opts = {
      let mut field_opts = struct_opts.field_opts();
      if let Some(getters_attr) = field.attrs.iter().find(|a| a.path().is_ident("getters")) {
        getters_attr.parse_nested_meta(|meta| {
          match ident_str!(meta) {
            "copy" => field_opts.copy = true,
            "skip" => field_opts.skip = true,
            "vis" => field_opts.vis = meta.value()?.parse::<Visibility>()?,
            other => Err(error!("Invalid option: {}", other))?,
          }
          Ok(())
        })?;
      }
      if field_opts.skip {
        continue;
      }
      field_opts
    };

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
      match answer.is_empty() {
        true => quote! {},
        false => quote! { #[doc = #answer] },
      }
    };

    // Render the appropriate accessor method.
    let span = field.span();
    let field_ident =
      &field.ident.ok_or_else(|| syn::Error::new(span, "Fields must be named."))?;
    let field_type = &field.ty;
    let vis = &field_opts.vis;
    let mut const_ = quote! { const };
    let (return_type, getter_impl) = match field_type {
      syn::Type::Path(p) => {
        let Some(segment) = &p.path.segments.last() else { Err(error!("Unparseable type."))? };
        match segment.ident.to_string().as_str() {
          // 1. Primitives return copies of themselves.
          "i8" | "i16" | "i32" | "i64" | "i128" | "isize" | "u8" | "u16" | "u32" | "u64"
          | "u128" | "usize" | "f32" | "f64" | "char" =>
            (quote! { #field_type }, quote! { self.#field_ident }),
          // 2. `String` returns `&str`
          "String" => (quote! { &str }, quote! { self.#field_ident.as_str() }),
          // 3. `Vec<T>` returns `&[T]`.
          "Vec" => {
            let ty = generic!(segment);
            (quote! { &[#ty] }, quote! { self.#field_ident.as_slice() })
          },
          // 4. `Box<T>` returns `&T`.
          "Box" => {
            let ty = generic!(segment);
            (quote! { &#ty }, quote! { &self.#field_ident })
          },
          // 5. `Option<T>` returns `Option<&T>`.
          "Option" => {
            let ty = generic!(segment);
            (quote! { Option<&#ty> }, quote! { self.#field_ident.as_ref() })
          },
          // 6. `Rc<T>` and `Arc<T>` return clones.
          "Rc" => {
            let ty = generic!(segment);
            const_ = quote! {};
            (quote! { ::std::rc::Rc<#ty> }, quote! { ::std::rc::Rc::clone(&self.#field_ident) })
          },
          "Arc" => {
            let ty = generic!(segment);
            const_ = quote! {};
            (
              quote! { ::std::sync::Arc<#ty> },
              quote! { ::std::sync::Arc::clone(&self.#field_ident)},
            )
          },
          // 8. Fields of any other type `T` return `&T`.
          _ => match field_opts.copy {
            true => (quote! { #field_type }, quote! { self.#field_ident }),
            false => (quote! { &#field_type }, quote! { &self.#field_ident }),
          },
        }
      },
      // 7. References and pointers return copies of themselves.
      syn::Type::Reference(ref_) => (quote! { #ref_ }, quote! { self.#field_ident }),
      syn::Type::Ptr(ptr) => (quote! { #ptr }, quote! { self.#field_ident }),
      _ => (quote! { &#field_type }, quote! { &self.#field_ident }),
    };
    getters.push(quote! {
      #doc #[inline]
      #vis #const_ fn #field_ident(&self) -> #return_type {
        #getter_impl
      }
    });
  }

  // Write the final output with the accessor implementation.
  let ident = &struct_.ident;
  let (impl_generics, ty_generics, where_clause) = &struct_.generics.split_for_impl();
  Ok(quote! {
    #[automatically_derived]
    impl #impl_generics #ident #ty_generics #where_clause {
      #(#getters)*
    }
  })
}

/// The options for the full struct.
struct StructOptions {
  copy: bool,
  vis: Visibility,
}

impl StructOptions {
  /// Initialize a [`FieldOptions`] with inherited defaults.
  fn field_opts(&self) -> FieldOptions {
    FieldOptions { copy: self.copy, skip: false, vis: self.vis.clone() }
  }
}

impl Default for StructOptions {
  fn default() -> Self {
    Self { copy: false, vis: Visibility::Public(syn::Token![pub](Span::call_site())) }
  }
}

/// The optoions for a specific field.
struct FieldOptions {
  copy: bool,
  skip: bool,
  vis: Visibility,
}
