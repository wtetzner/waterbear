#![recursion_limit = "300"]

#![allow(unused_variables)]

extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;
use syn::VariantData;

#[proc_macro_derive(Instruction, attributes(instr))]
pub fn instruction(input: TokenStream) -> TokenStream {
    // Construct a string representation of the type definition
    let s = input.to_string();
    
    // Parse the string representation
    let ast = syn::parse_derive_input(&s).unwrap();

    // Build the impl
    let gen = expand_instr(&ast);

    // let toks: TokenStream = gen.parse().unwrap();
    // println!("toks: {}", toks);
    
    // Return the generated impl
    // gen.parse().unwrap()
    gen.parse().unwrap()
}

fn expand_instr(ast: &syn::DeriveInput) -> quote::Tokens {
    match ast.body {
        syn::Body::Struct(_) => panic!("#[derive(Instruction)] can only be used with enums"),
        syn::Body::Enum(ref data) => {
            let name = &ast.ident;
            let fields: Vec<quote::Tokens> = data.iter().map(|varient| {
                let vname = &varient.ident;
                let ty = &varient.data;
                // if *ty != VariantData::Unit {
                //     panic!("Variant must be simple; #[derive(Instruction)] doesn't work with complex variants.");
                // }
                quote! { #name::#vname }
            }).collect();

            let num_fields = fields.len();

            quote! {
                impl<Ex,IM> #name<Ex,IM> {
                    #[inline]
                    pub fn size(&self) -> usize {
                        0
                    }
                }

                impl #name<i32,u8> {
                    pub fn encode(&self) -> Vec<u8> {
                        vec![]
                    }
                }
            }
        }
    }
}


