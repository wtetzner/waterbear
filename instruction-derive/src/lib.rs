#![recursion_limit = "300"]

#![allow(unused_variables)]

extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;
#[macro_use]
extern crate lazy_static;
extern crate regex;

use proc_macro::TokenStream;
use syn::{VariantData, MetaItem, NestedMetaItem, Lit};
use regex::Regex;

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
            let fields: Vec<quote::Tokens> = data.iter().for_each(|varient| {
                let vname = &varient.ident;
                let ty = &varient.data;
                let names = names(ty);
                let attr = varient.attrs.iter()
                    .filter(|attr| attr.name() == "instr")
                    .next()
                    .expect(&format!("No bit pattern on variant: {}", vname));
                println!("attr.value: {:?}", attr.value);
                match attr.value {
                    MetaItem::List(ref ident, ref nested)
                        if *ident == syn::Ident::new("instr") => {
                            nested.iter().for_each(|item| {
                                match item {
                                    NestedMetaItem::MetaItem(MetaItem::NameValue(ref bits_name, ref pat))
                                        if *bits_name == syn::Ident::new("bits") => {
                                            if let Lit::Str(ref text,_) = pat {
                                                
                                                quote! {
                                                    #name::#vname (#(#names),*) => vec![
                                                        
                                                    ]
                                                }
                                            } else {
                                                panic!("bit pattern should be a String");
                                            }
                                        },
                                    _ => panic!("Expected bit pattern on attr: {:?}", attr)
                                }
                            });
                            println!("val: {:?}", nested);
                        },
                    _ => panic!("instr attr is invalid: {:?}", attr)
                }
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

#[derive(Debug,Eq,PartialEq,Ord,PartialOrd,Hash,Clone)]
enum Bit {
    Literal(u8),
    Ref(String, usize)
}

impl Bit {
    fn is_literal(&self) -> bool {
        match self {
            Bit::Literal(_) => true,
            _ => false
        }
    }
}

#[derive(Debug,Eq,PartialEq,Ord,PartialOrd,Hash,Clone)]
enum BitRange {
    Literal(u8, usize),
    Ref { name: String, high: usize, low: usize }
}

fn match_token(pattern: &str) -> Option<(usize, Bit)> {
    lazy_static! {
        static ref BIT: Regex = Regex::new("^(:?[10]|\\[(:?[a-z][0-9]+)\\])").unwrap();
    }
    if let Some(cap) = BIT.captures_iter(pattern).next() {
        let text: &str = &cap[0];
        if text.starts_with("[") {
            Some((text.len(), Bit::Ref(
                text[1..2].to_owned(),
                usize::from_str_radix(&text[2..(text.len() - 1)], 10).unwrap()
            )))
        } else if text == "1" {
            Some((1, Bit::Literal(1u8)))
        } else if text == "0" {
            Some((1, Bit::Literal(0u8)))
        } else {
            panic!("Invalid bit token. Shouldn't ever happen.");
        }
    } else {
        None
    }
}

fn tokenize_bit_pattern(pattern: &str) -> Vec<Bit> {
    lazy_static! {
        static ref WS: Regex = Regex::new("\\s+").unwrap();
    }
    let mut results = vec![];
    let mut text: &str = &WS.replace_all(pattern, "");
    let mut pos = 0;
    while text.len() > 0 {
        match match_token(text) {
            Some((size, bit)) => {
                results.push(bit.clone());
                pos = pos + size;
                text = &text[size..text.len()];
            },
            None => panic!("Invalid bit pattern at {}: {}", pos, pattern)
        }
    }
    results
}

fn chunk_to_range(chunk: &[Bit]) -> BitRange {
    assert!(!chunk.is_empty());
    if chunk[0].is_literal() {
        let mut value: u8 = 0;
        for (idx, bit) in chunk.iter().enumerate() {
            if let Bit::Literal(data) = bit {
                value = value | (data << idx);
            } else {
                panic!("Found non-literal in literal chunk. Should never happen.");
            }
        }
        BitRange::Literal(value, chunk.len())
    } else {
        if let Bit::Ref(name, idx) = &chunk[0] {
            if let Bit::Ref(_, end) = chunk[chunk.len() - 1] {
                BitRange::Ref { name: name.to_owned(), high: *idx, low: end }
            } else {
                panic!("Found literal in non-literal chunk. Should never happen.");
            }
        } else {
            panic!("Found literal in non-literal chunk. Should never happen.");
        }
    }
}

fn bits_to_range(bits: &[Bit]) -> Vec<BitRange> {
    let mut chunks = vec![];
    let mut current = vec![];
    for bit in bits {
        if current.is_empty() {
            current.push(bit.clone());
        } else {
            let last = current[current.len() - 1].clone();
            match last {
                Bit::Literal(_) => if bit.is_literal() {
                    current.push(bit.clone());
                } else {
                    if !current.is_empty() {
                        chunks.push(current.clone());
                        current.clear();
                    }
                    current.push(bit.clone());
                },
                Bit::Ref(name, idx) => match bit {
                    Bit::Literal(_) => {
                        if !current.is_empty() {
                            chunks.push(current.clone());
                            current.clear();
                        }
                        current.push(bit.clone());
                    },
                    Bit::Ref(cname, cidx) =>
                        if name == *cname && (idx - 1) == *cidx {
                            current.push(bit.clone());
                        } else {
                            if !current.is_empty() {
                                chunks.push(current.clone());
                                current.clear();
                            }
                            current.push(bit.clone());
                        }
                }
            }
        }
    }
    if !current.is_empty() {
        chunks.push(current.clone());
        current.clear();
    }

    let mut results = vec![];
    for chunk in chunks {
        results.push(chunk_to_range(&chunk));
    }
    results
}

fn make_mask(num_bits: usize) -> u8 {
    let mut val: u8 = 0;
    for idx in 0..num_bits {
        val = val | (1 << idx)
    }
}

fn byte_to_tokens(name: syn::Ident, vname: syn::Ident, ranges: &[BitRange]) -> quote::Tokens {
    let mut toks: Vec<quote::Tokens> = vec![];
    let pos = 8;
    for range in ranges.iter() {
        match range {
            BitRange::Literal(bits, size) => {
                let val = bits << (pos - size);
                toks.push(quote! { #val });
                pos = pos - size;
            },
            BitRange::Ref { name: ref name, high: high, low: low } => {
                let ident = syn::Ident::new(name);
                let size = (1 + high) - low;
                let mut mask = make_mask(size) << (high - size);
                let shift = pos - size;
                toks.push(quote! {
                    ((#mask & #ident) >> #low) << #shift
                });
                pos = pos - size;
            }
        }
    }
    quote! {
        #name::#vname (#(#names),*) => vec![
            #(#names)|*
        ]
    }
}

fn parse_bit_pattern(pattern: &str) -> Vec<Vec<BitRange>> {
    let tokenized = tokenize_bit_pattern(pattern);
    if tokenized.len() % 8 != 0 {
        panic!("Bit patterns must be a multiple of 8. Found: {}", pattern);
    }
    let mut bytes: Vec<Vec<Bit>> = vec![];
    let mut current = vec![];
    for (idx, bit) in tokenized.iter().enumerate() {
        if idx % 8 == 0 && !current.is_empty() {
            bytes.push(current.clone());
            current.clear();
        }
        current.push(bit.clone());
    }
    if !current.is_empty() {
        bytes.push(current.clone());
    }

    let mut results = vec![];
    for byte in bytes {
        results.push(bits_to_range(byte.as_slice()));
    }
    results
}

const LETTERS: &'static [&'static str] = &["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"];

fn names(variant: &VariantData) -> Vec<syn::Ident> {
    let empty = vec![];
    let fields = match variant {
        VariantData::Struct(ref struct_fields) => {
            struct_fields
        },
        VariantData::Tuple(ref struct_fields) => {
            struct_fields
        },
        _ => &empty
    };
    let mut idents = vec![];
    for (idx, field) in fields.iter().enumerate() {
        idents.push(syn::Ident::new(format!("{}", LETTERS[idx])));
    }
    idents
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_bit_pattern() {
        let input = "10000001 [a7][a6][a5][a4][a3][a2][a1][a0]";
        let output = vec![
            Bit::Literal(1),
            Bit::Literal(0),
            Bit::Literal(0),
            Bit::Literal(0),
            Bit::Literal(0),
            Bit::Literal(0),
            Bit::Literal(0),
            Bit::Literal(1),

            Bit::Ref("a".to_owned(), 7),
            Bit::Ref("a".to_owned(), 6),
            Bit::Ref("a".to_owned(), 5),
            Bit::Ref("a".to_owned(), 4),
            Bit::Ref("a".to_owned(), 3),
            Bit::Ref("a".to_owned(), 2),
            Bit::Ref("a".to_owned(), 1),
            Bit::Ref("a".to_owned(), 0)
        ];
        assert_eq!(tokenize_bit_pattern(input), output);
    }

    #[test]
    fn test_parse_bit_pattern() {
        let input = "10000001 [a7][a6][a5][a4][a3][a2][a1][a0]";
        let output = vec![
            vec![BitRange::Literal(0b10000001, 8)],
            vec![BitRange::Ref { name: "a".to_owned(), high: 7, low: 0 }]
        ];
        assert_eq!(parse_bit_pattern(input), output);
    }

    #[test]
    fn test_parse_bit_pattern2() {
        let input = "1000001[a8] [a7][a6][a5][a4][a3][a2][a1][a0]";
        let output = vec![
            vec![
                BitRange::Literal(0b1000001, 7),
                BitRange::Ref { name: "a".to_owned(), high: 8, low: 8 }
            ],
            vec![BitRange::Ref { name: "a".to_owned(), high: 7, low: 0 }]
        ];
        assert_eq!(parse_bit_pattern(input), output);
    }
}
