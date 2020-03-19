#![recursion_limit = "300"]

#![allow(unused_variables)]

extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;
#[macro_use]
extern crate lazy_static;
extern crate regex;

use std::fmt;
use proc_macro::TokenStream;
use syn::{VariantData, Variant, MetaItem, Lit};
use regex::Regex;
use std::collections::HashMap;

#[proc_macro_derive(Instruction, attributes(instr))]
pub fn instruction(input: TokenStream) -> TokenStream {
    // Construct a string representation of the type definition
    let s = input.to_string();
    
    // Parse the string representation
    let ast = syn::parse_derive_input(&s).unwrap();

    // Build the impl
    let gen = expand_instr(&ast);

    let toks: TokenStream = gen.parse().unwrap();
    // println!("toks: {}", toks);
    
    // Return the generated impl
    gen.parse().unwrap()
}

fn expand_instr(ast: &syn::DeriveInput) -> quote::Tokens {
    match ast.body {
        syn::Body::Struct(_) => panic!("#[derive(Instruction)] can only be used with enums"),
        syn::Body::Enum(ref data) => {
            let name = &ast.ident;
            let fields: Vec<quote::Tokens> = data.iter().map(|variant| {
                let vname = &variant.ident;
                let ty = &variant.data;
                let names = names(ty);
                let bit_pattern = extract_bit_pattern(variant);
                let ranges = parse_bit_pattern(&bit_pattern);
                let byte_tokens: Vec<quote::Tokens> = ranges.iter().map(|byte| byte_to_tokens(&byte)).collect();
                if names.len() > 0 {
                    quote! {
                        #name::#vname (#(#names),*) => vec![
                            #(#byte_tokens),*
                        ]
                    }
                } else {
                    quote! {
                        #name::#vname => vec![
                            #(#byte_tokens),*
                        ]
                    }
                }
            }).collect();

            let bytes_ident = syn::Ident::new("bytes");

            let extractors: Vec<quote::Tokens> = data.iter().map(|variant| {
                let vname = &variant.ident;
                let ty = &variant.data;
                let bit_pattern = extract_bit_pattern(variant);
                let ranges = parse_bit_pattern(&bit_pattern);

                let opcodes = byte_to_matcher(&ranges[0]);
                let name_types = name_types(ty);
                let names = names(ty);

                let extractors = ranges_to_extractors(&bytes_ident, &ranges);

                let extracted: Vec<quote::Tokens> = name_types.iter().map(|(ident,tyident,ty)| {
                    let var_name = format!("{}", ident);
                    let body = &extractors[&var_name];
                    quote! {
                        let #ident = (#body) as #tyident;
                    }
                }).collect();

                let len = ranges.len();

                if extracted.len() == 0 {
                    quote! {
                        #opcodes => Some(#name :: #vname)
                    }
                } else {
                    quote! {
                        #opcodes if #bytes_ident.len() >= #len => {
                            #(#extracted)*
                            Some(#name :: #vname (#(#names),*))
                        }
                    }
                }
                
            }).collect();

            let size_fields: Vec<quote::Tokens> = data.iter().map(|variant| {
                let vname = &variant.ident;
                let ty = &variant.data;
                let names = names(ty);
                let bit_pattern = extract_bit_pattern(variant);
                let ranges = parse_bit_pattern(&bit_pattern);
                let size: usize = ranges.len();
                if names.len() > 0 {
                    quote! {
                        #name::#vname (#(#names),*) => #size
                    }
                } else {
                    quote! {
                        #name::#vname => #size
                    }
                }
            }).collect();

            let instr_names: Vec<String> = {
                let mut names: Vec<String> = data.iter().map(|variant| {
                    lazy_static! {
                        static ref NAME: Regex = Regex::new("^([^_]+)").unwrap();
                    }
                    let vname = &variant.ident;
                    let name_str = format!("{}", vname);
                    let name  = NAME.captures_iter(&name_str).next().unwrap()[1].to_lowercase();
                    name
                })
                .collect();
                names.dedup();
                names
            };

            quote! {
                impl<Ex,IM> #name<Ex,IM> {
                    #[inline]
                    #[allow(unused_variables)]
                    pub fn size(&self) -> usize {
                        match self {
                            #(#size_fields),*
                        }
                    }

                    #[inline]
                    pub fn exists(name: &str) -> bool {
                        match name {
                            #(#instr_names)|* => true,
                            _ => false
                        }
                    }
                }

                impl #name<i32,u8> {
                    pub fn encode(&self) -> Vec<u8> {
                        match self {
                            #(#fields),*
                        }
                    }

                    fn decode_raw(#bytes_ident: &[u8]) -> Option<#name<i32,u8>> {
                        match #bytes_ident[0] {
                            #(#extractors),* ,
                            _ => None
                        }
                    }
                }
            }
        }
    }
}

fn type_name(ty: &syn::Ty) -> String {
    match ty {
        syn::Ty::Path(qself, path) => {
            let mut result = String::new();
            for seg in path.segments.iter() {
                let name = format!("{}", seg.ident);
                result.push_str(&name);
            }
            result
        },
        _ => unreachable!()
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

#[derive(Eq,PartialEq,Ord,PartialOrd,Hash,Clone)]
struct BitLiteral(u8, usize);

impl fmt::Debug for BitLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let BitLiteral(value, size) = self;
        match size {
            1 => write!(f, "{:01b}", value),
            2 => write!(f, "{:02b}", value),
            3 => write!(f, "{:03b}", value),
            4 => write!(f, "{:04b}", value),
            5 => write!(f, "{:05b}", value),
            6 => write!(f, "{:06b}", value),
            7 => write!(f, "{:07b}", value),
            8 => write!(f, "{:08b}", value),
            _ => unreachable!()
        }
    }
}

impl fmt::Display for BitLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let BitLiteral(value, size) = self;
        match size {
            1 => write!(f, "{:01b}", value),
            2 => write!(f, "{:02b}", value),
            3 => write!(f, "{:03b}", value),
            4 => write!(f, "{:04b}", value),
            5 => write!(f, "{:05b}", value),
            6 => write!(f, "{:06b}", value),
            7 => write!(f, "{:07b}", value),
            8 => write!(f, "{:08b}", value),
            _ => unreachable!()
        }
    }
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
        for (idx, bit) in chunk.iter().rev().enumerate() {
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

fn make_mask(num_bits: usize) -> u32 {
    let mut val: u32 = 0;
    for idx in 0..num_bits {
        val = val | (1 << idx)
    }
    val
}

fn bit_combinations(size: usize) -> Vec<u8> {
    if size == 1 {
        vec![0b1, 0b0]
    } else {
        let mut results = vec![];
        let nums = bit_combinations(size - 1);
        for num in nums {
            results.push((1 << (size - 1)) | num);
            results.push((0 << (size - 1)) | num);
        }
        results
    }
}

fn literal_combinations(ranges: &[BitRange]) -> Vec<Vec<BitLiteral>> {
    if ranges.len() == 1 {
        match ranges[0] {
            BitRange::Literal(bits, size) => {
                vec![vec![BitLiteral(bits, size)]]
            },
            BitRange::Ref { ref name, high, low } => {
                let size = (1 + high) - low;
                let nums = bit_combinations(size);
                let literals: Vec<BitLiteral> = nums.iter().map(|num| BitLiteral(*num, size)).collect();
                vec![literals]
            }
        }
    } else {
        match ranges[0] {
            BitRange::Literal(bits, size) => {
                let mut combos = literal_combinations(&ranges[1..ranges.len()]);
                combos.push(vec![BitLiteral(bits, size)]);
                combos
            },
            BitRange::Ref { ref name, high, low } => {
                let size = (1 + high) - low;
                let mut combos = literal_combinations(&ranges[1..ranges.len()]);
                let bit_combos = bit_combinations(size);
                let literals: Vec<BitLiteral> = bit_combos.iter().map(|num| BitLiteral(*num, size)).collect();
                combos.push(literals);
                combos
            }
        }
    }
}

fn combinations_to_bits(combos: &[Vec<BitLiteral>]) -> (usize, Vec<u8>) {
    if combos.len() == 1 {
        let mut results = vec![];
        let BitLiteral(_,size) = combos[0][0];
        for literal in combos[0].iter() {
            let BitLiteral(num,_) = literal;
            results.push(*num);
        }
        (size,results)
    } else {
        let mut results = vec![];
        let (size, bytes) = combinations_to_bits(&combos[1..combos.len()]);
        let BitLiteral(_,local_size) = combos[0][0];
        for literal in combos[0].iter() {
            let BitLiteral(num,_) = literal;
            for byte in bytes.iter() {
                let new_num = (num << size) | byte;
                results.push(new_num);
            }
        }
        (size + local_size, results)
    }
}

fn all_possible_opcodes(ranges: &[BitRange]) -> Vec<u8> {
    let combos = {
        let mut vec = literal_combinations(ranges);
        vec.reverse();
        vec
    };
    let (size, mut bytes) = combinations_to_bits(&combos);
    bytes.sort();
    bytes
}

fn byte_to_matcher(ranges: &[BitRange]) -> quote::Tokens {
    let opcodes = all_possible_opcodes(ranges);
    quote! {
        #(#opcodes)|*
    }
}

fn ranges_to_extractors(bytes_ident: &syn::Ident, ranges: &[Vec<BitRange>]) -> HashMap<String,quote::Tokens> {
    let names = {
        let mut name_map = HashMap::new();
        for byte in ranges {
            for range in byte {
                match range {
                    BitRange::Literal(bits, size) => (),
                    BitRange::Ref { ref name, high, low } => {
                        name_map.insert(name.clone(), ());
                    }
                }
            }
        }
        let mut names: Vec<String> = name_map.keys().map(|text| text.clone()).collect();
        names.sort();
        names
    };
    let mut mapping: HashMap<String,Vec<quote::Tokens>> = HashMap::new();
    for name in names.iter() {
        mapping.insert(name.clone(), vec![]);
    }
    let mut byte_num: usize = 0;
    for byte in ranges {
        let mut bit: usize = 8;
        for range in byte {
            match range {
                BitRange::Literal(bits, size) => {
                    bit = bit - size;
                },
                BitRange::Ref { ref name, high, low } => {
                    let size = (high + 1) - low;
                    let mask = make_mask(size) << (bit - size);
                    let shift_back: usize = bit - size;
                    let shift_forward: usize = (high + 1) - size;

                    mapping.get_mut(name).unwrap().push(quote! {
                        ((((#bytes_ident[#byte_num] as u32) & #mask) >> #shift_back) << #shift_forward)
                    });

                    bit = bit - size;
                }
            }            
        }
        byte_num += 1;
    }
    let mut results = HashMap::new();
    for name in names.iter() {
        let toks = &mapping[name];
        results.insert(name.clone(), quote! {
            #(#toks)|*
        });
    }
    results
}

fn byte_to_tokens(ranges: &[BitRange]) -> quote::Tokens {
    let mut toks: Vec<quote::Tokens> = vec![];
    let mut pos = 8;
    for range in ranges.iter() {
        match range {
            BitRange::Literal(bits, size) => {
                let val: u8 = bits << (pos - size);
                toks.push(quote! { #val });
                pos = pos - size;
            },
            BitRange::Ref { ref name, high, low } => {
                let ident = syn::Ident::new(name.as_str());
                let size = (1 + high) - low;
                let mask: u32 = make_mask(size) << ((high + 1) - size);
                let lshift: i32 = (pos as i32) - (size as i32);
                let shift_amount = (*low as i32) - lshift;
                if shift_amount == 0 {
                    toks.push(quote! {
                        (#mask & (*#ident as u32)) as u8
                    })
                } else if shift_amount > 0 {
                    let shift = shift_amount as usize;
                    toks.push(quote! {
                        ((#mask & (*#ident as u32)) >> #shift) as u8
                    });
                } else {
                    let shift = shift_amount.abs() as usize;
                    toks.push(quote! {
                        ((#mask & (*#ident as u32)) << #shift) as u8
                    });
                }
                pos = pos - size;
            }
        }
    }
    quote! {
        #(#toks)|*
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

fn name_types(variant: &VariantData) -> Vec<(syn::Ident,syn::Ident,String)> {
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
        let type_name = type_name(&field.ty);
        idents.push((
            syn::Ident::new(format!("{}", LETTERS[idx])),
            match type_name.as_str() {
                "Ex" => syn::Ident::new("i32".to_string()),
                "IM" => syn::Ident::new("u8".to_string()),
                _ => unreachable!()
            },
            type_name));
    }
    idents
}

fn names(variant: &VariantData) -> Vec<syn::Ident> {
    name_types(variant).iter().map(|tuple| tuple.0.clone()).collect()
}

fn extract_bit_pattern(variant: &Variant) -> String {
    let attr = variant.attrs.iter()
        .filter(|attr| attr.name() == "instr")
        .next()
        .expect(&format!("No instr attribute with bit pattern on variant: {}", variant.ident));
    match attr.value {
        MetaItem::NameValue(_, ref value) => {
            return match value {
                Lit::Str(pattern, _) => pattern.clone(),
                _ => panic!("Expected string for bit pattern, found: {:?}", attr)
            };
        },
        _ => panic!("Invalid instr attribute: {:?}", attr)
    };
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

    #[test]
    fn test_bit_combinations() {
        assert_eq!(
            bit_combinations(4),
            vec![0b1111, 0b0111, 0b1011, 0b0011, 0b1101, 0b0101, 0b1001, 0b0001, 0b1110, 0b0110, 0b1010, 0b0010, 0b1100, 0b0100, 0b1000, 0b0000]
        );
    }

    #[test]
    fn test_combinations_to_bits() {
        let input = "000[a11]1[a10][a9][a8] [a7][a6][a5][a4][a3][a2][a1][a0]";
        let ranges = parse_bit_pattern(input);
        let range = &ranges[0];
        let combos = {
            let mut vec = literal_combinations(range);
            vec.reverse();
            vec
        };
        let (size, bytes) = combinations_to_bits(&combos);

        let mut results = bytes.clone();
        results.sort();

        let mut expected = vec![
            0b00011111,
            0b00001111,
            0b00011011,
            0b00001011,
            0b00011101,
            0b00001101,
            0b00011001,
            0b00001001,
            0b00011110,
            0b00001110,
            0b00011010,
            0b00001010,
            0b00011100,
            0b00001100,
            0b00011000,
            0b00001000
        ];
        expected.sort();

        assert_eq!(8, size);
        assert_eq!(
            results,
            expected
        );
    }

    #[test]
    fn test_all_possible_opcodes() {
        let input = "000[a11]1[a10][a9][a8] [a7][a6][a5][a4][a3][a2][a1][a0]";
        let ranges = parse_bit_pattern(input);
        let range = &ranges[0];
        let opcodes = all_possible_opcodes(range);
        let mut expected = vec![
            0b00011111,
            0b00001111,
            0b00011011,
            0b00001011,
            0b00011101,
            0b00001101,
            0b00011001,
            0b00001001,
            0b00011110,
            0b00001110,
            0b00011010,
            0b00001010,
            0b00011100,
            0b00001100,
            0b00011000,
            0b00001000
        ];
        expected.sort();

        assert_eq!(opcodes, expected);
    }
}
