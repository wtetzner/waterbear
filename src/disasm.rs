
use instruction::{Instr,IndirectionMode};
use std::collections::HashMap;
use std::collections::HashSet;
use ast::{Statements,Statement,Directive};
use location::{Span};
use expression::{Expr};
use std;
use std::fmt;

#[derive(Debug)]
pub enum DisasmError {
    NoInstruction(usize, u8),
    NoSuchFile(String, std::io::Error)
}

pub fn disassemble(xor_byte: Option<u8>, entry_points: &[usize], bytes: &[u8]) -> Result<Statements,DisasmError> {
    let graph = build_instruction_graph(entry_points, bytes);
    println!("{}", graph);
    if 9 == 10 - 1 {
        panic!("exit early");
    }
    let new_bytes = {
        let mut results: Vec<u8> = Vec::with_capacity(bytes.len());
        for (idx, byte) in bytes.iter().enumerate() {
            if idx == 0 {
                results.push(*byte);
            } else {
                match xor_byte {
                    Some(xor) => {
                        let new_byte: u8 = xor ^ byte;
                        results.push(new_byte);
                    },
                    None => {
                        results.push(*byte);
                    }
                }
            }
        }
        results
    };
    let bytes = new_bytes.as_slice();
    let mut seen = vec![false; bytes.len()];
    let mut instrs: HashMap<usize,Instr<i32,u8>> = HashMap::new();
    let mut positions: Vec<usize> = entry_points.iter().map(|val| *val).collect();
    let mut locs = vec![];
    while positions.len() > 0 {
        locs.clear();
        locs.append(&mut positions);

        for pos in locs.iter() {
            match follow(*pos, &mut seen, &mut instrs, bytes)? {
                Pos::One(val) => {
                    positions.push(val);
                },
                Pos::Two(val1, val2) => {
                    positions.push(val1);
                    positions.push(val2);
                },
                Pos::None => ()
            }
        }
    }

    let labels = compute_labels(&instrs);
    let aliases = compute_aliases(&instrs);
    let bit_aliases = compute_bit_aliases(&instrs);

    let mut statements = vec![];
    statements.push(Statement::comment("Special Function Registers"));
    for key in aliases.keys() {
        let name = &aliases[key];
        statements.push(Statement::Alias(
            Span::default(),
            name.clone(),
            Expr::Number(Span::default(), *key as i32)
        ));
    }

    statements.push(Statement::comment("\nPSW bits"));
    for key in bit_aliases.keys() {
        let name = &bit_aliases[key];
        statements.push(Statement::Alias(
            Span::default(),
            name.clone(),
            Expr::Number(Span::default(), *key as i32)
        ));
    }

    statements.push(Statement::comment("\nStart"));
    let mut pos: usize = 0;
    while pos < bytes.len() {
        let org = pos == 0 || (seen[pos] && !seen[pos - 1]);
        if org {
            if pos > 0 {
                statements.push(Statement::comment(""));
            }
            statements.push(Statement::Directive(Directive::Org(Span::default(), pos)));
        }
        if labels.contains_key(&pos) {
            if !org && !seen[pos - 1] {
                statements.push(Statement::comment(""));
            }
            statements.push(Statement::label(&labels[&pos]));
        }
        if pos == 0x31CE {
            println!("instrs.contains_key(&pos): {}", instrs.contains_key(&pos));
        }
        if instrs.contains_key(&pos) {
            let instr = &instrs[&pos];
            if pos < 0x31CE && pos > 0x31BF {
                println!("{:05X} {:?}", pos, instr);
            }
            let size = instr.size();
            let next = pos + size;
            use instruction::Instr::*;
            let hinstr = match instr {
                Add_i8(imm) => Statement::instr(Instr::Add_i8(Expr::num(*imm))),
                Add_d9(d9) => {
                    let expr = mem_expr(&aliases, *d9);
                    Statement::instr(Instr::Add_d9(expr))
                },
                Add_Ri(ri) => Statement::instr(Instr::Add_Ri(IndirectionMode::from(*ri))),

                Addc_i8(imm) => Statement::instr(Instr::Addc_i8(Expr::num(*imm))),
                Addc_d9(d9) => {
                    let expr = mem_expr(&aliases, *d9);
                    Statement::instr(Instr::Addc_d9(expr))
                },
                Addc_Ri(ri) => Statement::instr(Instr::Addc_Ri(IndirectionMode::from(*ri))),

                Sub_i8(imm) => Statement::instr(Instr::Sub_i8(Expr::num(*imm))),
                Sub_d9(d9) => {
                    let expr = mem_expr(&aliases, *d9);
                    Statement::instr(Instr::Sub_d9(expr))
                },
                Sub_Ri(ri) => Statement::instr(Instr::Sub_Ri(IndirectionMode::from(*ri))),

                Subc_i8(imm) => Statement::instr(Instr::Subc_i8(Expr::num(*imm))),
                Subc_d9(d9) => {
                    let expr = mem_expr(&aliases, *d9);
                    Statement::instr(Instr::Subc_d9(expr))
                },
                Subc_Ri(ri) => Statement::instr(Instr::Subc_Ri(IndirectionMode::from(*ri))),

                Inc_d9(d9) => {
                    let expr = mem_expr(&aliases, *d9);
                    Statement::instr(Instr::Inc_d9(expr))
                },
                Inc_Ri(ri) => Statement::instr(Instr::Inc_Ri(IndirectionMode::from(*ri))),

                Dec_d9(d9) => {
                    let expr = mem_expr(&aliases, *d9);
                    Statement::instr(Instr::Dec_d9(expr))
                },
                Dec_Ri(ri) => Statement::instr(Instr::Dec_Ri(IndirectionMode::from(*ri))),

                Mul => Statement::instr(Instr::Mul),
                Div => Statement::instr(Instr::Div),

                And_i8(imm) => Statement::instr(Instr::And_i8(Expr::num(*imm))),
                And_d9(d9) => {
                    let expr = mem_expr(&aliases, *d9);
                    Statement::instr(Instr::And_d9(expr))
                },
                And_Ri(ri) => Statement::instr(Instr::And_Ri(IndirectionMode::from(*ri))),

                Or_i8(imm) => Statement::instr(Instr::Or_i8(Expr::num(*imm))),
                Or_d9(d9) => {
                    let expr = mem_expr(&aliases, *d9);
                    Statement::instr(Instr::Or_d9(expr))
                },
                Or_Ri(ri) => Statement::instr(Instr::Or_Ri(IndirectionMode::from(*ri))),

                Xor_i8(imm) => Statement::instr(Instr::Xor_i8(Expr::num(*imm))),
                Xor_d9(d9) => {
                    let expr = mem_expr(&aliases, *d9);
                    Statement::instr(Instr::Xor_d9(expr))
                },
                Xor_Ri(ri) => Statement::instr(Instr::Xor_Ri(IndirectionMode::from(*ri))),

                Rol => Statement::instr(Instr::Rol),
                Rolc => Statement::instr(Instr::Rolc),

                Ror => Statement::instr(Instr::Ror),
                Rorc => Statement::instr(Instr::Rorc),

                Ld_d9(d9) => {
                    let expr = mem_expr(&aliases, *d9);
                    Statement::instr(Instr::Ld_d9(expr))
                },
                Ld_Ri(ri) => Statement::instr(Instr::Ld_Ri(IndirectionMode::from(*ri))),

                St_d9(d9) => {
                    let expr = mem_expr(&aliases, *d9);
                    Statement::instr(Instr::St_d9(expr))
                },
                St_Ri(ri) => Statement::instr(Instr::St_Ri(IndirectionMode::from(*ri))),

                Mov_d9(imm, d9) => {
                    let expr = mem_expr(&aliases, *d9);
                    Statement::instr(Instr::Mov_d9(Expr::num(*imm), expr))
                },
                Mov_Rj(imm, ri) => Statement::instr(Instr::Mov_Rj(Expr::num(*imm), IndirectionMode::from(*ri))),

                Ldc => Statement::instr(Instr::Ldc),

                Push(d9) => {
                    let expr = mem_expr(&aliases, *d9);
                    Statement::instr(Instr::Push(expr))
                },
                Pop(d9) => {
                    let expr = mem_expr(&aliases, *d9);
                    Statement::instr(Instr::Pop(expr))
                },

                Xch_d9(d9) => {
                    let expr = mem_expr(&aliases, *d9);
                    Statement::instr(Instr::Xch_d9(expr))
                },
                Xch_Ri(ri) => Statement::instr(Instr::Xch_Ri(IndirectionMode::from(*ri))),

                Jmp(abs) => {
                    let top_bits = next & 0xF000;
                    let bottom_bits = (*abs as usize) & 0x0FFF;
                    let address = (top_bits | bottom_bits) as usize;
                    let addr_expr = label_expr(&labels, address);
                    Statement::instr(Instr::Jmp(addr_expr))
                },
                Jmpf(abs) => {
                    let address = *abs as usize;
                    let addr_expr = label_expr(&labels, address);
                    Statement::instr(Instr::Jmpf(addr_expr))
                },

                Br(rel) => {
                    let address = rel8(*rel, next);
                    let addr_expr = label_expr(&labels, address);
                    Statement::instr(Instr::Br(addr_expr))
                },
                Brf(rel) => {
                    let address = rel16(*rel, next);
                    let addr_expr = label_expr(&labels, address);
                    Statement::instr(Instr::Brf(addr_expr))
                },
                Bz(rel) => {
                    let address = rel8(*rel, next);
                    let addr_expr = label_expr(&labels, address);
                    Statement::instr(Instr::Bz(addr_expr))
                },
                Bnz(rel) => {
                    let address = rel8(*rel, next);
                    let addr_expr = label_expr(&labels, address);
                    Statement::instr(Instr::Bnz(addr_expr))
                },
                Bp(dir, b3, rel) => {
                    let address = rel8(*rel, next);
                    let addr_expr = label_expr(&labels, address);
                    let d9 = mem_expr(&aliases, *dir);
                    let bits = bits_expr(&bit_aliases, *dir, *b3);
                    Statement::instr(Instr::Bp(d9, bits, addr_expr))
                },
                Bpc(dir, b3, rel) => {
                    let address = rel8(*rel, next);
                    let addr_expr = label_expr(&labels, address);
                    let d9 = mem_expr(&aliases, *dir);
                    let bits = bits_expr(&bit_aliases, *dir, *b3);
                    Statement::instr(Instr::Bpc(d9, bits, addr_expr))
                },
                Bn(dir, b3, rel) => {
                    let address = rel8(*rel, next);
                    let addr_expr = label_expr(&labels, address);
                    let d9 = mem_expr(&aliases, *dir);
                    let bits = bits_expr(&bit_aliases, *dir, *b3);
                    Statement::instr(Instr::Bn(d9, bits, addr_expr))
                },
                Dbnz_d9(dir, rel) => {
                    let address = rel8(*rel, next);
                    let addr_expr = label_expr(&labels, address);
                    let d9 = mem_expr(&aliases, *dir);
                    Statement::instr(Instr::Dbnz_d9(d9, addr_expr))
                },
                Dbnz_Ri(ind, rel) => {
                    let address = rel8(*rel, next);
                    let addr_expr = label_expr(&labels, address);
                    let im = IndirectionMode::from(*ind);
                    Statement::instr(Instr::Dbnz_Ri(im, addr_expr))
                },
                Be_i8(imm, rel) => {
                    let address = rel8(*rel, next);
                    let addr_expr = label_expr(&labels, address);
                    Statement::instr(Instr::Be_i8(Expr::num(*imm), addr_expr))
                },
                Be_d9(dir, rel) => {
                    let address = rel8(*rel, next);
                    let addr_expr = label_expr(&labels, address);
                    let d9 = mem_expr(&aliases, *dir);
                    Statement::instr(Instr::Be_d9(d9, addr_expr))
                },
                Be_Rj(ind, imm, rel) => {
                    let address = rel8(*rel, next);
                    let addr_expr = label_expr(&labels, address);
                    let im = IndirectionMode::from(*ind);
                    Statement::instr(Instr::Be_Rj(im, Expr::num(*imm), addr_expr))
                },
                Bne_i8(imm, rel) => {
                    let address = rel8(*rel, next);
                    let addr_expr = label_expr(&labels, address);
                    Statement::instr(Instr::Bne_i8(Expr::num(*imm), addr_expr))
                },
                Bne_d9(dir, rel) => {
                    let address = rel8(*rel, next);
                    let addr_expr = label_expr(&labels, address);
                    let d9 = mem_expr(&aliases, *dir);
                    Statement::instr(Instr::Bne_d9(d9, addr_expr))
                },
                Bne_Rj(ind, imm, rel) => {
                    let address = rel8(*rel, next);
                    let addr_expr = label_expr(&labels, address);
                    let im = IndirectionMode::from(*ind);
                    Statement::instr(Instr::Bne_Rj(im, Expr::num(*imm), addr_expr))
                },

                Call(a12) => {
                    let top_bits = next & 0xF000;
                    let bottom_bits = (*a12 as usize) & 0x0FFF;
                    let address = (top_bits | bottom_bits) as usize;
                    let addr_expr = label_expr(&labels, address);
                    Statement::instr(Instr::Call(addr_expr))
                },
                Callf(a16) => {
                    let address = *a16 as usize;
                    let addr_expr = label_expr(&labels, address);
                    Statement::instr(Instr::Callf(addr_expr))
                },
                Callr(r16) => {
                    let address = rel16(*r16, next);
                    let addr_expr = label_expr(&labels, address);
                    Statement::instr(Instr::Callr(addr_expr))
                },

                Ret => Statement::instr(Instr::Ret),
                Reti => Statement::instr(Instr::Reti),

                Clr1(dir, b3) => {
                    let d9 = mem_expr(&aliases, *dir);
                    let bits = bits_expr(&bit_aliases, *dir, *b3);
                    Statement::instr(Instr::Clr1(d9, bits))
                },
                Set1(dir, b3) => {
                    let d9 = mem_expr(&aliases, *dir);
                    let bits = bits_expr(&bit_aliases, *dir, *b3);
                    Statement::instr(Instr::Set1(d9, bits))
                },
                Not1(dir, b3) => {
                    let d9 = mem_expr(&aliases, *dir);
                    let bits = bits_expr(&bit_aliases, *dir, *b3);
                    Statement::instr(Instr::Not1(d9, bits))
                },

                Nop => Statement::instr(Instr::Nop),
                Ldf => Statement::instr(Instr::Ldf),
                Stf => Statement::instr(Instr::Stf)
            };
            statements.push(hinstr);
            pos = next;
        } else {
            if bytes[pos] == 0 && !seen[pos] {
                while pos < bytes.len() && bytes[pos] == 0 && !seen[pos] {
                    pos = pos + 1;
                }
            } else if pos < bytes.len() && !seen[pos] && bytes[pos] != 0 {
                let mut end = pos;
                while end < bytes.len() && !seen[end] && bytes[end] != 0 {
                    end += 1;
                }
                let chunk = &bytes[pos..end];
                if !chunk.is_empty() {
                    statements.push(Statement::comment(""));
                    statements.push(Statement::Directive(Directive::Org(Span::default(), pos)));
                    for stmt in make_bytes(chunk) {
                        statements.push(stmt);
                    }
                    pos = pos + chunk.len();
                }
            } else {
                println!("Expected instruction at 0x{:05X}", pos);
                pos += 1;
                //panic!("Expected instruction at 0x{:05X}", pos);
            }
        }
    }

    statements.push(Statement::Directive(Directive::Cnop(Span::default(), Expr::num(bytes.len() as i32), Expr::num(0))));
    Ok(Statements { statements: statements })
}

fn make_bytes(bytes: &[u8]) -> Vec<Statement> {
    let mut results = vec![];
    let mut chunk = vec![];
    for byte in bytes {
        if chunk.len() == 16 {
            results.push(Statement::Directive(Directive::Byte(Span::default(), chunk.clone())));
            chunk.clear();
        }
        chunk.push(Expr::num(*byte as i32));
    }
    if chunk.len() > 0 {
        results.push(Statement::Directive(Directive::Byte(Span::default(), chunk.clone())));
    }
    results
}

fn label_expr(labels: &HashMap<usize,String>, addr: usize) -> Expr {
    labels.get(&addr).map(|name| Expr::name(name)).unwrap_or(Expr::num(addr as i32))
}

fn mem_expr(aliases: &HashMap<usize,String>, addr: i32) -> Expr {
    aliases.get(&(addr as usize)).map(|name| Expr::name(name)).unwrap_or(Expr::num(addr as i32))
}

fn bits_expr(bit_aliases: &HashMap<usize,String>, mem: i32, addr: i32) -> Expr {
    if mem == 0x0101 {
        bit_aliases.get(&(addr as usize)).map(|name| Expr::name(name)).unwrap_or(Expr::num(addr as i32))
    } else {
        Expr::num(addr as i32)
    }
}

fn compute_bit_aliases(instrs: &HashMap<usize,Instr<i32,u8>>) -> HashMap<usize,String> {
    let mut results: HashMap<usize,String> = HashMap::new();
    results.insert(7, "cy".to_owned());
    results.insert(6, "ac".to_owned());
    results.insert(4, "irbk1".to_owned());
    results.insert(3, "irbk0".to_owned());
    results.insert(2, "ov".to_owned());
    results.insert(1, "rambk0".to_owned());
    results.insert(0, "p".to_owned());
    results
}

fn compute_aliases(instrs: &HashMap<usize,Instr<i32,u8>>) -> HashMap<usize,String> {
    let mut results: HashMap<usize,String> = HashMap::new();
    results.insert(0x100, "acc".to_owned());
    results.insert(0x101, "psw".to_owned());
    results.insert(0x102, "b".to_owned());
    results.insert(0x103, "c".to_owned());
    results.insert(0x104, "trl".to_owned());
    results.insert(0x105, "trh".to_owned());
    results.insert(0x106, "sp".to_owned());
    results.insert(0x107, "pcon".to_owned());
    results.insert(0x108, "ie".to_owned());
    results.insert(0x109, "ip".to_owned());
    results.insert(0x10D, "ext".to_owned());
    results.insert(0x10E, "ocr".to_owned());
    results.insert(0x110, "t0con".to_owned());
    results.insert(0x111, "t0prr".to_owned());
    results.insert(0x112, "t0l".to_owned());
    results.insert(0x113, "t0lr".to_owned());
    results.insert(0x114, "t0h".to_owned());
    results.insert(0x115, "t0hr".to_owned());
    results.insert(0x118, "t1cnt".to_owned());
    results.insert(0x11A, "t1lc".to_owned());
    results.insert(0x11B, "t1l".to_owned());
    results.insert(0x11B, "t1lr".to_owned());
    results.insert(0x11C, "t1hc".to_owned());
    results.insert(0x11D, "t1h".to_owned());
    results.insert(0x11D, "t1hr".to_owned());
    results.insert(0x120, "mcr".to_owned());
    results.insert(0x122, "stad".to_owned());
    results.insert(0x123, "cnr".to_owned());
    results.insert(0x124, "tdr".to_owned());
    results.insert(0x125, "xbnk".to_owned());
    results.insert(0x127, "vccr".to_owned());
    results.insert(0x130, "scon0".to_owned());
    results.insert(0x131, "sbuf0".to_owned());
    results.insert(0x132, "sbr".to_owned());
    results.insert(0x134, "scon1".to_owned());
    results.insert(0x135, "sbuf1".to_owned());
    results.insert(0x144, "p1".to_owned());
    results.insert(0x145, "p1ddr".to_owned());
    results.insert(0x146, "p1fcr".to_owned());
    results.insert(0x14C, "p3".to_owned());
    results.insert(0x14D, "p3ddr".to_owned());
    results.insert(0x14E, "p3int".to_owned());
    results.insert(0x15C, "p7".to_owned());
    results.insert(0x15D, "i01cr".to_owned());
    results.insert(0x15E, "i23cr".to_owned());
    results.insert(0x15F, "isl".to_owned());
    results.insert(0x163, "vsel".to_owned());
    results.insert(0x164, "vrmad1".to_owned());
    results.insert(0x165, "vrmad2".to_owned());
    results.insert(0x166, "vtrbf".to_owned());
    results.insert(0x167, "vlreg".to_owned());
    results.insert(0x17F, "btcr".to_owned());
    results.insert(0x180, "xram".to_owned());
    results
}

fn compute_labels(instrs: &HashMap<usize,Instr<i32,u8>>) -> HashMap<usize,String> {
    let mut results: HashMap<usize,String> = HashMap::new();
    for pos in instrs.keys() {
        let instr = &instrs[pos];
        let size = instr.size();
        let next = pos + size;
        use instruction::Instr::*;
        match instr {
            Add_i8(_) => (),
            Add_d9(_) => (),
            Add_Ri(_) => (),

            Addc_i8(_) => (),
            Addc_d9(_) => (),
            Addc_Ri(_) => (),

            Sub_i8(_) => (),
            Sub_d9(_) => (),
            Sub_Ri(_) => (),

            Subc_i8(_) => (),
            Subc_d9(_) => (),
            Subc_Ri(_) => (),

            Inc_d9(_) => (),
            Inc_Ri(_) => (),

            Dec_d9(_) => (),
            Dec_Ri(_) => (),

            Mul => (),
            Div => (),

            And_i8(_) => (),
            And_d9(_) => (),
            And_Ri(_) => (),

            Or_i8(_) => (),
            Or_d9(_) => (),
            Or_Ri(_) => (),

            Xor_i8(_) => (),
            Xor_d9(_) => (),
            Xor_Ri(_) => (),

            Rol => (),
            Rolc => (),

            Ror => (),
            Rorc => (),

            Ld_d9(_) => (),
            Ld_Ri(_) => (),

            St_d9(_) => (),
            St_Ri(_) => (),

            Mov_d9(_, _) => (),
            Mov_Rj(_, _) => (),

            Ldc => (),

            Push(_) => (),
            Pop(_) => (),

            Xch_d9(_) => (),
            Xch_Ri(_) => (),

            Jmp(abs) => {
                let top_bits = next & 0xF000;
                let bottom_bits = (*abs as usize) & 0x0FFF;
                let address = (top_bits | bottom_bits) as usize;
                if address == 0x31C2 {
                    println!("printing label {:04X}: {:?}", address, instr);
                }
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Jmpf(abs) => {
                let address = *abs as usize;
                if !results.contains_key(&address) {
                    if address == 0x31C2 {
                        println!("printing label {:04X}: {:?}", address, instr);
                    }
                    results.insert(address, format!("label_{:04X}", address));
                }
            },

            Br(rel) => {
                let address = rel8(*rel, next);
                if address == 0x31C2 {
                    println!("printing label {:04X}: {:?}", address, instr);
                }
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Brf(rel) => {
                let address = rel16(*rel, next);
                println!("brf {:04X}", address);
                if address == 0x31C2 {
                    println!("printing label {:04X}: {:?}", address, instr);
                }
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Bz(rel) => {
                let address = rel8(*rel, next);
                if address == 0x31C2 {
                    println!("printing label {:04X}: {:?}", address, instr);
                }
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Bnz(rel) => {
                let address = rel8(*rel, next);
                if address == 0x31C2 {
                    println!("printing label {:04X}: {:?}", address, instr);
                }
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Bp(_dir, _b3, rel) => {
                let address = rel8(*rel, next);
                if address == 0x31C2 {
                    println!("printing label {:04X}: {:?}", address, instr);
                }
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Bpc(_dir, _b3, rel) => {
                let address = rel8(*rel, next);
                if address == 0x31C2 {
                    println!("printing label {:04X}: {:?}", address, instr);
                }
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Bn(_dir, _b3, rel) => {
                let address = rel8(*rel, next);
                if address == 0x31C2 {
                    println!("printing label {:04X}: {:?}", address, instr);
                }
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Dbnz_d9(_dir, rel) => {
                let address = rel8(*rel, next);
                if address == 0x31C2 {
                    println!("printing label {:04X}: {:?}", address, instr);
                }
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Dbnz_Ri(_ind, rel) => {
                let address = rel8(*rel, next);
                if address == 0x31C2 {
                    println!("printing label {:04X}: {:?}", address, instr);
                }
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Be_i8(_imm, rel) => {
                let address = rel8(*rel, next);
                if address == 0x31C2 {
                    println!("printing label {:04X}: {:?}", address, instr);
                }
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Be_d9(_dir, rel) => {
                let address = rel8(*rel, next);
                if address == 0x31C2 {
                    println!("printing label {:04X}: {:?}", address, instr);
                }
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Be_Rj(_ind, _imm, rel) => {
                let address = rel8(*rel, next);
                if address == 0x31C2 {
                    println!("printing label {:04X}: {:?}", address, instr);
                }
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Bne_i8(_imm, rel) => {
                let address = rel8(*rel, next);
                if address == 0x31C2 {
                    println!("printing label {:04X}: {:?}", address, instr);
                }
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Bne_d9(_dir, rel) => {
                let address = rel8(*rel, next);
                if address == 0x31C2 {
                    println!("printing label {:04X}: {:?}", address, instr);
                }
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }

            },
            Bne_Rj(_ind, _imm, rel) => {
                let address = rel8(*rel, next);
                if address == 0x31C2 {
                    println!("printing label {:04X}: {:?}", address, instr);
                }
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },

            Call(a12) => {
                let top_bits = next & 0xF000;
                let bottom_bits = (*a12 as usize) & 0x0FFF;
                let address = (top_bits | bottom_bits) as usize;
                if address == 0x31C2 {
                    println!("printing label {:04X}: {:?}", address, instr);
                }
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Callf(a16) => {
                let address = *a16 as usize;
                if address == 0x31C2 {
                    println!("printing label {:04X}: {:?}", address, instr);
                }
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Callr(r16) => {
                let address = rel16(*r16, next);
                if address == 0x31C2 {
                    println!("printing label {:04X}: {:?}", address, instr);
                }
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },

            Ret => (),
            Reti => (),

            Clr1(_dir, _b3) => (),
            Set1(_dir, _b3) => (),
            Not1(_dir, _b3) => (),

            Nop => (),
            Ldf => (),
            Stf => ()
        }
    }
    results
}

#[derive(Debug,Eq,PartialEq,Ord,PartialOrd,Hash,Clone,Copy)]
enum Pos {
    One(usize),
    Two(usize, usize),
    None
}

fn follow(
    pos: usize,
    seen: &mut Vec<bool>,
    instrs: &mut HashMap<usize,Instr<i32,u8>>,
    bytes: &[u8]) -> Result<Pos,DisasmError> {
    if pos >= bytes.len() || seen[pos] {
        println!("{:04X} ++ exit because seen", pos);
        return Ok(Pos::None);
    }
    println!("follow({:04X})", pos);
    seen[pos] = true;

    let decoded = Instr::<i32,u8>::decode(&bytes[pos..bytes.len()]);
    if pos == 12750 || pos == 12752 {
        println!("{:04X} -- {:?}", pos, decoded);
    }
    match decoded {
        Some(instr) => {
            let size = instr.size();
            let next = pos + size;
            if let Instr::Ld_d9(92) = instr {
                println!("{:05X} XYZ {:?}", pos, instr);
            }
            instrs.insert(pos, instr.clone());
            for idx in 0..size {
                println!("{:04X} && {:04X} - {:?}", pos, pos + idx, instr);
                seen[pos + idx] = true;
            }
            use instruction::Instr::*;
            let results = match instr {
                Add_i8(_) => Ok(Pos::One(next)),
                Add_d9(_) => Ok(Pos::One(next)),
                Add_Ri(_) => Ok(Pos::One(next)),

                Addc_i8(_) => Ok(Pos::One(next)),
                Addc_d9(_) => Ok(Pos::One(next)),
                Addc_Ri(_) => Ok(Pos::One(next)),

                Sub_i8(_) => Ok(Pos::One(next)),
                Sub_d9(_) => Ok(Pos::One(next)),
                Sub_Ri(_) => Ok(Pos::One(next)),

                Subc_i8(_) => Ok(Pos::One(next)),
                Subc_d9(_) => Ok(Pos::One(next)),
                Subc_Ri(_) => Ok(Pos::One(next)),

                Inc_d9(_) => Ok(Pos::One(next)),
                Inc_Ri(_) => Ok(Pos::One(next)),

                Dec_d9(_) => Ok(Pos::One(next)),
                Dec_Ri(_) => Ok(Pos::One(next)),

                Mul => Ok(Pos::One(next)),
                Div => Ok(Pos::One(next)),

                And_i8(_) => Ok(Pos::One(next)),
                And_d9(_) => Ok(Pos::One(next)),
                And_Ri(_) => Ok(Pos::One(next)),

                Or_i8(_) => Ok(Pos::One(next)),
                Or_d9(_) => Ok(Pos::One(next)),
                Or_Ri(_) => Ok(Pos::One(next)),

                Xor_i8(_) => Ok(Pos::One(next)),
                Xor_d9(_) => Ok(Pos::One(next)),
                Xor_Ri(_) => Ok(Pos::One(next)),

                Rol => Ok(Pos::One(next)),
                Rolc => Ok(Pos::One(next)),

                Ror => Ok(Pos::One(next)),
                Rorc => Ok(Pos::One(next)),

                Ld_d9(_) => Ok(Pos::One(next)),
                Ld_Ri(_) => Ok(Pos::One(next)),

                St_d9(_) => Ok(Pos::One(next)),
                St_Ri(_) => Ok(Pos::One(next)),

                Mov_d9(_, _) => Ok(Pos::One(next)),
                Mov_Rj(_, _) => Ok(Pos::One(next)),

                Ldc => Ok(Pos::One(next)),

                Push(_) => Ok(Pos::One(next)),
                Pop(_) => Ok(Pos::One(next)),

                Xch_d9(_) => Ok(Pos::One(next)),
                Xch_Ri(_) => Ok(Pos::One(next)),

                Jmp(abs) => {
                    let top_bits = next & 0xF000;
                    let bottom_bits = (abs as usize) & 0x0FFF;
                    let address = top_bits | bottom_bits;
                    Ok(Pos::One(address))
                },
                Jmpf(abs) => {
                    if pos >= 2 {
                        // If the previous instruction was `not1 ext,0`,
                        // then this jmpf is jumping to flash, not to the
                        // same ROM. So don't follow the address.
                        if let Some(Instr::Not1(0x10D, 0)) = instrs.get(&(pos - 2)) {
                        // if let Some(Instr::Not1(0x10D, 0)) = Instr::<i32,u8>::decode(&bytes[(pos - 2)..]) {
                            println!("Skipping {:04X}", abs as usize);
                            return Ok(Pos::None)
                        }
                    }
                    let address = abs as usize;
                    Ok(Pos::One(address))
                },

                Br(rel) => {
                    let address = rel8(rel, next);
                    Ok(Pos::One(address as usize))
                },
                Brf(rel) => {
                    let address = rel8(rel, next);
                    println!("{:04X} flw brf {:04X}", pos, address);
                    Ok(Pos::One(address as usize))
                },
                Bz(rel) => {
                    let address = rel8(rel, next);
                    Ok(Pos::Two(address as usize, next))
                },
                Bnz(rel) => {
                    let address = rel8(rel, next);
                    Ok(Pos::Two(address as usize, next))
                },
                Bp(_dir, _b3, rel) => {
                    let address = rel8(rel, next);
                    Ok(Pos::Two(address as usize, next))
                },
                Bpc(_dir, _b3, rel) => {
                    let address = rel8(rel, next);
                    Ok(Pos::Two(address as usize, next))
                },
                Bn(_dir, _b3, rel) => {
                    let address = rel8(rel, next);
                    Ok(Pos::Two(address as usize, next))
                },
                Dbnz_d9(_dir, rel) => {
                    let address = rel8(rel, next);
                    Ok(Pos::Two(address as usize, next))
                },
                Dbnz_Ri(_ind, rel) => {
                    let address = rel8(rel, next);
                    Ok(Pos::Two(address as usize, next))
                },
                Be_i8(_imm, rel) => {
                    let address = rel8(rel, next);
                    Ok(Pos::Two(address as usize, next))
                },
                Be_d9(_dir, rel) => {
                    let address = rel8(rel, next);
                    Ok(Pos::Two(address as usize, next))
                },
                Be_Rj(_ind, _imm, rel) => {
                    let address = rel8(rel, next);
                    Ok(Pos::Two(address as usize, next))
                },
                Bne_i8(_imm, rel) => {
                    let address = rel8(rel, next);
                    Ok(Pos::Two(address as usize, next))
                },
                Bne_d9(_dir, rel) => {
                    let address = rel8(rel, next);
                    Ok(Pos::Two(address as usize, next))
                },
                Bne_Rj(_ind, _imm, rel) => {
                    let address = rel8(rel, next);
                    Ok(Pos::Two(address as usize, next))
                },

                Call(a12) => {
                    let top_bits = next & 0xF000;
                    let bottom_bits = (a12 as usize) & 0x0FFF;
                    let address = top_bits | bottom_bits;
                    Ok(Pos::Two(address, next))
                },
                Callf(a16) => {
                    let address = a16 as usize;
                    Ok(Pos::Two(address, next))
                },
                Callr(r16) => {
                    let address = rel16(r16, next);
                    Ok(Pos::Two(address as usize, next))
                },

                Ret => Ok(Pos::None),
                Reti => Ok(Pos::None),

                Clr1(_dir, _b3) => Ok(Pos::One(next)),
                Set1(_dir, _b3) => Ok(Pos::One(next)),
                Not1(_dir, _b3) => Ok(Pos::One(next)),

                Nop => Ok(Pos::One(next)),
                Ldf => Ok(Pos::One(next)),
                Stf => Ok(Pos::One(next))
            };
            if let Instr::Ld_d9(92) = instr {
                println!("{:04X} {:?}, {:?}", pos, instr, results);
            }
            results
        },
        None => Err(DisasmError::NoInstruction(pos, bytes[pos]))
    }
}

fn rel16(rel: i32, next: usize) -> usize {
    use std::mem;
    unsafe {
        let r16 = mem::transmute::<u16, i16>(rel as u16);
        ((r16 as i32) + (next as i32) - 1) as usize
    }
}

fn rel8(rel: i32, next: usize) -> usize {
    use std::mem;
    unsafe {
        let r8 = mem::transmute::<u8, i8>(rel as u8);
        ((r8 as i32) + (next as i32)) as usize
    }
}

fn targets(pos: usize, instr: &Instr<i32,u8>) -> Vec<usize> {
    let next = pos + instr.size();
    use instruction::Instr::*;
    match instr {
        Add_i8(_) => vec![next],
        Add_d9(_) => vec![next],
        Add_Ri(_) => vec![next],

        Addc_i8(_) => vec![next],
        Addc_d9(_) => vec![next],
        Addc_Ri(_) => vec![next],

        Sub_i8(_) => vec![next],
        Sub_d9(_) => vec![next],
        Sub_Ri(_) => vec![next],

        Subc_i8(_) => vec![next],
        Subc_d9(_) => vec![next],
        Subc_Ri(_) => vec![next],

        Inc_d9(_) => vec![next],
        Inc_Ri(_) => vec![next],

        Dec_d9(_) => vec![next],
        Dec_Ri(_) => vec![next],

        Mul => vec![next],
        Div => vec![next],

        And_i8(_) => vec![next],
        And_d9(_) => vec![next],
        And_Ri(_) => vec![next],

        Or_i8(_) => vec![next],
        Or_d9(_) => vec![next],
        Or_Ri(_) => vec![next],

        Xor_i8(_) => vec![next],
        Xor_d9(_) => vec![next],
        Xor_Ri(_) => vec![next],

        Rol => vec![next],
        Rolc => vec![next],

        Ror => vec![next],
        Rorc => vec![next],

        Ld_d9(_) => vec![next],
        Ld_Ri(_) => vec![next],

        St_d9(_) => vec![next],
        St_Ri(_) => vec![next],

        Mov_d9(_, _) => vec![next],
        Mov_Rj(_, _) => vec![next],

        Ldc => vec![next],

        Push(_) => vec![next],
        Pop(_) => vec![next],

        Xch_d9(_) => vec![next],
        Xch_Ri(_) => vec![next],

        Jmp(abs) => {
            let top_bits = next & 0xF000;
            let bottom_bits = (*abs as usize) & 0x0FFF;
            let address = top_bits | bottom_bits;
            vec![address]
        },
        Jmpf(abs) => {
            // if pos >= 2 {
            //     // If the previous instruction was `not1 ext,0`,
            //     // then this jmpf is jumping to flash, not to the
            //     // same ROM. So don't follow the address.
            //     if let Some(Instr::Not1(0x10D, 0)) = instrs.get(&(pos - 2)) {
            //         // if let Some(Instr::Not1(0x10D, 0)) = Instr::<i32,u8>::decode(&bytes[(pos - 2)..]) {
            //         return Ok(vec![])
            //     }
            // }
            let address = *abs as usize;
            vec![address]
        },

        Br(rel) => {
            let address = rel8(*rel, next);
            vec![address]
        },
        Brf(rel) => {
            let address = rel8(*rel, next);
            vec![address]
        },
        Bz(rel) => {
            let address = rel8(*rel, next);
            vec![address, next]
        },
        Bnz(rel) => {
            let address = rel8(*rel, next);
            vec![address, next]
        },
        Bp(_dir, _b3, rel) => {
            let address = rel8(*rel, next);
            vec![address, next]
        },
        Bpc(_dir, _b3, rel) => {
            let address = rel8(*rel, next);
            vec![address, next]
        },
        Bn(_dir, _b3, rel) => {
            let address = rel8(*rel, next);
            vec![address, next]
        },
        Dbnz_d9(_dir, rel) => {
            let address = rel8(*rel, next);
            vec![address, next]
        },
        Dbnz_Ri(_ind, rel) => {
            let address = rel8(*rel, next);
            vec![address, next]
        },
        Be_i8(_imm, rel) => {
            let address = rel8(*rel, next);
            vec![address, next]
        },
        Be_d9(_dir, rel) => {
            let address = rel8(*rel, next);
            vec![address, next]
        },
        Be_Rj(_ind, _imm, rel) => {
            let address = rel8(*rel, next);
            vec![address, next]
        },
        Bne_i8(_imm, rel) => {
            let address = rel8(*rel, next);
            vec![address, next]
        },
        Bne_d9(_dir, rel) => {
            let address = rel8(*rel, next);
            vec![address, next]
        },
        Bne_Rj(_ind, _imm, rel) => {
            let address = rel8(*rel, next);
            vec![address, next]
        },

        Call(a12) => {
            let top_bits = next & 0xF000;
            let bottom_bits = (*a12 as usize) & 0x0FFF;
            let address = top_bits | bottom_bits;
            vec![address, next]
        },
        Callf(a16) => {
            let address = *a16 as usize;
            vec![address, next]
        },
        Callr(r16) => {
            let address = rel16(*r16, next);
            vec![address, next]
        },

        Ret => vec![],
        Reti => vec![],

        Clr1(_dir, _b3) => vec![next],
        Set1(_dir, _b3) => vec![next],
        Not1(_dir, _b3) => vec![next],

        Nop => vec![next],
        Ldf => vec![next],
        Stf => vec![next]
    }
}

fn build_instruction_graph(entry_points: &[usize], bytes: &[u8]) -> InstructionGraph {
    let mut positions: Vec<usize> = entry_points.iter().map(|x| *x).collect();
    let mut locs = vec![];
    let mut graph = InstructionGraph::new();
    while !positions.is_empty() {
        locs.clear();
        locs.append(&mut positions);

        for pos in locs.iter() {
            let decoded = Instr::<i32,u8>::decode(&bytes[*pos..bytes.len()]);
            match decoded {
                Some(instr) => {
                    let targets = targets(*pos, &instr);
                    for target in targets.iter() {
                        graph.jump_to(*pos, *target);
                    }
                    graph.instr(*pos, instr);
                    for target in targets.iter() {
                        if !graph.contains_instr(*target) {
                            positions.push(*target);
                        }
                    }
                },
                None => eprintln!("No instruction at {:04X}", pos)
            }
        }
    }
    graph
}

struct InstructionGraph {
    instrs: HashMap<usize,Instr<i32,u8>>,
    jump_to: HashMap<usize,HashSet<usize>>,
    jump_from: HashMap<usize,HashSet<usize>>
}

impl InstructionGraph {
    pub fn new() -> InstructionGraph {
        InstructionGraph {
            instrs: HashMap::new(),
            jump_to: HashMap::new(),
            jump_from: HashMap::new()
        }
    }

    pub fn instr(&mut self, pos: usize, instr: Instr<i32,u8>) -> &Instr<i32,u8> {
        self.instrs.insert(pos, instr);
        self.instrs.get(&pos).unwrap()
    }

    pub fn jump_to(&mut self, from: usize, to: usize) {
        if !self.jump_to.contains_key(&from) {
            self.jump_to.insert(from, HashSet::new());
        }
        self.jump_to.get_mut(&from).unwrap().insert(to);
        if !self.jump_from.contains_key(&to) {
            self.jump_from.insert(to, HashSet::new());
        }
        self.jump_from.get_mut(&to).unwrap().insert(from);
    }

    pub fn contains_instr(&self, pos: usize) -> bool {
        self.instrs.contains_key(&pos)
    }
}

impl fmt::Display for InstructionGraph {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "digraph {{")?;
        writeln!(f, "  forcelabels=true;")?;
        for pos in self.instrs.keys() {
            writeln!(f, "  L{:04X} [label=\"{:?}\"];", pos, self.instrs[&pos])?;
        }

        // for pos in self.instrs.keys() {
        //     match self.jump_to.get(&pos) {
        //         Some(set) => {
        //             for loc in set {
        //                 writeln!(f, "  L{:04X} -> L{:04X};", pos, loc);
        //             }
        //         },
        //         None => {
        //             if !self.jump_from.contains_key(&pos) {
        //                 writeln!(f, "  L{:04X};", pos);
        //             }
        //         }
        //     }
        // }
        writeln!(f, "}}")
    }
}
