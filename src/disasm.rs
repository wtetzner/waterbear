
use instruction::{Instr,IndirectionMode};
use std::collections::HashMap;
use ast::{Statements,Statement,Directive};
use location::{Span};
use expression::{Expr};
use std;

#[derive(Debug)]
pub enum DisasmError {
    NoInstruction(usize, u8),
    NoSuchFile(String, std::io::Error)
}

pub fn disassemble(entry_points: &[usize], bytes: &[u8]) -> Result<Statements,DisasmError> {
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

    statements.push(Statement::comment("\n"));
    for pos in 0..bytes.len() {
        if labels.contains_key(&pos) {
            statements.push(Statement::label(&labels[&pos]));
        }
        if instrs.contains_key(&pos) {
            let instr = &instrs[&pos];
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
                    let address = rel8(*rel, next); //(rel + (next as i32)) as usize;
                    let addr_expr = label_expr(&labels, address);
                    Statement::instr(Instr::Br(addr_expr))
                },
                Brf(rel) => {
                    let address = (rel + (next as i32) - 1) as usize;
                    let addr_expr = label_expr(&labels, address);
                    Statement::instr(Instr::Brf(addr_expr))
                },
                Bz(rel) => {
                    let address = (rel + (next as i32)) as usize;
                    let addr_expr = label_expr(&labels, address);
                    Statement::instr(Instr::Bz(addr_expr))
                },
                Bnz(rel) => {
                    let address = rel8(*rel, next);// (rel + (next as i32)) as usize;
                    let addr_expr = label_expr(&labels, address);
                    Statement::instr(Instr::Bnz(addr_expr))
                },
                Bp(dir, b3, rel) => {
                    let address = (rel + (next as i32)) as usize;
                    let addr_expr = label_expr(&labels, address);
                    let d9 = mem_expr(&aliases, *dir);
                    let bits = bits_expr(&bit_aliases, *dir, *b3);
                    Statement::instr(Instr::Bp(d9, bits, addr_expr))
                },
                Bpc(dir, b3, rel) => {
                    let address = (rel + (next as i32)) as usize;
                    let addr_expr = label_expr(&labels, address);
                    let d9 = mem_expr(&aliases, *dir);
                    let bits = bits_expr(&bit_aliases, *dir, *b3);
                    Statement::instr(Instr::Bpc(d9, bits, addr_expr))
                },
                Bn(dir, b3, rel) => {
                    let address = (rel + (next as i32)) as usize;
                    let addr_expr = label_expr(&labels, address);
                    let d9 = mem_expr(&aliases, *dir);
                    let bits = bits_expr(&bit_aliases, *dir, *b3);
                    Statement::instr(Instr::Bn(d9, bits, addr_expr))
                },
                Dbnz_d9(dir, rel) => {
                    let address = (rel + (next as i32)) as usize;
                    let addr_expr = label_expr(&labels, address);
                    let d9 = mem_expr(&aliases, *dir);
                    Statement::instr(Instr::Dbnz_d9(d9, addr_expr))
                },
                Dbnz_Ri(ind, rel) => {
                    let address = (rel + (next as i32)) as usize;
                    let addr_expr = label_expr(&labels, address);
                    let im = IndirectionMode::from(*ind);
                    Statement::instr(Instr::Dbnz_Ri(im, addr_expr))
                },
                Be_i8(imm, rel) => {
                    let address = (rel + (next as i32)) as usize;
                    let addr_expr = label_expr(&labels, address);
                    Statement::instr(Instr::Be_i8(Expr::num(*imm), addr_expr))
                },
                Be_d9(dir, rel) => {
                    let address = (rel + (next as i32)) as usize;
                    let addr_expr = label_expr(&labels, address);
                    let d9 = mem_expr(&aliases, *dir);
                    Statement::instr(Instr::Be_d9(d9, addr_expr))
                },
                Be_Rj(ind, imm, rel) => {
                    let address = (rel + (next as i32)) as usize;
                    let addr_expr = label_expr(&labels, address);
                    let im = IndirectionMode::from(*ind);
                    Statement::instr(Instr::Be_Rj(im, Expr::num(*imm), addr_expr))
                },
                Bne_i8(imm, rel) => {
                    let address = (rel + (next as i32)) as usize;
                    let addr_expr = label_expr(&labels, address);
                    Statement::instr(Instr::Bne_i8(Expr::num(*imm), addr_expr))
                },
                Bne_d9(dir, rel) => {
                    let address = (rel + (next as i32)) as usize;
                    let addr_expr = label_expr(&labels, address);
                    let d9 = mem_expr(&aliases, *dir);
                    Statement::instr(Instr::Bne_d9(d9, addr_expr))
                },
                Bne_Rj(ind, imm, rel) => {
                    let address = (rel + (next as i32)) as usize;
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
                    let address = (r16 + (next as i32) - 1) as usize;
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
        }
    }

    Ok(Statements { statements: statements })
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
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Jmpf(abs) => {
                let address = *abs as usize;
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },

            Br(rel) => {
                let address = (rel + (next as i32)) as usize;
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Brf(rel) => {
                let address = (rel + (next as i32) - 1) as usize;
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Bz(rel) => {
                let address = (rel + (next as i32)) as usize;
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Bnz(rel) => {
                let address = rel8(*rel, next);// (rel + (next as i32)) as usize;
                println!("Bnz address (rel={} {:02X}): {} {:04X}", rel, rel, address, address);
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Bp(_dir, _b3, rel) => {
                let address = (rel + (next as i32)) as usize;
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Bpc(_dir, _b3, rel) => {
                let address = (rel + (next as i32)) as usize;
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Bn(_dir, _b3, rel) => {
                let address = (rel + (next as i32)) as usize;
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Dbnz_d9(_dir, rel) => {
                let address = (rel + (next as i32)) as usize;
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Dbnz_Ri(_ind, rel) => {
                let address = (rel + (next as i32)) as usize;
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Be_i8(_imm, rel) => {
                let address = (rel + (next as i32)) as usize;
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Be_d9(_dir, rel) => {
                let address = (rel + (next as i32)) as usize;
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Be_Rj(_ind, _imm, rel) => {
                let address = (rel + (next as i32)) as usize;
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Bne_i8(_imm, rel) => {
                let address = (rel + (next as i32)) as usize;
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Bne_d9(_dir, rel) => {
                let address = (rel + (next as i32)) as usize;
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }

            },
            Bne_Rj(_ind, _imm, rel) => {
                let address = (rel + (next as i32)) as usize;
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },

            Call(a12) => {
                let top_bits = next & 0xF000;
                let bottom_bits = (*a12 as usize) & 0x0FFF;
                let address = (top_bits | bottom_bits) as usize;
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Callf(a16) => {
                let address = *a16 as usize;
                if !results.contains_key(&address) {
                    results.insert(address, format!("label_{:04X}", address));
                }
            },
            Callr(r16) => {
                let address = (r16 + (next as i32) - 1) as usize;
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
        return Ok(Pos::None);
    }
    seen[pos] = true;

    match Instr::<i32,u8>::decode(&bytes[pos..bytes.len()]) {
        Some(instr) => {
            let size = instr.size();
            let next = pos + size;
            instrs.insert(pos, instr.clone());
            for idx in 0..size {
                seen[pos + idx] = true;
            }
            use instruction::Instr::*;
            match instr {
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
                    let address = abs as usize;
                    Ok(Pos::One(address))
                },

                Br(rel) => {
                    let address = rel + (next as i32);
                    Ok(Pos::One(address as usize))
                },
                Brf(rel) => {
                    let address = rel + (next as i32) - 1;
                    Ok(Pos::One(address as usize))
                },
                Bz(rel) => {
                    let address = rel + (next as i32);
                    Ok(Pos::Two(address as usize, next))
                },
                Bnz(rel) => {
                    let address = rel8(rel, next); //rel + (next as i32);
                    Ok(Pos::Two(address as usize, next))
                },
                Bp(_dir, _b3, rel) => {
                    let address = rel + (next as i32);
                    Ok(Pos::Two(address as usize, next))
                },
                Bpc(_dir, _b3, rel) => {
                    let address = rel + (next as i32);
                    Ok(Pos::Two(address as usize, next))
                },
                Bn(_dir, _b3, rel) => {
                    let address = rel + (next as i32);
                    Ok(Pos::Two(address as usize, next))
                },
                Dbnz_d9(_dir, rel) => {
                    let address = rel + (next as i32);
                    Ok(Pos::Two(address as usize, next))
                },
                Dbnz_Ri(_ind, rel) => {
                    let address = rel + (next as i32);
                    Ok(Pos::Two(address as usize, next))
                },
                Be_i8(_imm, rel) => {
                    let address = rel + (next as i32);
                    Ok(Pos::Two(address as usize, next))
                },
                Be_d9(_dir, rel) => {
                    let address = rel + (next as i32);
                    Ok(Pos::Two(address as usize, next))
                },
                Be_Rj(_ind, _imm, rel) => {
                    let address = rel + (next as i32);
                    Ok(Pos::Two(address as usize, next))
                },
                Bne_i8(_imm, rel) => {
                    let address = rel + (next as i32);
                    Ok(Pos::Two(address as usize, next))
                },
                Bne_d9(_dir, rel) => {
                    let address = rel + (next as i32);
                    Ok(Pos::Two(address as usize, next))
                },
                Bne_Rj(_ind, _imm, rel) => {
                    let address = rel + (next as i32);
                    Ok(Pos::Two(address as usize, next))
                },

                Call(a12) => {
                    let top_bits = next & 0xF000;
                    let bottom_bits = (a12 as usize) & 0x0FFF;
                    let address = top_bits | bottom_bits;
                    Ok(Pos::One(address))
                },
                Callf(a16) => {
                    let address = a16 as usize;
                    Ok(Pos::One(address))
                },
                Callr(r16) => {
                    let address = r16 + (next as i32) - 1;
                    Ok(Pos::One(address as usize))
                },

                Ret => Ok(Pos::None),
                Reti => Ok(Pos::None),

                Clr1(_dir, _b3) => Ok(Pos::One(next)),
                Set1(_dir, _b3) => Ok(Pos::One(next)),
                Not1(_dir, _b3) => Ok(Pos::One(next)),

                Nop => Ok(Pos::One(next)),
                Ldf => Ok(Pos::One(next)),
                Stf => Ok(Pos::One(next))
            }
        },
        None => Err(DisasmError::NoInstruction(pos, bytes[pos]))
    }
}

fn rel16(rel: i32, next: usize) -> usize {
    use std::mem;
    unsafe {
        let r16 = mem::transmute::<u8, i8>(rel as u16);
        ((r16 as i32) + (next as i32) - 1) as usize
    }
}

fn rel8(rel: i32, next: usize) -> usize {
    use std::mem;
    unsafe {
        let r8 = mem::transmute::<u8, i8>(rel as u8);
        println!("rel: {}, r8: {}", rel, r8);
        ((r8 as i32) + (next as i32)) as usize
    }
}
