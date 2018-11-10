
use instruction::Instr;
use std::collections::HashMap;
use std::collections::HashSet;
use ast::{Statement,Directive,ByteValue};
use location::{Span};
use expression::{Expr,IndirectionMode};
use std;

#[derive(Debug)]
pub enum DisasmError {
    NoInstruction(usize, u8),
    NoSuchFile(String, std::io::Error)
}

pub fn disassemble(
    arrived_from: bool,
    entry_points: &[usize],
    bytes: &[u8]) -> Result<DStatements,DisasmError> {
    let (graph, names) = {
        let graph = build_instruction_graph(entry_points, bytes)?;
        let names = build_names(&graph);
        (graph.lift_instrs(lift_instr, &names), names)
    };
    let entry_points = {
        let mut results: HashSet<usize> = HashSet::new();
        for entry_point in entry_points {
            results.insert(*entry_point);
        }
        results
    };

    let mask = build_byte_mask(bytes, &graph);
    let mut written_mask = vec![false; bytes.len()];

    let mut stmts = DStatements::new();
    let mut pos = 0;
    while pos < bytes.len() {
        if entry_points.contains(&pos) {
            stmts.push_comment("");
            stmts.push_comment("Entry point");
            stmts.push_org(pos);
        }

        if let Some(instr) = graph.instrs().get(&pos) {
            if pos > 0 && !mask[pos - 1] && !entry_points.contains(&pos) {
                stmts.push_comment("");
                if !written_mask[pos - 1] {
                    stmts.push_org(pos);
                }
            }
            if names.contains_label(pos) {
                stmts.push_label(names.label(pos).unwrap());
            }
            let overlaps = graph.find_overlaps(pos);
            if !overlaps.is_empty() {
                let mut comment = "Overlaps with: ".to_owned();
                let mut first = true;
                for overlap in overlaps {
                    if first {
                        first = false;
                    } else {
                        comment.push_str(", ");
                    }
                    comment.push_str(format!("{:04X}", overlap).as_str());
                }
                stmts.push_comment(comment.as_str());
            }
            let comment = if let Some(set) = graph.jump_from.get(&pos) {
                if arrived_from && !set.is_empty() {
                    let mut vec: Vec<usize> = set.iter().map(|v| *v).collect();
                    vec.sort();
                    let mut comment = "Arrived from: ".to_owned();
                    let mut first = true;
                    for from in vec {
                        if first {
                            first = false;
                        } else {
                            comment.push_str(", ");
                        }
                        comment.push_str(format!("{:04X}", from).as_str());
                    }
                    Some(comment)
                } else {
                    None
                }
            } else {
                None
            };
            match comment {
                Some(cmt) => stmts.push_instr_cmt(pos, instr.clone(), cmt),
                None => stmts.push_instr(pos, instr.clone())
            }
            pos = pos + 1;
        } else {
            if !mask[pos] {
                let bytes = byte_range(bytes, &mask, pos);
                let chunk = read_chunk(bytes, 16);
                if !all_zeros(chunk) {
                    if pos > 0 && mask[pos - 1] {
                        stmts.push_comment("");
                    }
                    if pos > 0 && !written_mask[pos - 1] && !mask[pos - 1] {
                        stmts.push_comment("");
                        stmts.push_org(pos);
                    }
                    stmts.push_bytes(pos, chunk);
                    for (idx, _) in chunk.iter().enumerate() {
                        written_mask[pos + idx] = true;
                    }
                } else {
                    
                }
                pos = pos + chunk.len();
            } else {
                pos = pos + 1;
            }
        }
    }

    stmts.push_comment("\nPad size of binary");
    stmts.push_cnop(bytes.len() % 0x200, 0x200);

    stmts.push_comment("\nPSW bits");
    for (name, value) in names.sorted_psw_bits() {
        stmts.push_alias(name.as_str(), Expr::num(value as i32));
    }

    stmts.push_comment("\nSpecial Function Registers");
    for (name, value) in names.sorted_aliases() {
        stmts.push_alias(name.as_str(), Expr::num(value as i32));
    }

    Ok(stmts)
}

// pub fn trace_instr(entry_points: &[usize], bytes: &[u8]) -> Result<,DisasmError> {
// }

fn all_zeros(bytes: &[u8]) -> bool {
    for byte in bytes.iter() {
        if *byte != 0 {
            return false;
        }
    }
    true
}

fn read_chunk<'a>(bytes: &'a [u8], size: usize) -> &'a [u8] {
    let mut end = 0;
    while end < bytes.len() && end < size {
        end = end + 1;
    }
    &bytes[0..end]
}

fn byte_range<'a>(bytes: &'a [u8], mask: &[bool], pos: usize) -> &'a [u8] {
    let mut end = pos;
    while end < bytes.len() && !mask[end] {
        end = end + 1;
    }
    &bytes[pos..end]
}

fn label_expr(names: &Names, addr: usize) -> Expr {
    names.label(addr).map(|name| Expr::name(name)).unwrap_or(Expr::num(addr as i32))
}

fn mem_expr(names: &Names, addr: i32) -> Expr {
    names.alias(addr as usize).map(|name| Expr::name(name)).unwrap_or(Expr::num(addr as i32))
}

fn bits_expr(names: &Names, mem: i32, addr: i32) -> Expr {
    if mem == 0x0101 {
        names.psw_bit(addr as usize).map(|name| Expr::name(name)).unwrap_or(Expr::num(addr as i32))
    } else {
        Expr::num(addr as i32)
    }
}

fn build_byte_mask<Ex,IM>(bytes: &[u8], graph: &InstructionGraph<Ex,IM>) -> Vec<bool> {
    let mut seen: Vec<bool> = vec![false; bytes.len()];
    for pos in 0..bytes.len() {
        if let Some(instr) = graph.instrs().get(&pos) {
            for idx in 0..instr.size() {
                seen[pos + idx] = true;
            }
        }
    }
    seen
}

fn build_names(graph: &InstructionGraph<i32,u8>) -> Names {
    let mut names = Names::new();
    compute_bit_aliases(&mut names);
    compute_aliases(&mut names);
    compute_labels(graph, &mut names);
    names
}

fn compute_bit_aliases(names: &mut Names) {
    names.push_psw_bit("cy",     7);
    names.push_psw_bit("ac",     6);
    names.push_psw_bit("irbk1",  4);
    names.push_psw_bit("irbk0",  3);
    names.push_psw_bit("ov",     2);
    names.push_psw_bit("rambk0", 1);
    names.push_psw_bit("p",      0);
}

fn compute_aliases(names: &mut Names) {
    names.push_alias("acc",    0x100);
    names.push_alias("psw",    0x101);
    names.push_alias("b",      0x102);
    names.push_alias("c",      0x103);
    names.push_alias("trl",    0x104);
    names.push_alias("trh",    0x105);
    names.push_alias("sp",     0x106);
    names.push_alias("pcon",   0x107);
    names.push_alias("ie",     0x108);
    names.push_alias("ip",     0x109);
    names.push_alias("ext",    0x10D);
    names.push_alias("ocr",    0x10E);
    names.push_alias("t0con",  0x110);
    names.push_alias("t0prr",  0x111);
    names.push_alias("t0l",    0x112);
    names.push_alias("t0lr",   0x113);
    names.push_alias("t0h",    0x114);
    names.push_alias("t0hr",   0x115);
    names.push_alias("t1cnt",  0x118);
    names.push_alias("t1lc",   0x11A);
    names.push_alias("t1l",    0x11B);
    names.push_alias("t1lr",   0x11B);
    names.push_alias("t1hc",   0x11C);
    names.push_alias("t1h",    0x11D);
    names.push_alias("t1hr",   0x11D);
    names.push_alias("mcr",    0x120);
    names.push_alias("stad",   0x122);
    names.push_alias("cnr",    0x123);
    names.push_alias("tdr",    0x124);
    names.push_alias("xbnk",   0x125);
    names.push_alias("vccr",   0x127);
    names.push_alias("scon0",  0x130);
    names.push_alias("sbuf0",  0x131);
    names.push_alias("sbr",    0x132);
    names.push_alias("scon1",  0x134);
    names.push_alias("sbuf1",  0x135);
    names.push_alias("p1",     0x144);
    names.push_alias("p1ddr",  0x145);
    names.push_alias("p1fcr",  0x146);
    names.push_alias("p3",     0x14C);
    names.push_alias("p3ddr",  0x14D);
    names.push_alias("p3int",  0x14E);
    names.push_alias("p7",     0x15C);
    names.push_alias("i01cr",  0x15D);
    names.push_alias("i23cr",  0x15E);
    names.push_alias("isl",    0x15F);
    names.push_alias("vsel",   0x163);
    names.push_alias("vrmad1", 0x164);
    names.push_alias("vrmad2", 0x165);
    names.push_alias("vtrbf",  0x166);
    names.push_alias("vlreg",  0x167);
    names.push_alias("btcr",   0x17F);
    names.push_alias("xram",   0x180);
    for loc in 0x100..=0x1FF {
        if !names.has_alias(loc) {
            if loc >= 0x180 && loc <= 0x1FB {
                names.push_alias(format!("xram_{:04x}", loc).as_str(), loc);
            } else {
                names.push_alias(format!("sfr_{:04x}", loc).as_str(), loc);
            }
        }
    }
}

fn compute_labels(graph: &InstructionGraph<i32,u8>, names: &mut Names) {
    for pos in graph.instrs().keys() {
        let instr = &graph.instrs()[pos];
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
                names.gen_label(address);
            },
            Jmpf(abs) => {
                let address = *abs as usize;
                let mut gen_label = true;
                if *pos >= 2 {
                    // If the previous instruction was `not1 ext,0`,
                    // then this jmpf is jumping to flash, not to the
                    // same ROM. So don't use a label here.
                    if let Some(Instr::Not1(0x10D, 0)) = graph.instrs().get(&(pos - 2)) {
                        if graph.instr_arrived_from(*pos, *pos - 2) {
                            gen_label = false;
                        }
                    }
                }
                if gen_label {
                    names.gen_label(address);
                }
            },

            Br(rel) => {
                let address = rel8(*rel, next);
                names.gen_label(address);
            },
            Brf(rel) => {
                let address = rel16(*rel, next);
                names.gen_label(address);
            },
            Bz(rel) => {
                let address = rel8(*rel, next);
                names.gen_label(address);
            },
            Bnz(rel) => {
                let address = rel8(*rel, next);
                names.gen_label(address);
            },
            Bp(_dir, _b3, rel) => {
                let address = rel8(*rel, next);
                names.gen_label(address);
            },
            Bpc(_dir, _b3, rel) => {
                let address = rel8(*rel, next);
                names.gen_label(address);
            },
            Bn(_dir, _b3, rel) => {
                let address = rel8(*rel, next);
                names.gen_label(address);
            },
            Dbnz_d9(_dir, rel) => {
                let address = rel8(*rel, next);
                names.gen_label(address);
            },
            Dbnz_Ri(_ind, rel) => {
                let address = rel8(*rel, next);
                names.gen_label(address);
            },
            Be_i8(_imm, rel) => {
                let address = rel8(*rel, next);
                names.gen_label(address);
            },
            Be_d9(_dir, rel) => {
                let address = rel8(*rel, next);
                names.gen_label(address);
            },
            Be_Rj(_ind, _imm, rel) => {
                let address = rel8(*rel, next);
                names.gen_label(address);
            },
            Bne_i8(_imm, rel) => {
                let address = rel8(*rel, next);
                names.gen_label(address);
            },
            Bne_d9(_dir, rel) => {
                let address = rel8(*rel, next);
                names.gen_label(address);
            },
            Bne_Rj(_ind, _imm, rel) => {
                let address = rel8(*rel, next);
                names.gen_label(address);
            },

            Call(a12) => {
                let top_bits = next & 0xF000;
                let bottom_bits = (*a12 as usize) & 0x0FFF;
                let address = (top_bits | bottom_bits) as usize;
                names.gen_label(address);
            },
            Callf(a16) => {
                let address = *a16 as usize;
                names.gen_label(address);
            },
            Callr(r16) => {
                let address = rel16(*r16, next);
                names.gen_label(address);
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
}

fn lift_instr(pos: usize, graph: &InstructionGraph<i32,u8>, instr: &Instr<i32,u8>, names: &Names) -> Instr<Expr,IndirectionMode> {
    let next = pos + instr.size();
    use instruction::Instr::*;
    match instr {
        Add_i8(imm) => Add_i8(Expr::num(*imm)),
        Add_d9(d9) => {
            let expr = mem_expr(names, *d9);
            Add_d9(expr)
        },
        Add_Ri(ri) => Add_Ri(IndirectionMode::from(*ri)),

        Addc_i8(imm) => Addc_i8(Expr::num(*imm)),
        Addc_d9(d9) => {
            let expr = mem_expr(names, *d9);
            Addc_d9(expr)
        },
        Addc_Ri(ri) => Addc_Ri(IndirectionMode::from(*ri)),

        Sub_i8(imm) => Sub_i8(Expr::num(*imm)),
        Sub_d9(d9) => {
            let expr = mem_expr(names, *d9);
            Sub_d9(expr)
        },
        Sub_Ri(ri) => Sub_Ri(IndirectionMode::from(*ri)),

        Subc_i8(imm) => Subc_i8(Expr::num(*imm)),
        Subc_d9(d9) => {
            let expr = mem_expr(names, *d9);
            Subc_d9(expr)
        },
        Subc_Ri(ri) => Subc_Ri(IndirectionMode::from(*ri)),

        Inc_d9(d9) => {
            let expr = mem_expr(names, *d9);
            Inc_d9(expr)
        },
        Inc_Ri(ri) => Inc_Ri(IndirectionMode::from(*ri)),

        Dec_d9(d9) => {
            let expr = mem_expr(names, *d9);
            Dec_d9(expr)
        },
        Dec_Ri(ri) => Dec_Ri(IndirectionMode::from(*ri)),

        Mul => Mul,
        Div => Div,

        And_i8(imm) => And_i8(Expr::num(*imm)),
        And_d9(d9) => {
            let expr = mem_expr(names, *d9);
            And_d9(expr)
        },
        And_Ri(ri) => And_Ri(IndirectionMode::from(*ri)),

        Or_i8(imm) => Or_i8(Expr::num(*imm)),
        Or_d9(d9) => {
            let expr = mem_expr(names, *d9);
            Or_d9(expr)
        },
        Or_Ri(ri) => Or_Ri(IndirectionMode::from(*ri)),

        Xor_i8(imm) => Xor_i8(Expr::num(*imm)),
        Xor_d9(d9) => {
            let expr = mem_expr(names, *d9);
            Xor_d9(expr)
        },
        Xor_Ri(ri) => Xor_Ri(IndirectionMode::from(*ri)),

        Rol => Rol,
        Rolc => Rolc,

        Ror => Ror,
        Rorc => Rorc,

        Ld_d9(d9) => {
            let expr = mem_expr(names, *d9);
            Ld_d9(expr)
        },
        Ld_Ri(ri) => Ld_Ri(IndirectionMode::from(*ri)),

        St_d9(d9) => {
            let expr = mem_expr(names, *d9);
            St_d9(expr)
        },
        St_Ri(ri) => St_Ri(IndirectionMode::from(*ri)),

        Mov_d9(imm, d9) => {
            let expr = mem_expr(names, *d9);
            Mov_d9(Expr::num(*imm), expr)
        },
        Mov_Rj(imm, ri) => Mov_Rj(Expr::num(*imm), IndirectionMode::from(*ri)),

        Ldc => Ldc,

        Push(d9) => {
            let expr = mem_expr(names, *d9);
            Push(expr)
        },
        Pop(d9) => {
            let expr = mem_expr(names, *d9);
            Pop(expr)
        },

        Xch_d9(d9) => {
            let expr = mem_expr(names, *d9);
            Xch_d9(expr)
        },
        Xch_Ri(ri) => Xch_Ri(IndirectionMode::from(*ri)),

        Jmp(abs) => {
            let top_bits = next & 0xF000;
            let bottom_bits = (*abs as usize) & 0x0FFF;
            let address = (top_bits | bottom_bits) as usize;
            let addr_expr = label_expr(names, address);
            Jmp(addr_expr)
        },
        Jmpf(abs) => {
            let address = *abs as usize;
            if pos >= 2 {
                // If the previous instruction was `not1 ext,0`,
                // then this jmpf is jumping to flash, not to the
                // same ROM. So don't use a label here.
                if let Some(Not1(0x10D, 0)) = graph.instrs().get(&(pos - 2)) {
                    if graph.instr_arrived_from(pos, pos - 2) {
                        return Jmpf(Expr::num(address as i32))
                    }
                }
            }
            let addr_expr = label_expr(names, address);
            Jmpf(addr_expr)
        },

        Br(rel) => {
            let address = rel8(*rel, next);
            let addr_expr = label_expr(names, address);
            Br(addr_expr)
        },
        Brf(rel) => {
            let address = rel16(*rel, next);
            let addr_expr = label_expr(names, address);
            Brf(addr_expr)
        },
        Bz(rel) => {
            let address = rel8(*rel, next);
            let addr_expr = label_expr(names, address);
            Bz(addr_expr)
        },
        Bnz(rel) => {
            let address = rel8(*rel, next);
            let addr_expr = label_expr(names, address);
            Bnz(addr_expr)
        },
        Bp(dir, b3, rel) => {
            let address = rel8(*rel, next);
            let addr_expr = label_expr(names, address);
            let d9 = mem_expr(names, *dir);
            let bits = bits_expr(names, *dir, *b3);
            Bp(d9, bits, addr_expr)
        },
        Bpc(dir, b3, rel) => {
            let address = rel8(*rel, next);
            let addr_expr = label_expr(names, address);
            let d9 = mem_expr(names, *dir);
            let bits = bits_expr(names, *dir, *b3);
            Bpc(d9, bits, addr_expr)
        },
        Bn(dir, b3, rel) => {
            let address = rel8(*rel, next);
            let addr_expr = label_expr(names, address);
            let d9 = mem_expr(names, *dir);
            let bits = bits_expr(names, *dir, *b3);
            Bn(d9, bits, addr_expr)
        },
        Dbnz_d9(dir, rel) => {
            let address = rel8(*rel, next);
            let addr_expr = label_expr(names, address);
            let d9 = mem_expr(names, *dir);
            Dbnz_d9(d9, addr_expr)
        },
        Dbnz_Ri(ind, rel) => {
            let address = rel8(*rel, next);
            let addr_expr = label_expr(names, address);
            let im = IndirectionMode::from(*ind);
            Dbnz_Ri(im, addr_expr)
        },
        Be_i8(imm, rel) => {
            let address = rel8(*rel, next);
            let addr_expr = label_expr(names, address);
            Be_i8(Expr::num(*imm), addr_expr)
        },
        Be_d9(dir, rel) => {
            let address = rel8(*rel, next);
            let addr_expr = label_expr(names, address);
            let d9 = mem_expr(names, *dir);
            Be_d9(d9, addr_expr)
        },
        Be_Rj(ind, imm, rel) => {
            let address = rel8(*rel, next);
            let addr_expr = label_expr(names, address);
            let im = IndirectionMode::from(*ind);
            Be_Rj(im, Expr::num(*imm), addr_expr)
        },
        Bne_i8(imm, rel) => {
            let address = rel8(*rel, next);
            let addr_expr = label_expr(names, address);
            Bne_i8(Expr::num(*imm), addr_expr)
        },
        Bne_d9(dir, rel) => {
            let address = rel8(*rel, next);
            let addr_expr = label_expr(names, address);
            let d9 = mem_expr(names, *dir);
            Bne_d9(d9, addr_expr)
        },
        Bne_Rj(ind, imm, rel) => {
            let address = rel8(*rel, next);
            let addr_expr = label_expr(names, address);
            let im = IndirectionMode::from(*ind);
            Bne_Rj(im, Expr::num(*imm), addr_expr)
        },

        Call(a12) => {
            let top_bits = next & 0xF000;
            let bottom_bits = (*a12 as usize) & 0x0FFF;
            let address = (top_bits | bottom_bits) as usize;
            let addr_expr = label_expr(names, address);
            Call(addr_expr)
        },
        Callf(a16) => {
            let address = *a16 as usize;
            let addr_expr = label_expr(names, address);
            Callf(addr_expr)
        },
        Callr(r16) => {
            let address = rel16(*r16, next);
            let addr_expr = label_expr(names, address);
            Callr(addr_expr)
        },

        Ret => Ret,
        Reti => Reti,

        Clr1(dir, b3) => {
            let d9 = mem_expr(names, *dir);
            let bits = bits_expr(names, *dir, *b3);
            Clr1(d9, bits)
        },
        Set1(dir, b3) => {
            let d9 = mem_expr(names, *dir);
            let bits = bits_expr(names, *dir, *b3);
            Set1(d9, bits)
        },
        Not1(dir, b3) => {
            let d9 = mem_expr(names, *dir);
            let bits = bits_expr(names, *dir, *b3);
            Not1(d9, bits)
        },

        Nop => Nop,
        Ldf => Ldf,
        Stf => Stf
    }
}

fn rel16(rel: i32, next: usize) -> usize {
    (rel + (next as i32) - 1) as usize
}

fn rel8(rel: i32, next: usize) -> usize {
    (rel + (next as i32)) as usize
}

fn targets(pos: usize, instr: &Instr<i32,u8>, graph: &InstructionGraph<i32,u8>) -> Vec<usize> {
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
            if pos >= 2 {
                // If the previous instruction was `not1 ext,0`,
                // then this jmpf is jumping to flash, not to the
                // same ROM. So don't follow the address.
                if let Some(Instr::Not1(0x10D, 0)) = graph.instrs().get(&(pos - 2)) {
                    if graph.instr_arrived_from(pos, pos - 2) {
                        return vec![]
                    }
                }
            }
            let address = *abs as usize;
            vec![address]
        },

        Br(rel) => {
            let address = rel8(*rel, next);
            vec![address]
        },
        Brf(rel) => {
            let address = rel16(*rel, next);
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

fn build_instruction_graph(entry_points: &[usize], bytes: &[u8]) -> Result<InstructionGraph<i32,u8>,DisasmError> {
    let mut positions: Vec<usize> = entry_points.iter().map(|x| *x).collect();
    let mut locs = vec![];
    let mut graph = InstructionGraph::<i32,u8>::new();
    while !positions.is_empty() {
        locs.clear();
        locs.append(&mut positions);

        for pos in locs.iter() {
            if *pos == bytes.len() {
                continue;
            }
            if *pos > bytes.len() {
                eprintln!("Invalid instruction position found: {}", pos);
                continue;
            }
            let decoded = Instr::<i32,u8>::decode(&bytes[*pos..bytes.len()]);
            match decoded {
                Some(instr) => {
                    let targets = targets(*pos, &instr, &graph);
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
                None => {
                    return Err(DisasmError::NoInstruction(*pos, bytes[*pos]));
                }
            }
        }
    }
    Ok(graph)
}

struct Names {
    labels: HashMap<usize,String>,
    psw_bits: HashMap<usize,String>,
    aliases: HashMap<usize,String>
}

impl Names {
    pub fn new() -> Names {
        Names {
            labels: HashMap::new(),
            psw_bits: HashMap::new(),
            aliases: HashMap::new()
        }
    }

    pub fn sorted_psw_bits(&self) -> Vec<(String,usize)> {
        let mut results = vec![];
        for key in self.psw_bits.keys() {
            results.push((self.psw_bits[key].clone(), *key));
        }
        results.sort_unstable_by_key(|pair| pair.1);
        results
    }

    pub fn sorted_aliases(&self) -> Vec<(String,usize)> {
        let mut results = vec![];
        for key in self.aliases.keys() {
            results.push((self.aliases[key].clone(), *key));
        }
        results.sort_unstable_by_key(|pair| pair.1);
        results
    }

    pub fn push_psw_bit(&mut self, name: &str, pos: usize) {
        self.psw_bits.insert(pos, name.to_owned());
    }

    pub fn has_alias(&self, pos: usize) -> bool {
        self.aliases.contains_key(&pos)
    }

    pub fn push_alias(&mut self, name: &str, pos: usize) {
        self.aliases.insert(pos, name.to_owned());
    }

    pub fn contains_label(&self, pos: usize) -> bool {
        self.labels.contains_key(&pos)
    }

    pub fn gen_label(&mut self, pos: usize) {
        if !self.contains_label(pos) {
            self.labels.insert(pos, format!("label_{:04X}", pos));
        }
    }

    pub fn alias(&self, pos: usize) -> Option<&str> {
        self.aliases.get(&pos).map(|string| string.as_str())
    }

    pub fn psw_bit(&self, pos: usize) -> Option<&str> {
        self.psw_bits.get(&pos).map(|string| string.as_str())
    }

    pub fn label(&self, pos: usize) -> Option<&str> {
        self.labels.get(&pos).map(|string| string.as_str())
    }
}

struct InstructionGraph<Ex,IM> {
    instrs: HashMap<usize,Instr<Ex,IM>>,
    jump_to: HashMap<usize,HashSet<usize>>,
    jump_from: HashMap<usize,HashSet<usize>>
}

impl InstructionGraph<i32,u8> {
    pub fn lift_instrs<F>(self, lift: F, names: &Names) -> InstructionGraph<Expr,IndirectionMode>
    where F: Fn(usize, &InstructionGraph<i32,u8>, &Instr<i32,u8>, &Names) -> Instr<Expr,IndirectionMode> {
        let mut results = HashMap::new();
        for key in self.instrs.keys() {
            results.insert(*key, lift(*key, &self, &self.instrs[key], names));
        }
        InstructionGraph {
            instrs: results,
            jump_to: self.jump_to,
            jump_from: self.jump_from
        }
    }
}

impl<Ex,IM> InstructionGraph<Ex,IM> {
    pub fn new() -> InstructionGraph<i32,u8> {
        InstructionGraph {
            instrs: HashMap::new(),
            jump_to: HashMap::new(),
            jump_from: HashMap::new()
        }
    }

    pub fn find_overlaps(&self, pos: usize) -> Vec<usize> {
        let mut overlaps = vec![];
        if let Some(instr) = self.instrs.get(&pos) {
            for idx in 1..2 {
                if pos >= idx {
                    let loc = pos - idx;
                    if let Some(instr2) = self.instrs.get(&loc) {
                        if instr2.size() + loc - 1 >= pos {
                            overlaps.push(loc);
                        }
                    }
                }
            }
            for idx in 1..instr.size() {
                let loc = idx + pos;
                if self.instrs.contains_key(&loc) {
                    overlaps.push(loc);
                }
            }
        }
        overlaps
    }

    pub fn instr_arrived_from(&self, instr_pos: usize, pos: usize) -> bool {
        match self.jump_from.get(&instr_pos) {
            Some(set) => set.contains(&pos),
            None => false
        }
    }

    pub fn instrs(&self) -> &HashMap<usize,Instr<Ex,IM>> {
        &self.instrs
    }

    pub fn instr(&mut self, pos: usize, instr: Instr<Ex,IM>) -> &Instr<Ex,IM> {
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

pub struct DStatements {
    statements: Vec<DStatement>
}

impl DStatements {
    pub fn new() -> DStatements {
        DStatements {
            statements: vec![]
        }
    }

    pub fn to_vec(self) -> Vec<DStatement> {
        self.statements
    }
    
    pub fn push_bytes(&mut self, pos: usize, bytes: &[u8]) {
        self.statements.push(DStatement::bytes(pos, bytes))
    }

    pub fn push_cnop(&mut self, add: usize, multiple: usize) {
        self.statements.push(DStatement::cnop(add, multiple));
    }

    pub fn push_org(&mut self, pos: usize) {
        self.statements.push(DStatement::org(pos));
    }

    pub fn push_label(&mut self, name: &str) {
        self.statements.push(DStatement::label(name));
    }

    pub fn push_comment(&mut self, comment: &str) {
        self.statements.push(DStatement::comment(comment));
    }

    pub fn push_instr(&mut self, pos: usize, instr: Instr<Expr,IndirectionMode>) {
        self.statements.push(DStatement::instr(pos, instr));
    }

    pub fn push_instr_cmt(&mut self, pos: usize, instr: Instr<Expr,IndirectionMode>, comment: String) {
        self.statements.push(DStatement::instr(pos, instr).plus_comment(comment));
    }

    pub fn push_alias(&mut self, name: &str, value: Expr) {
        self.statements.push(DStatement::alias(name, value));
    }
}

pub struct DStatement {
    pos: Option<usize>,
    statement: Statement,
    comment: Option<String>
}

impl DStatement {
    pub fn new(
        pos: Option<usize>,
        statement: Statement,
        comment: Option<String>) -> DStatement {
        DStatement {
            pos,
            statement,
            comment
        }
    }

    pub fn bytes(pos: usize, bytes: &[u8]) -> DStatement {
        let mut vec = vec![];
        for byte in bytes.iter() {
            vec.push(ByteValue::Expr(Expr::num(*byte as i32)));
        }
        DStatement::new(Some(pos), Statement::Directive(Directive::Byte(Span::default(), vec)), None)
    }

    pub fn statement(&self) -> &Statement {
        &self.statement
    }

    pub fn cmt(&self) -> Option<&str> {
        self.comment.as_ref().map(|x| x.as_str())
    }

    pub fn pos(&self) -> Option<usize> {
        self.pos.map(|x| x)
    }

    pub fn cnop(add: usize, multiple: usize) -> DStatement {
        DStatement::new(None, Statement::Directive(Directive::Cnop(Span::default(), Expr::num(add as i32), Expr::num(multiple as i32))), None)
    }

    pub fn alias(name: &str, value: Expr) -> DStatement {
        DStatement::new(None, Statement::Alias(Span::default(), name.to_owned(), value), None)
    }

    pub fn org(pos: usize) -> DStatement {
        DStatement::new(Some(pos), Statement::Directive(Directive::Org(Span::default(), pos)), None)
    }

    pub fn comment(comment: &str) -> DStatement {
        DStatement::new(None, Statement::comment(comment), None)
    }

    pub fn instr(pos: usize, instr: Instr<Expr,IndirectionMode>) -> DStatement {
        DStatement::new(Some(pos), Statement::instr(instr), None)
    }

    pub fn label(name: &str) -> DStatement {
        DStatement::new(None, Statement::label(name), None)
    }

    pub fn plus_comment(self, comment: String) -> DStatement {
        let cmt = if let Some(old) = self.comment {
            let mut new_comment = old.trim_right().to_owned();
            if old.trim_right().ends_with(".") {
                new_comment.push_str(" ");
                new_comment.push_str(comment.as_str());
            } else {
                new_comment.push_str(". ");
                new_comment.push_str(comment.as_str());
            }
            Some(new_comment)
        } else {
            Some(comment)
        };
        DStatement {
            pos: self.pos,
            statement: self.statement,
            comment: cmt
        }
    }
}
