use crate::compiler::{
    IrFunction, IrInstruction, IrInstructionType, IrPrimitiveType, IrTerminatorType, IrVariableType,
};

#[derive(Debug, Clone, Copy, PartialEq)]
enum PrimitiveValue {
    Boolean(bool),
    Natural(u64),
    Whole(i64),
    Real(f64),
    Character(char),
    Reference(usize),
}

pub fn run(
    IrFunction {
        vars,
        temps,
        blocks,
    }: IrFunction,
    output: impl Fn(&str),
    input: impl Fn() -> String,
) {
    let mut vars: Vec<_> = vars
        .iter()
        .map(|ty| match ty {
            IrVariableType::Primitive(IrPrimitiveType::Boolean) => PrimitiveValue::Boolean(false),
            IrVariableType::Primitive(IrPrimitiveType::Natural) => PrimitiveValue::Natural(0),
            IrVariableType::Primitive(IrPrimitiveType::Whole) => PrimitiveValue::Whole(0),
            IrVariableType::Primitive(IrPrimitiveType::Real) => PrimitiveValue::Real(0.0),
            IrVariableType::Primitive(IrPrimitiveType::Character) => {
                PrimitiveValue::Character('\0')
            }
            _ => unreachable!(),
        })
        .collect();
    let mut temps: Vec<_> = temps
        .iter()
        .map(|ty| match ty {
            IrPrimitiveType::Boolean => PrimitiveValue::Boolean(false),
            IrPrimitiveType::Natural => PrimitiveValue::Natural(0),
            IrPrimitiveType::Whole => PrimitiveValue::Whole(0),
            IrPrimitiveType::Real => PrimitiveValue::Real(0.0),
            IrPrimitiveType::Character => PrimitiveValue::Character('\0'),
            IrPrimitiveType::Reference(_) => PrimitiveValue::Reference(0),
        })
        .collect();
    let mut input_words = Vec::new();
    let mut block = 0;
    let mut inst = 0;
    loop {
        if inst == blocks[block].as_ref().unwrap().insts.len() {
            match blocks[block].as_ref().unwrap().term.ty {
                IrTerminatorType::Unconditional(goto_block) => {
                    block = goto_block;
                    inst = 0;
                }
                IrTerminatorType::Conditional(cond_temp, then_block, else_block) => {
                    let PrimitiveValue::Boolean(cond) = temps[cond_temp] else {
                        unreachable!();
                    };
                    block = if cond { then_block } else { else_block };
                    inst = 0;
                }
                IrTerminatorType::Return(_) => break,
            }
            continue;
        }
        let &IrInstruction { temp, ty, .. } = &blocks[block].as_ref().unwrap().insts[inst];
        match ty {
            IrInstructionType::Boolean(value) => temps[temp] = PrimitiveValue::Boolean(value),
            IrInstructionType::Natural(value) => temps[temp] = PrimitiveValue::Natural(value),
            IrInstructionType::Whole(value) => temps[temp] = PrimitiveValue::Whole(value),
            IrInstructionType::Real(value) => temps[temp] = PrimitiveValue::Real(value),
            IrInstructionType::Character(value) => temps[temp] = PrimitiveValue::Character(value),
            IrInstructionType::Null => todo!(),
            IrInstructionType::ToNatural(val_temp) => {
                temps[temp] = PrimitiveValue::Natural(match temps[val_temp] {
                    PrimitiveValue::Natural(value) => value,
                    PrimitiveValue::Whole(value) => value as _,
                    PrimitiveValue::Real(value) => value as _,
                    PrimitiveValue::Character(value) => value as _,
                    _ => unreachable!(),
                })
            }
            IrInstructionType::ToWhole(val_temp) => {
                temps[temp] = PrimitiveValue::Whole(match temps[val_temp] {
                    PrimitiveValue::Natural(value) => value as _,
                    PrimitiveValue::Whole(value) => value,
                    PrimitiveValue::Real(value) => value as _,
                    _ => unreachable!(),
                })
            }
            IrInstructionType::ToReal(val_temp) => {
                temps[temp] = PrimitiveValue::Real(match temps[val_temp] {
                    PrimitiveValue::Natural(value) => value as _,
                    PrimitiveValue::Whole(value) => value as _,
                    PrimitiveValue::Real(value) => value,
                    _ => unreachable!(),
                })
            }
            IrInstructionType::ToCharacter(val_temp) => {
                temps[temp] = PrimitiveValue::Real(match temps[val_temp] {
                    PrimitiveValue::Natural(value) => value as _,
                    _ => unreachable!(),
                })
            }
            IrInstructionType::And(a_temp, b_temp) => {
                temps[temp] = PrimitiveValue::Boolean(
                    if let (PrimitiveValue::Boolean(a), PrimitiveValue::Boolean(b)) =
                        (temps[a_temp], temps[b_temp])
                    {
                        a && b
                    } else {
                        unreachable!();
                    },
                )
            }
            IrInstructionType::Or(a_temp, b_temp) => {
                temps[temp] = PrimitiveValue::Boolean(
                    if let (PrimitiveValue::Boolean(a), PrimitiveValue::Boolean(b)) =
                        (temps[a_temp], temps[b_temp])
                    {
                        a || b
                    } else {
                        unreachable!();
                    },
                )
            }
            IrInstructionType::Not(val_temp) => {
                temps[temp] = PrimitiveValue::Boolean(
                    if let PrimitiveValue::Boolean(value) = temps[val_temp] {
                        !value
                    } else {
                        unreachable!();
                    },
                )
            }
            IrInstructionType::Add(a_temp, b_temp) => {
                temps[temp] = match (temps[a_temp], temps[b_temp]) {
                    (PrimitiveValue::Natural(b), PrimitiveValue::Natural(a)) => {
                        PrimitiveValue::Natural(a + b)
                    }
                    (PrimitiveValue::Whole(b), PrimitiveValue::Whole(a)) => {
                        PrimitiveValue::Whole(a + b)
                    }
                    (PrimitiveValue::Real(b), PrimitiveValue::Real(a)) => {
                        PrimitiveValue::Real(a + b)
                    }
                    _ => unreachable!(),
                }
            }
            IrInstructionType::Subtract(a_temp, b_temp) => {
                temps[temp] = match (temps[a_temp], temps[b_temp]) {
                    (PrimitiveValue::Whole(b), PrimitiveValue::Whole(a)) => {
                        PrimitiveValue::Whole(a - b)
                    }
                    (PrimitiveValue::Real(b), PrimitiveValue::Real(a)) => {
                        PrimitiveValue::Real(a - b)
                    }
                    _ => unreachable!(),
                }
            }
            IrInstructionType::Multiply(a_temp, b_temp) => {
                temps[temp] = match (temps[a_temp], temps[b_temp]) {
                    (PrimitiveValue::Natural(b), PrimitiveValue::Natural(a)) => {
                        PrimitiveValue::Natural(a * b)
                    }
                    (PrimitiveValue::Whole(b), PrimitiveValue::Whole(a)) => {
                        PrimitiveValue::Whole(a * b)
                    }
                    (PrimitiveValue::Real(b), PrimitiveValue::Real(a)) => {
                        PrimitiveValue::Real(a * b)
                    }
                    _ => unreachable!(),
                }
            }
            IrInstructionType::Divide(a_temp, b_temp) => {
                temps[temp] = match (temps[a_temp], temps[b_temp]) {
                    (PrimitiveValue::Real(b), PrimitiveValue::Real(a)) => {
                        PrimitiveValue::Real(a / b)
                    }
                    _ => unreachable!(),
                }
            }
            IrInstructionType::Remainder(a_temp, b_temp) => {
                temps[temp] = match (temps[a_temp], temps[b_temp]) {
                    (PrimitiveValue::Natural(b), PrimitiveValue::Natural(a)) => {
                        PrimitiveValue::Natural(a % b)
                    }
                    (PrimitiveValue::Whole(b), PrimitiveValue::Whole(a)) => {
                        PrimitiveValue::Whole(a % b)
                    }
                    (PrimitiveValue::Real(b), PrimitiveValue::Real(a)) => {
                        PrimitiveValue::Real(a % b)
                    }
                    _ => unreachable!(),
                }
            }
            IrInstructionType::Negate(val_temp) => {
                temps[temp] = match temps[val_temp] {
                    PrimitiveValue::Whole(value) => PrimitiveValue::Whole(-value),
                    PrimitiveValue::Real(value) => PrimitiveValue::Real(-value),
                    _ => unreachable!(),
                }
            }
            IrInstructionType::Equals(a_temp, b_temp) => {
                temps[temp] = PrimitiveValue::Boolean(temps[a_temp] == temps[b_temp])
            }
            IrInstructionType::LessThan(a_temp, b_temp) => {
                temps[temp] = PrimitiveValue::Boolean(match (temps[a_temp], temps[b_temp]) {
                    (PrimitiveValue::Natural(a), PrimitiveValue::Natural(b)) => a < b,
                    (PrimitiveValue::Whole(a), PrimitiveValue::Whole(b)) => a < b,
                    (PrimitiveValue::Real(a), PrimitiveValue::Real(b)) => a < b,
                    _ => unreachable!(),
                })
            }
            IrInstructionType::GreaterThan(a_temp, b_temp) => {
                temps[temp] = PrimitiveValue::Boolean(match (temps[a_temp], temps[b_temp]) {
                    (PrimitiveValue::Natural(a), PrimitiveValue::Natural(b)) => a > b,
                    (PrimitiveValue::Whole(a), PrimitiveValue::Whole(b)) => a > b,
                    (PrimitiveValue::Real(a), PrimitiveValue::Real(b)) => a > b,
                    _ => unreachable!(),
                })
            }
            IrInstructionType::Variable(var) => temps[temp] = PrimitiveValue::Reference(var),
            IrInstructionType::Field(..) => todo!(),
            IrInstructionType::Count(_) => todo!(),
            IrInstructionType::Capacity(_) => todo!(),
            IrInstructionType::Index(..) => todo!(),
            IrInstructionType::Assign(val_temp) => {
                vars[if let PrimitiveValue::Reference(var) = temps[temp] {
                    var
                } else {
                    unreachable!()
                }] = temps[val_temp]
            }
            IrInstructionType::Dereference(val_temp) => {
                temps[temp] = vars[if let PrimitiveValue::Reference(var) = temps[val_temp] {
                    var
                } else {
                    unreachable!()
                }]
            }
            IrInstructionType::Resize(..) => todo!(),
            IrInstructionType::Free(_) => todo!(),
            IrInstructionType::Write => match temps[temp] {
                PrimitiveValue::Boolean(value) => {
                    if value {
                        output("Ақиқат\n");
                    } else {
                        output("Жалған\n");
                    }
                }
                PrimitiveValue::Natural(value) => output(&format!("{value}\n")),
                PrimitiveValue::Whole(value) => output(&format!("{value}\n")),
                PrimitiveValue::Real(value) => output(&format!("{value}\n")),
                PrimitiveValue::Character(value) => output(&format!("{value}")),
                _ => unreachable!(),
            },
            IrInstructionType::Read => {
                match &mut vars[if let PrimitiveValue::Reference(var) = temps[temp] {
                    var
                } else {
                    unreachable!()
                }] {
                    PrimitiveValue::Natural(value) => {
                        *value = loop {
                            let Some(word) = input_words.pop() else {
                                input_words =
                                    input().split_whitespace().map(ToOwned::to_owned).collect();
                                input_words.reverse();
                                continue;
                            };
                            break word.parse().unwrap_or_else(|_| {
                                panic!(
                                    "invalid input at {}:{}",
                                    blocks[block].as_ref().unwrap().insts[inst].line + 1,
                                    blocks[block].as_ref().unwrap().insts[inst].column + 1
                                )
                            });
                        }
                    }
                    PrimitiveValue::Whole(value) => {
                        *value = loop {
                            let Some(word) = input_words.pop() else {
                                input_words =
                                    input().split_whitespace().map(ToOwned::to_owned).collect();
                                input_words.reverse();
                                continue;
                            };
                            break word.parse().unwrap_or_else(|_| {
                                panic!(
                                    "invalid input at {}:{}",
                                    blocks[block].as_ref().unwrap().insts[inst].line + 1,
                                    blocks[block].as_ref().unwrap().insts[inst].column + 1
                                )
                            });
                        }
                    }
                    PrimitiveValue::Real(value) => {
                        *value = loop {
                            let Some(word) = input_words.pop() else {
                                input_words =
                                    input().split_whitespace().map(ToOwned::to_owned).collect();
                                input_words.reverse();
                                continue;
                            };
                            break word.parse().unwrap_or_else(|_| {
                                panic!(
                                    "invalid input at {}:{}",
                                    blocks[block].as_ref().unwrap().insts[inst].line + 1,
                                    blocks[block].as_ref().unwrap().insts[inst].column + 1
                                )
                            });
                        }
                    }
                    PrimitiveValue::Character(value) => {
                        *value = loop {
                            let Some(mut word) = input_words.pop() else {
                                input_words =
                                    input().split_whitespace().map(ToOwned::to_owned).collect();
                                input_words.reverse();
                                continue;
                            };
                            let ch = word.chars().next().unwrap();
                            word.drain(0..ch.len_utf8());
                            if !word.is_empty() {
                                input_words.push(word);
                            }
                            break ch;
                        }
                    }
                    _ => unreachable!(),
                }
            }
        }
        inst += 1;
    }
}
