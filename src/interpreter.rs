use std::future::Future;

use crate::compiler::{
    IrFunction, IrInstruction, IrInstructionType, IrPrimitiveType, IrTerminatorType, IrVariableType,
};

#[derive(Debug, Clone, PartialEq)]
enum ReferenceValue {
    Variable(usize),
    Index(Box<ReferenceValue>, usize),
    Count(Box<ReferenceValue>),
    Capacity(Box<ReferenceValue>),
}

#[derive(Debug, Clone, PartialEq)]
enum PrimitiveValue {
    Boolean(bool),
    Natural(u64),
    Whole(i64),
    Real(f64),
    Character(char),
    Reference(ReferenceValue),
}

#[derive(Debug, Clone, PartialEq)]
enum VariableValue {
    Primitive(PrimitiveValue),
    Array {
        data: Vec<VariableValue>,
        count: Box<VariableValue>,
        capacity: Box<VariableValue>,
    },
}

fn dereference<'a>(
    vars: &'a mut [VariableValue],
    reference: &ReferenceValue,
) -> &'a mut VariableValue {
    match reference {
        &ReferenceValue::Variable(index) => &mut vars[index],
        &ReferenceValue::Index(ref reference, index) => {
            let VariableValue::Array { data, .. } = dereference(vars, reference) else {
                unreachable!();
            };
            &mut data[index]
        }
        ReferenceValue::Count(reference) => {
            let VariableValue::Array { count, .. } = dereference(vars, reference) else {
                unreachable!();
            };
            count
        }
        ReferenceValue::Capacity(reference) => {
            let VariableValue::Array { capacity, .. } = dereference(vars, reference) else {
                unreachable!();
            };
            capacity
        }
    }
}

pub async fn run<F: Future<Output = String>>(
    IrFunction {
        vars,
        temps,
        blocks,
    }: IrFunction,
    output: impl Fn(&str),
    input: impl Fn() -> F,
) {
    let mut vars: Vec<_> = vars
        .iter()
        .map(|ty| match ty {
            IrVariableType::Primitive(IrPrimitiveType::Boolean) => {
                VariableValue::Primitive(PrimitiveValue::Boolean(false))
            }
            IrVariableType::Primitive(IrPrimitiveType::Natural) => {
                VariableValue::Primitive(PrimitiveValue::Natural(0))
            }
            IrVariableType::Primitive(IrPrimitiveType::Whole) => {
                VariableValue::Primitive(PrimitiveValue::Whole(0))
            }
            IrVariableType::Primitive(IrPrimitiveType::Real) => {
                VariableValue::Primitive(PrimitiveValue::Real(0.0))
            }
            IrVariableType::Primitive(IrPrimitiveType::Character) => {
                VariableValue::Primitive(PrimitiveValue::Character('\0'))
            }
            IrVariableType::Array(_) => VariableValue::Array {
                data: Vec::new(),
                count: Box::new(VariableValue::Primitive(PrimitiveValue::Natural(0))),
                capacity: Box::new(VariableValue::Primitive(PrimitiveValue::Natural(0))),
            },
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
            IrPrimitiveType::Reference(_) => PrimitiveValue::Reference(ReferenceValue::Variable(0)),
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
                IrTerminatorType::Error(ref string) => {
                    eprintln!("finished with error: {string}");
                    break;
                }
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
            IrInstructionType::Null => {
                let PrimitiveValue::Reference(reference) = &temps[temp] else {
                    unreachable!();
                };
                *dereference(&mut vars, reference) = VariableValue::Array {
                    data: Vec::new(),
                    count: Box::new(VariableValue::Primitive(PrimitiveValue::Natural(0))),
                    capacity: Box::new(VariableValue::Primitive(PrimitiveValue::Natural(0))),
                };
            }
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
                    if let (&PrimitiveValue::Boolean(a), &PrimitiveValue::Boolean(b)) =
                        (&temps[a_temp], &temps[b_temp])
                    {
                        a && b
                    } else {
                        unreachable!();
                    },
                )
            }
            IrInstructionType::Or(a_temp, b_temp) => {
                temps[temp] = PrimitiveValue::Boolean(
                    if let (&PrimitiveValue::Boolean(a), &PrimitiveValue::Boolean(b)) =
                        (&temps[a_temp], &temps[b_temp])
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
                temps[temp] = match (&temps[a_temp], &temps[b_temp]) {
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
                temps[temp] = match (&temps[a_temp], &temps[b_temp]) {
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
                temps[temp] = match (&temps[a_temp], &temps[b_temp]) {
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
                temps[temp] = match (&temps[a_temp], &temps[b_temp]) {
                    (PrimitiveValue::Real(b), PrimitiveValue::Real(a)) => {
                        PrimitiveValue::Real(a / b)
                    }
                    _ => unreachable!(),
                }
            }
            IrInstructionType::Remainder(a_temp, b_temp) => {
                temps[temp] = match (&temps[a_temp], &temps[b_temp]) {
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
            IrInstructionType::MinimumCapacity(val_temp) => {
                let PrimitiveValue::Natural(value) = temps[val_temp] else {
                    unreachable!();
                };
                temps[temp] = PrimitiveValue::Natural((value + 1).next_power_of_two() - 1);
            }
            IrInstructionType::Equals(a_temp, b_temp) => {
                temps[temp] = PrimitiveValue::Boolean(temps[a_temp] == temps[b_temp])
            }
            IrInstructionType::LessThan(a_temp, b_temp) => {
                temps[temp] = PrimitiveValue::Boolean(match (&temps[a_temp], &temps[b_temp]) {
                    (PrimitiveValue::Natural(a), PrimitiveValue::Natural(b)) => a < b,
                    (PrimitiveValue::Whole(a), PrimitiveValue::Whole(b)) => a < b,
                    (PrimitiveValue::Real(a), PrimitiveValue::Real(b)) => a < b,
                    _ => unreachable!(),
                })
            }
            IrInstructionType::GreaterThan(a_temp, b_temp) => {
                temps[temp] = PrimitiveValue::Boolean(match (&temps[a_temp], &temps[b_temp]) {
                    (PrimitiveValue::Natural(a), PrimitiveValue::Natural(b)) => a > b,
                    (PrimitiveValue::Whole(a), PrimitiveValue::Whole(b)) => a > b,
                    (PrimitiveValue::Real(a), PrimitiveValue::Real(b)) => a > b,
                    _ => unreachable!(),
                })
            }
            IrInstructionType::Variable(var) => {
                temps[temp] = PrimitiveValue::Reference(ReferenceValue::Variable(var))
            }
            IrInstructionType::Field(..) => todo!(),
            IrInstructionType::Count(val_temp) => {
                let PrimitiveValue::Reference(reference) = &temps[val_temp] else {
                    unreachable!();
                };
                temps[temp] =
                    PrimitiveValue::Reference(ReferenceValue::Count(Box::new(reference.clone())));
            }
            IrInstructionType::Capacity(val_temp) => {
                let PrimitiveValue::Reference(reference) = &temps[val_temp] else {
                    unreachable!();
                };
                temps[temp] = PrimitiveValue::Reference(ReferenceValue::Capacity(Box::new(
                    reference.clone(),
                )));
            }
            IrInstructionType::Index(arr_temp, index_temp) => {
                let PrimitiveValue::Reference(reference) = &temps[arr_temp] else {
                    unreachable!();
                };
                let PrimitiveValue::Natural(index) = temps[index_temp] else {
                    unreachable!();
                };
                temps[temp] = PrimitiveValue::Reference(ReferenceValue::Index(
                    Box::new(reference.clone()),
                    index as _,
                ));
            }
            IrInstructionType::Assign(val_temp) => {
                let PrimitiveValue::Reference(reference) = &temps[temp] else {
                    unreachable!();
                };
                *dereference(&mut vars, reference) =
                    VariableValue::Primitive(temps[val_temp].clone());
            }
            IrInstructionType::Dereference(val_temp) => {
                let PrimitiveValue::Reference(reference) = &temps[val_temp] else {
                    unreachable!();
                };
                let VariableValue::Primitive(primitive) = dereference(&mut vars, reference) else {
                    unreachable!();
                };
                temps[temp] = primitive.clone();
            }
            IrInstructionType::Resize(val_temp) => {
                let PrimitiveValue::Reference(reference) = &temps[temp] else {
                    unreachable!();
                };
                let VariableValue::Array { data, .. } = dereference(&mut vars, reference) else {
                    unreachable!();
                };
                let PrimitiveValue::Natural(size) = temps[val_temp] else {
                    unreachable!();
                };
                data.resize(
                    size as _,
                    VariableValue::Primitive(PrimitiveValue::Boolean(false)),
                );
            }
            IrInstructionType::Free => {}
            IrInstructionType::Write(val_temp) => {
                match temps[val_temp] {
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
                };
                temps[temp] = PrimitiveValue::Boolean(true);
            }
            IrInstructionType::Read(val_temp) => {
                let PrimitiveValue::Reference(reference) = &temps[val_temp] else {
                    unreachable!();
                };
                match dereference(&mut vars, reference) {
                    VariableValue::Primitive(PrimitiveValue::Natural(value)) => loop {
                        let Some(word) = input_words.pop() else {
                            input_words = input()
                                .await
                                .split_whitespace()
                                .map(ToOwned::to_owned)
                                .collect();
                            input_words.reverse();
                            continue;
                        };
                        if let Ok(result) = word.parse() {
                            *value = result;
                            temps[temp] = PrimitiveValue::Boolean(true);
                        } else {
                            *value = 0;
                            temps[temp] = PrimitiveValue::Boolean(false);
                        }
                        break;
                    },
                    VariableValue::Primitive(PrimitiveValue::Whole(value)) => loop {
                        let Some(word) = input_words.pop() else {
                            input_words = input()
                                .await
                                .split_whitespace()
                                .map(ToOwned::to_owned)
                                .collect();
                            input_words.reverse();
                            continue;
                        };
                        if let Ok(result) = word.parse() {
                            *value = result;
                            temps[temp] = PrimitiveValue::Boolean(true);
                        } else {
                            *value = 0;
                            temps[temp] = PrimitiveValue::Boolean(false);
                        }
                        break;
                    },
                    VariableValue::Primitive(PrimitiveValue::Real(value)) => loop {
                        let Some(word) = input_words.pop() else {
                            input_words = input()
                                .await
                                .split_whitespace()
                                .map(ToOwned::to_owned)
                                .collect();
                            input_words.reverse();
                            continue;
                        };
                        if let Ok(result) = word.parse() {
                            *value = result;
                            temps[temp] = PrimitiveValue::Boolean(true);
                        } else {
                            *value = 0.0;
                            temps[temp] = PrimitiveValue::Boolean(false);
                        }
                        break;
                    },
                    VariableValue::Primitive(PrimitiveValue::Character(value)) => {
                        *value = loop {
                            let Some(mut word) = input_words.pop() else {
                                input_words = input()
                                    .await
                                    .split_whitespace()
                                    .map(ToOwned::to_owned)
                                    .collect();
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
