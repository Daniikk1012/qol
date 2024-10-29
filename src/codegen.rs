use gccjit::{BinaryOp, CType, ComparisonOp, Context, FunctionType, OutputKind, ToRValue, UnaryOp};

use crate::compiler::{
    IrFunction, IrInstruction, IrInstructionType, IrPrimitiveType, IrTerminatedBlock, IrTerminator,
    IrTerminatorType, IrVariableType,
};

const TMP_FILENAME: &str = "main.qol";

pub fn compile(function: IrFunction) {
    let context = Context::default();
    let gccjit_int = context.new_c_type(CType::Int);
    let gccjit_function =
        context.new_function(None, FunctionType::Exported, gccjit_int, &[], "main", false);
    let gccjit_boolean = context.new_type::<bool>();
    let gccjit_natural = context.new_type::<u64>();
    let gccjit_whole = context.new_type::<i64>();
    let gccjit_real = context.new_type::<f64>();
    let gccjit_character = context.new_type::<char>();
    let gccjit_reference = context.new_type::<()>().make_pointer();
    let gccjit_vars: Vec<_> = function
        .vars
        .iter()
        .enumerate()
        .map(|(index, ty)| {
            gccjit_function.new_local(
                None,
                match ty {
                    IrVariableType::Primitive(IrPrimitiveType::Boolean) => gccjit_boolean,
                    IrVariableType::Primitive(IrPrimitiveType::Natural) => gccjit_natural,
                    IrVariableType::Primitive(IrPrimitiveType::Whole) => gccjit_whole,
                    IrVariableType::Primitive(IrPrimitiveType::Real) => gccjit_real,
                    IrVariableType::Primitive(IrPrimitiveType::Character) => gccjit_character,
                    _ => todo!(),
                },
                format!("var_{index}"),
            )
        })
        .collect();
    let gccjit_temps: Vec<_> = function
        .temps
        .iter()
        .enumerate()
        .map(|(index, ty)| {
            gccjit_function.new_local(
                None,
                match ty {
                    IrPrimitiveType::Boolean => gccjit_boolean,
                    IrPrimitiveType::Natural => gccjit_natural,
                    IrPrimitiveType::Whole => gccjit_whole,
                    IrPrimitiveType::Real => gccjit_real,
                    IrPrimitiveType::Character => gccjit_character,
                    IrPrimitiveType::Reference(_) => gccjit_reference,
                },
                format!("temp_{index}"),
            )
        })
        .collect();
    let gccjit_blocks: Vec<_> = (0..function.blocks.len())
        .map(|index| {
            if function.blocks[index].is_some() {
                Some(gccjit_function.new_block(format!("block_{index}")))
            } else {
                None
            }
        })
        .collect();
    let gccjit_printf = context.new_function(
        None,
        FunctionType::Extern,
        gccjit_int,
        &[context.new_parameter(None, context.new_c_type(CType::ConstCharPtr), "format")],
        "printf",
        true,
    );
    let gccjit_scanf = context.new_function(
        None,
        FunctionType::Extern,
        gccjit_int,
        &[context.new_parameter(None, context.new_c_type(CType::ConstCharPtr), "format")],
        "scanf",
        true,
    );
    for (block_index, block) in function.blocks.into_iter().enumerate() {
        let Some(IrTerminatedBlock {
            insts,
            term: IrTerminator { ty, line, column },
        }) = block
        else {
            continue;
        };
        let loc = Some(context.new_location(TMP_FILENAME, line as i32 + 1, column as i32 + 1));
        let mut gccjit_block = gccjit_blocks[block_index].unwrap();
        for IrInstruction {
            temp,
            ty,
            line,
            column,
        } in insts.into_iter()
        {
            let loc = Some(context.new_location(TMP_FILENAME, line as i32 + 1, column as i32 + 1));
            match ty {
                IrInstructionType::Boolean(value) => {
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        if value {
                            context.new_rvalue_one(gccjit_boolean)
                        } else {
                            context.new_rvalue_zero(gccjit_boolean)
                        },
                    );
                }
                IrInstructionType::Natural(value) => {
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_binary_op(
                            loc,
                            BinaryOp::Plus,
                            gccjit_natural,
                            context.new_binary_op(
                                loc,
                                BinaryOp::LShift,
                                gccjit_natural,
                                context.new_rvalue_from_long(gccjit_natural, (value >> 32) as _),
                                context.new_rvalue_from_int(gccjit_natural, 32),
                            ),
                            context.new_rvalue_from_long(gccjit_natural, (value & 0xFFFFFFFF) as _),
                        ),
                    );
                }
                IrInstructionType::Whole(value) => {
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_binary_op(
                            loc,
                            BinaryOp::Plus,
                            gccjit_whole,
                            context.new_binary_op(
                                loc,
                                BinaryOp::LShift,
                                gccjit_whole,
                                context.new_rvalue_from_long(gccjit_whole, (value >> 32) as _),
                                context.new_rvalue_from_int(gccjit_whole, 32),
                            ),
                            context.new_rvalue_from_long(gccjit_whole, (value & 0xFFFFFFFF) as _),
                        ),
                    );
                }
                IrInstructionType::Real(value) => {
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_rvalue_from_double(gccjit_real, value),
                    );
                }
                IrInstructionType::Character(value) => {
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_rvalue_from_int(gccjit_character, value as _),
                    );
                }
                IrInstructionType::Null => todo!(),
                IrInstructionType::ToNatural(value_temp) => {
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_cast(loc, gccjit_temps[value_temp], gccjit_natural),
                    );
                }
                IrInstructionType::ToWhole(value_temp) => {
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_cast(loc, gccjit_temps[value_temp], gccjit_whole),
                    );
                }
                IrInstructionType::ToReal(value_temp) => {
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_cast(loc, gccjit_temps[value_temp], gccjit_real),
                    );
                }
                IrInstructionType::ToCharacter(value_temp) => {
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_cast(loc, gccjit_temps[value_temp], gccjit_character),
                    );
                }
                IrInstructionType::And(a_temp, b_temp) => {
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_binary_op(
                            loc,
                            BinaryOp::LogicalAnd,
                            gccjit_boolean,
                            gccjit_temps[a_temp],
                            gccjit_temps[b_temp],
                        ),
                    );
                }
                IrInstructionType::Or(a_temp, b_temp) => {
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_binary_op(
                            loc,
                            BinaryOp::LogicalOr,
                            gccjit_boolean,
                            gccjit_temps[a_temp],
                            gccjit_temps[b_temp],
                        ),
                    );
                }
                IrInstructionType::Not(value_temp) => {
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_unary_op(
                            loc,
                            UnaryOp::LogicalNegate,
                            gccjit_boolean,
                            gccjit_temps[value_temp],
                        ),
                    );
                }
                IrInstructionType::Add(a_temp, b_temp) => {
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_binary_op(
                            loc,
                            BinaryOp::Plus,
                            match &function.temps[temp] {
                                IrPrimitiveType::Natural => gccjit_natural,
                                IrPrimitiveType::Whole => gccjit_whole,
                                IrPrimitiveType::Real => gccjit_real,
                                _ => unreachable!(),
                            },
                            gccjit_temps[a_temp],
                            gccjit_temps[b_temp],
                        ),
                    );
                }
                IrInstructionType::Subtract(a_temp, b_temp) => {
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_binary_op(
                            loc,
                            BinaryOp::Minus,
                            match &function.temps[temp] {
                                IrPrimitiveType::Whole => gccjit_whole,
                                IrPrimitiveType::Real => gccjit_real,
                                _ => unreachable!(),
                            },
                            gccjit_temps[a_temp],
                            gccjit_temps[b_temp],
                        ),
                    );
                }
                IrInstructionType::Multiply(a_temp, b_temp) => {
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_binary_op(
                            loc,
                            BinaryOp::Mult,
                            match &function.temps[temp] {
                                IrPrimitiveType::Natural => gccjit_natural,
                                IrPrimitiveType::Whole => gccjit_whole,
                                IrPrimitiveType::Real => gccjit_real,
                                _ => unreachable!(),
                            },
                            gccjit_temps[a_temp],
                            gccjit_temps[b_temp],
                        ),
                    );
                }
                IrInstructionType::Divide(a_temp, b_temp) => {
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_binary_op(
                            loc,
                            BinaryOp::Divide,
                            gccjit_real,
                            gccjit_temps[a_temp],
                            gccjit_temps[b_temp],
                        ),
                    );
                }
                IrInstructionType::Remainder(a_temp, b_temp)
                    if function.temps[temp] == IrPrimitiveType::Real =>
                {
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_call(
                            loc,
                            context.get_builtin_function("__builtin_fmod"),
                            &[
                                gccjit_temps[a_temp].to_rvalue(),
                                gccjit_temps[b_temp].to_rvalue(),
                            ],
                        ),
                    );
                }
                IrInstructionType::Remainder(a_temp, b_temp) => {
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_binary_op(
                            loc,
                            BinaryOp::Modulo,
                            match function.temps[temp] {
                                IrPrimitiveType::Natural => gccjit_natural,
                                IrPrimitiveType::Whole => gccjit_whole,
                                _ => unreachable!(),
                            },
                            gccjit_temps[a_temp],
                            gccjit_temps[b_temp],
                        ),
                    );
                }
                IrInstructionType::Negate(value_temp) => {
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_unary_op(
                            loc,
                            UnaryOp::Minus,
                            match &function.temps[temp] {
                                IrPrimitiveType::Whole => gccjit_whole,
                                IrPrimitiveType::Real => gccjit_real,
                                _ => unreachable!(),
                            },
                            gccjit_temps[value_temp],
                        ),
                    );
                }
                IrInstructionType::Equals(a_temp, b_temp) => {
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_comparison(
                            loc,
                            ComparisonOp::Equals,
                            gccjit_temps[a_temp],
                            gccjit_temps[b_temp],
                        ),
                    );
                }
                IrInstructionType::LessThan(a_temp, b_temp) => {
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_comparison(
                            loc,
                            ComparisonOp::LessThan,
                            gccjit_temps[a_temp],
                            gccjit_temps[b_temp],
                        ),
                    );
                }
                IrInstructionType::GreaterThan(a_temp, b_temp) => {
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_comparison(
                            loc,
                            ComparisonOp::GreaterThan,
                            gccjit_temps[a_temp],
                            gccjit_temps[b_temp],
                        ),
                    );
                }
                IrInstructionType::Variable(var) => {
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_cast(loc, gccjit_vars[var].get_address(loc), gccjit_reference),
                    );
                }
                // TODO More here
                IrInstructionType::Assign(val_temp) => {
                    let gccjit_type = match function.temps[val_temp] {
                        IrPrimitiveType::Boolean => gccjit_boolean,
                        IrPrimitiveType::Natural => gccjit_natural,
                        IrPrimitiveType::Whole => gccjit_whole,
                        IrPrimitiveType::Real => gccjit_real,
                        IrPrimitiveType::Character => gccjit_character,
                        IrPrimitiveType::Reference(_) => gccjit_reference,
                    };
                    gccjit_block.add_assignment(
                        loc,
                        context.new_array_access(
                            loc,
                            context.new_cast(loc, gccjit_temps[temp], gccjit_type.make_pointer()),
                            context.new_rvalue_zero(gccjit_int),
                        ),
                        gccjit_temps[val_temp],
                    );
                }
                IrInstructionType::Dereference(value_temp) => {
                    let gccjit_type = match function.temps[temp] {
                        IrPrimitiveType::Boolean => gccjit_boolean,
                        IrPrimitiveType::Natural => gccjit_natural,
                        IrPrimitiveType::Whole => gccjit_whole,
                        IrPrimitiveType::Real => gccjit_real,
                        IrPrimitiveType::Character => gccjit_character,
                        IrPrimitiveType::Reference(_) => gccjit_reference,
                    };
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_cast(
                            loc,
                            context.new_array_access(
                                loc,
                                context.new_cast(
                                    loc,
                                    gccjit_temps[value_temp],
                                    gccjit_type.make_pointer(),
                                ),
                                context.new_rvalue_zero(gccjit_int),
                            ),
                            gccjit_type,
                        ),
                    );
                }
                // TODO More here
                IrInstructionType::Write if function.temps[temp] == IrPrimitiveType::Boolean => {
                    let gccjit_then_block =
                        gccjit_function.new_block(format!("block_{block_index}_then"));
                    let gccjit_else_block =
                        gccjit_function.new_block(format!("block_{block_index}_else"));
                    let gccjit_end_block =
                        gccjit_function.new_block(format!("block_{block_index}_end"));
                    gccjit_block.end_with_conditional(
                        loc,
                        gccjit_temps[temp],
                        gccjit_then_block,
                        gccjit_else_block,
                    );
                    gccjit_then_block.add_eval(
                        loc,
                        context.new_call(
                            loc,
                            gccjit_printf,
                            &[context.new_string_literal("Ақиқат\n")],
                        ),
                    );
                    gccjit_then_block.end_with_jump(loc, gccjit_end_block);
                    gccjit_else_block.add_eval(
                        loc,
                        context.new_call(
                            loc,
                            gccjit_printf,
                            &[context.new_string_literal("Жалған\n")],
                        ),
                    );
                    gccjit_else_block.end_with_jump(loc, gccjit_end_block);
                    gccjit_block = gccjit_end_block;
                }
                IrInstructionType::Write => {
                    gccjit_block.add_eval(
                        loc,
                        context.new_call(
                            loc,
                            gccjit_printf,
                            &[
                                context.new_string_literal(match function.temps[temp] {
                                    IrPrimitiveType::Natural => "%llu\n",
                                    IrPrimitiveType::Whole => "%lld\n",
                                    IrPrimitiveType::Real => "%f\n",
                                    IrPrimitiveType::Character => "%c",
                                    _ => unreachable!(),
                                }),
                                gccjit_temps[temp].to_rvalue(),
                            ],
                        ),
                    );
                }
                IrInstructionType::Read => {
                    let IrPrimitiveType::Reference(ty) = &function.temps[temp] else {
                        unreachable!();
                    };
                    gccjit_block.add_eval(
                        loc,
                        context.new_call(
                            loc,
                            gccjit_scanf,
                            &[
                                context.new_string_literal(match ty.as_ref() {
                                    IrVariableType::Primitive(IrPrimitiveType::Natural) => "%llu",
                                    IrVariableType::Primitive(IrPrimitiveType::Whole) => "%lld",
                                    IrVariableType::Primitive(IrPrimitiveType::Real) => "%lf",
                                    IrVariableType::Primitive(IrPrimitiveType::Character) => " %c",
                                    _ => unreachable!(),
                                }),
                                context.new_cast(
                                    loc,
                                    gccjit_temps[temp],
                                    match ty.as_ref() {
                                        IrVariableType::Primitive(IrPrimitiveType::Natural) => {
                                            gccjit_natural.make_pointer()
                                        }
                                        IrVariableType::Primitive(IrPrimitiveType::Whole) => {
                                            gccjit_whole.make_pointer()
                                        }
                                        IrVariableType::Primitive(IrPrimitiveType::Real) => {
                                            gccjit_real.make_pointer()
                                        }
                                        IrVariableType::Primitive(IrPrimitiveType::Character) => {
                                            gccjit_character.make_pointer()
                                        }
                                        _ => unreachable!(),
                                    },
                                ),
                            ],
                        ),
                    );
                }
                _ => todo!(),
            }
        }
        match ty {
            IrTerminatorType::Unconditional(goto_block) => {
                gccjit_block.end_with_jump(loc, gccjit_blocks[goto_block].unwrap());
            }
            IrTerminatorType::Conditional(cond_temp, then_block, else_block) => {
                gccjit_block.end_with_conditional(
                    loc,
                    gccjit_temps[cond_temp],
                    gccjit_blocks[then_block].unwrap(),
                    gccjit_blocks[else_block].unwrap(),
                );
            }
            IrTerminatorType::Return(temp) => {
                gccjit_block
                    .end_with_return(loc, context.new_cast(loc, gccjit_temps[temp], gccjit_int));
            }
        }
    }
    context.add_driver_option("-lm");
    context.dump_to_file("tmp", true);
    context.compile_to_file(OutputKind::Executable, "main");
}
