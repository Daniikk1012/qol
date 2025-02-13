// TODO Switch to using only types hashmap instead of gccjit_* types
// TODO String newlines
// TODO UTF-8 printing
// TODO Stirng input
// TODO Resize failures

use std::collections::HashMap;

use gccjit::{
    BinaryOp, CType, ComparisonOp, Context, Field, FunctionType, OutputKind, ToRValue, Type,
    UnaryOp,
};

use crate::compiler::{
    IrFunction, IrInstruction, IrInstructionType, IrPrimitiveType, IrTerminatedBlock, IrTerminator,
    IrTerminatorType, IrVariableType,
};

const TMP_FILENAME: &str = "main.qol";

#[derive(Debug, Clone, PartialEq, Eq)]
struct GccjitType<'a> {
    ty: Type<'a>,
    fields: Vec<Field<'a>>,
}

impl<'a> GccjitType<'a> {
    fn new(ty: Type<'a>) -> Self {
        Self {
            ty,
            fields: Vec::new(),
        }
    }
}

fn put_gccjit_type<'a>(
    context: &'a Context,
    cache: &mut HashMap<IrVariableType, GccjitType<'a>>,
    ty: &IrVariableType,
) {
    if cache.contains_key(ty) {
        return;
    }
    let gccjit_type = match ty {
        IrVariableType::Primitive(IrPrimitiveType::Boolean) => {
            GccjitType::new(context.new_type::<bool>())
        }
        IrVariableType::Primitive(IrPrimitiveType::Natural) => {
            GccjitType::new(context.new_type::<u64>())
        }
        IrVariableType::Primitive(IrPrimitiveType::Whole) => {
            GccjitType::new(context.new_type::<i64>())
        }
        IrVariableType::Primitive(IrPrimitiveType::Real) => {
            GccjitType::new(context.new_type::<f64>())
        }
        IrVariableType::Primitive(IrPrimitiveType::Character) => {
            GccjitType::new(context.new_type::<u32>())
        }
        IrVariableType::Primitive(IrPrimitiveType::Reference(ty)) => {
            put_gccjit_type(context, cache, ty);
            GccjitType::new(cache[ty].ty.make_pointer())
        }
        IrVariableType::Array(ty) => {
            put_gccjit_type(context, cache, ty);
            let fields = vec![
                context.new_field(None, cache[ty].ty.make_pointer(), "data"),
                context.new_field(
                    None,
                    cache[&IrVariableType::Primitive(IrPrimitiveType::Natural)].ty,
                    "count",
                ),
                context.new_field(
                    None,
                    cache[&IrVariableType::Primitive(IrPrimitiveType::Natural)].ty,
                    "capacity",
                ),
            ];
            let ty = context.new_struct_type(None, "array", &fields).as_type();
            GccjitType { ty, fields }
        }
        _ => todo!(),
    };
    cache.insert(ty.clone(), gccjit_type);
}

fn get_size(ty: &IrVariableType) -> usize {
    match ty {
        IrVariableType::Primitive(IrPrimitiveType::Boolean) => 1,
        IrVariableType::Primitive(IrPrimitiveType::Character) => 4,
        IrVariableType::Primitive(_) => 8,
        IrVariableType::Struct(..) => todo!(),
        IrVariableType::Array(_) => 24,
    }
}

pub fn compile(function: IrFunction) {
    let context = Context::default();
    let gccjit_int = context.new_c_type(CType::Int);
    let gccjit_function =
        context.new_function(None, FunctionType::Exported, gccjit_int, &[], "main", false);
    let mut types = HashMap::new();
    put_gccjit_type(
        &context,
        &mut types,
        &IrVariableType::Primitive(IrPrimitiveType::Boolean),
    );
    let gccjit_boolean = types[&IrVariableType::Primitive(IrPrimitiveType::Boolean)].ty;
    put_gccjit_type(
        &context,
        &mut types,
        &IrVariableType::Primitive(IrPrimitiveType::Natural),
    );
    let gccjit_natural = types[&IrVariableType::Primitive(IrPrimitiveType::Natural)].ty;
    put_gccjit_type(
        &context,
        &mut types,
        &IrVariableType::Primitive(IrPrimitiveType::Whole),
    );
    let gccjit_whole = types[&IrVariableType::Primitive(IrPrimitiveType::Whole)].ty;
    put_gccjit_type(
        &context,
        &mut types,
        &IrVariableType::Primitive(IrPrimitiveType::Real),
    );
    let gccjit_real = types[&IrVariableType::Primitive(IrPrimitiveType::Real)].ty;
    put_gccjit_type(
        &context,
        &mut types,
        &IrVariableType::Primitive(IrPrimitiveType::Character),
    );
    let gccjit_character = types[&IrVariableType::Primitive(IrPrimitiveType::Character)].ty;
    let gccjit_vars: Vec<_> = function
        .vars
        .iter()
        .enumerate()
        .map(|(index, ty)| {
            put_gccjit_type(&context, &mut types, ty);
            gccjit_function.new_local(None, types[ty].ty, format!("var_{index}"))
        })
        .collect();
    let (gccjit_temps, temp_var_types): (Vec<_>, Vec<_>) = function
        .temps
        .iter()
        .enumerate()
        .map(|(index, ty)| {
            let ty = IrVariableType::Primitive(ty.clone());
            put_gccjit_type(&context, &mut types, &ty);
            (
                gccjit_function.new_local(None, types[&ty].ty, format!("temp_{index}")),
                ty,
            )
        })
        .unzip();
    let gccjit_blocks: Vec<_> = (0..function.blocks.len())
        .map(|index| {
            if function.blocks[index].is_some() {
                Some(gccjit_function.new_block(format!("block_{index}")))
            } else {
                None
            }
        })
        .collect();
    let gccjit_void_ptr = context.new_type::<()>().make_pointer();
    let gccjit_size_t = context.new_c_type(CType::SizeT);
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
                IrInstructionType::Null => {
                    let IrPrimitiveType::Reference(ty) = &function.temps[temp] else {
                        unreachable!();
                    };
                    let gccjit_type = &types[ty];
                    let IrVariableType::Array(ty) = ty.as_ref() else {
                        unreachable!();
                    };
                    gccjit_block.add_assignment(
                        loc,
                        context
                            .new_array_access(
                                loc,
                                gccjit_temps[temp],
                                context.new_rvalue_zero(gccjit_int),
                            )
                            .access_field(loc, gccjit_type.fields[0]),
                        context.new_null(types[ty].ty.make_pointer()),
                    );
                }
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
                IrInstructionType::MinimumCapacity(val_temp) => {
                    gccjit_block.add_assignment(loc, gccjit_temps[temp], gccjit_temps[val_temp]);
                    gccjit_block.add_assignment_op(
                        loc,
                        gccjit_temps[temp],
                        BinaryOp::BitwiseOr,
                        context.new_binary_op(
                            loc,
                            BinaryOp::RShift,
                            gccjit_natural,
                            gccjit_temps[temp],
                            context.new_rvalue_from_int(gccjit_natural, 1),
                        ),
                    );
                    gccjit_block.add_assignment_op(
                        loc,
                        gccjit_temps[temp],
                        BinaryOp::BitwiseOr,
                        context.new_binary_op(
                            loc,
                            BinaryOp::RShift,
                            gccjit_natural,
                            gccjit_temps[temp],
                            context.new_rvalue_from_int(gccjit_natural, 2),
                        ),
                    );
                    gccjit_block.add_assignment_op(
                        loc,
                        gccjit_temps[temp],
                        BinaryOp::BitwiseOr,
                        context.new_binary_op(
                            loc,
                            BinaryOp::RShift,
                            gccjit_natural,
                            gccjit_temps[temp],
                            context.new_rvalue_from_int(gccjit_natural, 4),
                        ),
                    );
                    gccjit_block.add_assignment_op(
                        loc,
                        gccjit_temps[temp],
                        BinaryOp::BitwiseOr,
                        context.new_binary_op(
                            loc,
                            BinaryOp::RShift,
                            gccjit_natural,
                            gccjit_temps[temp],
                            context.new_rvalue_from_int(gccjit_natural, 8),
                        ),
                    );
                    gccjit_block.add_assignment_op(
                        loc,
                        gccjit_temps[temp],
                        BinaryOp::BitwiseOr,
                        context.new_binary_op(
                            loc,
                            BinaryOp::RShift,
                            gccjit_natural,
                            gccjit_temps[temp],
                            context.new_rvalue_from_int(gccjit_natural, 16),
                        ),
                    );
                    gccjit_block.add_assignment_op(
                        loc,
                        gccjit_temps[temp],
                        BinaryOp::BitwiseOr,
                        context.new_binary_op(
                            loc,
                            BinaryOp::RShift,
                            gccjit_natural,
                            gccjit_temps[temp],
                            context.new_rvalue_from_int(gccjit_natural, 32),
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
                        context.new_cast(
                            loc,
                            gccjit_vars[var].get_address(loc),
                            types[&temp_var_types[temp]].ty,
                        ),
                    );
                }
                IrInstructionType::Field(..) => todo!(),
                IrInstructionType::Count(arr_temp) => {
                    let IrPrimitiveType::Reference(ty) = &function.temps[arr_temp] else {
                        unreachable!();
                    };
                    let gccjit_type = &types[ty];
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context
                            .new_array_access(
                                loc,
                                gccjit_temps[arr_temp],
                                context.new_rvalue_zero(gccjit_int),
                            )
                            .access_field(loc, gccjit_type.fields[1])
                            .get_address(loc),
                    );
                }
                IrInstructionType::Capacity(arr_temp) => {
                    let IrPrimitiveType::Reference(ty) = &function.temps[arr_temp] else {
                        unreachable!();
                    };
                    let gccjit_type = &types[ty];
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context
                            .new_array_access(
                                loc,
                                gccjit_temps[arr_temp],
                                context.new_rvalue_zero(gccjit_int),
                            )
                            .access_field(loc, gccjit_type.fields[2])
                            .get_address(loc),
                    );
                }
                IrInstructionType::Index(arr_temp, index_temp) => {
                    let IrPrimitiveType::Reference(ty) = &function.temps[arr_temp] else {
                        unreachable!();
                    };
                    let gccjit_type = &types[ty];
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context
                            .new_array_access(
                                loc,
                                context
                                    .new_array_access(
                                        loc,
                                        gccjit_temps[arr_temp],
                                        context.new_rvalue_zero(gccjit_int),
                                    )
                                    .access_field(loc, gccjit_type.fields[0]),
                                gccjit_temps[index_temp],
                            )
                            .get_address(loc),
                    );
                }
                IrInstructionType::Assign(val_temp) => {
                    gccjit_block.add_assignment(
                        loc,
                        context.new_array_access(
                            loc,
                            gccjit_temps[temp],
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
                        IrPrimitiveType::Reference(_) => types[&temp_var_types[temp]].ty,
                    };
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_cast(
                            loc,
                            context.new_array_access(
                                loc,
                                gccjit_temps[value_temp],
                                context.new_rvalue_zero(gccjit_int),
                            ),
                            gccjit_type,
                        ),
                    );
                }
                IrInstructionType::Resize(size_temp) => {
                    // TODO Failure
                    let IrPrimitiveType::Reference(ty) = &function.temps[temp] else {
                        unreachable!();
                    };
                    let gccjit_type = &types[ty];
                    let IrVariableType::Array(ty) = ty.as_ref() else {
                        unreachable!();
                    };
                    let data = context
                        .new_array_access(
                            loc,
                            gccjit_temps[temp],
                            context.new_rvalue_zero(gccjit_int),
                        )
                        .access_field(loc, gccjit_type.fields[0]);
                    gccjit_block.add_assignment(
                        loc,
                        data,
                        context.new_cast(
                            loc,
                            context.new_call(
                                loc,
                                context.get_builtin_function("__builtin_realloc"),
                                &[
                                    context.new_cast(loc, data, gccjit_void_ptr),
                                    context.new_cast(
                                        loc,
                                        context.new_binary_op(
                                            loc,
                                            BinaryOp::Mult,
                                            gccjit_natural,
                                            gccjit_temps[size_temp],
                                            context.new_rvalue_from_int(
                                                gccjit_natural,
                                                get_size(ty) as _,
                                            ),
                                        ),
                                        gccjit_size_t,
                                    ),
                                ],
                            ),
                            types[ty].ty.make_pointer(),
                        ),
                    );
                }
                IrInstructionType::Free => {
                    let IrPrimitiveType::Reference(ty) = &function.temps[temp] else {
                        unreachable!();
                    };
                    let gccjit_type = &types[ty];
                    gccjit_block.add_eval(
                        loc,
                        context.new_call(
                            loc,
                            context.get_builtin_function("__builtin_free"),
                            &[context.new_cast(
                                loc,
                                context
                                    .new_array_access(
                                        loc,
                                        gccjit_temps[temp],
                                        context.new_rvalue_zero(gccjit_int),
                                    )
                                    .access_field(loc, gccjit_type.fields[0]),
                                gccjit_void_ptr,
                            )],
                        ),
                    );
                }
                IrInstructionType::Write(val_temp)
                    if function.temps[val_temp] == IrPrimitiveType::Boolean =>
                {
                    let gccjit_then_block =
                        gccjit_function.new_block(format!("block_{block_index}_then"));
                    let gccjit_else_block =
                        gccjit_function.new_block(format!("block_{block_index}_else"));
                    let gccjit_end_block =
                        gccjit_function.new_block(format!("block_{block_index}_end"));
                    gccjit_block.end_with_conditional(
                        loc,
                        gccjit_temps[val_temp],
                        gccjit_then_block,
                        gccjit_else_block,
                    );
                    gccjit_then_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_comparison(
                            loc,
                            ComparisonOp::GreaterThanEquals,
                            context.new_call(
                                loc,
                                context.get_builtin_function("__builtin_puts"),
                                &[context.new_string_literal("Ақиқат")],
                            ),
                            context.new_rvalue_zero(gccjit_int),
                        ),
                    );
                    gccjit_then_block.end_with_jump(loc, gccjit_end_block);
                    gccjit_else_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_comparison(
                            loc,
                            ComparisonOp::GreaterThanEquals,
                            context.new_call(
                                loc,
                                context.get_builtin_function("__builtin_puts"),
                                &[context.new_string_literal("Жалған\n")],
                            ),
                            context.new_rvalue_zero(gccjit_int),
                        ),
                    );
                    gccjit_else_block.end_with_jump(loc, gccjit_end_block);
                    gccjit_block = gccjit_end_block;
                }
                IrInstructionType::Write(val_temp) => {
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_comparison(
                            loc,
                            ComparisonOp::GreaterThanEquals,
                            context.new_call(
                                loc,
                                context.get_builtin_function("__builtin_printf"),
                                &[
                                    context.new_string_literal(match function.temps[val_temp] {
                                        IrPrimitiveType::Natural => "%llu\n",
                                        IrPrimitiveType::Whole => "%lld\n",
                                        IrPrimitiveType::Real => "%f\n",
                                        // TODO UTF-8
                                        IrPrimitiveType::Character => "%c",
                                        _ => unreachable!(),
                                    }),
                                    gccjit_temps[val_temp].to_rvalue(),
                                ],
                            ),
                            context.new_rvalue_zero(gccjit_int),
                        ),
                    );
                }
                IrInstructionType::Read(val_temp) => {
                    let IrPrimitiveType::Reference(ty) = &function.temps[val_temp] else {
                        unreachable!();
                    };
                    gccjit_block.add_assignment(
                        loc,
                        gccjit_temps[temp],
                        context.new_comparison(
                            loc,
                            ComparisonOp::GreaterThan,
                            context.new_call(
                                loc,
                                context.get_builtin_function("__builtin_scanf"),
                                &[
                                    context.new_string_literal(match ty.as_ref() {
                                        IrVariableType::Primitive(IrPrimitiveType::Natural) => {
                                            "%llu"
                                        }
                                        IrVariableType::Primitive(IrPrimitiveType::Whole) => "%lld",
                                        IrVariableType::Primitive(IrPrimitiveType::Real) => "%lf",
                                        // TODO UTF-8
                                        IrVariableType::Primitive(IrPrimitiveType::Character) => {
                                            " %c"
                                        }
                                        _ => unreachable!(),
                                    }),
                                    context.new_cast(
                                        loc,
                                        gccjit_temps[val_temp],
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
                                            IrVariableType::Primitive(
                                                IrPrimitiveType::Character,
                                            ) => gccjit_character.make_pointer(),
                                            _ => unreachable!(),
                                        },
                                    ),
                                ],
                            ),
                            context.new_rvalue_zero(gccjit_int),
                        ),
                    );
                }
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
            IrTerminatorType::Error(string) => {
                gccjit_block.add_eval(
                    loc,
                    context.new_call(
                        loc,
                        context.get_builtin_function("__builtin_puts"),
                        &[context.new_string_literal(string)],
                    ),
                );
                gccjit_block.add_eval(
                    loc,
                    context.new_call(
                        loc,
                        context.get_builtin_function("__builtin_exit"),
                        &[context.new_rvalue_zero(gccjit_int)],
                    ),
                );
                gccjit_block.end_with_jump(loc, gccjit_block);
            }
        }
    }
    context.add_driver_option("-lm");
    context.set_debug_info(true);
    context.dump_to_file("tmp", true);
    context.compile_to_file(OutputKind::Executable, "main");
}
