// TODO Remove use of newer features of libgccjit (Apparently, the library is VERY young), like:
// new_sizeof -> pointer arithmetic like (char *)&((T *)0)[1] - (char *)(T *)0;
// is_struct -> remove completely, as without get_field it's useless
// new_struct_constructor -> manually zero-initialize all fields;
// get_field -> store the fields alongside with the types themselves.
// TODO Move to cranelift for codegen, as that will remove the dynamic library dependency

use std::{
    collections::HashMap,
    fmt::Write,
    hash::{Hash, Hasher},
};

use gccjit::{
    BinaryOp, Block, CType, ComparisonOp, Context, Function, FunctionType, LValue, OutputKind,
    RValue, ToRValue, Type, UnaryOp,
};

use crate::{
    lexer::BinaryOperator,
    parser::{self, Expression, ExpressionType, Statement, StatementType},
};

const TMP_FILENAME: &str = "main.qol";
const MANGLE_PREFIX: &str = "_Q";

#[derive(Debug, Clone, Eq)]
enum ConcreteType {
    Boolean,
    Natural,
    Whole,
    Real,
    Character,
    Array(Box<ConcreteType>),
    Struct(Vec<(Option<ConcreteType>, String)>),
}

impl PartialEq for ConcreteType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ConcreteType::Boolean, ConcreteType::Boolean)
            | (ConcreteType::Natural, ConcreteType::Natural)
            | (ConcreteType::Whole, ConcreteType::Whole)
            | (ConcreteType::Real, ConcreteType::Real)
            | (ConcreteType::Character, ConcreteType::Character) => true,
            (ConcreteType::Array(a), ConcreteType::Array(b)) if a == b => true,
            (ConcreteType::Struct(a), ConcreteType::Struct(b))
                if a.len() == b.len() && a.iter().zip(b).all(|((a, _), (b, _))| a == b) =>
            {
                true
            }
            _ => false,
        }
    }
}

impl Hash for ConcreteType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            ConcreteType::Boolean => state.write_u8(0),
            ConcreteType::Natural => state.write_u8(1),
            ConcreteType::Whole => state.write_u8(2),
            ConcreteType::Real => state.write_u8(3),
            ConcreteType::Character => state.write_u8(4),
            ConcreteType::Array(ty) => {
                state.write_u8(5);
                ty.hash(state);
            }
            ConcreteType::Struct(vec) => {
                state.write_u8(6);
                for (ty, _) in vec {
                    ty.hash(state);
                }
            }
        }
    }
}

fn mangle_string(s: &str) -> String {
    s.bytes().fold(String::new(), |mut result, byte| {
        write!(result, "{byte:02x}").unwrap();
        result
    })
}

impl ConcreteType {
    fn mangled_name(&self) -> String {
        match self {
            ConcreteType::Boolean => "B".to_string(),
            ConcreteType::Natural => "N".to_string(),
            ConcreteType::Whole => "W".to_string(),
            ConcreteType::Real => "R".to_string(),
            ConcreteType::Character => "C".to_string(),
            ConcreteType::Array(ty) => format!("A{}", ty.mangled_name()),
            ConcreteType::Struct(fields) => format!(
                "S{}_",
                fields
                    .iter()
                    .map(|(ty, _)| if let Some(ty) = ty {
                        ty.mangled_name()
                    } else {
                        "R".to_string()
                    })
                    .collect::<String>()
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum GenericType {
    Parameter(usize),
    Concrete(ConcreteType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum BoundType<'a> {
    Noun(ConcreteType, LValue<'a>),
    Function(Vec<GenericType>, GenericType),
}

#[derive(Debug)]
struct Compiler<'a, 'ctx> {
    context: &'a Context<'ctx>,
    block: Option<Block<'a>>,
    types: Vec<HashMap<String, Vec<BoundType<'a>>>>,
    gccjit_types: HashMap<ConcreteType, Type<'a>>,
    functions: HashMap<String, Function<'a>>,
    loop_ends: Vec<(Block<'a>, usize)>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    fn get_type(&self, expr: &Expression) -> ConcreteType {
        match &expr.expression_type {
            ExpressionType::Boolean(_) => ConcreteType::Boolean,
            ExpressionType::Natural(_) => ConcreteType::Natural,
            ExpressionType::Real(_) => ConcreteType::Real,
            ExpressionType::Character(_) => ConcreteType::Character,
            ExpressionType::String(_) => ConcreteType::Array(Box::new(ConcreteType::Character)),
            ExpressionType::Noun(name) => {
                if let Some(BoundType::Noun(ty, _)) = self
                    .types
                    .iter()
                    .rev()
                    .find_map(|map| map.get(name))
                    .and_then(|vec| vec.last())
                {
                    ty.clone()
                } else {
                    panic!(
                        "incorrect noun type at {}:{}",
                        expr.line + 1,
                        expr.column + 1,
                    );
                }
            }
            ExpressionType::Negate(x) => match self.get_type(x) {
                ConcreteType::Real => ConcreteType::Real,
                ConcreteType::Natural | ConcreteType::Whole => ConcreteType::Whole,
                _ => panic!(
                    "invalid type for negation at {}:{}",
                    x.line + 1,
                    x.column + 1,
                ),
            },
            ExpressionType::BinaryOperation(a, op, b) => {
                let a_type = self.get_type(a);
                let b_type = self.get_type(b);
                let a_number = a_type == ConcreteType::Natural
                    || a_type == ConcreteType::Whole
                    || a_type == ConcreteType::Real;
                let b_number = b_type == ConcreteType::Natural
                    || b_type == ConcreteType::Whole
                    || b_type == ConcreteType::Real;
                match op {
                    BinaryOperator::Add => match (a_number, b_number) {
                        (true, true) => match (a_type, b_type) {
                            (ConcreteType::Real, _) | (_, ConcreteType::Real) => ConcreteType::Real,
                            (ConcreteType::Whole, _) | (_, ConcreteType::Whole) => {
                                ConcreteType::Whole
                            }
                            (ConcreteType::Natural, _) | (_, ConcreteType::Natural) => {
                                ConcreteType::Natural
                            }
                            _ => unreachable!("resolved in outer match"),
                        },
                        (false, _) => panic!(
                            "invalid type for addition at {}:{}",
                            a.line + 1,
                            a.column + 1,
                        ),
                        _ => panic!(
                            "invalid type for addition at {}:{}",
                            b.line + 1,
                            b.column + 1,
                        ),
                    },
                    BinaryOperator::Subtract => match (a_number, b_number) {
                        (true, true) => match (a_type, b_type) {
                            (ConcreteType::Real, _) | (_, ConcreteType::Real) => ConcreteType::Real,
                            (ConcreteType::Whole | ConcreteType::Natural, _)
                            | (_, ConcreteType::Whole | ConcreteType::Natural) => {
                                ConcreteType::Whole
                            }
                            _ => unreachable!("resolved in outer match"),
                        },
                        (false, _) => panic!(
                            "invalid type for subtraction at {}:{}",
                            a.line + 1,
                            a.column + 1,
                        ),
                        _ => panic!(
                            "invalid type for subtraction at {}:{}",
                            b.line + 1,
                            b.column + 1,
                        ),
                    },
                    BinaryOperator::Multiply => match (a_number, b_number) {
                        (true, true) => match (a_type, b_type) {
                            (ConcreteType::Real, _) | (_, ConcreteType::Real) => ConcreteType::Real,
                            (ConcreteType::Whole, _) | (_, ConcreteType::Whole) => {
                                ConcreteType::Whole
                            }
                            (ConcreteType::Natural, _) | (_, ConcreteType::Natural) => {
                                ConcreteType::Natural
                            }
                            _ => unreachable!("resolved in outer match"),
                        },
                        (false, _) => panic!(
                            "invalid type for multiplication at {}:{}",
                            a.line + 1,
                            a.column + 1,
                        ),
                        _ => panic!(
                            "invalid type for multiplication at {}:{}",
                            b.line + 1,
                            b.column + 1,
                        ),
                    },
                    BinaryOperator::Divide => match (a_number, b_number) {
                        (true, true) => ConcreteType::Real,
                        (false, _) => panic!(
                            "invalid type for division at {}:{}",
                            a.line + 1,
                            a.column + 1,
                        ),
                        _ => panic!(
                            "invalid type for division at {}:{}",
                            b.line + 1,
                            b.column + 1,
                        ),
                    },
                    BinaryOperator::Index => match (a_type, b_type) {
                        (ConcreteType::Array(ty), ConcreteType::Natural) => *ty,
                        (_, ConcreteType::Natural) => {
                            panic!("invalid array type at {}:{}", a.line + 1, a.column + 1);
                        }
                        _ => panic!("invalid index type at {}:{}", b.line + 1, b.column + 1),
                    },
                    BinaryOperator::And => match (a_type, b_type) {
                        (ConcreteType::Boolean, ConcreteType::Boolean) => ConcreteType::Boolean,
                        (_, ConcreteType::Boolean) => {
                            panic!("invalid type for and at {}:{}", a.line + 1, a.column + 1)
                        }
                        _ => panic!("invalid type for and at {}:{}", b.line + 1, b.column + 1),
                    },
                    BinaryOperator::Or => match (a_type, b_type) {
                        (ConcreteType::Boolean, ConcreteType::Boolean) => ConcreteType::Boolean,
                        (_, ConcreteType::Boolean) => {
                            panic!("invalid type for or at {}:{}", a.line + 1, a.column + 1)
                        }
                        _ => panic!("invalid type for or at {}:{}", b.line + 1, b.column + 1),
                    },
                }
            }
            ExpressionType::Field(x, field) => match (self.get_type(x), field.name.as_str()) {
                (ConcreteType::Array(_), "Саны") => ConcreteType::Natural,
                (ref ty @ ConcreteType::Struct(ref fields), _) => fields
                    .iter()
                    .find_map(|(field_type, name)| {
                        if *name == field.name {
                            Some(field_type.clone().unwrap_or_else(|| ty.clone()))
                        } else {
                            None
                        }
                    })
                    .unwrap_or_else(|| {
                        panic!(
                            "cannot find field {} at {}:{}",
                            field.name,
                            field.line + 1,
                            field.column + 1,
                        )
                    }),
                _ => panic!(
                    "invalid type for field access at {}:{}",
                    x.line + 1,
                    x.column + 1,
                ),
            },
            ExpressionType::Call(args, function) => {
                if let Some(BoundType::Function(arg_types, return_type)) =
                    self.types.iter().rev().find_map(|map| {
                        map.get(&function.name).and_then(|vec| {
                            vec.iter().rev().find(|ty| {
                                if let BoundType::Function(arg_types, _) = ty {
                                    if args.len() != arg_types.len() {
                                        return false;
                                    }
                                    let mut params = HashMap::new();
                                    for (arg, ty) in args
                                        .iter()
                                        .map(|arg| self.get_type(arg))
                                        .zip(arg_types.iter())
                                    {
                                        match ty {
                                            &GenericType::Parameter(param) => {
                                                if params.get(&param).map_or(false, |ty| *ty != arg)
                                                {
                                                    return false;
                                                }
                                                params.insert(param, arg);
                                            }
                                            GenericType::Concrete(ty) => {
                                                if *ty != arg {
                                                    return false;
                                                }
                                            }
                                        }
                                    }
                                    true
                                } else {
                                    false
                                }
                            })
                        })
                    })
                {
                    match return_type {
                        &GenericType::Parameter(return_param) => arg_types
                            .iter()
                            .enumerate()
                            .find_map(|(index, ty)| {
                                if let &GenericType::Parameter(param) = ty {
                                    if param == return_param {
                                        Some(self.get_type(&args[index]))
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            })
                            .unwrap_or_else(|| {
                                panic!(
                                    "incorrect function call at {}:{}",
                                    function.line + 1,
                                    function.column + 1
                                )
                            }),
                        GenericType::Concrete(ty) => ty.clone(),
                    }
                } else {
                    panic!(
                        "incorrect function call at {}:{}",
                        function.line + 1,
                        function.column + 1,
                    );
                }
            }
        }
    }

    fn get_gccjit_type(&mut self, ty: &ConcreteType) -> Type<'a> {
        if !self.gccjit_types.contains_key(ty) {
            let gccjit_type = match &ty {
                ConcreteType::Boolean => self.context.new_type::<bool>(),
                ConcreteType::Natural => self.context.new_type::<u64>(),
                ConcreteType::Whole => self.context.new_type::<i64>(),
                ConcreteType::Real => self.context.new_type::<f64>(),
                ConcreteType::Character => self.context.new_type::<char>(),
                ConcreteType::Array(element_type) => {
                    let values_type = self.get_gccjit_type(element_type).make_pointer();
                    let usize_type = self.context.new_type::<usize>();
                    let fields = &[
                        self.context.new_field(None, values_type, "values"),
                        self.context.new_field(None, usize_type, "count"),
                        self.context.new_field(None, usize_type, "capacity"),
                    ];
                    self.context
                        .new_struct_type(
                            None,
                            format!("{MANGLE_PREFIX}{}", ty.mangled_name()),
                            fields,
                        )
                        .as_type()
                }
                ConcreteType::Struct(fields) => {
                    let struct_type = self.context.new_opaque_struct_type(None, ty.mangled_name());
                    struct_type.set_fields(
                        None,
                        &fields
                            .iter()
                            .enumerate()
                            .map(|(index, (field_type, _))| {
                                self.context.new_field(
                                    None,
                                    self.get_gccjit_type(field_type.as_ref().unwrap_or(ty)),
                                    format!("t{index}"),
                                )
                            })
                            .collect::<Vec<_>>(),
                    );
                    todo!()
                }
            };
            self.gccjit_types.insert(ty.clone(), gccjit_type);
        }
        self.gccjit_types[ty]
    }

    fn get_concrete(&self, ty: &parser::Type) -> ConcreteType {
        let parser::Type(params, name) = ty;
        match (params.as_slice(), name.name.as_str()) {
            ([], "логикалық") => ConcreteType::Boolean,
            ([], "натурал") => ConcreteType::Natural,
            ([], "бүтін") => ConcreteType::Whole,
            ([], "нақты") => ConcreteType::Real,
            ([ty], "жиым") => ConcreteType::Array(Box::new(self.get_concrete(ty))),
            _ => todo!(),
        }
    }

    fn push_scope(&mut self) {
        self.types.push(HashMap::new());
    }

    fn compile_free(&mut self, ty: &ConcreteType, lvalue_variable: LValue<'a>) {
        let block = self
            .block
            .expect("freeing must happen within a non-terminated block");
        if let ConcreteType::Array(element_type) = &ty {
            let struct_array = self
                .get_gccjit_type(ty)
                .is_struct()
                .expect("arrays should always be represented as structures");
            let rvalue_values = lvalue_variable
                .access_field(None, struct_array.get_field(0))
                .to_rvalue();
            let ty_usize = self.context.new_type::<usize>();
            let lvalue_count = block.get_function().new_local(None, ty_usize, "count");
            block.add_assignment(
                None,
                lvalue_count,
                lvalue_variable
                    .access_field(None, struct_array.get_field(1))
                    .to_rvalue(),
            );
            let rvalue_one = self.context.new_rvalue_one(ty_usize);
            let block_condition = block.get_function().new_block("array free condition");
            let block_start = block.get_function().new_block("array free start");
            let block_end = block.get_function().new_block("array free end");
            block.end_with_jump(None, block_condition);
            block_condition.end_with_conditional(
                None,
                self.context.new_comparison(
                    None,
                    ComparisonOp::GreaterThan,
                    lvalue_count,
                    self.context.new_rvalue_zero(ty_usize),
                ),
                block_start,
                block_end,
            );
            self.block = Some(block_start);
            self.compile_free(
                element_type,
                self.context.new_array_access(
                    None,
                    rvalue_values,
                    self.context.new_binary_op(
                        None,
                        BinaryOp::Minus,
                        ty_usize,
                        lvalue_count,
                        rvalue_one,
                    ),
                ),
            );
            let block = self.block.expect("freeing should not terminate scope");
            block.add_assignment_op(None, lvalue_count, BinaryOp::Minus, rvalue_one);
            block.end_with_jump(None, block_condition);
            block_end.add_eval(
                None,
                self.context.new_call(
                    None,
                    self.functions["free"],
                    &[self.context.new_cast(
                        None,
                        rvalue_values,
                        self.context.new_type::<()>().make_pointer(),
                    )],
                ),
            );
            self.block = Some(block_end);
        }
    }

    fn free_scope(&mut self, index: usize) {
        if self.block.is_none() {
            return;
        }
        let types = self
            .types
            .get_mut(index)
            .expect("scope does not have enough levels")
            .clone();
        for vec in types.into_values() {
            for (ty, variable) in vec.into_iter().rev().filter_map(|ty| {
                if let BoundType::Noun(ty, lvalue) = ty {
                    Some((ty, lvalue))
                } else {
                    None
                }
            }) {
                self.compile_free(&ty, variable);
            }
        }
    }

    fn pop_scope(&mut self) {
        self.free_scope(self.types.len() - 1);
        self.types.pop().expect("scope is empty");
    }

    fn compile_lvalue(&mut self, expr: &Expression) -> LValue<'a> {
        let loc =
            self.context
                .new_location(TMP_FILENAME, expr.line as i32 + 1, expr.column as i32 + 1);
        match &expr.expression_type {
            ExpressionType::Noun(name) => *self
                .types
                .iter()
                .rev()
                .find_map(|map| map.get(name))
                .unwrap_or_else(|| {
                    panic!(
                        "no variable named {name} found at {}:{}",
                        expr.line + 1,
                        expr.column + 1
                    )
                })
                .iter()
                .rev()
                .find_map(|ty| {
                    if let BoundType::Noun(_, lvalue) = ty {
                        Some(lvalue)
                    } else {
                        None
                    }
                })
                .unwrap_or_else(|| panic!("{name} is not a variable")),
            ExpressionType::BinaryOperation(array, BinaryOperator::Index, index) => {
                let struct_array = self
                    .get_gccjit_type(&self.get_type(array))
                    .is_struct()
                    .expect("arrays must be represented as structs");
                let ty_usize = self.context.new_type::<usize>();
                let lvalue_array = self.compile_lvalue(array);
                let block = self
                    .block
                    .expect("compiling lvalue should not terminate a block");
                let lvalue_index =
                    block
                        .get_function()
                        .new_local(Some(loc), ty_usize, "lvalue index");
                block.add_assignment(Some(loc), lvalue_index, self.compile_expression(index));
                let block_then = block.get_function().new_block("lvalue out of bounds");
                let block_else = block.get_function().new_block("lvalue within bounds");
                block.end_with_conditional(
                    Some(loc),
                    self.context.new_comparison(
                        Some(loc),
                        ComparisonOp::GreaterThanEquals,
                        lvalue_index,
                        lvalue_array.access_field(Some(loc), struct_array.get_field(1)),
                    ),
                    block_then,
                    block_else,
                );
                block_then.add_eval(
                    Some(loc),
                    self.context.new_call(
                        Some(loc),
                        self.functions["printf"],
                        &[self.context.new_string_literal(format!(
                            "array access out of bounds at {}:{}",
                            expr.line + 1,
                            expr.column + 1
                        ))],
                    ),
                );
                block_then.add_eval(
                    Some(loc),
                    self.context.new_call(
                        Some(loc),
                        self.functions["exit"],
                        &[self
                            .context
                            .new_rvalue_one(self.context.new_c_type(CType::Int))],
                    ),
                );
                block_then.end_with_jump(Some(loc), block_else);
                self.block = Some(block_else);
                self.context.new_array_access(
                    Some(loc),
                    lvalue_array.access_field(Some(loc), struct_array.get_field(0)),
                    lvalue_index,
                )
            }
            ExpressionType::Field(expr, ident) => {
                let ty = self.get_type(expr);
                let lvalue_struct = self.compile_lvalue(expr);
                lvalue_struct.access_field(
                    Some(loc),
                    self.get_gccjit_type(&ty)
                        .is_struct()
                        .expect("fields can only be accessed on structs")
                        .get_field(
                            if let (ConcreteType::Array(_), "Саны") = (ty, ident.name.as_str())
                            {
                                1
                            } else {
                                todo!()
                            },
                        ),
                )
            }
            _ => todo!(),
        }
    }

    fn compile_binary(&mut self, expr: &Expression) -> RValue<'a> {
        let &Expression {
            expression_type: ExpressionType::BinaryOperation(ref left, ref op, ref right),
            line,
            column,
        } = expr
        else {
            unreachable!("should always be called with a binary operation");
        };
        let loc = self
            .context
            .new_location(TMP_FILENAME, line as i32 + 1, column as i32 + 1);
        let right_value = self.compile_expression(right);
        if *op == BinaryOperator::Index {
            let struct_array = self
                .get_gccjit_type(&self.get_type(left))
                .is_struct()
                .expect("arrays should be represented as structs");
            let lvalue_array = self.compile_lvalue(left);
            let block = self
                .block
                .expect("should always be called with a non-terminated block");
            let lvalue_index = block.get_function().new_local(
                Some(loc),
                self.context.new_type::<usize>(),
                "index",
            );
            block.add_assignment(Some(loc), lvalue_index, right_value);
            let block_then = block.get_function().new_block("then");
            let block_else = block.get_function().new_block("else");
            block.end_with_conditional(
                Some(loc),
                self.context.new_comparison(
                    Some(loc),
                    ComparisonOp::GreaterThanEquals,
                    lvalue_index.to_rvalue(),
                    lvalue_array.access_field(Some(loc), struct_array.get_field(1)),
                ),
                block_then,
                block_else,
            );
            block_then.add_eval(
                Some(loc),
                self.context.new_call(
                    Some(loc),
                    self.functions["printf"],
                    &[self.context.new_string_literal(format!(
                        "index out of bounds at {}:{}",
                        line + 1,
                        column + 1
                    ))],
                ),
            );
            block_then.add_eval(
                Some(loc),
                self.context.new_call(
                    Some(loc),
                    self.functions["exit"],
                    &[self
                        .context
                        .new_rvalue_one(self.context.new_c_type(CType::Int))],
                ),
            );
            block_then.end_with_jump(Some(loc), block_else);
            self.block = Some(block_else);
            self.context
                .new_array_access(
                    Some(loc),
                    lvalue_array.access_field(Some(loc), struct_array.get_field(0)),
                    lvalue_index.to_rvalue(),
                )
                .to_rvalue()
        } else {
            let result_type = self.get_type(expr);
            let left_value = self.compile_expression(left);
            let result_gccjit_type = self.get_gccjit_type(&result_type);
            self.context.new_binary_op(
                Some(loc),
                match op {
                    BinaryOperator::Add => BinaryOp::Plus,
                    BinaryOperator::Subtract => BinaryOp::Minus,
                    BinaryOperator::Multiply => BinaryOp::Mult,
                    BinaryOperator::Divide => BinaryOp::Divide,
                    BinaryOperator::And => BinaryOp::LogicalAnd,
                    BinaryOperator::Or => BinaryOp::LogicalOr,
                    _ => unreachable!("handled in the outher match"),
                },
                result_gccjit_type,
                self.context.new_cast(
                    Some(self.context.new_location(
                        TMP_FILENAME,
                        left.line as i32 + 1,
                        left.column as i32 + 1,
                    )),
                    left_value,
                    result_gccjit_type,
                ),
                self.context.new_cast(
                    Some(self.context.new_location(
                        TMP_FILENAME,
                        right.line as i32 + 1,
                        right.column as i32 + 1,
                    )),
                    right_value,
                    result_gccjit_type,
                ),
            )
        }
    }

    fn compile_call(&mut self, expr: &Expression) -> RValue<'a> {
        let &Expression {
            expression_type: ExpressionType::Call(ref args, ref name),
            line,
            column,
        } = expr
        else {
            unreachable!("should only be called with function call");
        };
        let loc = self
            .context
            .new_location(TMP_FILENAME, line as i32 + 1, column as i32 + 1);
        // Type checking should happen before compilation
        match (args.as_slice(), name.name.as_str()) {
            ([expr], "емес") => self.context.new_unary_op(
                Some(loc),
                UnaryOp::LogicalNegate,
                self.get_gccjit_type(&ConcreteType::Boolean),
                self.compile_expression(expr),
            ),
            ([a, b], "артық") => self.context.new_comparison(
                Some(loc),
                ComparisonOp::GreaterThan,
                self.compile_expression(a),
                self.compile_expression(b),
            ),
            ([a, b], "кем") => self.context.new_comparison(
                Some(loc),
                ComparisonOp::LessThan,
                self.compile_expression(a),
                self.compile_expression(b),
            ),
            ([a, b], "қалдық") => {
                let ty_a = self.get_type(a);
                let ty_b = self.get_type(b);
                let result_type = self.get_gccjit_type(&if ty_a == ConcreteType::Real
                    || ty_b == ConcreteType::Real
                {
                    ConcreteType::Real
                } else if ty_a == ConcreteType::Whole || ty_b == ConcreteType::Whole {
                    ConcreteType::Whole
                } else {
                    ConcreteType::Natural
                });
                self.context.new_binary_op(
                    Some(loc),
                    BinaryOp::Modulo,
                    result_type,
                    self.context
                        .new_cast(None, self.compile_expression(a), result_type),
                    self.context
                        .new_cast(None, self.compile_expression(b), result_type),
                )
            }
            ([a, b], "тең") => {
                let ty_a = self.get_type(a);
                let ty_b = self.get_type(b);
                let rvalue_a = self.compile_expression(a);
                let rvalue_b = self.compile_expression(b);
                let (rvalue_a, rvalue_b) = if ty_a == ty_b {
                    (rvalue_a, rvalue_b)
                } else if ty_a == ConcreteType::Natural
                    || ty_a == ConcreteType::Whole && ty_b == ConcreteType::Real
                {
                    (
                        self.context
                            .new_cast(None, rvalue_a, self.get_gccjit_type(&ty_b)),
                        rvalue_b,
                    )
                } else {
                    (
                        rvalue_a,
                        self.context
                            .new_cast(None, rvalue_b, self.get_gccjit_type(&ty_a)),
                    )
                };
                self.context
                    .new_comparison(Some(loc), ComparisonOp::Equals, rvalue_a, rvalue_b)
            }
            _ => todo!(),
        }
    }

    fn compile_expression(&mut self, expr: &Expression) -> RValue<'a> {
        let loc =
            self.context
                .new_location(TMP_FILENAME, expr.line as i32 + 1, expr.column as i32 + 1);
        match &expr.expression_type {
            ExpressionType::Boolean(false) => self
                .context
                .new_rvalue_zero(self.get_gccjit_type(&ConcreteType::Boolean)),
            ExpressionType::Boolean(true) => self
                .context
                .new_rvalue_one(self.get_gccjit_type(&ConcreteType::Boolean)),
            &ExpressionType::Natural(number) => self
                .context
                .new_rvalue_from_long(self.get_gccjit_type(&ConcreteType::Natural), number as i64),
            // TODO Make sure it always fits into a long
            &ExpressionType::Real(number) => self
                .context
                .new_rvalue_from_double(self.get_gccjit_type(&ConcreteType::Real), number),
            &ExpressionType::Character(ch) => self
                .context
                .new_rvalue_from_long(self.get_gccjit_type(&ConcreteType::Character), ch as i64),
            ExpressionType::Noun(..) | ExpressionType::Field(..) => {
                let rvalue = self.compile_lvalue(expr).to_rvalue();
                if let ExpressionType::Field(array_expr, ident) = &expr.expression_type {
                    if let (ConcreteType::Array(_), "Саны") =
                        (self.get_type(array_expr), ident.name.as_str())
                    {
                        self.context
                            .new_cast(Some(loc), rvalue, self.context.new_type::<u64>())
                    } else {
                        rvalue
                    }
                } else {
                    rvalue
                }
            }
            ExpressionType::Negate(value) => self.context.new_unary_op(
                Some(loc),
                UnaryOp::Minus,
                self.get_gccjit_type(&self.get_type(value)),
                self.compile_expression(value),
            ),
            ExpressionType::BinaryOperation(..) => self.compile_binary(expr),
            ExpressionType::Call(..) => self.compile_call(expr),
            _ => todo!(),
        }
    }

    fn compile_conditional(&mut self, stmt: &Statement) {
        let &Statement {
            statement_type: StatementType::Conditional(ref expr, ref stmt_then, ref stmt_else),
            line,
            column,
        } = stmt
        else {
            unreachable!("should only be called with a conditional");
        };
        if self.get_type(expr) != ConcreteType::Boolean {
            panic!(
                "wrong type for condition at {}:{}",
                expr.line + 1,
                expr.column + 1
            );
        }
        let rvalue_condition = self.compile_expression(expr);
        let block = self
            .block
            .expect("block must not terminated after compiling an expression");
        self.push_scope();
        let block_then = block.get_function().new_block("then");
        self.block = Some(block_then);
        self.compile_statement(stmt_then);
        let block_then_end = self.block;
        self.pop_scope();
        self.push_scope();
        let block_else = block.get_function().new_block("else");
        let block_else_end = if let Some(stmt_else) = stmt_else {
            self.block = Some(block_else);
            self.compile_statement(stmt_else);
            self.block
        } else {
            Some(block_else)
        };
        self.pop_scope();
        block.end_with_conditional(
            Some(
                self.context
                    .new_location(TMP_FILENAME, line as i32 + 1, column as i32 + 1),
            ),
            rvalue_condition,
            block_then,
            block_else,
        );
        self.block = if block_then_end.is_some() || block_else_end.is_some() {
            let block_after = block.get_function().new_block("after");
            if let Some(block_then_end) = block_then_end {
                block_then_end.end_with_jump(None, block_after);
            }
            if let Some(block_else_end) = block_else_end {
                block_else_end.end_with_jump(None, block_after);
            }
            Some(block_after)
        } else {
            None
        };
    }

    fn compile_copy(
        &mut self,
        ty: &ConcreteType,
        lvalue_copy: LValue<'a>,
        rvalue_initial: RValue<'a>,
    ) {
        let block = self
            .block
            .expect("should only be called with a valid block");
        match &ty {
            ConcreteType::Boolean
            | ConcreteType::Natural
            | ConcreteType::Whole
            | ConcreteType::Real
            | ConcreteType::Character => block.add_assignment(None, lvalue_copy, rvalue_initial),
            ConcreteType::Array(element_type) => {
                let struct_array = self
                    .get_gccjit_type(ty)
                    .is_struct()
                    .expect("arrays must be represented as structs");
                let field_values = struct_array.get_field(0);
                let field_count = struct_array.get_field(1);
                let field_capacity = struct_array.get_field(2);
                let ty_value = self.get_gccjit_type(element_type);
                let ty_values = ty_value.make_pointer();
                let ty_usize = self.context.new_type::<usize>();
                let lvalue_initial =
                    block
                        .get_function()
                        .new_local(None, struct_array.as_type(), "to_copy");
                block.add_assignment(None, lvalue_initial, rvalue_initial);
                let lvalue_values = lvalue_copy.access_field(None, field_values);
                let lvalue_count = lvalue_copy.access_field(None, field_count);
                let rvalue_capacity = lvalue_initial
                    .to_rvalue()
                    .access_field(None, field_capacity);
                block.add_assignment(
                    None,
                    lvalue_values,
                    self.context.new_cast(
                        None,
                        self.context.new_call(
                            None,
                            self.functions["realloc"],
                            &[
                                self.context
                                    .new_null(self.context.new_type::<()>().make_pointer()),
                                self.context.new_binary_op(
                                    None,
                                    BinaryOp::Mult,
                                    ty_usize,
                                    rvalue_capacity,
                                    self.context
                                        .new_rvalue_from_long(ty_usize, ty_value.get_size() as i64),
                                ),
                            ],
                        ),
                        ty_values,
                    ),
                );
                let block_error = block.get_function().new_block("error");
                let block_continue = block.get_function().new_block("continue");
                block.end_with_conditional(
                    None,
                    self.context.new_comparison(
                        None,
                        ComparisonOp::Equals,
                        lvalue_values.to_rvalue(),
                        self.context.new_null(ty_values),
                    ),
                    block_error,
                    block_continue,
                );
                block_error.add_eval(
                    None,
                    self.context.new_call(
                        None,
                        self.functions["printf"],
                        &[self
                            .context
                            .new_string_literal("couldn't allocate a copy of array\n")],
                    ),
                );
                block_error.add_eval(
                    None,
                    self.context.new_call(
                        None,
                        self.functions["exit"],
                        &[self
                            .context
                            .new_rvalue_one(self.context.new_c_type(CType::Int))],
                    ),
                );
                block_error.end_with_jump(None, block_continue);
                block_continue.add_assignment(
                    None,
                    lvalue_count,
                    lvalue_initial.access_field(None, field_count).to_rvalue(),
                );
                block_continue.add_assignment(
                    None,
                    lvalue_copy.access_field(None, field_capacity),
                    rvalue_capacity,
                );
                let lvalue_index = block_continue
                    .get_function()
                    .new_local(None, ty_usize, "index");
                block_continue.add_assignment(
                    None,
                    lvalue_index,
                    self.context.new_rvalue_zero(ty_usize),
                );
                let block_condition = block_continue.get_function().new_block("condition");
                let block_start = block_continue.get_function().new_block("start");
                let block_end = block_continue.get_function().new_block("end");
                block_continue.end_with_jump(None, block_condition);
                block_condition.end_with_conditional(
                    None,
                    self.context.new_comparison(
                        None,
                        ComparisonOp::LessThan,
                        lvalue_index.to_rvalue(),
                        lvalue_count,
                    ),
                    block_start,
                    block_end,
                );
                self.block = Some(block_start);
                self.compile_copy(
                    element_type,
                    self.context.new_array_access(
                        None,
                        lvalue_values.to_rvalue(),
                        lvalue_index.to_rvalue(),
                    ),
                    self.context
                        .new_array_access(
                            None,
                            lvalue_initial.access_field(None, field_values).to_rvalue(),
                            lvalue_index.to_rvalue(),
                        )
                        .to_rvalue(),
                );
                let block = self.block.expect("should not end block after a copy");
                block.add_assignment_op(
                    None,
                    lvalue_index,
                    BinaryOp::Plus,
                    self.context.new_rvalue_one(ty_usize),
                );
                block.end_with_jump(None, block_condition);
                self.block = Some(block_end);
            }
            _ => todo!(),
        }
    }

    fn compile_loop(&mut self, stmt: &Statement) {
        let &Statement {
            statement_type: StatementType::Loop(ref stmt),
            line,
            column,
        } = stmt
        else {
            unreachable!("should only be called with a loop");
        };
        let block = self
            .block
            .expect("should only be called with a valid block");
        let block_start = block.get_function().new_block("start");
        block.end_with_jump(
            Some(
                self.context
                    .new_location(TMP_FILENAME, line as i32 + 1, column as i32 + 1),
            ),
            block_start,
        );
        let block_end = block.get_function().new_block("end");
        self.loop_ends.push((block_end, self.types.len()));
        self.push_scope();
        self.block = Some(block_start);
        self.compile_statement(stmt);
        let block = self.block;
        self.pop_scope();
        self.loop_ends.pop().expect("loop stack is empty");
        block
            .unwrap_or_else(|| panic!("loop doesn't iterate at {}:{}", line + 1, column + 1))
            .end_with_jump(None, block_start);
        self.block = Some(block_end);
    }

    fn compile_break(&mut self, stmt: &Statement) {
        let &Statement {
            statement_type: StatementType::Break,
            line,
            column,
        } = stmt
        else {
            unreachable!("should only be called with a break");
        };
        let (block_end, len) = *self.loop_ends.last().unwrap_or_else(|| {
            panic!(
                "break should be called from loop at {}:{}",
                line + 1,
                column + 1
            )
        });
        for index in (len + 1..self.types.len()).rev() {
            self.free_scope(index);
        }
        let block = self
            .block
            .expect("cleaning scope should not terminate the block");
        block.end_with_jump(
            Some(
                self.context
                    .new_location(TMP_FILENAME, line as i32 + 1, column as i32 + 1),
            ),
            block_end,
        );
        self.block = None;
    }

    fn compile_zero_initialization(&mut self, ty: &ConcreteType, lvalue: LValue<'a>) {
        let gccjit_type = self.get_gccjit_type(ty);
        self.block
            .expect("should only be called with a non-terminated block")
            .add_assignment(
                None,
                lvalue,
                if let ConcreteType::Boolean
                | ConcreteType::Natural
                | ConcreteType::Whole
                | ConcreteType::Real
                | ConcreteType::Character = ty
                {
                    self.context.new_rvalue_zero(gccjit_type)
                } else {
                    self.context
                        .new_struct_constructor(None, gccjit_type, None, &[])
                },
            )
    }

    fn compile_variable(&mut self, stmt: &Statement) {
        let &Statement {
            statement_type: StatementType::Variable(ref value, ref ty, ref name),
            line,
            column,
        } = stmt
        else {
            unreachable!("should only be called with variable declaration");
        };
        let loc = self
            .context
            .new_location(TMP_FILENAME, line as i32 + 1, column as i32 + 1);
        let concrete_type = self.get_concrete(ty);
        let gccjit_type = self.get_gccjit_type(&concrete_type);
        let block = self
            .block
            .expect("should only be called with a valid block");
        let variable = block
            .get_function()
            .new_local(Some(loc), gccjit_type, &name.name);
        self.types
            .last_mut()
            .expect("scope is empty")
            .entry(name.name.clone())
            .or_default()
            .push(BoundType::Noun(concrete_type.clone(), variable));
        if let Some(value) = value {
            let ty = self.get_type(value);
            if ty != concrete_type
                && (ty != ConcreteType::Natural
                    || concrete_type != ConcreteType::Whole && concrete_type != ConcreteType::Real)
                && (ty != ConcreteType::Whole || concrete_type != ConcreteType::Real)
            {
                panic!(
                    "wrong type for variable initialization at {}:{}",
                    value.line + 1,
                    value.column + 1
                );
            }
            let rvalue = self.compile_expression(value);
            let rvalue = if ty == concrete_type {
                rvalue
            } else {
                self.context.new_cast(None, rvalue, gccjit_type)
            };
            self.compile_copy(&ty, variable, rvalue);
        } else {
            self.compile_zero_initialization(&concrete_type, variable);
        }
    }

    fn compile_block(&mut self, stmt: &Statement) {
        let Statement {
            statement_type: StatementType::Block(stmts),
            ..
        } = stmt
        else {
            unreachable!("should always be called with a block");
        };
        self.push_scope();
        for stmt in stmts {
            if self.block.is_none() {
                panic!("unreachable code at {}:{}", stmt.line + 1, stmt.column + 1);
            };
            self.compile_statement(stmt);
        }
        self.pop_scope();
    }

    fn compile_array_resize(
        &mut self,
        line: usize,
        column: usize,
        array_type: &ConcreteType,
        lvalue_old_count: LValue<'a>,
        lvalue_array: LValue<'a>,
    ) {
        let loc = self
            .context
            .new_location(TMP_FILENAME, line as i32 + 1, column as i32 + 1);
        let ConcreteType::Array(element_type) = array_type else {
            unreachable!("should only be called with an array");
        };
        let block = self
            .block
            .expect("should only be called with a non-terminated block");
        let struct_array = self
            .get_gccjit_type(array_type)
            .is_struct()
            .expect("arrays must be represented as structs");
        let ty_natural = self.context.new_type::<u64>();
        let ty_usize = self.context.new_type::<usize>();
        let ty_int = self.context.new_c_type(CType::Int);
        let ty_element = self.get_gccjit_type(element_type);
        let lvalue_values = lvalue_array.access_field(Some(loc), struct_array.get_field(0));
        let lvalue_count = lvalue_array.access_field(Some(loc), struct_array.get_field(1));
        let lvalue_capacity = lvalue_array.access_field(Some(loc), struct_array.get_field(2));
        let block_increase = block.get_function().new_block("array capacity increase");
        let block_else = block.get_function().new_block("array size decrease");
        block.end_with_conditional(
            Some(loc),
            self.context.new_comparison(
                Some(loc),
                ComparisonOp::LessThan,
                lvalue_capacity,
                lvalue_count,
            ),
            block_increase,
            block_else,
        );
        block_increase.add_assignment(Some(loc), lvalue_capacity, lvalue_count);
        block_increase.add_assignment_op(
            Some(loc),
            lvalue_capacity,
            BinaryOp::BitwiseOr,
            self.context.new_binary_op(
                Some(loc),
                BinaryOp::RShift,
                ty_natural,
                lvalue_capacity,
                self.context.new_rvalue_from_int(ty_natural, 1),
            ),
        );
        block_increase.add_assignment_op(
            Some(loc),
            lvalue_capacity,
            BinaryOp::BitwiseOr,
            self.context.new_binary_op(
                Some(loc),
                BinaryOp::RShift,
                ty_natural,
                lvalue_capacity,
                self.context.new_rvalue_from_int(ty_natural, 2),
            ),
        );
        block_increase.add_assignment_op(
            Some(loc),
            lvalue_capacity,
            BinaryOp::BitwiseOr,
            self.context.new_binary_op(
                Some(loc),
                BinaryOp::RShift,
                ty_natural,
                lvalue_capacity,
                self.context.new_rvalue_from_int(ty_natural, 4),
            ),
        );
        block_increase.add_assignment_op(
            Some(loc),
            lvalue_capacity,
            BinaryOp::BitwiseOr,
            self.context.new_binary_op(
                Some(loc),
                BinaryOp::RShift,
                ty_natural,
                lvalue_capacity,
                self.context.new_rvalue_from_int(ty_natural, 8),
            ),
        );
        block_increase.add_assignment_op(
            Some(loc),
            lvalue_capacity,
            BinaryOp::BitwiseOr,
            self.context.new_binary_op(
                Some(loc),
                BinaryOp::RShift,
                ty_natural,
                lvalue_capacity,
                self.context.new_rvalue_from_int(ty_natural, 16),
            ),
        );
        block_increase.add_assignment_op(
            Some(loc),
            lvalue_capacity,
            BinaryOp::BitwiseOr,
            self.context.new_binary_op(
                Some(loc),
                BinaryOp::RShift,
                ty_natural,
                lvalue_capacity,
                self.context.new_rvalue_from_int(ty_natural, 32),
            ),
        );
        block_increase.add_assignment(
            Some(loc),
            lvalue_values,
            self.context.new_cast(
                Some(loc),
                self.context.new_call(
                    Some(loc),
                    self.functions["realloc"],
                    &[
                        self.context.new_cast(
                            Some(loc),
                            lvalue_values.to_rvalue(),
                            self.context.new_type::<()>().make_pointer(),
                        ),
                        self.context.new_binary_op(
                            Some(loc),
                            BinaryOp::Mult,
                            ty_usize,
                            lvalue_capacity,
                            self.context.new_cast(
                                Some(loc),
                                self.context.new_sizeof(ty_element),
                                ty_usize,
                            ),
                        ),
                    ],
                ),
                ty_element.make_pointer(),
            ),
        );
        let block_error = block_increase.get_function().new_block("alloc error");
        let block_zero = block_increase
            .get_function()
            .new_block("zero initialization");
        block_increase.end_with_conditional(
            Some(loc),
            self.context.new_comparison(
                Some(loc),
                ComparisonOp::Equals,
                lvalue_values,
                self.context.new_null(ty_element.make_pointer()),
            ),
            block_error,
            block_zero,
        );
        block_error.add_eval(
            Some(loc),
            self.context.new_call(
                Some(loc),
                self.functions["printf"],
                &[self.context.new_string_literal(format!(
                    "error resizing array at {}:{}",
                    line + 1,
                    column + 1
                ))],
            ),
        );
        block_error.add_eval(
            Some(loc),
            self.context.new_call(
                Some(loc),
                self.functions["exit"],
                &[self.context.new_rvalue_one(ty_int)],
            ),
        );
        block_error.end_with_jump(Some(loc), block_zero);
        let block_zero_loop = block_zero
            .get_function()
            .new_block("zero initialization loop");
        let block_continue = block_zero.get_function().new_block("after array resize");
        block_zero.end_with_conditional(
            Some(loc),
            self.context.new_comparison(
                Some(loc),
                ComparisonOp::LessThan,
                lvalue_old_count,
                self.context.new_cast(Some(loc), lvalue_count, ty_natural),
            ),
            block_zero_loop,
            block_continue,
        );
        self.block = Some(block_zero_loop);
        self.compile_zero_initialization(
            element_type,
            self.context
                .new_array_access(Some(loc), lvalue_values, lvalue_old_count),
        );
        let block = self
            .block
            .expect("block should not be empty after zero initialization");
        block.add_assignment_op(
            Some(loc),
            lvalue_old_count,
            BinaryOp::Plus,
            self.context.new_rvalue_one(ty_natural),
        );
        block.end_with_jump(Some(loc), block_zero);
        let block_loop = block_else.get_function().new_block("resize free loop");
        block_else.end_with_conditional(
            Some(loc),
            self.context.new_comparison(
                Some(loc),
                ComparisonOp::GreaterThan,
                lvalue_old_count,
                self.context.new_cast(Some(loc), lvalue_count, ty_natural),
            ),
            block_loop,
            block_continue,
        );
        self.block = Some(block_loop);
        self.compile_free(
            element_type,
            self.context.new_array_access(
                Some(loc),
                lvalue_values,
                self.context.new_binary_op(
                    Some(loc),
                    BinaryOp::Minus,
                    ty_natural,
                    lvalue_old_count,
                    self.context.new_rvalue_one(ty_natural),
                ),
            ),
        );
        let block = self.block.expect("free should not terminate the block");
        block.add_assignment_op(
            Some(loc),
            lvalue_old_count,
            BinaryOp::Minus,
            self.context.new_rvalue_one(ty_natural),
        );
        block.end_with_jump(Some(loc), block_else);
        self.block = Some(block_continue);
    }

    fn compile_procedure_call(&mut self, stmt: &Statement) {
        let &Statement {
            statement_type: StatementType::Call(ref args, ref name),
            line,
            column,
        } = stmt
        else {
            unreachable!("should always be called with a procedure call");
        };
        let block = self
            .block
            .expect("should only be called with a valid block");
        let loc = self
            .context
            .new_location(TMP_FILENAME, line as i32 + 1, column as i32 + 1);
        match (name.name.as_str(), args.as_slice()) {
            ("ал", [arg]) => {
                let array_type = self.get_type(arg);
                let ConcreteType::Array(ty) = &array_type else {
                    panic!("expected array at {}:{}", arg.line + 1, arg.column + 1);
                };
                let ty_usize = self.context.new_type::<usize>();
                let struct_array = self
                    .get_gccjit_type(&array_type)
                    .is_struct()
                    .expect("arrays should always be represented as structs");
                let lvalue_array = self.compile_lvalue(arg);
                let block = self
                    .block
                    .expect("block must not be terminated after compiling an expression");
                let lvalue_values = lvalue_array.access_field(Some(loc), struct_array.get_field(0));
                let lvalue_count = lvalue_array.access_field(Some(loc), struct_array.get_field(1));
                let block_then = block.get_function().new_block("then");
                let block_else = block.get_function().new_block("else");
                block.end_with_conditional(
                    Some(loc),
                    self.context.new_comparison(
                        Some(loc),
                        ComparisonOp::Equals,
                        lvalue_count.to_rvalue(),
                        self.context.new_rvalue_zero(ty_usize),
                    ),
                    block_then,
                    block_else,
                );
                block_then.add_eval(
                    Some(loc),
                    self.context.new_call(
                        Some(loc),
                        self.functions["printf"],
                        &[self.context.new_string_literal(format!(
                            "attempt to pop an empty array at {}:{}",
                            arg.line + 1,
                            arg.column + 1
                        ))],
                    ),
                );
                block_then.add_eval(
                    Some(loc),
                    self.context.new_call(
                        Some(loc),
                        self.functions["exit"],
                        &[self
                            .context
                            .new_rvalue_one(self.context.new_c_type(CType::Int))],
                    ),
                );
                block_then.end_with_jump(Some(loc), block_else);
                block_else.add_assignment_op(
                    Some(loc),
                    lvalue_count,
                    BinaryOp::Minus,
                    self.context.new_rvalue_one(ty_usize),
                );
                self.block = Some(block_else);
                self.compile_free(
                    ty,
                    self.context
                        .new_array_access(Some(loc), lvalue_values, lvalue_count),
                );
                return;
            }
            ("жаз", [arg]) => {
                match self.get_type(arg) {
                    ConcreteType::Boolean => {
                        let block_true = block.get_function().new_block("true");
                        block_true.add_eval(
                            None,
                            self.context.new_call(
                                None,
                                self.functions["printf"],
                                &[self.context.new_string_literal("Ақиқат\n")],
                            ),
                        );
                        let block_false = block.get_function().new_block("false");
                        block_false.add_eval(
                            None,
                            self.context.new_call(
                                None,
                                self.functions["printf"],
                                &[self.context.new_string_literal("Жалған\n")],
                            ),
                        );
                        let rvalue_condition = self.compile_expression(arg);
                        let block = self
                            .block
                            .expect("block must not be terminated after compiling an expression");
                        block.end_with_conditional(
                            Some(loc),
                            rvalue_condition,
                            block_true,
                            block_false,
                        );
                        let block_after = block.get_function().new_block("after");
                        block_true.end_with_jump(None, block_after);
                        block_false.end_with_jump(None, block_after);
                        self.block = Some(block_after);
                        return;
                    }
                    ConcreteType::Natural => {
                        let rvalue = self.compile_expression(arg);
                        let block = self
                            .block
                            .expect("block must not be terminated after compiling an expression");
                        block.add_eval(
                            Some(loc),
                            self.context.new_call(
                                Some(loc),
                                self.functions["printf"],
                                &[self.context.new_string_literal("%llu\n"), rvalue],
                            ),
                        );
                    }
                    ConcreteType::Whole => {
                        let rvalue = self.compile_expression(arg);
                        let block = self
                            .block
                            .expect("block must not be terminated after compiling an expression");
                        block.add_eval(
                            Some(loc),
                            self.context.new_call(
                                Some(loc),
                                self.functions["printf"],
                                &[self.context.new_string_literal("%lld\n"), rvalue],
                            ),
                        );
                    }
                    ConcreteType::Real => {
                        let rvalue = self.compile_expression(arg);
                        let block = self
                            .block
                            .expect("block must not be terminated after compiling an expression");
                        block.add_eval(
                            Some(loc),
                            self.context.new_call(
                                Some(loc),
                                self.functions["printf"],
                                &[self.context.new_string_literal("%f\n"), rvalue],
                            ),
                        );
                    }
                    // TODO String printing
                    _ => panic!(
                        "invalid type for printing at {}:{}",
                        arg.line + 1,
                        arg.column + 1,
                    ),
                }
                return;
            }
            ("қос", [variable, value]) => {
                let array_type = self.get_type(variable);
                let ConcreteType::Array(ty) = &array_type else {
                    panic!(
                        "expected array at {}:{}",
                        variable.line + 1,
                        variable.column + 1
                    );
                };
                if self.get_type(value) != **ty {
                    panic!(
                        "invalid type for pushing at {}:{}",
                        value.line + 1,
                        value.column + 1
                    );
                }
                let gccjit_type = self
                    .get_gccjit_type(&array_type)
                    .is_struct()
                    .expect("should only be called with arrays");
                let lvalue = self.compile_lvalue(variable);
                let rvalue = self.compile_expression(value);
                let block = self
                    .block
                    .expect("block must not be terminated after compiling an expression");
                let block_capacity = block.get_function().new_block("capacity");
                let block_error = block.get_function().new_block("error");
                let block_push = block.get_function().new_block("push");
                block.end_with_conditional(
                    Some(loc),
                    self.context.new_comparison(
                        None,
                        ComparisonOp::Equals,
                        lvalue
                            .access_field(None, gccjit_type.get_field(1))
                            .to_rvalue(),
                        lvalue
                            .access_field(None, gccjit_type.get_field(2))
                            .to_rvalue(),
                    ),
                    block_capacity,
                    block_push,
                );
                block_capacity.add_assignment(
                    None,
                    lvalue.access_field(None, gccjit_type.get_field(2)),
                    self.context.new_binary_op(
                        None,
                        BinaryOp::Plus,
                        self.context.new_type::<usize>(),
                        self.context.new_binary_op(
                            None,
                            BinaryOp::Mult,
                            self.context.new_type::<usize>(),
                            lvalue.access_field(None, gccjit_type.get_field(2)),
                            self.context
                                .new_rvalue_from_int(self.context.new_type::<usize>(), 2),
                        ),
                        self.context
                            .new_rvalue_from_int(self.context.new_type::<usize>(), 1),
                    ),
                );
                block_capacity.add_assignment(
                    None,
                    lvalue.access_field(None, gccjit_type.get_field(0)),
                    self.context.new_cast(
                        None,
                        self.context.new_call(
                            None,
                            self.functions["realloc"],
                            &[
                                self.context.new_cast(
                                    None,
                                    lvalue
                                        .access_field(None, gccjit_type.get_field(0))
                                        .to_rvalue(),
                                    self.context.new_type::<()>().make_pointer(),
                                ),
                                self.context.new_binary_op(
                                    None,
                                    BinaryOp::Mult,
                                    self.context.new_type::<usize>(),
                                    lvalue
                                        .access_field(None, gccjit_type.get_field(2))
                                        .to_rvalue(),
                                    self.context.new_cast(
                                        Some(loc),
                                        self.context.new_sizeof(self.get_gccjit_type(ty)),
                                        self.context.new_type::<usize>(),
                                    ),
                                ),
                            ],
                        ),
                        self.get_gccjit_type(ty).make_pointer(),
                    ),
                );
                block_capacity.end_with_conditional(
                    None,
                    self.context.new_comparison(
                        None,
                        ComparisonOp::Equals,
                        lvalue
                            .access_field(None, gccjit_type.get_field(0))
                            .to_rvalue(),
                        self.context
                            .new_null(self.get_gccjit_type(ty).make_pointer()),
                    ),
                    block_error,
                    block_push,
                );
                block_error.add_eval(
                    None,
                    self.context.new_call(
                        None,
                        self.functions["printf"],
                        &[self.context.new_string_literal(format!(
                            "error pushing into array, out of memory at {}:{}",
                            line + 1,
                            column + 1
                        ))],
                    ),
                );
                block_error.add_eval(
                    None,
                    self.context.new_call(
                        None,
                        self.functions["exit"],
                        &[self
                            .context
                            .new_rvalue_one(self.context.new_c_type(CType::Int))],
                    ),
                );
                block_error.end_with_jump(None, block_push);
                block_push.add_assignment(
                    None,
                    self.context.new_array_access(
                        None,
                        lvalue.access_field(None, gccjit_type.get_field(0)),
                        lvalue
                            .access_field(None, gccjit_type.get_field(1))
                            .to_rvalue(),
                    ),
                    rvalue,
                );
                block_push.add_assignment_op(
                    None,
                    lvalue.access_field(None, gccjit_type.get_field(1)),
                    BinaryOp::Plus,
                    self.context
                        .new_rvalue_one(self.context.new_type::<usize>()),
                );
                self.block = Some(block_push);
                return;
            }
            ("оқы", [variable]) => {
                match self.get_type(variable) {
                    ConcreteType::Natural => {
                        let ty_natural = self.context.new_type::<u64>();
                        let lvalue_old =
                            block
                                .get_function()
                                .new_local(Some(loc), ty_natural, "old value");
                        let lvalue = self.compile_lvalue(variable);
                        block.add_assignment(Some(loc), lvalue_old, lvalue);
                        let block = self
                            .block
                            .expect("block must not be terminated after compiling an expression");
                        block.add_eval(
                            Some(loc),
                            self.context.new_call(
                                Some(loc),
                                self.functions["scanf"],
                                &[
                                    self.context.new_string_literal("%llu"),
                                    lvalue.get_address(Some(loc)),
                                ],
                            ),
                        );
                        if let ExpressionType::Field(expr, ident) = &variable.expression_type {
                            let array_type = self.get_type(expr);
                            if let (ConcreteType::Array(_), "Саны") =
                                (&array_type, ident.name.as_str())
                            {
                                let lvalue = self.compile_lvalue(expr);
                                self.compile_array_resize(
                                    line,
                                    column,
                                    &array_type,
                                    lvalue_old,
                                    lvalue,
                                );
                            }
                        }
                    }
                    ConcreteType::Whole => {
                        let lvalue = self.compile_lvalue(variable).get_address(None);
                        let block = self
                            .block
                            .expect("block must not be terminated after compiling an expression");
                        block.add_eval(
                            Some(loc),
                            self.context.new_call(
                                Some(loc),
                                self.functions["scanf"],
                                &[self.context.new_string_literal("%lld"), lvalue],
                            ),
                        );
                    }
                    ConcreteType::Real => {
                        let lvalue = self.compile_lvalue(variable).get_address(None);
                        let block = self
                            .block
                            .expect("block must not be terminated after compiling an expression");
                        block.add_eval(
                            Some(loc),
                            self.context.new_call(
                                Some(loc),
                                self.functions["scanf"],
                                &[self.context.new_string_literal("%lf"), lvalue],
                            ),
                        );
                    }
                    _ => todo!(),
                }
                return;
            }
            ("орнат", [variable, value]) => {
                let ty_variable = self.get_type(variable);
                let ty_value = self.get_type(value);
                if ty_variable != ty_value
                    && (ty_variable != ConcreteType::Real
                        || ty_value != ConcreteType::Whole && ty_value != ConcreteType::Natural)
                    && (ty_variable != ConcreteType::Whole || ty_value != ConcreteType::Natural)
                {
                    panic!(
                        "incompatible type for assignment at {}:{}",
                        value.line + 1,
                        value.column + 1
                    );
                }
                let lvalue = self.compile_lvalue(variable);
                let rvalue = self.compile_expression(value);
                let gccjit_type = self.get_gccjit_type(&ty_variable);
                let rvalue = if ty_variable == ty_value {
                    rvalue
                } else {
                    self.context.new_cast(
                        Some(self.context.new_location(
                            TMP_FILENAME,
                            value.line as i32 + 1,
                            value.column as i32 + 1,
                        )),
                        rvalue,
                        gccjit_type,
                    )
                };
                self.compile_free(&ty_variable, lvalue);
                let block = self.block.expect("block must not terminate after free");
                let lvalue_old =
                    block
                        .get_function()
                        .new_local(Some(loc), gccjit_type, "old value");
                block.add_assignment(Some(loc), lvalue_old, lvalue);
                self.compile_copy(&ty_variable, lvalue, rvalue);
                if let ExpressionType::Field(expr, ident) = &variable.expression_type {
                    let array_type = self.get_type(expr);
                    if let (ConcreteType::Array(_), "Саны") = (&array_type, ident.name.as_str())
                    {
                        let lvalue = self.compile_lvalue(expr);
                        self.compile_array_resize(line, column, &array_type, lvalue_old, lvalue);
                    }
                }
                return;
            }
            // TODO: More
            _ => {}
        }
        todo!();
    }

    fn compile_statement(&mut self, stmt: &Statement) {
        match &stmt.statement_type {
            StatementType::Conditional(..) => self.compile_conditional(stmt),
            StatementType::Loop(..) => self.compile_loop(stmt),
            StatementType::Break => self.compile_break(stmt),
            StatementType::Variable(..) => self.compile_variable(stmt),
            StatementType::Block(_) => self.compile_block(stmt),
            StatementType::Call(..) => self.compile_procedure_call(stmt),
            _ => todo!(),
        }
    }

    fn compile(mut self, stmt: &Statement) {
        let ty_int = self.context.new_c_type(CType::Int);
        let fn_main =
            self.context
                .new_function(None, FunctionType::Exported, ty_int, &[], "main", false);
        self.block = Some(fn_main.new_block("entry"));
        self.types.push({
            let mut types = HashMap::new();
            types.insert(
                "емес".to_string(),
                vec![BoundType::Function(
                    vec![GenericType::Concrete(ConcreteType::Boolean)],
                    GenericType::Concrete(ConcreteType::Boolean),
                )],
            );
            types.insert(
                "артық".to_string(),
                vec![
                    BoundType::Function(
                        vec![
                            GenericType::Concrete(ConcreteType::Natural),
                            GenericType::Concrete(ConcreteType::Natural),
                        ],
                        GenericType::Concrete(ConcreteType::Boolean),
                    ),
                    BoundType::Function(
                        vec![
                            GenericType::Concrete(ConcreteType::Whole),
                            GenericType::Concrete(ConcreteType::Whole),
                        ],
                        GenericType::Concrete(ConcreteType::Boolean),
                    ),
                    BoundType::Function(
                        vec![
                            GenericType::Concrete(ConcreteType::Real),
                            GenericType::Concrete(ConcreteType::Real),
                        ],
                        GenericType::Concrete(ConcreteType::Boolean),
                    ),
                ],
            );
            types.insert(
                "кем".to_string(),
                vec![
                    BoundType::Function(
                        vec![
                            GenericType::Concrete(ConcreteType::Natural),
                            GenericType::Concrete(ConcreteType::Natural),
                        ],
                        GenericType::Concrete(ConcreteType::Boolean),
                    ),
                    BoundType::Function(
                        vec![
                            GenericType::Concrete(ConcreteType::Whole),
                            GenericType::Concrete(ConcreteType::Whole),
                        ],
                        GenericType::Concrete(ConcreteType::Boolean),
                    ),
                    BoundType::Function(
                        vec![
                            GenericType::Concrete(ConcreteType::Real),
                            GenericType::Concrete(ConcreteType::Real),
                        ],
                        GenericType::Concrete(ConcreteType::Boolean),
                    ),
                ],
            );
            types.insert(
                "қалдық".to_string(),
                vec![
                    BoundType::Function(
                        vec![
                            GenericType::Concrete(ConcreteType::Natural),
                            GenericType::Concrete(ConcreteType::Natural),
                        ],
                        GenericType::Concrete(ConcreteType::Natural),
                    ),
                    BoundType::Function(
                        vec![
                            GenericType::Concrete(ConcreteType::Natural),
                            GenericType::Concrete(ConcreteType::Whole),
                        ],
                        GenericType::Concrete(ConcreteType::Whole),
                    ),
                    BoundType::Function(
                        vec![
                            GenericType::Concrete(ConcreteType::Whole),
                            GenericType::Concrete(ConcreteType::Natural),
                        ],
                        GenericType::Concrete(ConcreteType::Whole),
                    ),
                    BoundType::Function(
                        vec![
                            GenericType::Concrete(ConcreteType::Whole),
                            GenericType::Concrete(ConcreteType::Whole),
                        ],
                        GenericType::Concrete(ConcreteType::Whole),
                    ),
                    BoundType::Function(
                        vec![
                            GenericType::Concrete(ConcreteType::Natural),
                            GenericType::Concrete(ConcreteType::Real),
                        ],
                        GenericType::Concrete(ConcreteType::Real),
                    ),
                    BoundType::Function(
                        vec![
                            GenericType::Concrete(ConcreteType::Whole),
                            GenericType::Concrete(ConcreteType::Real),
                        ],
                        GenericType::Concrete(ConcreteType::Real),
                    ),
                    BoundType::Function(
                        vec![
                            GenericType::Concrete(ConcreteType::Real),
                            GenericType::Concrete(ConcreteType::Natural),
                        ],
                        GenericType::Concrete(ConcreteType::Real),
                    ),
                    BoundType::Function(
                        vec![
                            GenericType::Concrete(ConcreteType::Real),
                            GenericType::Concrete(ConcreteType::Whole),
                        ],
                        GenericType::Concrete(ConcreteType::Real),
                    ),
                    BoundType::Function(
                        vec![
                            GenericType::Concrete(ConcreteType::Real),
                            GenericType::Concrete(ConcreteType::Real),
                        ],
                        GenericType::Concrete(ConcreteType::Real),
                    ),
                ],
            );
            types.insert(
                "тең".to_string(),
                vec![
                    BoundType::Function(
                        vec![GenericType::Parameter(0), GenericType::Parameter(0)],
                        GenericType::Concrete(ConcreteType::Boolean),
                    ),
                    BoundType::Function(
                        vec![
                            GenericType::Concrete(ConcreteType::Natural),
                            GenericType::Concrete(ConcreteType::Whole),
                        ],
                        GenericType::Concrete(ConcreteType::Boolean),
                    ),
                    BoundType::Function(
                        vec![
                            GenericType::Concrete(ConcreteType::Natural),
                            GenericType::Concrete(ConcreteType::Real),
                        ],
                        GenericType::Concrete(ConcreteType::Boolean),
                    ),
                    BoundType::Function(
                        vec![
                            GenericType::Concrete(ConcreteType::Whole),
                            GenericType::Concrete(ConcreteType::Natural),
                        ],
                        GenericType::Concrete(ConcreteType::Boolean),
                    ),
                    BoundType::Function(
                        vec![
                            GenericType::Concrete(ConcreteType::Whole),
                            GenericType::Concrete(ConcreteType::Real),
                        ],
                        GenericType::Concrete(ConcreteType::Boolean),
                    ),
                    BoundType::Function(
                        vec![
                            GenericType::Concrete(ConcreteType::Real),
                            GenericType::Concrete(ConcreteType::Natural),
                        ],
                        GenericType::Concrete(ConcreteType::Boolean),
                    ),
                    BoundType::Function(
                        vec![
                            GenericType::Concrete(ConcreteType::Real),
                            GenericType::Concrete(ConcreteType::Whole),
                        ],
                        GenericType::Concrete(ConcreteType::Boolean),
                    ),
                ],
            );
            types
        });
        self.push_scope();
        self.compile_statement(stmt);
        let block = self
            .block
            .expect("can only return from a function or a procedure");
        self.pop_scope();
        block.end_with_return(None, self.context.new_rvalue_zero(ty_int));
        self.context.dump_to_file("tmp", true);
        self.context.compile_to_file(OutputKind::Executable, "main");
    }
}

pub fn compile(stmt: &Statement) {
    let context = Context::default();
    Compiler {
        context: &context,
        block: None,
        types: Vec::new(),
        gccjit_types: HashMap::new(),
        functions: {
            let mut functions = HashMap::new();
            functions.insert(
                "exit".to_string(),
                context.new_function(
                    None,
                    FunctionType::Extern,
                    context.new_type::<()>(),
                    &[context.new_parameter(None, context.new_c_type(CType::Int), "code")],
                    "exit",
                    false,
                ),
            );
            functions.insert(
                "free".to_string(),
                context.new_function(
                    None,
                    FunctionType::Extern,
                    context.new_type::<()>(),
                    &[context.new_parameter(None, context.new_type::<()>().make_pointer(), "ptr")],
                    "free",
                    false,
                ),
            );
            functions.insert(
                "printf".to_string(),
                context.new_function(
                    None,
                    FunctionType::Extern,
                    context.new_c_type(CType::Int),
                    &[context.new_parameter(
                        None,
                        context.new_c_type(CType::ConstCharPtr),
                        "format",
                    )],
                    "printf",
                    true,
                ),
            );
            functions.insert(
                "realloc".to_string(),
                context.new_function(
                    None,
                    FunctionType::Extern,
                    context.new_type::<()>().make_pointer(),
                    &[
                        context.new_parameter(None, context.new_type::<()>().make_pointer(), "ptr"),
                        context.new_parameter(None, context.new_type::<usize>(), "size"),
                    ],
                    "realloc",
                    false,
                ),
            );
            functions.insert(
                "scanf".to_string(),
                context.new_function(
                    None,
                    FunctionType::Extern,
                    context.new_c_type(CType::Int),
                    &[context.new_parameter(
                        None,
                        context.new_c_type(CType::ConstCharPtr),
                        "format",
                    )],
                    "scanf",
                    true,
                ),
            );
            functions
        },
        loop_ends: Vec::new(),
    }
    .compile(stmt);
}
