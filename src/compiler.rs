// TODO String newlines
// TODO Short-circuiting
// TODO Custom functions, procedures, and structures
// TODO Generics

use std::{collections::HashMap, mem};

use crate::{
    lexer::BinaryOperator,
    parser::{
        AstExpression, AstExpressionType, AstIdentifier, AstStatement, AstStatementType, AstType,
    },
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IrInstructionType {
    // Literals
    Boolean(bool),
    Natural(u64),
    Whole(i64),
    Real(f64),
    Character(char),
    Null,
    // Casts
    ToNatural(usize),
    ToWhole(usize),
    ToReal(usize),
    ToCharacter(usize),
    // Boolean logic
    And(usize, usize),
    Or(usize, usize),
    Not(usize),
    // Arithmetic
    Add(usize, usize),
    Subtract(usize, usize),
    Multiply(usize, usize),
    Divide(usize, usize),
    Remainder(usize, usize),
    Negate(usize),
    MinimumCapacity(usize),
    // Comparisons
    Equals(usize, usize),
    LessThan(usize, usize),
    GreaterThan(usize, usize),
    // References
    Variable(usize),
    Field(usize, usize),
    Count(usize),
    Capacity(usize),
    Index(usize, usize),
    Assign(usize),
    Dereference(usize),
    // Arrays
    Resize(usize),
    Free,
    // Input/Output
    Write(usize),
    Read(usize),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IrInstruction {
    pub temp: usize,
    pub ty: IrInstructionType,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrTerminatorType {
    Unconditional(usize),
    Conditional(usize, usize, usize),
    Return(usize),
    Error(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrTerminator {
    pub ty: IrTerminatorType,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq)]
enum IrBlock {
    NonTerminated(Vec<IrInstruction>),
    Terminated(Vec<IrInstruction>, IrTerminator),
}

impl IrBlock {
    fn new() -> Self {
        IrBlock::NonTerminated(Vec::new())
    }

    fn add_instruction(&mut self, inst: IrInstruction) {
        if let IrBlock::NonTerminated(insts) = self {
            insts.push(inst);
        } else {
            panic!("cannot add to a terminated block");
        }
    }

    fn terminate(&mut self, term: IrTerminator) {
        let mut block = IrBlock::new();
        mem::swap(&mut block, self);
        match block {
            IrBlock::NonTerminated(insts) => {
                *self = IrBlock::Terminated(insts, term);
            }
            _ => panic!("block is already terminated"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IrPrimitiveType {
    Boolean,
    Natural,
    Whole,
    Real,
    Character,
    Reference(Box<IrVariableType>),
}

impl IrPrimitiveType {
    fn is_numeric(&self) -> bool {
        matches!(
            self,
            IrPrimitiveType::Natural | IrPrimitiveType::Whole | IrPrimitiveType::Real
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IrVariableType {
    Primitive(IrPrimitiveType),
    Struct(Vec<IrVariableType>),
    Array(Box<IrVariableType>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct NearestLoop {
    start_scope: usize,
    end_block: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IrCompiler {
    vars: Vec<IrVariableType>,
    temps: Vec<IrPrimitiveType>,
    blocks: Vec<IrBlock>,
    scope: Vec<HashMap<String, usize>>,
    block: usize,
    nearest_loops: Vec<NearestLoop>,
}

impl IrCompiler {
    pub fn new() -> Self {
        Self {
            vars: Vec::new(),
            temps: Vec::new(),
            blocks: vec![IrBlock::new()],
            scope: Vec::new(),
            block: 0,
            nearest_loops: Vec::new(),
        }
    }

    fn new_variable(&mut self, ty: IrVariableType) -> usize {
        let var = self.vars.len();
        self.vars.push(ty);
        var
    }

    fn get_variable_type(AstType(params, AstIdentifier { name, .. }): &AstType) -> IrVariableType {
        // TODO Scopes and redefinition of types
        match name.as_str() {
            "логикалық" => IrVariableType::Primitive(IrPrimitiveType::Boolean),
            "натурал" => IrVariableType::Primitive(IrPrimitiveType::Natural),
            "бүтін" => IrVariableType::Primitive(IrPrimitiveType::Whole),
            "нақты" => IrVariableType::Primitive(IrPrimitiveType::Real),
            "символ" => IrVariableType::Primitive(IrPrimitiveType::Character),
            "жиым" => IrVariableType::Array(Box::new(Self::get_variable_type(&params[0]))),
            _ => todo!(),
        }
    }

    fn new_temporary(&mut self, ty: IrPrimitiveType) -> usize {
        let temp = self.temps.len();
        self.temps.push(ty);
        temp
    }

    fn new_block(&mut self) -> usize {
        let block = self.blocks.len();
        self.blocks.push(IrBlock::new());
        block
    }

    fn current_block(&mut self) -> &mut IrBlock {
        &mut self.blocks[self.block]
    }

    fn push_scope(&mut self) {
        self.scope.push(HashMap::new())
    }

    fn free_scope(&mut self, start_scope: usize, line: usize, column: usize) {
        for scope_index in start_scope..self.scope.len() {
            for var in self.scope[scope_index].clone().into_values() {
                let var_temp = self.add_instruction(IrInstructionType::Variable(var), line, column);
                self.add_free(var_temp, line, column);
            }
        }
    }

    fn pop_scope(&mut self, line: usize, column: usize) {
        self.free_scope(self.scope.len() - 1, line, column);
        self.scope.pop().unwrap();
    }

    fn get_variable_for_name(&self, name: &str, line: usize, column: usize) -> usize {
        self.scope
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).copied())
            .unwrap_or_else(|| panic!("no name {name} found {}:{}", line + 1, column + 1))
    }

    fn add_instruction(&mut self, ty: IrInstructionType, line: usize, column: usize) -> usize {
        let temp_ty = match ty {
            IrInstructionType::Boolean(_) => IrPrimitiveType::Boolean,
            IrInstructionType::Natural(_) => IrPrimitiveType::Natural,
            IrInstructionType::Whole(_) => IrPrimitiveType::Whole,
            IrInstructionType::Real(_) => IrPrimitiveType::Real,
            IrInstructionType::Character(_) => IrPrimitiveType::Character,
            IrInstructionType::Null => {
                panic!("cannot know type of null at {}:{}", line + 1, column + 1)
            }
            IrInstructionType::ToNatural(temp)
                if self.temps[temp].is_numeric()
                    || self.temps[temp] == IrPrimitiveType::Character =>
            {
                IrPrimitiveType::Natural
            }
            IrInstructionType::ToNatural(_) => {
                panic!("cannot convert to natural at {}:{}", line + 1, column + 1)
            }
            IrInstructionType::ToWhole(temp) if self.temps[temp].is_numeric() => {
                IrPrimitiveType::Whole
            }
            IrInstructionType::ToWhole(_) => {
                panic!("cannot convert to whole at {}:{}", line + 1, column + 1)
            }
            IrInstructionType::ToReal(temp) if self.temps[temp].is_numeric() => {
                IrPrimitiveType::Real
            }
            IrInstructionType::ToReal(_) => {
                panic!("cannot convert to real at {}:{}", line + 1, column + 1)
            }
            IrInstructionType::ToCharacter(temp)
                if self.temps[temp] == IrPrimitiveType::Natural =>
            {
                IrPrimitiveType::Character
            }
            IrInstructionType::ToCharacter(_) => {
                panic!("cannot convert to character at {}:{}", line + 1, column + 1)
            }
            IrInstructionType::And(a, b)
                if self.temps[a] == IrPrimitiveType::Boolean
                    && self.temps[b] == IrPrimitiveType::Boolean =>
            {
                IrPrimitiveType::Boolean
            }
            IrInstructionType::And(..) => {
                panic!("invalid operands for and at {}:{}", line + 1, column + 1)
            }
            IrInstructionType::Or(a, b)
                if self.temps[a] == IrPrimitiveType::Boolean
                    && self.temps[b] == IrPrimitiveType::Boolean =>
            {
                IrPrimitiveType::Boolean
            }
            IrInstructionType::Or(..) => {
                panic!("invalid operands for or at {}:{}", line + 1, column + 1)
            }
            IrInstructionType::Not(temp) if self.temps[temp] == IrPrimitiveType::Boolean => {
                IrPrimitiveType::Boolean
            }
            IrInstructionType::Not(_) => {
                panic!("invalid operand for not at {}:{}", line + 1, column + 1)
            }
            IrInstructionType::Add(a, b)
                if self.temps[a] == IrPrimitiveType::Natural
                    && self.temps[b] == IrPrimitiveType::Natural =>
            {
                IrPrimitiveType::Natural
            }
            IrInstructionType::Add(a, b)
                if self.temps[a] == IrPrimitiveType::Whole
                    && self.temps[b] == IrPrimitiveType::Whole =>
            {
                IrPrimitiveType::Whole
            }
            IrInstructionType::Add(a, b)
                if self.temps[a] == IrPrimitiveType::Real
                    && self.temps[b] == IrPrimitiveType::Real =>
            {
                IrPrimitiveType::Real
            }
            IrInstructionType::Add(a, b)
                if matches!(
                    (&self.temps[a], &self.temps[b]),
                    (IrPrimitiveType::Character, IrPrimitiveType::Whole)
                        | (IrPrimitiveType::Whole, IrPrimitiveType::Character)
                ) =>
            {
                IrPrimitiveType::Character
            }
            IrInstructionType::Add(..) => {
                panic!("incorrect operands for add at {}:{}", line + 1, column + 1)
            }
            IrInstructionType::Subtract(a, b)
                if self.temps[a] == IrPrimitiveType::Whole
                    && self.temps[b] == IrPrimitiveType::Whole =>
            {
                IrPrimitiveType::Whole
            }
            IrInstructionType::Subtract(a, b)
                if self.temps[a] == IrPrimitiveType::Real
                    && self.temps[b] == IrPrimitiveType::Real =>
            {
                IrPrimitiveType::Real
            }
            IrInstructionType::Subtract(a, b)
                if self.temps[a] == IrPrimitiveType::Character
                    && matches!(&self.temps[b], IrPrimitiveType::Whole) =>
            {
                IrPrimitiveType::Whole
            }
            IrInstructionType::Subtract(..) => {
                panic!(
                    "incorrect operands for subtract at {}:{}",
                    line + 1,
                    column + 1
                )
            }
            IrInstructionType::Multiply(a, b)
                if self.temps[a] == IrPrimitiveType::Natural
                    && self.temps[b] == IrPrimitiveType::Natural =>
            {
                IrPrimitiveType::Natural
            }
            IrInstructionType::Multiply(a, b)
                if self.temps[a] == IrPrimitiveType::Whole
                    && self.temps[b] == IrPrimitiveType::Whole =>
            {
                IrPrimitiveType::Whole
            }
            IrInstructionType::Multiply(a, b)
                if self.temps[a] == IrPrimitiveType::Real
                    && self.temps[b] == IrPrimitiveType::Real =>
            {
                IrPrimitiveType::Real
            }
            IrInstructionType::Multiply(..) => {
                panic!(
                    "incorrect operands for multiply at {}:{}",
                    line + 1,
                    column + 1
                )
            }
            IrInstructionType::Divide(a, b)
                if self.temps[a] == IrPrimitiveType::Real
                    && self.temps[b] == IrPrimitiveType::Real =>
            {
                IrPrimitiveType::Real
            }
            IrInstructionType::Divide(..) => {
                panic!(
                    "incorrect operands for divide at {}:{}",
                    line + 1,
                    column + 1
                )
            }
            IrInstructionType::Remainder(a_temp, b_temp)
                if self.temps[a_temp] == IrPrimitiveType::Natural
                    && self.temps[b_temp] == IrPrimitiveType::Natural =>
            {
                IrPrimitiveType::Natural
            }
            IrInstructionType::Remainder(a_temp, b_temp)
                if self.temps[a_temp] == IrPrimitiveType::Whole
                    && self.temps[b_temp] == IrPrimitiveType::Whole =>
            {
                IrPrimitiveType::Whole
            }
            IrInstructionType::Remainder(a_temp, b_temp)
                if self.temps[a_temp] == IrPrimitiveType::Real
                    && self.temps[b_temp] == IrPrimitiveType::Real =>
            {
                IrPrimitiveType::Real
            }
            IrInstructionType::Remainder(..) => panic!(
                "incorrect operands for remainder at {}:{}",
                line + 1,
                column + 1
            ),
            IrInstructionType::Negate(temp) if self.temps[temp] == IrPrimitiveType::Whole => {
                IrPrimitiveType::Whole
            }
            IrInstructionType::Negate(temp) if self.temps[temp] == IrPrimitiveType::Real => {
                IrPrimitiveType::Real
            }
            IrInstructionType::Negate(_) => {
                panic!(
                    "incorrect operand for negate at {}:{}",
                    line + 1,
                    column + 1
                )
            }
            IrInstructionType::MinimumCapacity(temp)
                if self.temps[temp] == IrPrimitiveType::Natural =>
            {
                IrPrimitiveType::Natural
            }
            IrInstructionType::MinimumCapacity(_) => panic!(
                "can only calculate capacity for natural length {}:{}",
                line + 1,
                column + 1
            ),
            IrInstructionType::Equals(a_temp, b_temp)
                if matches!(
                    (&self.temps[a_temp], &self.temps[b_temp]),
                    (IrPrimitiveType::Boolean, IrPrimitiveType::Boolean)
                        | (IrPrimitiveType::Natural, IrPrimitiveType::Natural)
                        | (IrPrimitiveType::Whole, IrPrimitiveType::Whole)
                        | (IrPrimitiveType::Real, IrPrimitiveType::Real)
                        | (IrPrimitiveType::Character, IrPrimitiveType::Character)
                ) =>
            {
                IrPrimitiveType::Boolean
            }
            IrInstructionType::Equals(..) => panic!(
                "incorrect operands for equals at {}:{}",
                line + 1,
                column + 1
            ),
            IrInstructionType::LessThan(a_temp, b_temp)
                if matches!(
                    (&self.temps[a_temp], &self.temps[b_temp]),
                    (IrPrimitiveType::Natural, IrPrimitiveType::Natural)
                        | (IrPrimitiveType::Whole, IrPrimitiveType::Whole)
                        | (IrPrimitiveType::Real, IrPrimitiveType::Real)
                ) =>
            {
                IrPrimitiveType::Boolean
            }
            IrInstructionType::LessThan(..) => panic!(
                "incorrect operands for less than at {}:{}",
                line + 1,
                column + 1
            ),
            IrInstructionType::GreaterThan(a_temp, b_temp)
                if matches!(
                    (&self.temps[a_temp], &self.temps[b_temp]),
                    (IrPrimitiveType::Natural, IrPrimitiveType::Natural)
                        | (IrPrimitiveType::Whole, IrPrimitiveType::Whole)
                        | (IrPrimitiveType::Real, IrPrimitiveType::Real)
                ) =>
            {
                IrPrimitiveType::Boolean
            }
            IrInstructionType::GreaterThan(..) => panic!(
                "incorrect operands for greater than at {}:{}",
                line + 1,
                column + 1
            ),
            IrInstructionType::Variable(var) => {
                IrPrimitiveType::Reference(Box::new(self.vars[var].clone()))
            }
            // TODO More here
            IrInstructionType::Count(temp)
                if matches!(
                    &self.temps[temp],
                    IrPrimitiveType::Reference(ty)
                        if matches!(ty.as_ref(), IrVariableType::Array(_))
                ) =>
            {
                IrPrimitiveType::Reference(Box::new(IrVariableType::Primitive(
                    IrPrimitiveType::Natural,
                )))
            }
            IrInstructionType::Count(_) => {
                panic!("can only take count of arrays {}:{}", line + 1, column + 1)
            }
            IrInstructionType::Capacity(temp)
                if matches!(
                    &self.temps[temp],
                    IrPrimitiveType::Reference(ty)
                        if matches!(ty.as_ref(), IrVariableType::Array(_))
                ) =>
            {
                IrPrimitiveType::Reference(Box::new(IrVariableType::Primitive(
                    IrPrimitiveType::Natural,
                )))
            }
            IrInstructionType::Capacity(_) => panic!(
                "can only take capacity of arrays {}:{}",
                line + 1,
                column + 1
            ),
            IrInstructionType::Index(arr_temp, index_temp)
                if self.temps[index_temp] == IrPrimitiveType::Natural =>
            {
                match &self.temps[arr_temp] {
                    IrPrimitiveType::Reference(ty) => match ty.as_ref() {
                        IrVariableType::Array(ty) => IrPrimitiveType::Reference(ty.clone()),
                        _ => panic!(
                            "can only index array references {}:{}",
                            line + 1,
                            column + 1
                        ),
                    },
                    _ => panic!("can only index references {}:{}", line + 1, column + 1),
                }
            }
            IrInstructionType::Index(..) => panic!(
                "can only index using natural numbers {}:{}",
                line + 1,
                column + 1
            ),
            IrInstructionType::Assign(_) => panic!(
                "cannot know what to assign to at {}:{}",
                line + 1,
                column + 1
            ),
            IrInstructionType::Dereference(temp) => match &self.temps[temp] {
                IrPrimitiveType::Reference(ty) => match ty.as_ref() {
                    IrVariableType::Primitive(ty) => ty.clone(),
                    _ => panic!(
                        "can only dereference references to primitive types {}:{}",
                        line + 1,
                        column + 1
                    ),
                },
                _ => panic!(
                    "can only dereference references {}:{}",
                    line + 1,
                    column + 1
                ),
            },
            IrInstructionType::Resize(..) => {
                panic!("cannot know what to resize at {}:{}", line + 1, column + 1)
            }
            IrInstructionType::Free => {
                panic!("cannot know what to free at {}:{}", line + 1, column + 1)
            }
            IrInstructionType::Write(temp) => match &self.temps[temp] {
                IrPrimitiveType::Boolean
                | IrPrimitiveType::Natural
                | IrPrimitiveType::Whole
                | IrPrimitiveType::Real
                | IrPrimitiveType::Character => IrPrimitiveType::Boolean,
                _ => panic!("cannot write references at {}:{}", line + 1, column + 1),
            },
            IrInstructionType::Read(temp) => match &self.temps[temp] {
                IrPrimitiveType::Reference(ty) => match ty.as_ref() {
                    IrVariableType::Primitive(
                        IrPrimitiveType::Boolean
                        | IrPrimitiveType::Natural
                        | IrPrimitiveType::Whole
                        | IrPrimitiveType::Real
                        | IrPrimitiveType::Character,
                    ) => IrPrimitiveType::Boolean,
                    _ => panic!(
                        "can only read into referneces to primitives at {}:{}",
                        line + 1,
                        column + 1
                    ),
                },
                _ => panic!(
                    "can only read into references at {}:{}",
                    line + 1,
                    column + 1
                ),
            },
            _ => todo!(),
        };
        let temp = self.new_temporary(temp_ty);
        self.current_block().add_instruction(IrInstruction {
            temp,
            ty,
            line,
            column,
        });
        temp
    }

    fn add_dereference(&mut self, temp: usize, line: usize, column: usize) -> usize {
        if let IrPrimitiveType::Reference(_) = self.temps[temp] {
            self.add_instruction(IrInstructionType::Dereference(temp), line, column)
        } else {
            temp
        }
    }

    fn compile_expression(
        &mut self,
        &AstExpression {
            ref expression_type,
            line,
            column,
        }: &AstExpression,
    ) -> usize {
        match expression_type {
            &AstExpressionType::Boolean(value) => {
                self.add_instruction(IrInstructionType::Boolean(value), line, column)
            }
            &AstExpressionType::Natural(value) => {
                self.add_instruction(IrInstructionType::Natural(value), line, column)
            }
            &AstExpressionType::Real(value) => {
                self.add_instruction(IrInstructionType::Real(value), line, column)
            }
            &AstExpressionType::Character(value) => {
                self.add_instruction(IrInstructionType::Character(value), line, column)
            }
            AstExpressionType::String(string) => {
                let arr_var = self.new_variable(IrVariableType::Array(Box::new(
                    IrVariableType::Primitive(IrPrimitiveType::Character),
                )));
                let arr_temp =
                    self.add_instruction(IrInstructionType::Variable(arr_var), line, column);
                self.add_zero_initialization(arr_temp, line, column);
                let len_temp = self.add_instruction(
                    IrInstructionType::Natural(string.chars().count() as _),
                    line,
                    column,
                );
                let new_capacity_temp = self.add_instruction(
                    IrInstructionType::MinimumCapacity(len_temp),
                    line,
                    column,
                );
                self.current_block().add_instruction(IrInstruction {
                    temp: arr_temp,
                    ty: IrInstructionType::Resize(new_capacity_temp),
                    line,
                    column,
                });
                let capacity_temp =
                    self.add_instruction(IrInstructionType::Capacity(arr_temp), line, column);
                self.add_assignment(capacity_temp, new_capacity_temp, line, column);
                for (index, ch) in string.chars().enumerate() {
                    let index_temp =
                        self.add_instruction(IrInstructionType::Natural(index as _), line, column);
                    let item_temp = self.add_instruction(
                        IrInstructionType::Index(arr_temp, index_temp),
                        line,
                        column,
                    );
                    let char_temp =
                        self.add_instruction(IrInstructionType::Character(ch), line, column);
                    self.add_assignment(item_temp, char_temp, line, column);
                }
                let count_temp =
                    self.add_instruction(IrInstructionType::Count(arr_temp), line, column);
                self.add_assignment(count_temp, len_temp, line, column);
                self.scope
                    .last_mut()
                    .expect("scope should not be empty")
                    .insert(String::new(), arr_var);
                arr_temp
            }
            AstExpressionType::Noun(name) => {
                let var = self.get_variable_for_name(name, line, column);
                self.add_instruction(IrInstructionType::Variable(var), line, column)
            }
            AstExpressionType::Negate(expr) => {
                let temp = self.compile_expression(expr);
                let temp = self.add_dereference(temp, line, column);
                let temp = if self.temps[temp] == IrPrimitiveType::Natural {
                    self.add_instruction(IrInstructionType::ToWhole(temp), line, column)
                } else {
                    temp
                };
                self.add_instruction(IrInstructionType::Negate(temp), line, column)
            }
            &AstExpressionType::BinaryOperation(ref a, op, ref b) => {
                let a_temp = self.compile_expression(a);
                let b_temp = self.compile_expression(b);
                match op {
                    BinaryOperator::Add => {
                        let a_temp = self.add_dereference(a_temp, line, column);
                        let b_temp = self.add_dereference(b_temp, line, column);
                        self.add_instruction(IrInstructionType::Add(a_temp, b_temp), line, column)
                    }
                    BinaryOperator::Subtract => {
                        let a_temp = self.add_dereference(a_temp, line, column);
                        let b_temp = self.add_dereference(b_temp, line, column);
                        let a_temp = if self.temps[a_temp] == IrPrimitiveType::Natural {
                            self.add_instruction(IrInstructionType::ToWhole(a_temp), line, column)
                        } else {
                            a_temp
                        };
                        let b_temp = if self.temps[b_temp] == IrPrimitiveType::Natural {
                            self.add_instruction(IrInstructionType::ToWhole(b_temp), line, column)
                        } else {
                            b_temp
                        };
                        self.add_instruction(
                            IrInstructionType::Subtract(a_temp, b_temp),
                            line,
                            column,
                        )
                    }
                    BinaryOperator::Multiply => {
                        let a_temp = self.add_dereference(a_temp, line, column);
                        let b_temp = self.add_dereference(b_temp, line, column);
                        self.add_instruction(
                            IrInstructionType::Multiply(a_temp, b_temp),
                            line,
                            column,
                        )
                    }
                    BinaryOperator::Divide => {
                        let a_temp = self.add_dereference(a_temp, line, column);
                        let b_temp = self.add_dereference(b_temp, line, column);
                        let a_temp = if matches!(
                            &self.temps[a_temp],
                            IrPrimitiveType::Natural | IrPrimitiveType::Whole
                        ) {
                            self.add_instruction(IrInstructionType::ToReal(a_temp), line, column)
                        } else {
                            a_temp
                        };
                        let b_temp = if matches!(
                            &self.temps[b_temp],
                            IrPrimitiveType::Natural | IrPrimitiveType::Whole
                        ) {
                            self.add_instruction(IrInstructionType::ToReal(b_temp), line, column)
                        } else {
                            b_temp
                        };
                        self.add_instruction(
                            IrInstructionType::Divide(a_temp, b_temp),
                            line,
                            column,
                        )
                    }
                    BinaryOperator::Index => {
                        let b_temp = self.add_dereference(b_temp, line, column);
                        let count_temp =
                            self.add_instruction(IrInstructionType::Count(a_temp), line, column);
                        let count_deref_temp = self.add_dereference(count_temp, line, column);
                        let not_cond_temp = self.add_instruction(
                            IrInstructionType::LessThan(b_temp, count_deref_temp),
                            line,
                            column,
                        );
                        let cond_temp = self.add_instruction(
                            IrInstructionType::Not(not_cond_temp),
                            line,
                            column,
                        );
                        let then_block = self.new_block();
                        let end_block = self.new_block();
                        self.current_block().terminate(IrTerminator {
                            ty: IrTerminatorType::Conditional(cond_temp, then_block, end_block),
                            line,
                            column,
                        });
                        self.block = then_block;
                        self.current_block().terminate(IrTerminator {
                            ty: IrTerminatorType::Error(format!(
                                "out of bounds error at {}:{}",
                                line + 1,
                                column + 1
                            )),
                            line,
                            column,
                        });
                        self.block = end_block;
                        self.add_instruction(IrInstructionType::Index(a_temp, b_temp), line, column)
                    }
                    BinaryOperator::And => {
                        let a_temp = self.add_dereference(a_temp, line, column);
                        let b_temp = self.add_dereference(b_temp, line, column);
                        self.add_instruction(IrInstructionType::And(a_temp, b_temp), line, column)
                    }
                    BinaryOperator::Or => {
                        let a_temp = self.add_dereference(a_temp, line, column);
                        let b_temp = self.add_dereference(b_temp, line, column);
                        self.add_instruction(IrInstructionType::Or(a_temp, b_temp), line, column)
                    }
                }
            }
            AstExpressionType::Field(..) => todo!(),
            AstExpressionType::Call(args, ident) => match ident.name.as_str() {
                "натурал" => {
                    let temp = self.compile_expression(&args[0]);
                    self.add_instruction(IrInstructionType::ToNatural(temp), line, column)
                }
                "бүтін" => {
                    let temp = self.compile_expression(&args[0]);
                    self.add_instruction(IrInstructionType::ToWhole(temp), line, column)
                }
                "нақты" => {
                    let temp = self.compile_expression(&args[0]);
                    self.add_instruction(IrInstructionType::ToReal(temp), line, column)
                }
                "символ" => {
                    let temp = self.compile_expression(&args[0]);
                    self.add_instruction(IrInstructionType::ToCharacter(temp), line, column)
                }
                "емес" => {
                    let temp = self.compile_expression(&args[0]);
                    let temp = self.add_dereference(temp, line, column);
                    self.add_instruction(IrInstructionType::Not(temp), line, column)
                }
                "саны" => {
                    let temp = self.compile_expression(&args[0]);
                    let temp = self.add_instruction(IrInstructionType::Count(temp), line, column);
                    self.add_dereference(temp, line, column)
                }
                "артық" => {
                    let a_temp = self.compile_expression(&args[0]);
                    let b_temp = self.compile_expression(&args[1]);
                    let a_temp = self.add_dereference(a_temp, line, column);
                    let b_temp = self.add_dereference(b_temp, line, column);
                    self.add_instruction(
                        IrInstructionType::GreaterThan(a_temp, b_temp),
                        line,
                        column,
                    )
                }
                "кем" => {
                    let a_temp = self.compile_expression(&args[0]);
                    let b_temp = self.compile_expression(&args[1]);
                    let a_temp = self.add_dereference(a_temp, line, column);
                    let b_temp = self.add_dereference(b_temp, line, column);
                    self.add_instruction(IrInstructionType::LessThan(a_temp, b_temp), line, column)
                }
                "қалдық" => {
                    let a_temp = self.compile_expression(&args[0]);
                    let b_temp = self.compile_expression(&args[1]);
                    let a_temp = self.add_dereference(a_temp, line, column);
                    let b_temp = self.add_dereference(b_temp, line, column);
                    self.add_instruction(IrInstructionType::Remainder(a_temp, b_temp), line, column)
                }
                "тең" => {
                    let a_temp = self.compile_expression(&args[0]);
                    let b_temp = self.compile_expression(&args[1]);
                    let a_temp = self.add_dereference(a_temp, line, column);
                    let b_temp = self.add_dereference(b_temp, line, column);
                    self.add_instruction(IrInstructionType::Equals(a_temp, b_temp), line, column)
                }
                "жазылды" => {
                    let temp = self.compile_expression(&args[0]);
                    let temp = match &self.temps[temp] {
                        IrPrimitiveType::Reference(ty)
                            if matches!(ty.as_ref(), IrVariableType::Primitive(_)) =>
                        {
                            self.add_instruction(IrInstructionType::Dereference(temp), line, column)
                        }
                        _ => temp,
                    };
                    if matches!(
                        &self.temps[temp],
                        IrPrimitiveType::Reference(ty)
                            if matches!(
                                ty.as_ref(),
                                IrVariableType::Array(ty)
                                    if *ty.as_ref()
                                        == IrVariableType::Primitive(IrPrimitiveType::Character)
                            )
                    ) {
                        let index_temp =
                            self.add_instruction(IrInstructionType::Natural(0), line, column);
                        let count_temp =
                            self.add_instruction(IrInstructionType::Count(temp), line, column);
                        let end_success_temp =
                            self.add_instruction(IrInstructionType::Boolean(true), line, column);
                        let count_deref_temp = self.add_dereference(count_temp, line, column);
                        let cond_block = self.new_block();
                        let body_block = self.new_block();
                        let end_block = self.new_block();
                        self.current_block().terminate(IrTerminator {
                            ty: IrTerminatorType::Unconditional(cond_block),
                            line,
                            column,
                        });
                        self.block = cond_block;
                        let cond_temp = self.add_instruction(
                            IrInstructionType::LessThan(index_temp, count_deref_temp),
                            line,
                            column,
                        );
                        self.current_block().terminate(IrTerminator {
                            ty: IrTerminatorType::Conditional(cond_temp, body_block, end_block),
                            line,
                            column,
                        });
                        self.block = body_block;
                        let char_temp = self.add_instruction(
                            IrInstructionType::Index(temp, index_temp),
                            line,
                            column,
                        );
                        let char_deref_temp = self.add_dereference(char_temp, line, column);
                        let success_temp = self.add_instruction(
                            IrInstructionType::Write(char_deref_temp),
                            line,
                            column,
                        );
                        let success_block = self.new_block();
                        let error_block = self.new_block();
                        self.current_block().terminate(IrTerminator {
                            ty: IrTerminatorType::Conditional(
                                success_temp,
                                success_block,
                                error_block,
                            ),
                            line,
                            column,
                        });
                        self.block = success_block;
                        let one_temp =
                            self.add_instruction(IrInstructionType::Natural(1), line, column);
                        self.current_block().add_instruction(IrInstruction {
                            temp: index_temp,
                            ty: IrInstructionType::Add(index_temp, one_temp),
                            line,
                            column,
                        });
                        self.current_block().terminate(IrTerminator {
                            ty: IrTerminatorType::Unconditional(cond_block),
                            line,
                            column,
                        });
                        self.block = error_block;
                        self.current_block().add_instruction(IrInstruction {
                            temp: end_success_temp,
                            ty: IrInstructionType::Boolean(false),
                            line,
                            column,
                        });
                        self.current_block().terminate(IrTerminator {
                            ty: IrTerminatorType::Unconditional(end_block),
                            line,
                            column,
                        });
                        self.block = end_block;
                        end_success_temp
                    } else {
                        self.add_instruction(IrInstructionType::Write(temp), line, column)
                    }
                }
                "оқылды" => {
                    let temp = self.compile_expression(&args[0]);
                    self.add_instruction(IrInstructionType::Read(temp), line, column)
                }
                _ => todo!(),
            },
        }
    }

    fn add_assignment(&mut self, var_temp: usize, val_temp: usize, line: usize, column: usize) {
        let IrPrimitiveType::Reference(ty) = &self.temps[var_temp] else {
            unreachable!("should only be called for references");
        };
        match ty.as_ref() {
            IrVariableType::Primitive(var_type) => {
                let var_type = var_type.clone();
                let val_temp = self.add_dereference(val_temp, line, column);
                if var_type != self.temps[val_temp] {
                    panic!(
                        "incompatible types for assignment at {}:{}",
                        line + 1,
                        column + 1
                    );
                }
                self.current_block().add_instruction(IrInstruction {
                    temp: var_temp,
                    ty: IrInstructionType::Assign(val_temp),
                    line,
                    column,
                });
            }
            IrVariableType::Struct(types) if self.temps[var_temp] == self.temps[val_temp] => {
                for index in 0..types.len() {
                    let var_field_temp = self.add_instruction(
                        IrInstructionType::Field(var_temp, index),
                        line,
                        column,
                    );
                    let val_field_temp = self.add_instruction(
                        IrInstructionType::Field(val_temp, index),
                        line,
                        column,
                    );
                    self.add_assignment(var_field_temp, val_field_temp, line, column);
                }
            }
            IrVariableType::Struct(_) => panic!(
                "incompatible types for assignment as {}:{}",
                line + 1,
                column + 1
            ),
            IrVariableType::Array(_) if self.temps[var_temp] == self.temps[val_temp] => {
                let val_count_temp =
                    self.add_instruction(IrInstructionType::Count(val_temp), line, column);
                let val_count_deref_temp = self.add_instruction(
                    IrInstructionType::Dereference(val_count_temp),
                    line,
                    column,
                );
                let val_capacity_temp =
                    self.add_instruction(IrInstructionType::Capacity(val_temp), line, column);
                let val_capacity_deref_temp = self.add_instruction(
                    IrInstructionType::Dereference(val_capacity_temp),
                    line,
                    column,
                );
                self.current_block().add_instruction(IrInstruction {
                    temp: var_temp,
                    ty: IrInstructionType::Resize(val_capacity_deref_temp),
                    line,
                    column,
                });
                let var_count_temp =
                    self.add_instruction(IrInstructionType::Count(var_temp), line, column);
                let var_capacity_temp =
                    self.add_instruction(IrInstructionType::Capacity(var_temp), line, column);
                self.add_assignment(val_count_temp, val_count_deref_temp, line, column);
                let index_temp = self.add_instruction(IrInstructionType::Natural(0), line, column);
                let cond_block = self.new_block();
                let body_block = self.new_block();
                let end_block = self.new_block();
                self.current_block().terminate(IrTerminator {
                    ty: IrTerminatorType::Unconditional(cond_block),
                    line,
                    column,
                });
                self.block = cond_block;
                let cond_temp = self.add_instruction(
                    IrInstructionType::LessThan(index_temp, val_count_deref_temp),
                    line,
                    column,
                );
                self.current_block().terminate(IrTerminator {
                    ty: IrTerminatorType::Conditional(cond_temp, body_block, end_block),
                    line,
                    column,
                });
                self.block = body_block;
                let var_element_temp = self.add_instruction(
                    IrInstructionType::Index(var_temp, index_temp),
                    line,
                    column,
                );
                let val_element_temp = self.add_instruction(
                    IrInstructionType::Index(val_temp, index_temp),
                    line,
                    column,
                );
                self.add_assignment(var_element_temp, val_element_temp, line, column);
                let one_temp = self.add_instruction(IrInstructionType::Natural(1), line, column);
                self.current_block().add_instruction(IrInstruction {
                    temp: index_temp,
                    ty: IrInstructionType::Add(index_temp, one_temp),
                    line,
                    column,
                });
                self.current_block().terminate(IrTerminator {
                    ty: IrTerminatorType::Unconditional(cond_block),
                    line,
                    column,
                });
                self.block = end_block;
                self.add_assignment(var_count_temp, val_count_deref_temp, line, column);
                self.add_assignment(var_capacity_temp, val_capacity_deref_temp, line, column);
            }
            IrVariableType::Array(_) => panic!(
                "incompatible types for assignment as {}:{}",
                line + 1,
                column + 1
            ),
        }
    }

    fn add_zero_initialization(&mut self, var_temp: usize, line: usize, column: usize) {
        let IrPrimitiveType::Reference(ty) = &self.temps[var_temp] else {
            unreachable!("should only be called for references");
        };
        match ty.as_ref() {
            IrVariableType::Primitive(ty) => {
                let val_temp = self.add_instruction(
                    match ty {
                        IrPrimitiveType::Boolean => IrInstructionType::Boolean(false),
                        IrPrimitiveType::Natural => IrInstructionType::Natural(0),
                        IrPrimitiveType::Whole => IrInstructionType::Whole(0),
                        IrPrimitiveType::Real => IrInstructionType::Real(0.0),
                        IrPrimitiveType::Character => IrInstructionType::Character('\0'),
                        _ => unreachable!("variables cannot be references"),
                    },
                    line,
                    column,
                );
                self.current_block().add_instruction(IrInstruction {
                    temp: var_temp,
                    ty: IrInstructionType::Assign(val_temp),
                    line,
                    column,
                });
            }
            IrVariableType::Struct(types) => {
                for index in 0..types.len() {
                    let field_temp = self.add_instruction(
                        IrInstructionType::Field(var_temp, index),
                        line,
                        column,
                    );
                    self.add_zero_initialization(field_temp, line, column);
                }
            }
            IrVariableType::Array(_) => {
                self.current_block().add_instruction(IrInstruction {
                    temp: var_temp,
                    ty: IrInstructionType::Null,
                    line,
                    column,
                });
                let zero_temp = self.add_instruction(IrInstructionType::Natural(0), line, column);
                let count_temp =
                    self.add_instruction(IrInstructionType::Count(var_temp), line, column);
                self.add_assignment(count_temp, zero_temp, line, column);
                let capacity_temp =
                    self.add_instruction(IrInstructionType::Capacity(var_temp), line, column);
                self.add_assignment(capacity_temp, zero_temp, line, column);
            }
        }
    }

    fn add_free(&mut self, var_temp: usize, line: usize, column: usize) {
        let IrPrimitiveType::Reference(ty) = &self.temps[var_temp] else {
            unreachable!("should only be called for references");
        };
        match ty.as_ref() {
            IrVariableType::Primitive(_) => {}
            IrVariableType::Struct(types) => {
                for index in 0..types.len() {
                    let field_temp = self.add_instruction(
                        IrInstructionType::Field(var_temp, index),
                        line,
                        column,
                    );
                    self.add_free(field_temp, line, column);
                }
            }
            IrVariableType::Array(_) => {
                let index_temp = self.add_instruction(IrInstructionType::Natural(0), line, column);
                let count_temp =
                    self.add_instruction(IrInstructionType::Count(var_temp), line, column);
                let count_deref_temp =
                    self.add_instruction(IrInstructionType::Dereference(count_temp), line, column);
                let one_temp = self.add_instruction(IrInstructionType::Natural(1), line, column);
                let cond_block = self.new_block();
                let body_block = self.new_block();
                let end_block = self.new_block();
                self.current_block().terminate(IrTerminator {
                    ty: IrTerminatorType::Unconditional(cond_block),
                    line,
                    column,
                });
                self.block = cond_block;
                let cond_temp = self.add_instruction(
                    IrInstructionType::LessThan(index_temp, count_deref_temp),
                    line,
                    column,
                );
                self.current_block().terminate(IrTerminator {
                    ty: IrTerminatorType::Conditional(cond_temp, body_block, end_block),
                    line,
                    column,
                });
                self.block = body_block;
                let element_temp = self.add_instruction(
                    IrInstructionType::Index(var_temp, index_temp),
                    line,
                    column,
                );
                self.add_free(element_temp, line, column);
                self.current_block().add_instruction(IrInstruction {
                    temp: index_temp,
                    ty: IrInstructionType::Add(index_temp, one_temp),
                    line,
                    column,
                });
                self.current_block().terminate(IrTerminator {
                    ty: IrTerminatorType::Unconditional(cond_block),
                    line,
                    column,
                });
                self.block = end_block;
                self.current_block().add_instruction(IrInstruction {
                    temp: var_temp,
                    ty: IrInstructionType::Free,
                    line,
                    column,
                });
            }
        }
    }

    fn compile_statement(&mut self, stmt: &AstStatement) {
        match &stmt.statement_type {
            AstStatementType::Conditional(expr, then_stmt, else_stmt) => {
                let index = self.compile_expression(expr);
                if self.temps[index] != IrPrimitiveType::Boolean {
                    panic!(
                        "wrong type for condition at {}:{}",
                        expr.line + 1,
                        expr.column + 1
                    );
                }
                let cond_block = self.block;
                let then_block = self.new_block();
                let else_block = self.new_block();
                let end_block = self.new_block();
                self.block = then_block;
                self.push_scope();
                self.compile_statement(then_stmt);
                self.pop_scope(stmt.line, stmt.column);
                let then_end_block = self.block;
                self.block = else_block;
                if let Some(else_stmt) = else_stmt {
                    self.push_scope();
                    self.compile_statement(else_stmt);
                    self.pop_scope(stmt.line, stmt.column);
                }
                let else_end_block = self.block;
                self.blocks[cond_block].terminate(IrTerminator {
                    ty: IrTerminatorType::Conditional(index, then_block, else_block),
                    line: stmt.line,
                    column: stmt.column,
                });
                self.blocks[then_end_block].terminate(IrTerminator {
                    ty: IrTerminatorType::Unconditional(end_block),
                    line: stmt.line,
                    column: stmt.column,
                });
                self.blocks[else_end_block].terminate(IrTerminator {
                    ty: IrTerminatorType::Unconditional(end_block),
                    line: stmt.line,
                    column: stmt.column,
                });
                self.block = end_block;
            }
            AstStatementType::Loop(body_stmt) => {
                let body_block = self.new_block();
                let end_block = self.new_block();
                self.current_block().terminate(IrTerminator {
                    ty: IrTerminatorType::Unconditional(body_block),
                    line: stmt.line,
                    column: stmt.column,
                });
                self.block = body_block;
                self.nearest_loops.push(NearestLoop {
                    start_scope: self.scope.len(),
                    end_block,
                });
                self.push_scope();
                self.compile_statement(body_stmt);
                self.pop_scope(stmt.line, stmt.column);
                self.nearest_loops
                    .pop()
                    .expect("loop ends should only be popped where they are created");
                self.current_block().terminate(IrTerminator {
                    ty: IrTerminatorType::Unconditional(body_block),
                    line: stmt.line,
                    column: stmt.column,
                });
                self.block = end_block;
            }
            AstStatementType::Break => {
                let NearestLoop {
                    start_scope,
                    end_block,
                } = *self.nearest_loops.last().unwrap_or_else(|| {
                    panic!(
                        "break can only be used in a loop: {}:{}",
                        stmt.line + 1,
                        stmt.column + 1
                    )
                });
                self.free_scope(start_scope, stmt.line, stmt.column);
                self.current_block().terminate(IrTerminator {
                    ty: IrTerminatorType::Unconditional(end_block),
                    line: stmt.line,
                    column: stmt.column,
                });
                self.block = self.new_block();
            }
            AstStatementType::Constant(..) => todo!(),
            &AstStatementType::Variable(
                ref expr,
                ref ty,
                AstIdentifier {
                    ref name,
                    line,
                    column,
                },
            ) => {
                let var_var = self.new_variable(Self::get_variable_type(ty));
                let var_temp =
                    self.add_instruction(IrInstructionType::Variable(var_var), line, column);
                if let Some(expr) = expr {
                    let val_temp = self.compile_expression(expr);
                    self.add_assignment(var_temp, val_temp, line, column);
                } else {
                    self.add_zero_initialization(var_temp, line, column);
                };
                self.scope
                    .last_mut()
                    .expect("scope should not be empty")
                    .insert(name.clone(), var_var);
            }
            // TODO More here
            AstStatementType::Block(stmts) => {
                self.push_scope();
                for stmt in stmts {
                    self.compile_statement(stmt);
                }
                self.pop_scope(stmt.line, stmt.column);
            }
            &AstStatementType::Call(
                ref args,
                AstIdentifier {
                    ref name,
                    line,
                    column,
                },
            ) => match (args.len(), name.as_str()) {
                (1, "ал") => {
                    let arr_temp = self.compile_expression(&args[0]);
                    let count_temp =
                        self.add_instruction(IrInstructionType::Count(arr_temp), line, column);
                    let count_deref_temp = self.add_dereference(count_temp, line, column);
                    let zero_temp =
                        self.add_instruction(IrInstructionType::Natural(0), line, column);
                    let cond_temp = self.add_instruction(
                        IrInstructionType::GreaterThan(count_deref_temp, zero_temp),
                        line,
                        column,
                    );
                    let then_block = self.new_block();
                    let else_block = self.new_block();
                    self.current_block().terminate(IrTerminator {
                        ty: IrTerminatorType::Conditional(cond_temp, then_block, else_block),
                        line,
                        column,
                    });
                    self.block = then_block;
                    self.current_block().terminate(IrTerminator {
                        ty: IrTerminatorType::Error(format!(
                            "cannot pop from empty array at {}:{}",
                            line + 1,
                            column + 1
                        )),
                        line,
                        column,
                    });
                    self.block = else_block;
                    let one_temp =
                        self.add_instruction(IrInstructionType::Natural(1), line, column);
                    let new_count_temp = self.add_instruction(
                        IrInstructionType::Subtract(count_deref_temp, one_temp),
                        line,
                        column,
                    );
                    let item_temp = self.add_instruction(
                        IrInstructionType::Index(arr_temp, new_count_temp),
                        line,
                        column,
                    );
                    self.add_free(item_temp, line, column);
                    self.add_assignment(count_temp, new_count_temp, line, column);
                }
                (1, "жаз") => {
                    let temp = self.compile_expression(&args[0]);
                    let temp = match &self.temps[temp] {
                        IrPrimitiveType::Reference(ty)
                            if matches!(ty.as_ref(), IrVariableType::Primitive(_)) =>
                        {
                            self.add_instruction(IrInstructionType::Dereference(temp), line, column)
                        }
                        _ => temp,
                    };
                    if matches!(
                        &self.temps[temp],
                        IrPrimitiveType::Reference(ty)
                            if matches!(
                                ty.as_ref(),
                                IrVariableType::Array(ty)
                                    if *ty.as_ref()
                                        == IrVariableType::Primitive(IrPrimitiveType::Character)
                            )
                    ) {
                        let index_temp =
                            self.add_instruction(IrInstructionType::Natural(0), line, column);
                        let count_temp =
                            self.add_instruction(IrInstructionType::Count(temp), line, column);
                        let count_deref_temp = self.add_dereference(count_temp, line, column);
                        let cond_block = self.new_block();
                        let body_block = self.new_block();
                        let end_block = self.new_block();
                        self.current_block().terminate(IrTerminator {
                            ty: IrTerminatorType::Unconditional(cond_block),
                            line,
                            column,
                        });
                        self.block = cond_block;
                        let cond_temp = self.add_instruction(
                            IrInstructionType::LessThan(index_temp, count_deref_temp),
                            line,
                            column,
                        );
                        self.current_block().terminate(IrTerminator {
                            ty: IrTerminatorType::Conditional(cond_temp, body_block, end_block),
                            line,
                            column,
                        });
                        self.block = body_block;
                        let char_temp = self.add_instruction(
                            IrInstructionType::Index(temp, index_temp),
                            line,
                            column,
                        );
                        let char_deref_temp = self.add_dereference(char_temp, line, column);
                        let success_temp = self.add_instruction(
                            IrInstructionType::Write(char_deref_temp),
                            line,
                            column,
                        );
                        let success_block = self.new_block();
                        let error_block = self.new_block();
                        self.current_block().terminate(IrTerminator {
                            ty: IrTerminatorType::Conditional(
                                success_temp,
                                success_block,
                                error_block,
                            ),
                            line,
                            column,
                        });
                        self.block = success_block;
                        let one_temp =
                            self.add_instruction(IrInstructionType::Natural(1), line, column);
                        self.current_block().add_instruction(IrInstruction {
                            temp: index_temp,
                            ty: IrInstructionType::Add(index_temp, one_temp),
                            line,
                            column,
                        });
                        self.current_block().terminate(IrTerminator {
                            ty: IrTerminatorType::Unconditional(cond_block),
                            line,
                            column,
                        });
                        self.block = error_block;
                        self.current_block().terminate(IrTerminator {
                            ty: IrTerminatorType::Error(format!(
                                "write error at {}:{}",
                                line + 1,
                                column + 1
                            )),
                            line,
                            column,
                        });
                        self.block = end_block;
                    } else {
                        let success_temp =
                            self.add_instruction(IrInstructionType::Write(temp), line, column);
                        let success_block = self.new_block();
                        let error_block = self.new_block();
                        self.current_block().terminate(IrTerminator {
                            ty: IrTerminatorType::Conditional(
                                success_temp,
                                success_block,
                                error_block,
                            ),
                            line,
                            column,
                        });
                        self.block = error_block;
                        self.current_block().terminate(IrTerminator {
                            ty: IrTerminatorType::Error(format!(
                                "write error at {}:{}",
                                line + 1,
                                column + 1
                            )),
                            line,
                            column,
                        });
                        self.block = success_block;
                    }
                }
                (1, "оқы") => {
                    let temp = self.compile_expression(&args[0]);
                    let success_temp =
                        self.add_instruction(IrInstructionType::Read(temp), line, column);
                    let success_block = self.new_block();
                    let error_block = self.new_block();
                    self.current_block().terminate(IrTerminator {
                        ty: IrTerminatorType::Conditional(success_temp, success_block, error_block),
                        line,
                        column,
                    });
                    self.block = error_block;
                    self.current_block().terminate(IrTerminator {
                        ty: IrTerminatorType::Error(format!(
                            "error reading at {}:{}",
                            line + 1,
                            column + 1
                        )),
                        line,
                        column,
                    });
                    self.block = success_block;
                }
                (2, "қос") => {
                    let arr_temp = self.compile_expression(&args[0]);
                    let val_temp = self.compile_expression(&args[1]);
                    let count_temp =
                        self.add_instruction(IrInstructionType::Count(arr_temp), line, column);
                    let count_deref_temp = self.add_dereference(count_temp, line, column);
                    let one_temp =
                        self.add_instruction(IrInstructionType::Natural(1), line, column);
                    let new_count_temp = self.add_instruction(
                        IrInstructionType::Add(count_deref_temp, one_temp),
                        line,
                        column,
                    );
                    let capacity_temp =
                        self.add_instruction(IrInstructionType::Capacity(arr_temp), line, column);
                    let capacity_deref_temp = self.add_dereference(capacity_temp, line, column);
                    let new_capacity_temp = self.add_instruction(
                        IrInstructionType::MinimumCapacity(new_count_temp),
                        line,
                        column,
                    );
                    let cond_temp = self.add_instruction(
                        IrInstructionType::LessThan(capacity_deref_temp, new_capacity_temp),
                        line,
                        column,
                    );
                    let then_block = self.new_block();
                    let end_block = self.new_block();
                    self.current_block().terminate(IrTerminator {
                        ty: IrTerminatorType::Conditional(cond_temp, then_block, end_block),
                        line,
                        column,
                    });
                    self.block = then_block;
                    self.current_block().add_instruction(IrInstruction {
                        temp: arr_temp,
                        ty: IrInstructionType::Resize(new_capacity_temp),
                        line,
                        column,
                    });
                    self.add_assignment(capacity_temp, new_capacity_temp, line, column);
                    self.current_block().terminate(IrTerminator {
                        ty: IrTerminatorType::Unconditional(end_block),
                        line,
                        column,
                    });
                    self.block = end_block;
                    let item_temp = self.add_instruction(
                        IrInstructionType::Index(arr_temp, count_deref_temp),
                        line,
                        column,
                    );
                    self.add_zero_initialization(item_temp, line, column);
                    self.add_assignment(item_temp, val_temp, line, column);
                    self.add_assignment(count_temp, new_count_temp, line, column);
                }
                (2, "орнат") => {
                    let var_temp = self.compile_expression(&args[0]);
                    let val_temp = self.compile_expression(&args[1]);
                    self.add_assignment(var_temp, val_temp, line, column);
                }
                (2, "санынОрнат") => {
                    let arr_temp = self.compile_expression(&args[0]);
                    let new_count_temp = self.compile_expression(&args[1]);
                    let count_temp =
                        self.add_instruction(IrInstructionType::Count(arr_temp), line, column);
                    let count_deref_temp = self.add_dereference(count_temp, line, column);
                    let cond_temp = self.add_instruction(
                        IrInstructionType::LessThan(count_deref_temp, new_count_temp),
                        line,
                        column,
                    );
                    let then_block = self.new_block();
                    let else_block = self.new_block();
                    let end_block = self.new_block();
                    self.current_block().terminate(IrTerminator {
                        ty: IrTerminatorType::Conditional(cond_temp, then_block, else_block),
                        line,
                        column,
                    });
                    self.block = then_block;
                    let capacity_temp =
                        self.add_instruction(IrInstructionType::Capacity(arr_temp), line, column);
                    let capacity_deref_temp = self.add_dereference(capacity_temp, line, column);
                    let new_capacity_temp = self.add_instruction(
                        IrInstructionType::MinimumCapacity(new_count_temp),
                        line,
                        column,
                    );
                    let cond_temp = self.add_instruction(
                        IrInstructionType::LessThan(capacity_deref_temp, new_capacity_temp),
                        line,
                        column,
                    );
                    let then_then_block = self.new_block();
                    let then_end_block = self.new_block();
                    self.current_block().terminate(IrTerminator {
                        ty: IrTerminatorType::Conditional(
                            cond_temp,
                            then_then_block,
                            then_end_block,
                        ),
                        line,
                        column,
                    });
                    self.block = then_then_block;
                    self.current_block().add_instruction(IrInstruction {
                        temp: arr_temp,
                        ty: IrInstructionType::Resize(new_capacity_temp),
                        line,
                        column,
                    });
                    self.add_assignment(capacity_temp, new_capacity_temp, line, column);
                    self.current_block().terminate(IrTerminator {
                        ty: IrTerminatorType::Unconditional(then_end_block),
                        line,
                        column,
                    });
                    self.block = then_end_block;
                    let then_cond_block = self.new_block();
                    let then_body_block = self.new_block();
                    self.current_block().terminate(IrTerminator {
                        ty: IrTerminatorType::Unconditional(then_cond_block),
                        line,
                        column,
                    });
                    self.block = then_cond_block;
                    let cond_temp = self.add_instruction(
                        IrInstructionType::LessThan(count_deref_temp, new_count_temp),
                        line,
                        column,
                    );
                    self.current_block().terminate(IrTerminator {
                        ty: IrTerminatorType::Conditional(cond_temp, then_body_block, end_block),
                        line,
                        column,
                    });
                    self.block = then_body_block;
                    let item_temp = self.add_instruction(
                        IrInstructionType::Index(arr_temp, count_deref_temp),
                        line,
                        column,
                    );
                    self.add_zero_initialization(item_temp, line, column);
                    let one_temp =
                        self.add_instruction(IrInstructionType::Natural(1), line, column);
                    self.current_block().add_instruction(IrInstruction {
                        temp: count_deref_temp,
                        ty: IrInstructionType::Add(count_deref_temp, one_temp),
                        line,
                        column,
                    });
                    self.current_block().terminate(IrTerminator {
                        ty: IrTerminatorType::Unconditional(then_cond_block),
                        line,
                        column,
                    });
                    self.block = else_block;
                    let cond_temp = self.add_instruction(
                        IrInstructionType::GreaterThan(count_deref_temp, new_count_temp),
                        line,
                        column,
                    );
                    let else_body_block = self.new_block();
                    self.current_block().terminate(IrTerminator {
                        ty: IrTerminatorType::Conditional(cond_temp, else_body_block, end_block),
                        line,
                        column,
                    });
                    self.block = else_body_block;
                    let whole_count_temp = self.add_instruction(
                        IrInstructionType::ToWhole(count_deref_temp),
                        line,
                        column,
                    );
                    let whole_one_temp =
                        self.add_instruction(IrInstructionType::Whole(1), line, column);
                    let next_whole_count_temp = self.add_instruction(
                        IrInstructionType::Subtract(whole_count_temp, whole_one_temp),
                        line,
                        column,
                    );
                    self.current_block().add_instruction(IrInstruction {
                        temp: count_deref_temp,
                        ty: IrInstructionType::ToNatural(next_whole_count_temp),
                        line,
                        column,
                    });
                    let item_temp = self.add_instruction(
                        IrInstructionType::Index(arr_temp, count_deref_temp),
                        line,
                        column,
                    );
                    self.add_free(item_temp, line, column);
                    self.current_block().terminate(IrTerminator {
                        ty: IrTerminatorType::Unconditional(else_block),
                        line,
                        column,
                    });
                    self.block = end_block;
                    self.add_assignment(count_temp, new_count_temp, line, column);
                }
                _ => todo!(),
            },
            _ => todo!(),
        }
    }

    fn mark_reachable(&self, reachable: &mut [bool], mut block: usize) {
        loop {
            if reachable[block] {
                break;
            }
            reachable[block] = true;
            let IrBlock::Terminated(_, term) = &self.blocks[block] else {
                panic!("not all blocks are terminated");
            };
            match term.ty {
                IrTerminatorType::Unconditional(goto_block) => block = goto_block,
                IrTerminatorType::Conditional(_, then_block, else_block) => {
                    self.mark_reachable(reachable, then_block);
                    block = else_block
                }
                _ => break,
            }
        }
    }

    pub fn compile(mut self, stmt: &AstStatement) -> IrFunction {
        self.push_scope();
        self.compile_statement(stmt);
        self.pop_scope(stmt.line, stmt.column);
        let temp = self.add_instruction(IrInstructionType::Natural(0), stmt.line, stmt.column);
        self.current_block().terminate(IrTerminator {
            ty: IrTerminatorType::Return(temp),
            line: stmt.line,
            column: stmt.column,
        });
        let mut reachable = vec![false; self.blocks.len()];
        self.mark_reachable(&mut reachable, 0);
        IrFunction {
            vars: self.vars,
            temps: self.temps,
            blocks: self
                .blocks
                .into_iter()
                .zip(reachable)
                .map(|(block, reachable)| {
                    let IrBlock::Terminated(insts, term) = block else {
                        panic!("not all blocks are terminated");
                    };
                    if reachable {
                        Some(IrTerminatedBlock { insts, term })
                    } else {
                        None
                    }
                })
                .collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IrTerminatedBlock {
    pub insts: Vec<IrInstruction>,
    pub term: IrTerminator,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IrFunction {
    pub vars: Vec<IrVariableType>,
    pub temps: Vec<IrPrimitiveType>,
    pub blocks: Vec<Option<IrTerminatedBlock>>,
}
