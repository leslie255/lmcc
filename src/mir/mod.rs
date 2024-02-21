#![allow(dead_code)]
use std::{
    collections::HashMap,
    fmt::{self, Debug, Display},
};

use index_vec::IndexVec;

use crate::{
    ast::{EnumFields, Signature, StructFields, Ty},
    error::Spanned,
    utils::{index_vec_kv_pairs, DoInBetween, IdentStr},
};

mod builder;
pub use builder::*;

#[derive(Debug, Clone, Default)]
pub struct NamesContext {
    pub funcs: HashMap<IdentStr, FuncData>,
    /// TODO: Static variables.
    pub statics: (),
    /// The first layer of locals are globals.
    pub locals: Locals,
}

#[derive(Debug, Clone, Default)]
pub struct LocalsLayer {
    pub vars: HashMap<IdentStr, VarId>,
    pub struct_names: HashMap<IdentStr, StructFields>,
    pub union_names: HashMap<IdentStr, StructFields>,
    pub enum_names: HashMap<IdentStr, EnumFields>,
}

#[derive(Debug, Clone)]
pub struct Locals(Vec<LocalsLayer>);
impl Default for Locals {
    fn default() -> Self {
        Self(vec![LocalsLayer::default()])
    }
}
impl Locals {
    pub fn enters_block(&mut self) {
        self.0.push(LocalsLayer::default());
    }
    pub fn leaves_block(&mut self) {
        self.0.pop().unwrap();
    }
}
macro impl_name_type($add_name:ident, $name:ident, $map:ident, $V:ty) {
    impl Locals {
        /// Returns `Err` for conflicting names.
        pub fn $add_name(&mut self, name: IdentStr, v: $V) -> Result<(), ()> {
            if self.0.last_mut().unwrap().$map.insert(name, v).is_some() {
                Err(())
            } else {
                Ok(())
            }
        }
        pub fn $name(&self, name: IdentStr) -> Option<&$V> {
            self.0.iter().rev().find_map(|names| names.$map.get(&name))
        }
    }
}
impl_name_type!(add_var, var, vars, VarId);
impl_name_type!(add_struct, struct_, struct_names, StructFields);
impl_name_type!(add_union, union_, union_names, StructFields);
impl_name_type!(add_enum, enum_, enum_names, EnumFields);

#[derive(Debug, Clone)]
pub struct FuncData {
    /// `extern` on function has no effect, we only care about `static`.
    /// `inline` is also ignored.
    pub is_static: bool,
    pub sig: Signature,
    pub mir_func: Option<MirFunction>,
}

#[derive(Clone, PartialEq)]
pub struct MirFunction {
    pub args: Vec<VarId>,
    pub ret: Ty,
    pub vars: IndexVec<VarId, VarInfo>,
    pub blocks: IndexVec<BlockId, MirBlock>,
}

impl Debug for MirFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("MirFunction")
            .field("args", &self.args)
            .field("ret", &self.ret)
            .field("vars", index_vec_kv_pairs(&self.vars))
            .field("body", index_vec_kv_pairs(&self.blocks))
            .finish()
    }
}

#[derive(Clone, PartialEq, Default)]
pub struct MirBlock {
    /// Instructions before the first terminator.
    pub insts: Vec<MirInst>,
    /// Instructions after the first terminator.
    pub unreachable_insts: Vec<MirInst>,
    /// Flag used during construction of this MIR block, invalid afterwards.
    pub(self) is_terminated: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(pub(self) usize);
impl index_vec::Idx for BlockId {
    fn from_usize(idx: usize) -> Self {
        Self(idx)
    }
    fn index(self) -> usize {
        self.0
    }
}

#[derive(Clone, PartialEq)]
pub enum MirInst {
    Assign(Place, Value),
    /// TODO
    BitfieldAssign(std::convert::Infallible),
    /// Unary operator operation.
    /// Not including inc/dec operations, those are elaborated into another instruction.
    UnOp(Place, UnOpKind, Value),
    /// Binary operator operation.
    /// Not including:
    /// - bit shifts (one of operand must be const.),
    /// - comma (converted into multiple instructions).
    /// - assignments (one of the operand is a place not a value).
    /// - member-accessing (see `Place`).
    BinOp(Place, Value, BinOpKind, Value),
    /// Call a function statically, store it into a place if result isn't `void`.
    CallStatic(Option<Place>, IdentStr, Vec<Value>),
    /// Call a function dynamically, store it into a place if result isn't `void`.
    CallDynamic(Option<Place>, Value, Vec<Value>),
    /// Block terminator.
    Term(MirTerm),
}

#[derive(Clone, PartialEq)]
pub enum MirTerm {
    /// Unconditional jump.
    Jump(BlockId),
    /// Conditional jump.
    JumpIf(Value, BlockId, BlockId),
    /// `Value::Void` if returning void.
    Return(Value),
    /// Unreachables is always trapped for easier debug.
    Unreachable,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarId(pub(self) usize);
impl index_vec::Idx for VarId {
    fn from_usize(idx: usize) -> Self {
        Self(idx)
    }
    fn index(self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarInfo {
    /// Is it an argument of the function?
    pub(self) is_arg: bool,
    pub(self) ty: Ty,
    /// If it's an actual variable declared by the user, tag it with a name for debug.
    pub(self) name: Option<Spanned<IdentStr>>,
    /// A variable can be flagged as a a tmp variable if it is single assigned and declared/used within the same block.
    /// This information is a useful for later stages in the compilation.
    pub(self) is_tmp: bool,
}

/// A place is an l-value, assignable and addressable.
/// There's `Value::{CopyPlace, RefPlace}`, for yielding values from a place by value or by reference, respectively.
///
/// One interesting exception, that fits neither in the definition of a place or a value, are bitfield values (assignable but not addressable),
/// which are taken care of by `MirInst::BitfieldAssign`.
///
/// A place is represented as one variable as a root with some projections on top of it, for example, in the case of:
///
/// ```c
/// *var->field0[idx].field1
/// ```
///
/// ..., variable `var` is the root, the projections are:
///
/// ```c
/// "var".field_ind("field0").index("idx").field_dir("field1").deref
/// ```
///
/// Projections are stored in reverse of their actual order due to the way it's parsed.
#[derive(Clone, PartialEq)]
pub struct Place {
    /// See `Place`.
    pub(self) root: VarId,
    /// See `Place`.
    /// Note that they are stored in reverse of their actual order due to the way it's parsed.
    pub(self) projections: Vec<PlaceProjection>,
}

impl Place {
    /// Create a place with no projections.
    pub const fn just_var(root: VarId) -> Self {
        Self {
            root,
            projections: Vec::new(),
        }
    }
}

/// See `Place`.
#[derive(Clone, PartialEq)]
pub enum PlaceProjection {
    FieldDir(IdentStr),
    FieldInd(IdentStr),
    Deref,
    /// To avoid nesting values, operand of index could only be a variable.
    Index(VarId),
}

/// A `Value` is the representation of an r-value.
#[derive(Clone, PartialEq)]
pub enum Value {
    Num(NumLiteralContent),
    Char(u8),
    /// TODO
    Str(std::convert::Infallible),
    CopyPlace(Place),
    RefPlace(Place),
    Void,
}

#[derive(Clone, Copy, PartialEq)]
pub enum NumLiteralContent {
    U(u64),
    I(i64),
    F(f64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOpKind {
    Deref,
    UnarySub,
    UnaryAdd,
    Not,
    BitNot,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitAnd,
    BitOr,
    BitXor,
    And,
    Or,
    Gt,
    Ge,
    Lt,
    Le,
    Eq,
    Ne,
}

impl Debug for VarId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "var{}", self.0)
    }
}

impl Debug for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "block{}", self.0)
    }
}

impl Debug for NumLiteralContent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::U(n) => write!(f, "{n}u"),
            Self::I(n) => write!(f, "{n}i"),
            Self::F(n) => write!(f, "{n}f"),
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Num(n) => write!(f, "num({n:?})"),
            Self::Char(c) => write!(f, "char({c:?})"),
            Self::Str(..) => write!(f, "str(TODO)"),
            Self::CopyPlace(place) => write!(f, "copy({place:?})"),
            Self::RefPlace(place) => write!(f, "ref({place:?})"),
            Self::Void => write!(f, "void"),
        }
    }
}

impl Debug for PlaceProjection {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FieldDir(name) => write!(f, ".field_dir({name:?})"),
            Self::FieldInd(name) => write!(f, ".field_ind({name:?})"),
            Self::Deref => write!(f, ".deref"),
            Self::Index(var) => write!(f, ".index({var:?})"),
        }
    }
}

impl Debug for Place {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.root, f)?;
        for proj in self.projections.iter().rev() {
            Debug::fmt(proj, f)?;
        }
        Ok(())
    }
}

impl Debug for MirInst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Assign(lhs, rhs) => write!(f, "{lhs:?} = {rhs:?}"),
            Self::BitfieldAssign(..) => write!(f, "TODO(bitfield assign)"),
            Self::UnOp(place, op, oper) => write!(f, "{place:?} = {op} {oper:?}"),
            Self::BinOp(place, lhs, op, rhs) => write!(f, "{place:?} = {lhs:?} {op} {rhs:?}"),
            Self::CallStatic(Some(lhs), callee, args) => {
                write!(f, "{lhs:?} = call {callee:?}(")?;
                let f = args.iter().try_do_in_between(
                    f,
                    |f| write!(f, ","),
                    |f, arg| write!(f, "{arg:?}"),
                )?;
                write!(f, ")")
            }
            Self::CallStatic(None, callee, args) => {
                write!(f, "call {callee:?}(")?;
                let f = args.iter().try_do_in_between(
                    f,
                    |f| write!(f, ","),
                    |f, arg| write!(f, "{arg:?}"),
                )?;
                write!(f, ")")
            }
            Self::CallDynamic(Some(lhs), callee, args) => {
                write!(f, "{lhs:?} = call ({callee:?})(")?;
                let f = args.iter().try_do_in_between(
                    f,
                    |f| write!(f, ","),
                    |f, arg| write!(f, "{arg:?}"),
                )?;
                write!(f, ")")
            }
            Self::CallDynamic(None, callee, args) => {
                write!(f, "call ({callee:?})(")?;
                let f = args.iter().try_do_in_between(
                    f,
                    |f| write!(f, ","),
                    |f, arg| write!(f, "{arg:?}"),
                )?;
                write!(f, ")")
            }
            Self::Term(term) => Debug::fmt(term, f),
        }
    }
}

impl Debug for MirTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Jump(b) => write!(f, "jump {b:?}"),
            Self::JumpIf(cond, b0, b1) => write!(f, "jump {cond:?} ? {b0:?} : {b1:?}"),
            Self::Return(val) => write!(f, "return {val:?}"),
            Self::Unreachable => write!(f, "unreachable"),
        }
    }
}

impl Display for UnOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Deref => write!(f, "*"),
            Self::UnarySub => write!(f, "-"),
            Self::UnaryAdd => write!(f, "+"),
            Self::Not => write!(f, "!"),
            Self::BitNot => write!(f, "^"),
        }
    }
}

impl Display for BinOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Rem => write!(f, "%"),
            Self::BitAnd => write!(f, "&"),
            Self::BitOr => write!(f, "|"),
            Self::BitXor => write!(f, "^"),
            Self::And => write!(f, "&&"),
            Self::Or => write!(f, "||"),
            Self::Gt => write!(f, ">"),
            Self::Ge => write!(f, ">="),
            Self::Lt => write!(f, "<"),
            Self::Le => write!(f, "<="),
            Self::Eq => write!(f, "=="),
            Self::Ne => write!(f, "!="),
        }
    }
}

impl Debug for MirBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.unreachable_insts.is_empty() {
            f.debug_struct("MirBlock")
                .field("insts", &self.insts)
                .finish()
        } else {
            f.debug_struct("MirBlock")
                .field("insts", &self.insts)
                .field("unreachable_insts", &self.unreachable_insts)
                .finish()
        }
    }
}
