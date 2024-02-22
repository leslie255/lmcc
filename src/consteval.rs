use crate::{
    ast::{Expr, InfixOpKind, PostfixOpKind, PrefixOpKind},
    error::Spanned,
    token::NumValue,
};

/// Performs const eval on AST level.
/// Can only handle number literals and arithmatic operators.
pub fn consteval_ast(expr: Spanned<&Expr>) -> Option<NumValue> {
    macro numval_op($op:tt, $lhs:expr, $rhs:expr $(,)?) {
        match ($lhs, $rhs) {
            (NumValue::I(l), NumValue::I(r)) => NumValue::from(l $op r),
            (NumValue::I(i), NumValue::F(f)) | (NumValue::F(f), NumValue::I(i))
                => NumValue::from((i as f64) $op f),
            (NumValue::F(l), NumValue::F(r)) => NumValue::from(l $op r),
        }
    }
    macro numval_op_const_rhs($op:tt, $lhs:expr, $rhs:literal $(,)?) {
        match $lhs {
            NumValue::I(i) => NumValue::from(i $op ($rhs as i128)),
            NumValue::F(f) => NumValue::from(f $op ($rhs as f64)),
        }
    }
    macro numval_cmp($op:tt, $lhs:expr, $rhs:expr $(,)?) {{
        let bool_ = match ($lhs, $rhs) {
            (NumValue::I(l), NumValue::I(r)) => (l $op r),
            (NumValue::I(i), NumValue::F(f)) => ((i as f64) $op f),
            (NumValue::F(f), NumValue::I(i)) => (f $op (i as f64)),
            (NumValue::F(l), NumValue::F(r)) => (l $op r),
        };
        if bool_ {
            NumValue::from(1i128)
        } else {
            NumValue::from(0i128)
        }
    }}
    match expr.into_inner() {
        &Expr::NumLiteral(num) => Some(num),
        &Expr::InfixOp(ref lhs, op, ref rhs) => {
            let lhs = consteval_ast(lhs.as_deref())?;
            let rhs = consteval_ast(rhs.as_deref())?;
            match op {
                InfixOpKind::Assign => None,
                InfixOpKind::Add => Some(numval_op!(+, lhs, rhs)),
                InfixOpKind::Sub => Some(numval_op!(-, lhs, rhs)),
                InfixOpKind::Mul => Some(numval_op!(*, lhs, rhs)),
                InfixOpKind::Div => Some(numval_op!(/, lhs, rhs)),
                InfixOpKind::Rem => Some(numval_op!(%, lhs, rhs)),
                InfixOpKind::BitAnd => todo!(),
                InfixOpKind::BitOr => todo!(),
                InfixOpKind::BitXor => todo!(),
                InfixOpKind::And => todo!(),
                InfixOpKind::Or => todo!(),
                InfixOpKind::Gt => Some(numval_cmp!(>, lhs, rhs)),
                InfixOpKind::Ge => Some(numval_cmp!(>, lhs, rhs)),
                InfixOpKind::Lt => Some(numval_cmp!(>, lhs, rhs)),
                InfixOpKind::Le => Some(numval_cmp!(>, lhs, rhs)),
                InfixOpKind::Eq => Some(numval_cmp!(>, lhs, rhs)),
                InfixOpKind::Ne => Some(numval_cmp!(>, lhs, rhs)),
                InfixOpKind::Comma => Some(rhs),
                InfixOpKind::Bsl => todo!(),
                InfixOpKind::Bsr => todo!(),
            }
        }
        Expr::PrefixOp(op, oper) => {
            let oper = consteval_ast(oper.as_deref())?;
            match op {
                PrefixOpKind::BitNot => todo!(),
                PrefixOpKind::Not => todo!(),
                PrefixOpKind::UnaryAdd => Some(oper),
                PrefixOpKind::UnarySub => Some(match oper {
                    NumValue::I(i) => NumValue::from(-i),
                    NumValue::F(f) => NumValue::from(-f),
                }),
                PrefixOpKind::PreInc => Some(numval_op_const_rhs!(+, oper, 1)),
                PrefixOpKind::PreDec => Some(numval_op_const_rhs!(-, oper, 1)),
                PrefixOpKind::Ref | PrefixOpKind::Deref => None,
            }
        }
        Expr::PostfixOp(oper, op) => {
            let oper = consteval_ast(oper.as_deref())?;
            match op {
                PostfixOpKind::PostInc => Some(numval_op_const_rhs!(+, oper, 1)),
                PostfixOpKind::PostDec => Some(numval_op_const_rhs!(-, oper, 1)),
            }
        },
        _ => None,
    }
}
