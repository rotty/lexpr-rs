//! Support to avoid stack overflows that otherwise could occur when the implicit compiler-added
//! dropping does its automatic final dropping of fields via recursive calls.
//!
//! Any deep tree of `Value`s, of any variants and any shapes, could cause stack overflow when
//! dropped, due to the recursive function calls that do the final dropping.  E.g. a long chain of
//! cons cells is deep down the `cdr` side.  When any deep `Value` is wrapped with
//! [`DeepSafeValueDropper`], stack overflow is prevented (by preventing those recursive calls) by
//! first mutating the children and root `Value`s to become leafs before their final dropping.

// Note: This module does `match`es without wildcard arms, because if which variants have children
// ever changes, this module will need to adjust for that.

use crate::Value;
use deep_safe_drop::{deep_safe_drop, DeepSafeDrop, Link, SetParent};
use std::mem::replace;

/// Wrap a [`Value`] with this when you want dropping of it to use [`mod@deep_safe_drop`], to
/// ensure that dropping of deep `Value`s (e.g. a long list, or a long chain of cons cells) cannot
/// cause stack overflow.
///
/// ([`Drop`] isn't impl'ed for `Value` directly, because that can cause Rust error E0509 which
/// would be an undesirable limitation.)
pub struct DeepSafeValueDropper(pub Value);

impl Drop for DeepSafeValueDropper {
    fn drop(&mut self) {
        deep_safe_drop(&mut self.0);
    }
}

impl DeepSafeDrop<Self> for Value {
    fn set_parent_at_index_0(&mut self, parent: Self) -> SetParent<Self> {
        match child_at_index_0(self) {
            Some(child0) => match replace_branch_node(child0, parent) {
                Some(child0) => SetParent::YesReplacedChild { child0 },
                None => SetParent::Yes,
            },
            None => SetParent::No {
                returned_parent: parent,
            },
        }
    }

    fn take_child_at_index_0(&mut self) -> Option<Self> {
        child_at_index_0(self).and_then(take_branch_node)
    }

    fn take_next_child_at_pos_index(&mut self) -> Option<Self> {
        match self {
            Value::Cons(cons) => take_branch_node(cons.cdr_mut()),
            Value::Vector(vector) => vector.get_mut(1..).and_then(vector_take_branch_node),
            Value::Nil
            | Value::Null
            | Value::Bool(_)
            | Value::Number(_)
            | Value::Char(_)
            | Value::String(_)
            | Value::Symbol(_)
            | Value::Keyword(_)
            | Value::Bytes(_) => None,
        }
    }
}

impl Link<Self> for Value {
    fn get_mut(&mut self) -> &mut Self {
        self
    }
}

fn child_at_index_0(value: &mut Value) -> Option<&mut Value> {
    match value {
        Value::Cons(cons) => Some(cons.car_mut()),
        Value::Vector(vector) => vector.get_mut(0),
        Value::Nil
        | Value::Null
        | Value::Bool(_)
        | Value::Number(_)
        | Value::Char(_)
        | Value::String(_)
        | Value::Symbol(_)
        | Value::Keyword(_)
        | Value::Bytes(_) => None,
    }
}

/// If `*value` has one or more children, it's a branch (i.e. non-leaf) node that we want
/// by-value.  If it's a leaf then return `None`.  In both cases, `*value` is replaced by a
/// leaf.
fn take_branch_node(value: &mut Value) -> Option<Value> {
    const LEAF: Value = Value::Nil;
    replace_branch_node(value, LEAF)
}

fn replace_branch_node(dest: &mut Value, src: Value) -> Option<Value> {
    let prev = replace(dest, src);
    match &prev {
        Value::Cons(_) | Value::Vector(_) => Some(prev),
        Value::Nil
        | Value::Null
        | Value::Bool(_)
        | Value::Number(_)
        | Value::Char(_)
        | Value::String(_)
        | Value::Symbol(_)
        | Value::Keyword(_)
        | Value::Bytes(_) => None,
    }
}

fn vector_take_branch_node(vector: &mut [Value]) -> Option<Value> {
    vector.iter_mut().find_map(take_branch_node)
}
