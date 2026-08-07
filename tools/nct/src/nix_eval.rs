//! Nix evaluation helpers built on the nix-bindings-* crates.
//!
//! All nix EvalState work happens on a dedicated OS thread registered with the
//! BDW garbage collector (`gc_register_my_thread`), because EvalState is only
//! safe to touch from a GC-registered thread.

use std::collections::HashMap;
use std::io::Write;

use anyhow::{Context as _, Result};
use nix_bindings_expr::eval_state::{EvalState, EvalStateBuilder, gc_register_my_thread, init};
use nix_bindings_expr::value::{Value, ValueType};
use nix_bindings_flake::EvalStateBuilderExt;
use nix_bindings_store::store::Store;

/// Evaluate `expr` applied to this flake's `nixosConfigurations.<machine>`.
///
/// `flake_root` is the directory containing `flake.nix` (defaults to the
/// current working directory). The machine value passed to the user's lambda
/// is the full `nixosConfigurations.<name>` attrset (i.e. `{ config, pkgs, ... }`
/// result), so `m.config.X` is the expected access pattern.
///
/// Prints the result (Nix-style) and returns it as a string.
pub fn eval_machine_expr(flake_root: &str, machine: &str, expr: &str) -> Result<String> {
    eval_flake_apply(flake_root, &format!("nixosConfigurations.{machine}"), expr)
}

/// Generic: load the flake at `flake_root`, navigate the dotted `attr_path`,
/// then apply the lambda `expr` to the resulting value and print it.
pub fn eval_flake_apply(flake_root: &str, attr_path: &str, expr: &str) -> Result<String> {
    // init() is process-global and idempotent.
    init().context("nix library init")?;

    // getFlake requires an absolute path; resolve relative roots against cwd.
    let flake_root =
        std::fs::canonicalize(flake_root).unwrap_or_else(|_| std::path::PathBuf::from(flake_root));
    let flake_root = flake_root
        .to_str()
        .context("flake root path is not valid UTF-8")?;

    let out = std::thread::scope(|s| {
        s.spawn(|| -> Result<String> {
            let _guard = gc_register_my_thread().context("gc_register_my_thread")?;

            let store = Store::open(None, HashMap::new()).context("open nix store")?;
            let mut es = EvalStateBuilder::new(store.clone())?
                .flakes(&nix_bindings_flake::FlakeSettings::new()?)?
                .build()?;

            // Load this flake via `builtins.getFlake` with an explicit
            // absolute root path (no registry dependency).
            let flake_value = es
                .eval_from_string(
                    &format!("builtins.getFlake \"{flake_root}\""),
                    "<nct-getFlake>",
                )
                .context("evaluating getFlake")?;

            // Navigate the dotted attr path.
            let mut current = flake_value;
            for attr in attr_path.split('.') {
                if attr.is_empty() {
                    continue;
                }
                current = es.require_attrs_select(&current, attr).with_context(|| {
                    format!("attribute `{attr}` not found in flake path `{attr_path}`")
                })?;
            }

            let lambda = es
                .eval_from_string(expr, "<nct-apply>")
                .context("evaluating apply expression")?;

            let result = es
                .call(lambda, current)
                .context("applying expression to value")?;

            let mut buf = Vec::new();
            print_value_into(&mut es, &result, &mut buf, 0)?;
            String::from_utf8(buf).context("result was not valid UTF-8")
        })
        .join()
        .unwrap_or_else(|e| std::panic::resume_unwind(e))
    })?;
    Ok(out)
}

/// Recursively print a Nix value, roughly matching `nix eval` output:
/// strings unquoted, ints/bools/null as-is, lists/attrsets pretty-printed.
///
/// Public so the long-lived [`crate::nix_flake::NixFlake`] worker can reuse it.
pub fn print_value_into(
    es: &mut EvalState,
    v: &Value,
    out: &mut Vec<u8>,
    depth: usize,
) -> Result<()> {
    let t = es.value_type(v)?;
    match t {
        ValueType::String => {
            let s = es.require_string(v)?;
            writeln!(out, "{s}")?;
        }
        ValueType::Int => {
            writeln!(out, "{}", es.require_int(v)?)?;
        }
        ValueType::Bool => {
            writeln!(out, "{}", es.require_bool(v)?)?;
        }
        ValueType::Null => {
            writeln!(out, "null")?;
        }
        ValueType::List => {
            let elems: Vec<Value> = es.require_list_strict(v)?;
            if elems.is_empty() {
                writeln!(out, "[ ]")?;
            } else {
                writeln!(out, "[")?;
                for e in &elems {
                    write!(out, "{}", "  ".repeat(depth + 1))?;
                    // Elements may be multi-line (attrsets); print inline-ish.
                    print_inline_into(es, e, out, depth + 1)?;
                }
                writeln!(out, "{}]", "  ".repeat(depth))?;
            }
        }
        ValueType::AttrSet => {
            let names = es.require_attrs_names(v)?;
            if names.is_empty() {
                writeln!(out, "{{ }}")?;
            } else {
                writeln!(out, "{{")?;
                for name in names {
                    let av = es.require_attrs_select(v, &name)?;
                    write!(out, "{}{name} = ", "  ".repeat(depth + 1))?;
                    print_inline_into(es, &av, out, depth + 1)?;
                }
                writeln!(out, "{}}}", "  ".repeat(depth))?;
            }
        }
        ValueType::Path => {
            // Paths coerce to strings.
            let s = es.require_string(v).unwrap_or_else(|_| "<path>".into());
            writeln!(out, "{s}")?;
        }
        other => {
            writeln!(out, "<{other:?}>")?;
        }
    }
    Ok(())
}

/// Print a value followed by its terminating `;`/newline, used inside
/// attrsets/lists where each entry ends with a semicolon (Nix-style).
fn print_inline_into(es: &mut EvalState, v: &Value, out: &mut Vec<u8>, depth: usize) -> Result<()> {
    let t = es.value_type(v)?;
    match t {
        ValueType::String | ValueType::Path => {
            let s = es
                .require_string(v)
                .unwrap_or_else(|_| "<unprintable>".into());
            writeln!(out, "\"{s}\";")?;
        }
        ValueType::Int => writeln!(out, "{};", es.require_int(v)?)?,
        ValueType::Bool => writeln!(out, "{};", es.require_bool(v)?)?,
        ValueType::Null => writeln!(out, "null;")?,
        ValueType::List => {
            // Defer to full pretty-printer for nested compound values.
            print_value_into(es, v, out, depth)?;
        }
        ValueType::AttrSet => {
            print_value_into(es, v, out, depth)?;
        }
        other => writeln!(out, "<{other:?}>;")?,
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Bootstrap smoke test: doesn't require a real flake, just proves the
    /// init + GC + store path works and errors cleanly.
    #[test]
    fn eval_flake_apply_missing_flake_is_err() {
        let res = eval_flake_apply("/nonexistent-flake-root", "anything", "m: m");
        assert!(res.is_err());
    }

    /// Integration test against the bundled fixture flake. Requires a working
    /// nix store + network for flake fetching, so it's `#[ignore]` by
    /// default; run with `cargo test -- --ignored`.
    #[test]
    #[ignore]
    fn eval_flake_apply_fixture_attr_names() {
        let fixture = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/min-flake");
        let out = eval_flake_apply(fixture, "fixtureValue.names", "xs: builtins.length xs")
            .expect("eval should succeed");
        // fixtureValue.names has 3 elements
        assert!(out.trim().ends_with('3'), "got: {out}");
    }

    #[test]
    #[ignore]
    fn eval_flake_apply_fixture_nested_attr() {
        let fixture = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/min-flake");
        let out = eval_flake_apply(fixture, "fixtureValue.nested", "a: a.b")
            .expect("eval should succeed");
        assert!(out.trim().ends_with('2'), "got: {out}");
    }
}
