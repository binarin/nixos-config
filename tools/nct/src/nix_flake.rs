//! A long-lived handle to this flake's evaluation, backed by a single
//! dedicated GC-registered worker thread.
//!
//! `EvalState` / `Value` from nix-bindings hold raw pointers (`*mut raw::...`),
//! so they are `!Send` / `!Sync` by construction — they must live on the one
//! thread that created them. The BDW garbage collector itself supports many
//! registered threads, but the handle objects are thread-affine, so we own
//! the whole eval on a single worker for the lifetime of this handle and
//! ferry closures to it over a channel.
//!
//! The payoff: `builtins.getFlake` + the full `nixosConfigurations.<name>`
//! module eval is computed **once** (when the machine value is first forced),
//! and every subsequent `config.*` read is a `require_attrs_select` /
//! `require_string` / ... over already-forced thunks — near-free. This is the
//! whole point of using nix-bindings instead of spawning `nix eval` per attr.

use std::collections::HashMap;
use std::sync::mpsc;

use anyhow::{Context as _, Result};
use nix_bindings_expr::eval_state::{EvalState, EvalStateBuilder, gc_register_my_thread, init};
use nix_bindings_expr::value::{Value, ValueType};
use nix_bindings_flake::EvalStateBuilderExt;
use nix_bindings_store::store::Store;

/// A job sent to the worker thread: run `f` against the worker's `EvalState`.
///
/// Because closures capturing `!Send` values can't cross threads, the closure
/// only receives `&mut EvalState` (which lives on the worker) and returns a
/// serialized string. Callers that want typed data parse it back out (JSON via
/// a tiny Nix lambda, or leaf extracts via the typed accessors below).
type Job = Box<dyn FnOnce(&mut EvalState, &Value) -> Result<String> + Send + 'static>;

pub struct NixFlake {
    sender: Option<mpsc::Sender<Job>>,
    handle: Option<std::thread::JoinHandle<Result<()>>>,
}

impl NixFlake {
    /// Open the flake at `flake_root` (directory containing flake.nix) and
    /// pre-force nothing — evaluation is lazy. `getFlake` itself runs eagerly
    /// so that subsequent `attr()` calls can navigate the flake's outputs.
    pub fn open(flake_root: &str) -> Result<Self> {
        init().context("nix library init")?;

        let flake_root = std::fs::canonicalize(flake_root)
            .unwrap_or_else(|_| std::path::PathBuf::from(flake_root));
        let flake_root_str = flake_root
            .to_str()
            .context("flake root path is not valid UTF-8")?
            .to_owned();

        let (tx, rx) = mpsc::channel::<Job>();

        let handle = std::thread::Builder::new()
            .name("nct-nix-worker".into())
            .spawn(move || -> Result<()> {
                let _guard = gc_register_my_thread().context("gc_register_my_thread")?;

                let store = Store::open(None, HashMap::new()).context("open nix store")?;
                let mut es = EvalStateBuilder::new(store.clone())?
                    .flakes(&nix_bindings_flake::FlakeSettings::new()?)?
                    .build()?;

                // Load the flake once; reuse for every subsequent navigation.
                // Cloned cheaply (Value is a handle, not the underlying nix
                // object — that stays alive in the EvalState's GC heap).
                let flake_value = es
                    .eval_from_string(
                        &format!("builtins.getFlake \"{flake_root_str}\""),
                        "<nct-getFlake>",
                    )
                    .context("evaluating getFlake")?;

                for job in rx.iter() {
                    let _ = job(&mut es, &flake_value);
                }
                Ok(())
            })?;

        Ok(Self {
            sender: Some(tx),
            handle: Some(handle),
        })
    }

    /// Run `f` on the worker's `EvalState` with the flake root value,
    /// returning its string output.
    fn eval<F>(&self, f: F) -> Result<String>
    where
        F: FnOnce(&mut EvalState, &Value) -> Result<String> + Send + 'static,
    {
        let (tx, rx) = mpsc::sync_channel(1);
        let job: Job = Box::new(move |es: &mut EvalState, root: &Value| {
            let out = f(es, root)?;
            let _ = tx.send(Ok(out));
            Ok(String::new())
        });
        let sender = self.sender.as_ref().context("nix worker already dropped")?;
        sender
            .send(job)
            .map_err(|e| anyhow::anyhow!("nix worker thread died: {e}"))?;
        rx.recv().context("nix worker dropped response channel")?
    }

    /// Navigate a dotted attr path under the flake root (e.g.
    /// `nixosConfigurations.xray-exit.config.networking.hostName`) and return
    /// the leaf as a string. Non-string leaves error; use [`attr_json`] for
    /// compound values.
    pub fn attr_str(&self, path: &str) -> Result<String> {
        let path = path.to_owned();
        self.eval(move |es, root| {
            let v = navigate(es, root, &path)?;
            let t = es.value_type(&v)?;
            match t {
                ValueType::String | ValueType::Path => Ok(es.require_string(&v)?),
                ValueType::Int => Ok(es.require_int(&v)?.to_string()),
                ValueType::Bool => Ok(es.require_bool(&v)?.to_string()),
                other => anyhow::bail!("attr `{path}` is {other:?}, not a string-like leaf"),
            }
        })
    }

    /// Navigate `path` and apply `lambda` to the value, returning the lambda's
    /// result printed Nix-style (same printer as `eval_flake_apply`).
    pub fn apply(&self, path: &str, lambda: &str) -> Result<String> {
        let lambda = lambda.to_owned();
        let path = path.to_owned();
        self.eval(move |es, root| {
            let v = navigate(es, root, &path)?;
            let lam = es.eval_from_string(&lambda, "<nct-apply>")?;
            let result = es.call(lam, v)?;
            let mut buf = Vec::new();
            crate::nix_eval::print_value_into(es, &result, &mut buf, 0)?;
            String::from_utf8(buf).context("result was not valid UTF-8")
        })
    }
}

impl Drop for NixFlake {
    fn drop(&mut self) {
        // Drop the sender so the worker's `rx.iter()` ends and the thread
        // exits cleanly. Join to surface panics.
        drop(self.sender.take());
        if let Some(h) = self.handle.take() {
            let _ = h.join();
        }
    }
}

/// Navigate a dotted attr path under `root`, forcing as we go.
fn navigate(es: &mut EvalState, root: &Value, path: &str) -> Result<Value> {
    let mut current = root.clone();
    for attr in path.split('.') {
        if attr.is_empty() {
            continue;
        }
        current = es
            .require_attrs_select(&current, attr)
            .with_context(|| format!("attribute `{attr}` not found while navigating `{path}`"))?;
    }
    Ok(current)
}
