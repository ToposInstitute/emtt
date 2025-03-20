use fexplib::types::*;
use tattle::{declare_error, Reporter, ReporterOutput};

use crate::toplevel::{with_parsed, TopElaborator};

declare_error!(TOP_ERROR, "top", "error while parsing the toplevel");

pub fn check(input: &str) -> Option<()> {
    let mut success = true;
    let reporter = Reporter::new(ReporterOutput::Stdout, input.to_string());
    success &= with_parsed(input, reporter.clone(), |ast| {
        let mut toplevel = TopElaborator::empty(reporter.clone());
        match ast.ast0() {
            Block(decls, None) => {
                for decl in decls.iter() {
                    if toplevel.elab_sequent(decl).is_none() {
                        success = false;
                    }
                }
            }
            _ => {
                reporter.error(ast.loc(), TOP_ERROR, |f| {
                    write!(f, "could not parse toplevel, expected {{ <decl>; ... }}")
                });
                success = false;
            }
        }
    })
    .is_some();
    if success {
        Some(())
    } else {
        None
    }
}
