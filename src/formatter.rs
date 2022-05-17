/*
use ariadne::{Report, ReportKind, Label, Source};
use crate::lexer::Span;

enum ErrorHandling {
    SemanticError(crate::compiler::semantics::Error, Span),
}

fn create_report(error: ErrorHandling, src: &str) -> Report {
    match error {
        ErrorHandling::SemanticError(error, span) => {
            match error {
                crate::compiler::semantics::Error::TypeMismatch(src, type1, type2) => {
                    Report::build(ReportKind::Error, (), span.start)
                        .with_message(error.to_string())
                        .finish()
                }
                crate::compiler::semantics::Error::Undefined(src) => {
                    Report::build(ReportKind::Error, (), span.start)
                    .with_message(error.to_string())
                    .finish()
                }
            }
        }
    }
}
*/