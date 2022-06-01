use crate::{
    compiler::{lexer::Token, semantics::Error as SemErr},
    shared::{Span, Spanned},
};
use ariadne::{sources, Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::prelude::Simple;

pub(crate) enum Error {
    SemanticError(Spanned<SemErr>),
    LexicalError(Simple<String>),
    SyntaxError(Simple<String>),
}

impl From<&Simple<Token>> for Error {
    fn from(tok_err: &Simple<Token>) -> Self {
        Error::SyntaxError(tok_err.clone().map(|t| t.to_string()))
    }
}

impl From<&Simple<char>> for Error {
    fn from(ch_err: &Simple<char>) -> Self {
        Error::LexicalError(ch_err.clone().map(|c| c.to_string()))
    }
}

impl From<&Spanned<SemErr>> for Error {
    fn from(sem_err: &Spanned<SemErr>) -> Self {
        Error::SemanticError(sem_err.clone())
    }
}

impl Error {
    pub(crate) fn report(self, src: &str) -> Report<(&str, std::ops::Range<usize>)> {
        match self {
            Error::SemanticError((sem_err, span)) => {
                Report::build(ReportKind::Error, src.clone(), span.start)
                    .with_message(sem_err.to_string())
                    .with_label(
                        Label::new((src.clone(), span))
                            .with_message(sem_err.msg())
                            .with_color(Color::Red),
                    )
                    .finish()
            }
            Error::LexicalError(e) | Error::SyntaxError(e) => {
                // code adapted from https://github.com/zesterer/chumsky/blob/2cdcd859ccd1ba90869631c2117be4185f44ac63/examples/nano_rust.rs#L570-L631
                let mut report = Report::build(ReportKind::Error, src.clone(), e.span().start);

                report = match e.reason() {
                    chumsky::error::SimpleReason::Unexpected => report
                        .with_message(format!(
                            "{}, expected {}",
                            if e.found().is_some() {
                                "Unexpected token in input"
                            } else {
                                "Unexpected end of input"
                            },
                            if e.expected().len() == 0 {
                                "something else".to_string()
                            } else {
                                e.expected()
                                    .map(|expected| match expected {
                                        Some(expected) => expected.to_string(),
                                        None => "end of input".to_string(),
                                    })
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            }
                        ))
                        .with_label(
                            Label::new((src.clone(), e.span()))
                                .with_message(format!(
                                    "Unexpected token {}",
                                    e.found()
                                        .unwrap_or(&"end of file".to_string())
                                        .fg(Color::Red)
                                ))
                                .with_color(Color::Red),
                        ),

                    chumsky::error::SimpleReason::Unclosed { span, delimiter } => report
                        .with_message(format!(
                            "Unclosed delimiter {}",
                            delimiter.fg(Color::Yellow)
                        ))
                        .with_label(
                            Label::new((src.clone(), span.clone()))
                                .with_message(format!(
                                    "Unclosed delimiter {}",
                                    delimiter.fg(Color::Yellow)
                                ))
                                .with_color(Color::Yellow),
                        )
                        .with_label(
                            Label::new((src.clone(), span.clone()))
                                .with_message(format!(
                                    "Must be closed before this {}",
                                    e.found()
                                        .unwrap_or(&"end of file".to_string())
                                        .fg(Color::Red)
                                ))
                                .with_color(Color::Red),
                        ),
                    chumsky::error::SimpleReason::Custom(msg) => {
                        report.with_message(msg).with_label(
                            Label::new((src.clone(), e.span()))
                                .with_message(format!("{}", msg.fg(Color::Red)))
                                .with_color(Color::Red),
                        )
                    }
                };
                report.finish()
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[ignore]
    #[test]
    fn test_semantic_error() {
        let src = "tests/foo.gu";
        let error = Error::SemanticError((SemErr::Undefined("x".to_string()), 0..1));

        let report = error.report(src.clone());

        report
            .print((src.clone(), Source::from(include_str!("tests/foo.gu"))))
            .unwrap();
    }

    #[ignore]
    #[test]
    fn test_lexical_error() {
        let src = "tests/foo.gu";
        let error = Error::LexicalError(chumsky::error::Simple::custom(
            4..6,
            "Sike! Nothing wrong, just a custom message",
        ));

        let report = error.report(src.clone());

        report
            .print((src.clone(), Source::from(include_str!("tests/foo.gu"))))
            .unwrap();
    }
}
