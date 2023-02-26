use super::TokenKind;
use crate::ast::Span;

#[allow(dead_code)]
#[derive(Debug)]
pub enum ErrorLevel {
    Warning,
    Error,
}

#[derive(Debug)]
pub struct ParseError {
    pub level: ErrorLevel,
    pub message: String,
    // pub code // TODO: errors should have codes
    pub span: Option<Span>,
}

impl ParseError {
    pub fn unhandled() -> Self {
        Self {
            level: ErrorLevel::Error,
            message: String::from("unhandled error"),
            span: None,
        }
    }

    // pub fn unexpected_token(token: &TokenKind, expected: &TokenKind, span: Span) -> Self {
    //     Self {
    //         level: ErrorLevel::Error,
    //         message: format!("unexpected token `{:?}` (expected `{:?}`)", token, expected),
    //         span: Some(span),
    //     }
    // }

    pub fn expected_expression(span: Span) -> Self {
        Self {
            level: ErrorLevel::Error,
            message: String::from("expected expression"),
            span: Some(span),
        }
    }

    pub fn expected_token(token: &TokenKind, span: Span) -> Self {
        Self {
            level: ErrorLevel::Error,
            message: format!("expected `{:?}`", token),
            span: Some(span),
        }
    }

    pub fn expected_keyword(keyword: &str, span: Span) -> Self {
        Self {
            level: ErrorLevel::Error,
            message: format!("expected `{keyword}`"),
            span: Some(span),
        }
    }
}

impl ParseError {
    pub fn emit(&self, file: &str, src: &str) {
        let level = match self.level {
            ErrorLevel::Error => "error",
            ErrorLevel::Warning => "warning",
        };
        eprintln!("{level}: {}", self.message);
        if let Some(span) = self.span {
            let loc = span.loc as usize;
            let source_up_to = &src[..loc];
            let line = bytecount::count(source_up_to.as_bytes(), b'\n') + 1;
            let col = loc - source_up_to.rfind('\n').unwrap_or(0);
            let culprit = &src[loc..loc + span.len as usize];
            eprintln!("--> {file}:{line}:{col}");
            eprintln!("{culprit}");
        }
    }
}
