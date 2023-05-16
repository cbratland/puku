use super::*;

#[test]
fn empty_string() {
    assert_eq!(tokenize(""), vec![Token::new(TokenKind::Eof, 0, 0)]);
}

#[test]
fn function() {
    assert_eq!(
        tokenize("export func add(a: i32, b: i32) -> i32 { return a + b }"),
        vec![
            Token::new(TokenKind::Identifier, 0, 6),
            Token::new(TokenKind::Identifier, 7, 4),
            Token::new(TokenKind::Identifier, 12, 3),
            Token::new(TokenKind::OpenDelimiter(Delimiter::Parenthesis), 15, 1),
            Token::new(TokenKind::Identifier, 16, 1),
            Token::new(TokenKind::Colon, 17, 1),
            Token::new(TokenKind::Identifier, 19, 3),
            Token::new(TokenKind::Comma, 22, 1),
            Token::new(TokenKind::Identifier, 24, 1),
            Token::new(TokenKind::Colon, 25, 1),
            Token::new(TokenKind::Identifier, 27, 3),
            Token::new(TokenKind::CloseDelimiter(Delimiter::Parenthesis), 30, 1),
            Token::new(TokenKind::Arrow, 32, 2),
            Token::new(TokenKind::Identifier, 35, 3),
            Token::new(TokenKind::OpenDelimiter(Delimiter::Brace), 39, 1),
            Token::new(TokenKind::Identifier, 41, 6),
            Token::new(TokenKind::Identifier, 48, 1),
            Token::new(TokenKind::BinOp(BinOpToken::Plus), 50, 1),
            Token::new(TokenKind::Identifier, 52, 1),
            Token::new(TokenKind::CloseDelimiter(Delimiter::Brace), 54, 1),
            Token::new(TokenKind::Eof, 55, 0)
        ]
    )
}

#[test]
fn comments() {
    // line comment
    assert_eq!(
        tokenize("// comment\nx"),
        vec![
            Token::new(TokenKind::LineComment, 0, 10),
            Token::new(TokenKind::Identifier, 11, 1),
            Token::new(TokenKind::Eof, 12, 0)
        ]
    );
    // doc comment
    assert_eq!(
        tokenize("/// comment\nx"),
        vec![
            Token::new(TokenKind::DocComment, 0, 11),
            Token::new(TokenKind::Identifier, 12, 1),
            Token::new(TokenKind::Eof, 13, 0)
        ]
    );
    // block comment
    assert_eq!(
        tokenize("/* comment\ncomment */x"),
        vec![
            Token::new(TokenKind::BlockComment, 0, 21),
            Token::new(TokenKind::Identifier, 21, 1),
            Token::new(TokenKind::Eof, 22, 0)
        ]
    );
}

#[test]
fn binary_operators() {
    for (op, token) in vec![
        ("+", BinOpToken::Plus),
        ("-", BinOpToken::Minus),
        ("*", BinOpToken::Star),
        ("/", BinOpToken::Slash),
        ("%", BinOpToken::Percent),
        ("^", BinOpToken::Caret),
        ("&", BinOpToken::And),
        ("|", BinOpToken::Pipe),
        ("<<", BinOpToken::ShiftLeft),
        (">>", BinOpToken::ShiftRight),
    ] {
        let len = op.len() as u16;
        assert_eq!(
            tokenize(&format!("a{op}b")),
            vec![
                Token::new(TokenKind::Identifier, 0, 1),
                Token::new(TokenKind::BinOp(token), 1, len),
                Token::new(TokenKind::Identifier, 1 + len as u32, 1),
                Token::new(TokenKind::Eof, 2 + len as u32, 0)
            ]
        );
    }
    // addition with spaces
    assert_eq!(
        tokenize("a      + b"),
        vec![
            Token::new(TokenKind::Identifier, 0, 1),
            Token::new(TokenKind::BinOp(BinOpToken::Plus), 7, 1),
            Token::new(TokenKind::Identifier, 9, 1),
            Token::new(TokenKind::Eof, 10, 0)
        ]
    );
}

#[test]
fn logical_operators() {
    for (op, token) in vec![
        ("=", TokenKind::Equal),
        ("==", TokenKind::EqualEqual),
        ("!=", TokenKind::NotEqual),
        ("<", TokenKind::LThan),
        ("<=", TokenKind::LThanEqual),
        (">", TokenKind::GThan),
        (">=", TokenKind::GThanEqual),
        ("&&", TokenKind::AndAnd),
        ("||", TokenKind::PipePipe),
    ] {
        let len = op.len() as u16;
        assert_eq!(
            tokenize(&format!("a{op}b")),
            vec![
                Token::new(TokenKind::Identifier, 0, 1),
                Token::new(token, 1, len),
                Token::new(TokenKind::Identifier, 1 + len as u32, 1),
                Token::new(TokenKind::Eof, 2 + len as u32, 0)
            ]
        );
    }
}

#[test]
fn unary_operators() {
    for (op, token) in vec![
        ("-", TokenKind::BinOp(BinOpToken::Minus)),
        ("+", TokenKind::BinOp(BinOpToken::Plus)),
        ("!", TokenKind::Exclamation),
    ] {
        let len = op.len() as u16;
        assert_eq!(
            tokenize(&format!("{op}a")),
            vec![
                Token::new(token, 0, len),
                Token::new(TokenKind::Identifier, len as u32, 1),
                Token::new(TokenKind::Eof, 1 + len as u32, 0)
            ]
        );
    }
}

#[test]
fn string_char_literals() {
    // empty string
    assert_eq!(
        tokenize("\"\""),
        vec![
            Token::new(TokenKind::Literal(LiteralKind::String), 0, 2),
            Token::new(TokenKind::Eof, 2, 0)
        ]
    );
    // empty char
    assert_eq!(
        tokenize("''"),
        vec![
            Token::new(TokenKind::Literal(LiteralKind::Char), 0, 2),
            Token::new(TokenKind::Eof, 2, 0)
        ]
    );
    // regular string
    assert_eq!(
        tokenize("\"test\""),
        vec![
            Token::new(TokenKind::Literal(LiteralKind::String), 0, 6),
            Token::new(TokenKind::Eof, 6, 0)
        ]
    );
    // regular char
    assert_eq!(
        tokenize("'a'"),
        vec![
            Token::new(TokenKind::Literal(LiteralKind::Char), 0, 3),
            Token::new(TokenKind::Eof, 3, 0)
        ]
    );
    // char string (this isn't an error in the lexer)
    assert_eq!(
        tokenize("'test'"),
        vec![
            Token::new(TokenKind::Literal(LiteralKind::Char), 0, 6),
            Token::new(TokenKind::Eof, 6, 0)
        ]
    );
    // unclosed quotes
    assert!(std::panic::catch_unwind(|| tokenize("\"")).is_err());
    assert!(std::panic::catch_unwind(|| tokenize("\'")).is_err());
    // todo: escapes ("test\"test" and '\'')
}

#[test]
fn number_literals() {
    // digit
    assert_eq!(
        tokenize("1"),
        vec![
            Token::new(TokenKind::Literal(LiteralKind::Integer), 0, 1),
            Token::new(TokenKind::Eof, 1, 0)
        ]
    );
    // long number (int64 max)
    assert_eq!(
        tokenize("9223372036854775807"),
        vec![
            Token::new(TokenKind::Literal(LiteralKind::Integer), 0, 19),
            Token::new(TokenKind::Eof, 19, 0)
        ]
    );
    // float
    assert_eq!(
        tokenize("1.0"),
        vec![
            Token::new(TokenKind::Literal(LiteralKind::Float), 0, 3),
            Token::new(TokenKind::Eof, 3, 0)
        ]
    );
}
