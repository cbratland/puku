use super::*;

#[test]
fn empty_string() {
    assert_eq!(tokenize(""), vec![Token::new(TokenKind::Eof, 0, 0)]);
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
    // TODO: block comment
    // assert_eq!(
    //     tokenize("/* comment\ncomment */x"),
    //     vec![
    //         Token::new(TokenKind::BlockComment, 0, 20),
    //         Token::new(TokenKind::Identifier, 21, 1),
    //         Token::new(TokenKind::Eof, 0, 0)
    //     ]
    // );
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
