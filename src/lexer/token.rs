#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Operator {
    Plus,
    Minus,
    Times,
    Divide,
    Less,
    LessEqual,
    More,
    MoreEqual,
    Not,
    Equal,
    Unequal,
    PipeRight,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Literal {
    String(String),
    Int(String),
    Bool(bool),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Func,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Colon,
    Comma,
    Semicolon,
    Right,
    Anonymous,
    Operator(Operator),
    Ident(String),
    Literal(Literal),
    Illegal(Illegal),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Illegal {
    UnclosedComment,
    UnclosedString,
}

impl Operator {
    pub fn repr(&self) -> &str {
        match self {
            Operator::Plus => "+",
            Operator::Minus => "-",
            Operator::Times => "*",
            Operator::Divide => "/",
            Operator::Less => "<",
            Operator::LessEqual => "<=",
            Operator::More => ">",
            Operator::MoreEqual => ">=",
            Operator::Not => "!",
            Operator::Equal => "==",
            Operator::Unequal => "!=",
            Operator::PipeRight => "|>",
        }
    }
}

impl Literal {
    pub fn repr(&self) -> String {
        match self {
            Literal::String(s) => format!("%String: {}%", s),
            Literal::Int(s) => format!("%Int: {}%", s),
            Literal::Bool(b) => format!("%{}%", b),
        }
    }
}

impl Token {
    pub fn repr(&self) -> String {
        match self {
            Token::Func => "fn".to_owned(),
            Token::LParen => "(".to_owned(),
            Token::RParen => ")".to_owned(),
            Token::LBrace => "{".to_owned(),
            Token::RBrace => "}".to_owned(),
            Token::LBracket => "[".to_owned(),
            Token::RBracket => "]".to_owned(),
            Token::Colon => ":".to_owned(),
            Token::Comma => ",".to_owned(),
            Token::Semicolon => ";".to_owned(),
            Token::Right => "->".to_owned(),
            Token::Anonymous => "||".to_owned(),
            Token::Operator(o) => o.repr().to_owned(),
            Token::Literal(l) => l.repr().to_owned(),
            Token::Ident(v) => format!("%{}%", v),
            Token::Illegal(_) => "ILLEGAL".to_owned(),
        }
    }
}

pub struct TokenPacket {
    pub token: Token,
    pub start_pos: usize,
    pub end_pos: usize,
}
