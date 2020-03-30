mod token;

pub use self::token::*;

use std::iter::Peekable;
use std::str::Chars;

struct InputIter<'a> {
    iter: Peekable<Chars<'a>>,
    pos: usize,
}

impl<'a> Iterator for InputIter<'a> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        self.pos += 1;
        self.iter.next()
    }
}

impl<'a> InputIter<'a> {
    fn new(input: &'a str) -> InputIter {
        InputIter {
            iter: input.chars().peekable(),
            pos: 0,
        }
    }

    fn next_pos(&self) -> usize {
        self.pos
    }

    fn prev_pos(&self) -> usize {
        self.pos - 1
    }

    fn peek(&mut self) -> Option<&char> {
        self.iter.peek()
    }
}

pub struct Lexer<'a> {
    input_iter: InputIter<'a>,
}

impl<'a> Lexer<'a> {
    pub fn lex(input: &'a str) -> Lexer<'a> {
        Lexer {
            input_iter: InputIter::new(input),
        }
    }

    fn is_letter(c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }

    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.peek_char() {
            if c.is_whitespace() {
                self.read_char();
            } else {
                break;
            }
        }
    }

    fn read_identifier(&mut self, c: char) -> String {
        let mut ident = String::new();
        ident.push(c);

        while let Some(&c) = self.peek_char() {
            if Self::is_letter(c) {
                ident.push(self.read_char().unwrap());
            } else {
                break;
            }
        }
        ident
    }

    fn lookup_keyword(id: String) -> Token {
        match id.as_str() {
            "fn" => Token::Func,
            "true" => Token::Literal(Literal::Bool(true)),
            "false" => Token::Literal(Literal::Bool(true)),
            _ => Token::Ident(id),
        }
    }

    fn read_number(&mut self, c: char) -> String {
        let mut number = String::new();
        number.push(c);

        while let Some(&c) = self.peek_char() {
            if c.is_digit(10) {
                number.push(self.read_char().unwrap())
            } else {
                break;
            }
        }
        number
    }

    fn read_char(&mut self) -> Option<char> {
        self.input_iter.next()
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.input_iter.peek()
    }

    fn ignore_until(&mut self, c: char) -> bool {
        loop {
            match self.read_char() {
                Some(t) if t == c => return true,
                Some(_) => continue,
                None => return false,
            }
        }
    }

    fn read_string(&mut self, delim: char) -> Option<String> {
        let mut s = String::new();

        loop {
            match self.read_char() {
                Some(t) if t == delim => return Some(s),
                Some(t) => s.push(t),
                None => return None,
            }
        }
    }

    pub fn next_pos(&self) -> usize {
        self.input_iter.next_pos()
    }

    pub fn prev_pos(&self) -> usize {
        self.input_iter.prev_pos()
    }

    fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        if let Some(c) = self.read_char() {
            match c {
                '<' => match self.peek_char() {
                    Some(&'=') => {
                        self.read_char();
                        Some(Token::Operator(Operator::LessEqual))
                    }
                    _ => Some(Token::Operator(Operator::Less)),
                },

                '>' => {
                    if let Some(&'=') = self.peek_char() {
                        Some(Token::Operator(Operator::MoreEqual))
                    } else {
                        Some(Token::Operator(Operator::More))
                    }
                }

                '!' => {
                    if let Some(&'=') = self.peek_char() {
                        self.read_char();
                        Some(Token::Operator(Operator::Unequal))
                    } else {
                        Some(Token::Operator(Operator::Not))
                    }
                }

                '=' => {
                    if let Some(&'=') = self.peek_char() {
                        self.read_char();
                        Some(Token::Operator(Operator::Equal))
                    } else {
                        panic!("Assignment not supported.")
                    }
                }

                '-' => {
                    if let Some(&'>') = self.peek_char() {
                        self.read_char();
                        Some(Token::Right)
                    } else {
                        Some(Token::Operator(Operator::Minus))
                    }
                }
                '|' => match self.peek_char() {
                    Some(&'>') => {
                        self.read_char();
                        Some(Token::Operator(Operator::PipeRight))
                    }
                    Some(&'|') => {
                        self.read_char();
                        Some(Token::Anonymous)
                    }
                    _ => panic!("Unsupported operator '|'."),
                },

                '#' => {
                    if self.ignore_until('#') {
                        self.next_token()
                    } else {
                        Some(Token::Illegal(Illegal::UnclosedComment))
                    }
                }

                '+' => Some(Token::Operator(Operator::Plus)),
                '*' => Some(Token::Operator(Operator::Times)),
                '/' => Some(Token::Operator(Operator::Divide)),
                '(' => Some(Token::LParen),
                ')' => Some(Token::RParen),
                '[' => Some(Token::LBracket),
                ']' => Some(Token::RBracket),
                '{' => Some(Token::LBrace),
                '}' => Some(Token::RBrace),
                ':' => Some(Token::Colon),
                ';' => Some(Token::Semicolon),
                ',' => Some(Token::Comma),

                d @ '"' | d @ '\'' => match self.read_string(d) {
                    Some(s) => Some(Token::Literal(Literal::String(s))),
                    None => Some(Token::Illegal(Illegal::UnclosedString)),
                },

                _ => {
                    if c.is_digit(10) {
                        Some(Token::Literal(Literal::Int(self.read_number(c))))
                    } else {
                        Some(Self::lookup_keyword(self.read_identifier(c)))
                    }
                }
            }
        } else {
            None
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = TokenPacket;

    fn next(&mut self) -> Option<TokenPacket> {
        let start_pos = self.next_pos();

        match self.next_token() {
            Some(t) => {
                let end_pos = self.prev_pos();

                Some(TokenPacket {
                    token: t,
                    start_pos: start_pos,
                    end_pos: end_pos,
                })
            }
            None => None,
        }
    }
}
