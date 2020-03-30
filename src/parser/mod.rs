use lexer::Illegal;
use lexer::Literal;
use lexer::Operator;
use lexer::Token;
use lexer::TokenPacket;

use std::iter::Peekable;

pub enum Expr {
    Unary(Box<UnaryExpr>),
    Binary(Box<BinaryExpr>),
    Tuple(Box<TupleExpr>),
    Literal(Box<LiteralExpr>),
    Group(Box<GroupExpr>),
    Anon(Box<AnonExpr>),
    Var(Box<VarExpr>),
}

pub struct AnonExpr {
    pub expr: Expr,
}

pub struct LiteralExpr {
    pub literal: Literal,
}

pub struct VarExpr {
    pub id: String,
}

pub struct GroupExpr {
    pub expr: Expr,
}
pub struct TupleExpr {
    pub elems: Vec<Expr>,
}

pub struct UnaryExpr {
    pub op: Operator,
    pub expr: Expr,
}

pub struct BinaryExpr {
    pub op: Operator,
    pub left: Expr,
    pub right: Expr,
}

pub struct RootNode {
    pub funcs: Vec<FuncNode>,
}

pub struct FuncNode {
    pub name: String,
    pub params: Vec<Param>,
    pub body: Vec<Expr>,
}

pub struct Param {
    pub name: String,
    pub type_: String,
}

pub struct SyntaxIssue {
    pub msg: String,
    pub pos: usize,
}

pub struct SyntaxError {
    pub errors: Vec<SyntaxIssue>,
}

impl SyntaxError {
    fn combine(mut self, mut error: SyntaxError) -> SyntaxError {
        self.errors.append(error.errors.as_mut());
        self
    }

    fn new_empty() -> SyntaxError {
        SyntaxError { errors: Vec::new() }
    }

    fn is_empty(&mut self) -> bool {
        self.errors.is_empty()
    }
}

pub struct Parser<I>
where
    I: Iterator<Item = TokenPacket>,
{
    token_iter: Peekable<I>,
    pos: usize,
    pub warnings: Vec<SyntaxIssue>,
}

impl<I> Parser<I>
where
    I: Iterator<Item = TokenPacket>,
{
    pub fn new(lex: I) -> Parser<I> {
        Parser {
            token_iter: lex.peekable(),
            pos: 0,
            warnings: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> Result<RootNode, SyntaxError> {
        let mut funcs = Vec::new();
        let mut error = SyntaxError::new_empty();

        loop {
            if let Some(t) = self.read_token()? {
                let result = match t {
                    Token::Func => self.read_func(),
                    _ => Err(self.gen_error("expected function")),
                };

                match result {
                    Ok(t) => funcs.push(t),
                    Err(e) => {
                        error = error.combine(e);
                        if !self.sync_func() {
                            return Err(error);
                        }
                    }
                }
            } else {
                break;
            }
        }

        if error.is_empty() {
            Ok(RootNode { funcs })
        } else {
            Err(error)
        }
    }

    fn expect(&mut self, token: Token) -> Result<(), SyntaxError> {
        match self.read_token()? {
            Some(ref t) if *t == token => Ok(()),
            _ => Err(self.gen_error(&format!("expected '{}'", token.repr()))),
        }
    }

    fn warn(&mut self, msg: &str) {
        self.warnings.push(SyntaxIssue {
            msg: msg.to_owned(),
            pos: self.pos,
        });
    }

    fn gen_error(&self, msg: &str) -> SyntaxError {
        SyntaxError {
            errors: vec![SyntaxIssue {
                msg: msg.to_owned(),
                pos: self.pos,
            }],
        }
    }

    fn err_from_illegal(&self, b: &Illegal) -> SyntaxError {
        match &b {
            Illegal::UnclosedComment => self.gen_error("unclosed comment"),
            Illegal::UnclosedString => self.gen_error("unclosed string"),
        }
    }

    fn verify_token(&self, token: &Token) -> Result<(), SyntaxError> {
        match token {
            Token::Illegal(b) => Err(self.err_from_illegal(b)),
            _ => Ok(()),
        }
    }

    fn read_token(&mut self) -> Result<Option<Token>, SyntaxError> {
        match self.token_iter.next() {
            Some(p) => {
                self.pos = p.start_pos;
                self.verify_token(&p.token)?;
                Ok(Some(p.token))
            }
            None => Ok(None),
        }
    }

    fn ignore_token(&mut self) {
        self.token_iter.next();
    }

    fn peek_token(&mut self) -> Option<&Token> {
        if let Some(p) = self.token_iter.peek() {
            self.pos = p.start_pos;
            Some(&p.token)
        } else {
            None
        }
    }

    fn read_ident(&mut self) -> Result<String, SyntaxError> {
        if let Some(Token::Ident(n)) = self.read_token()? {
            Ok(n)
        } else {
            Err(self.gen_error("invalid token"))
        }
    }

    fn read_params(&mut self) -> Result<Vec<Param>, SyntaxError> {
        self.expect(Token::LParen)?;

        let mut args = Vec::new();

        if let Some(&Token::RParen) = self.peek_token() {
            self.ignore_token();
            return Ok(args);
        }

        loop {
            let name = self.read_ident()?;
            self.expect(Token::Colon)?;

            let type_ = self.read_ident()?;
            args.push(Param { name, type_ });

            match self.read_token()? {
                Some(Token::Comma) => {}
                Some(Token::RParen) => break,
                _ => return Err(self.gen_error("failed to parse function parameters")),
            }
        }
        Ok(args)
    }

    fn read_tuple(&mut self) -> Result<Expr, SyntaxError> {
        let mut elems = Vec::new();
        if let Some(&Token::RBracket) = self.peek_token() {
            self.ignore_token();
        } else {
            loop {
                let expr = self.read_expr()?;
                elems.push(expr);

                match self.read_token()? {
                    Some(Token::Comma) => continue,
                    Some(Token::RBracket) => break,
                    _ => return Err(self.gen_error("expected ',' or ']'")),
                };
            }
        }
        Ok(Expr::Tuple(Box::new(TupleExpr { elems })))
    }

    fn get_op(token: Token) -> Operator {
        match token {
            Token::Operator(op) => op,
            _ => panic!("INTERNAL ERROR: FAILED TO UNWRAP OPERATOR TOKEN."),
        }
    }

    fn read_primary(&mut self) -> Result<Expr, SyntaxError> {
        match self.read_token()? {
            Some(Token::Literal(l)) => Ok(Expr::Literal(Box::new(LiteralExpr { literal: l }))),

            Some(Token::Ident(v)) => Ok(Expr::Var(Box::new(VarExpr { id: v }))),

            Some(Token::LParen) => {
                let expr = self.read_expr()?;
                self.expect(Token::RParen)?;
                Ok(Expr::Group(Box::new(GroupExpr { expr })))
            }

            Some(Token::Anonymous) => {
                let expr = self.read_expr()?;
                self.expect(Token::Anonymous)?;
                Ok(Expr::Anon(Box::new(AnonExpr { expr })))
            }

            Some(Token::LBracket) => self.read_tuple(),

            _ => return Err(self.gen_error("invalid primary token")),
        }
    }

    fn read_unary(&mut self) -> Result<Expr, SyntaxError> {
        match self.peek_token() {
            Some(&Token::Operator(Operator::Not)) | Some(&Token::Operator(Operator::Minus)) => {
                let op = Self::get_op(self.read_token()?.unwrap());
                let expr = self.read_unary()?;
                Ok(Expr::Unary(Box::new(UnaryExpr { op, expr })))
            }
            _ => self.read_primary(),
        }
    }

    fn read_times(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.read_unary()?;

        loop {
            match self.peek_token() {
                Some(&Token::Operator(Operator::Times))
                | Some(&Token::Operator(Operator::Divide)) => {
                    let op = Self::get_op(self.read_token()?.unwrap());
                    let right = self.read_unary()?;
                    expr = Expr::Binary(Box::new(BinaryExpr {
                        left: expr,
                        op,
                        right,
                    }))
                }

                _ => break,
            }
        }

        Ok(expr)
    }

    fn read_addition(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.read_times()?;

        loop {
            match self.peek_token() {
                Some(&Token::Operator(Operator::Plus))
                | Some(&Token::Operator(Operator::Minus)) => {
                    let op = Self::get_op(self.read_token()?.unwrap());
                    let right = self.read_times()?;
                    expr = Expr::Binary(Box::new(BinaryExpr {
                        left: expr,
                        op,
                        right,
                    }))
                }

                _ => break,
            }
        }

        Ok(expr)
    }

    fn read_comparison(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.read_addition()?;

        loop {
            match self.peek_token() {
                Some(&Token::Operator(Operator::Less))
                | Some(&Token::Operator(Operator::LessEqual))
                | Some(&Token::Operator(Operator::More))
                | Some(&Token::Operator(Operator::MoreEqual)) => {
                    let op = Self::get_op(self.read_token()?.unwrap());
                    let right = self.read_addition()?;
                    expr = Expr::Binary(Box::new(BinaryExpr {
                        left: expr,
                        op,
                        right,
                    }))
                }

                _ => break,
            }
        }

        Ok(expr)
    }

    fn read_equality(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.read_comparison()?;

        loop {
            match self.peek_token() {
                Some(&Token::Operator(Operator::Equal))
                | Some(&Token::Operator(Operator::Unequal)) => {
                    let op = Self::get_op(self.read_token()?.unwrap());
                    let right = self.read_comparison()?;
                    expr = Expr::Binary(Box::new(BinaryExpr {
                        left: expr,
                        op,
                        right,
                    }))
                }

                _ => break,
            }
        }

        Ok(expr)
    }

    fn read_pipe(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.read_equality()?;

        loop {
            match self.peek_token() {
                Some(&Token::Operator(Operator::PipeRight)) => {
                    let op = Self::get_op(self.read_token()?.unwrap());

                    let right = self.read_equality()?;
                    expr = Expr::Binary(Box::new(BinaryExpr {
                        left: expr,
                        op,
                        right,
                    }))
                }

                _ => break,
            }
        }

        Ok(expr)
    }

    fn read_expr(&mut self) -> Result<Expr, SyntaxError> {
        self.read_pipe()
    }

    fn read_statement(&mut self) -> Result<Expr, SyntaxError> {
        let result = self.read_expr();
        self.expect(Token::Semicolon)?;
        result
    }

    fn sync_statement(&mut self) -> bool {
        loop {
            match self.peek_token() {
                Some(Token::Semicolon) => {
                    self.ignore_token();
                    return true;
                }
                Some(Token::RBrace) => return false,
                Some(_) => self.ignore_token(),
                None => return false,
            };
        }
    }

    fn sync_func(&mut self) -> bool {
        loop {
            match self.peek_token() {
                Some(Token::RBrace) => {
                    self.ignore_token();
                    return true;
                }
                Some(_) => self.ignore_token(),
                None => return false,
            };
        }
    }

    fn read_statements(&mut self) -> Result<Vec<Expr>, SyntaxError> {
        let mut exprs = Vec::new();
        let mut error = SyntaxError::new_empty();

        loop {
            if let Some(&Token::RBrace) = self.peek_token() {
                break;
            }

            match self.read_statement() {
                Ok(t) => exprs.push(t),
                Err(e) => {
                    error = error.combine(e);
                    if !self.sync_statement() {
                        return Err(error);
                    }
                }
            }
        }
        if error.is_empty() {
            Ok(exprs)
        } else {
            Err(error)
        }
    }

    fn read_func(&mut self) -> Result<FuncNode, SyntaxError> {
        let name = self.read_ident()?;

        let params = self.read_params()?;

        self.expect(Token::LBrace)?;

        let exprs = self.read_statements()?;

        self.expect(Token::RBrace)?;

        Ok(FuncNode {
            name,
            params,
            body: exprs,
        })
    }
}
