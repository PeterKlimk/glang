use lexer::{Literal, Operator};
use parser::*;
use std::clone::Clone;
use std::collections::HashMap;

#[derive(Clone)]
enum Object<'a> {
    Func(&'a FuncNode),
    AFunc(&'a Expr),
    IFunc(String),
    String(String),
    Int(i64),
    Tuple(Vec<Object<'a>>),
    Bool(bool),
    None,
}

impl<'a> Object<'a> {
    fn unwrap_func(&self) -> &'a FuncNode {
        match self {
            &Object::Func(b) => b,
            _ => panic!("Not a func!"),
        }
    }

    fn unwrap_int(&self) -> i64 {
        match self {
            &Object::Int(b) => b,
            _ => panic!("Not an int!"),
        }
    }

    fn unwrap_bool(&self) -> bool {
        match self {
            &Object::Bool(b) => b,
            _ => panic!("Not a bool!"),
        }
    }

    fn unwrap_string(&self) -> String {
        match self {
            &Object::String(ref s) => s.clone(),
            _ => panic!("Not a string!"),
        }
    }
}

pub struct Scope<'a> {
    vars: HashMap<String, Object<'a>>,
}

impl<'a> Scope<'a> {
    fn new() -> Scope<'a> {
        Scope {
            vars: HashMap::new(),
        }
    }
}

pub struct Runtime<'a> {
    globals: HashMap<String, &'a FuncNode>,
}

impl<'a: 'b, 'b> Runtime<'a> {
    pub fn new() -> Runtime<'a> {
        Runtime {
            globals: HashMap::new(),
        }
    }

    pub fn run(&mut self, root: &'a RootNode) {
        for func_node in &root.funcs {
            self.globals.insert(func_node.name.clone(), func_node);
        }

        let main = *self.globals.get("main").unwrap();

        self.exec_func(main, Vec::new());
    }

    fn exec_func(&mut self, func_node: &'b FuncNode, args: Vec<Object<'b>>) -> Object<'b> {
        let mut scope = Scope::new();

        if func_node.params.len() != args.len() {
            panic!("Wrong number of arguments.");
        }

        for i in 0..args.len() {
            let name = func_node.params[i].name.clone();
            let value = args[i].clone();
            scope.vars.insert(name, value);
        }

        for expr in &func_node.body {
            self.eval_expr(expr, &scope);
        }

        Object::None
    }

    fn eval_expr(&mut self, expr: &'b Expr, scope: &'b Scope) -> Object<'b> {
        match expr {
            Expr::Unary(e) => self.eval_unary(e, scope),
            Expr::Binary(e) => self.eval_binary(e, scope),
            Expr::Literal(l) => self.eval_literal(l, scope),
            Expr::Group(g) => self.eval_group(g, scope),
            Expr::Anon(a) => self.eval_anon(a, scope),
            Expr::Tuple(t) => self.eval_tuple(t, scope),
            Expr::Var(v) => self.eval_var(v, scope),
        }
    }

    fn eval_anon(&mut self, anon: &'b AnonExpr, scope: &'b Scope) -> Object<'b> {
        Object::AFunc(&anon.expr)
    }

    fn eval_binary(&mut self, binary: &'b BinaryExpr, scope: &'b Scope) -> Object<'b> {
        let left = self.eval_expr(&binary.left, scope);
        let right = self.eval_expr(&binary.right, scope);

        match binary.op {
            Operator::Plus => Object::Int(left.unwrap_int() + right.unwrap_int()),
            Operator::Minus => Object::Int(left.unwrap_int() - right.unwrap_int()),
            Operator::Times => Object::Int(left.unwrap_int() * right.unwrap_int()),
            Operator::Divide => Object::Int(left.unwrap_int() / right.unwrap_int()),
            Operator::Less => Object::Bool(left.unwrap_int() < right.unwrap_int()),
            Operator::LessEqual => Object::Bool(left.unwrap_int() <= right.unwrap_int()),
            Operator::More => Object::Bool(left.unwrap_int() > right.unwrap_int()),
            Operator::MoreEqual => Object::Bool(left.unwrap_int() >= right.unwrap_int()),
            Operator::Unequal => Object::Bool(left.unwrap_int() != right.unwrap_int()),
            Operator::Equal => Object::Bool(left.unwrap_int() == right.unwrap_int()),
            Operator::PipeRight => self.eval_pipe_right(left, right, scope),
            _ => panic!("Not a binary operator!"),
        }
    }

    fn exec_ifunc(&mut self, name: &str, args: Vec<Object<'b>>) -> Object<'b> {
        match name {
            "print" => {
                print!("{}", args[0].unwrap_string());
                Object::None
            },
            _ => panic!("Invalid internal function."),
        }
    }

    fn eval_pipe_right(
        &mut self,
        left: Object<'b>,
        right: Object<'b>,
        scope: &'b Scope,
    ) -> Object<'b> {
        match right {
            Object::AFunc(expr) => {
                self.eval_expr(expr, scope)
            }
            Object::Func(func) => {
                let args = match left {
                    Object::Tuple(v) => v,
                    o => vec![o],
                };

                self.exec_func(func, args)
            }
            Object::IFunc(name) => {
                let args = match left {
                    Object::Tuple(v) => v,
                    o => vec![o],
                };

                self.exec_ifunc(&name, args)
            }
            _ => panic!("Not a func!"),
        }
    }

    fn eval_group(&mut self, group: &'b GroupExpr, scope: &'b Scope) -> Object<'b> {
        self.eval_expr(&group.expr, scope)
    }

    fn eval_literal(&mut self, literal: &'b LiteralExpr, scope: &'b Scope) -> Object<'b> {
        match &literal.literal {
            Literal::Int(s) => Object::Int(s.parse::<i64>().unwrap()),
            Literal::Bool(b) => Object::Bool(*b),
            Literal::String(s) => Object::String(s.clone()),
        }
    }

    fn eval_tuple(&mut self, tuple: &'b TupleExpr, scope: &'b Scope) -> Object<'b> {
        let mut objects = Vec::new();
        for elem in &tuple.elems {
            objects.push(self.eval_expr(elem, scope));
        }
        Object::Tuple(objects)
    }

    fn eval_var(&mut self, var: &'b VarExpr, scope: &'b Scope) -> Object<'b> {
        if scope.vars.contains_key(&var.id) {
            scope.vars.get(&var.id).unwrap().clone()
        } else {
            match var.id.as_ref() {
                "print" => Object::IFunc(var.id.clone()),
                x => Object::Func(*self.globals.get(x).unwrap()),
            }
        }
    }

    fn eval_unary(&mut self, unary: &'b UnaryExpr, scope: &'b Scope) -> Object<'b> {
        let right = self.eval_expr(&unary.expr, scope);

        match unary.op {
            Operator::Not => Object::Bool(!right.unwrap_bool()),
            Operator::Minus => Object::Int(-right.unwrap_int()),
            _ => panic!("Not a unary operator!"),
        }
    }
}
