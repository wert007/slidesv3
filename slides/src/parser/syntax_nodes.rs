use crate::{
    lexer::syntax_token::{SyntaxToken, SyntaxTokenKind},
    text::TextSpan,
    value::Value,
};

#[derive(Debug, Clone)]
pub struct SyntaxNode<'a> {
    pub kind: SyntaxNodeKind<'a>,
    pub span: TextSpan,
    pub is_inserted: bool,
}

impl<'a> SyntaxNode<'a> {
    pub fn binary(
        lhs: SyntaxNode<'a>,
        operator_token: SyntaxToken<'a>,
        rhs: SyntaxNode<'a>,
    ) -> Self {
        let span = TextSpan::bounds(lhs.span, rhs.span);
        Self {
            kind: SyntaxNodeKind::Binary(BinaryNodeKind {
                lhs: Box::new(lhs),
                operator_token,
                rhs: Box::new(rhs),
            }),
            span,
            is_inserted: false,
        }
    }

    pub fn error(start: usize) -> Self {
        let mut result = Self::literal(SyntaxToken::number_literal_no_diagnostics(start, "0"));
        result.is_inserted = true;
        result
    }

    pub fn function_declaration(
        func_keyword: SyntaxToken<'a>,
        identifier: SyntaxToken<'a>,
        function_type: FunctionTypeNode<'a>,
        body: SyntaxNode<'a>,
    ) -> Self {
        let span = TextSpan::bounds(func_keyword.span(), body.span());
        Self {
            span,
            kind: SyntaxNodeKind::FunctionDeclaration(FunctionDeclarationNodeKind {
                identifier,
                function_type,
                body: Box::new(body),
            }),
            is_inserted: false,
        }
    }

    pub fn literal(token: SyntaxToken<'a>) -> Self {
        let value = match &token.kind {
            SyntaxTokenKind::NumberLiteral(n) => (n.value as i64).into(),
            SyntaxTokenKind::StringLiteral(s) => s.value.clone().into(),
            error => unreachable!("Unexpected Literal SyntaxToken {:#?}!", error),
        };
        Self {
            span: token.span(),
            kind: SyntaxNodeKind::Literal(LiteralNodeKind { token, value }),
            is_inserted: false,
        }
    }

    pub fn true_literal(token: SyntaxToken<'a>) -> Self {
        let value = true.into();
        Self {
            span: token.span(),
            kind: SyntaxNodeKind::Literal(LiteralNodeKind { token, value }),
            is_inserted: false,
        }
    }

    pub fn false_literal(token: SyntaxToken<'a>) -> Self {
        let value = false.into();
        Self {
            span: token.span(),
            kind: SyntaxNodeKind::Literal(LiteralNodeKind { token, value }),
            is_inserted: false,
        }
    }

    pub fn array_literal(
        lbracket: SyntaxToken<'a>,
        children: Vec<SyntaxNode<'a>>,
        comma_tokens: Vec<SyntaxToken<'a>>,
        rbracket: SyntaxToken<'a>,
    ) -> Self {
        let span = TextSpan::bounds(lbracket.span(), rbracket.span());
        Self {
            span,
            kind: SyntaxNodeKind::ArrayLiteral(ArrayLiteralNodeKind {
                lbracket,
                children,
                comma_tokens,
                rbracket,
            }),
            is_inserted: false,
        }
    }

    pub fn variable(token: SyntaxToken<'a>) -> Self {
        Self {
            span: token.span(),
            kind: SyntaxNodeKind::Variable(VariableNodeKind { token }),
            is_inserted: false,
        }
    }

    pub fn unary(operator_token: SyntaxToken<'a>, operand: SyntaxNode<'a>) -> Self {
        let span = TextSpan::bounds(operator_token.span(), operand.span());
        Self {
            kind: SyntaxNodeKind::Unary(UnaryNodeKind {
                operator_token,
                operand: Box::new(operand),
            }),
            span,
            is_inserted: false,
        }
    }

    pub fn parenthesized(
        lparen: SyntaxToken<'a>,
        expression: SyntaxNode<'a>,
        rparen: SyntaxToken<'a>,
    ) -> Self {
        let span = TextSpan::bounds(lparen.span(), rparen.span());
        Self {
            kind: SyntaxNodeKind::Parenthesized(ParenthesizedNodeKind {
                lparen,
                expression: Box::new(expression),
                rparen,
            }),
            span,
            is_inserted: false,
        }
    }

    pub fn function_call(
        base: SyntaxNode<'a>,
        open_parenthesis_token: SyntaxToken<'a>,
        arguments: Vec<SyntaxNode<'a>>,
        comma_tokens: Vec<SyntaxToken<'a>>,
        close_parenthesis_token: SyntaxToken<'a>,
    ) -> Self {
        let span = TextSpan::bounds(base.span(), close_parenthesis_token.span());
        Self {
            kind: SyntaxNodeKind::FunctionCall(FunctionCallNodeKind {
                base: Box::new(base),
                open_parenthesis_token,
                arguments,
                comma_tokens,
                close_parenthesis_token,
            }),
            span,
            is_inserted: false,
        }
    }

    pub fn array_index(
        base: SyntaxNode<'a>,
        lbracket: SyntaxToken<'a>,
        index: SyntaxNode<'a>,
        rbracket: SyntaxToken<'a>,
    ) -> Self {
        let span = TextSpan::bounds(base.span(), rbracket.span());
        Self {
            kind: SyntaxNodeKind::ArrayIndex(ArrayIndexNodeKind {
                base: Box::new(base),
                lbracket,
                index: Box::new(index),
                rbracket,
            }),
            span,
            is_inserted: false,
        }
    }

    pub fn field_access(
        base: SyntaxNode<'a>,
        period: SyntaxToken<'a>,
        field: SyntaxToken<'a>,
    ) -> Self {
        let span = TextSpan::bounds(base.span(), field.span());
        Self {
            kind: SyntaxNodeKind::FieldAccess(FieldAccessNodeKind {
                base: Box::new(base),
                period,
                field,
            }),
            span,
            is_inserted: false,
        }
    }

    pub fn for_statement(
        for_keyword: SyntaxToken<'a>,
        optional_index_variable: Option<SyntaxToken<'a>>,
        variable: SyntaxToken<'a>,
        in_keyword: SyntaxToken<'a>,
        collection: SyntaxNode<'a>,
        body: SyntaxNode<'a>,
    ) -> Self {
        let span = TextSpan::bounds(for_keyword.span(), body.span());
        Self {
            kind: SyntaxNodeKind::ForStatement(ForStatementNodeKind {
                for_keyword,
                optional_index_variable,
                variable,
                in_keyword,
                collection: Box::new(collection),
                body: Box::new(body),
            }),
            span,
            is_inserted: false,
        }
    }

    pub fn if_statement(
        if_keyword: SyntaxToken<'a>,
        condition: SyntaxNode<'a>,
        body: SyntaxNode<'a>,
        else_clause: Option<ElseClause<'a>>,
    ) -> Self {
        let span = TextSpan::bounds(if_keyword.span(), body.span());
        Self {
            kind: SyntaxNodeKind::IfStatement(IfStatementNodeKind {
                if_keyword,
                condition: Box::new(condition),
                body: Box::new(body),
                else_clause,
            }),
            span,
            is_inserted: false,
        }
    }

    pub fn variable_declaration(
        let_keyword: SyntaxToken<'a>,
        identifier: SyntaxToken<'a>,
        equals_token: SyntaxToken<'a>,
        initializer: SyntaxNode<'a>,
        semicolon_token: SyntaxToken<'a>,
    ) -> Self {
        let span = TextSpan::bounds(let_keyword.span(), semicolon_token.span());
        Self {
            kind: SyntaxNodeKind::VariableDeclaration(VariableDeclarationNodeKind {
                let_keyword,
                identifier,
                equals_token,
                initializer: Box::new(initializer),
                semicolon_token,
            }),
            span,
            is_inserted: false,
        }
    }

    pub fn return_statement(
        return_keyword: SyntaxToken<'a>,
        optional_expression: Option<SyntaxNode<'a>>,
        semicolon_token: SyntaxToken<'a>,
    ) -> Self {
        let span = TextSpan::bounds(return_keyword.span(), semicolon_token.span());
        Self {
            kind: SyntaxNodeKind::ReturnStatement(ReturnStatementNodeKind {
                return_keyword,
                optional_expression: optional_expression.map(|n| Box::new(n)),
                semicolon_token,
            }),
            span,
            is_inserted: false,
        }
    }

    pub fn while_statement(
        while_keyword: SyntaxToken<'a>,
        condition: SyntaxNode<'a>,
        body: SyntaxNode<'a>,
    ) -> Self {
        let span = TextSpan::bounds(while_keyword.span(), body.span());
        Self {
            kind: SyntaxNodeKind::WhileStatement(WhileStatementNodeKind {
                while_keyword,
                condition: Box::new(condition),
                body: Box::new(body),
            }),
            span,
            is_inserted: false,
        }
    }

    pub fn assignment(
        lhs: SyntaxNode<'a>,
        expression: SyntaxNode<'a>,
        semicolon_token: SyntaxToken<'a>,
    ) -> Self {
        let span = TextSpan::bounds(lhs.span(), semicolon_token.span());
        Self {
            kind: SyntaxNodeKind::Assignment(AssignmentNodeKind {
                lhs: Box::new(lhs),
                expression: Box::new(expression),
                semicolon_token,
            }),
            span,
            is_inserted: false,
        }
    }

    pub fn block_statement(
        lbrace: SyntaxToken<'a>,
        statements: Vec<SyntaxNode<'a>>,
        rbrace: SyntaxToken<'a>,
    ) -> Self {
        let span = TextSpan::bounds(lbrace.span(), rbrace.span());
        Self {
            kind: SyntaxNodeKind::BlockStatement(BlockStatementNodeKind {
                lbrace,
                statements,
                rbrace,
            }),
            span,
            is_inserted: false,
        }
    }

    pub fn expression_statement(
        expression: SyntaxNode<'a>,
        semicolon_token: SyntaxToken<'a>,
    ) -> Self {
        let span = TextSpan::bounds(expression.span(), semicolon_token.span());
        Self {
            kind: SyntaxNodeKind::ExpressionStatement(ExpressionStatementNodeKind {
                expression: Box::new(expression),
                semicolon_token,
            }),
            span,
            is_inserted: false,
        }
    }

    pub fn compilation_unit(statements: Vec<SyntaxNode<'a>>, eoi: SyntaxToken<'a>) -> Self {
        let span = TextSpan::bounds(statements.first().unwrap().span(), statements.last().unwrap().span());
        Self {
            kind: SyntaxNodeKind::CompilationUnit(CompilationUnitNodeKind {
                statements,
                eoi,
            }),
            span,
            is_inserted: false,
        }
    }

    pub fn span(&self) -> TextSpan {
        self.span
    }
}

#[derive(Debug, Clone)]
pub enum SyntaxNodeKind<'a> {
    // Top Level Statements
    CompilationUnit(CompilationUnitNodeKind<'a>),
    FunctionDeclaration(FunctionDeclarationNodeKind<'a>),

    //Expressions
    Literal(LiteralNodeKind<'a>),
    ArrayLiteral(ArrayLiteralNodeKind<'a>),
    Variable(VariableNodeKind<'a>),
    Binary(BinaryNodeKind<'a>),
    Unary(UnaryNodeKind<'a>),
    Parenthesized(ParenthesizedNodeKind<'a>),
    FunctionCall(FunctionCallNodeKind<'a>),
    ArrayIndex(ArrayIndexNodeKind<'a>),
    FieldAccess(FieldAccessNodeKind<'a>),

    // Statements
    BlockStatement(BlockStatementNodeKind<'a>),
    ForStatement(ForStatementNodeKind<'a>),
    IfStatement(IfStatementNodeKind<'a>),
    VariableDeclaration(VariableDeclarationNodeKind<'a>),
    ReturnStatement(ReturnStatementNodeKind<'a>),
    WhileStatement(WhileStatementNodeKind<'a>),
    Assignment(AssignmentNodeKind<'a>),
    ExpressionStatement(ExpressionStatementNodeKind<'a>),
}

impl SyntaxNodeKind<'_> {
    pub fn is_assignable(&self) -> bool {
        matches!(self, Self::Variable(_) | Self::ArrayIndex(_))
    }
}

#[derive(Debug, Clone)]
pub struct CompilationUnitNodeKind<'a> {
    pub statements: Vec<SyntaxNode<'a>>,
    pub eoi: SyntaxToken<'a>,
}


#[derive(Debug, Clone)]
pub struct FunctionDeclarationNodeKind<'a> {
    pub identifier: SyntaxToken<'a>,
    pub function_type: FunctionTypeNode<'a>,
    pub body: Box<SyntaxNode<'a>>,
}

#[derive(Debug, Clone)]
pub struct FunctionTypeNode<'a> {
    pub lparen: SyntaxToken<'a>,
    pub parameters: Vec<ParameterNode<'a>>,
    pub comma_tokens: Vec<SyntaxToken<'a>>,
    pub rparen: SyntaxToken<'a>,
}

impl<'a> FunctionTypeNode<'a> {
    pub fn new(lparen: SyntaxToken<'a>, parameters: Vec<ParameterNode<'a>>, comma_tokens: Vec<SyntaxToken<'a>>, rparen: SyntaxToken<'a>) -> Self {
        Self {
            lparen,
            parameters,
            comma_tokens,
            rparen,
        }
    }
}


#[derive(Debug, Clone)]
pub struct ParameterNode<'a> {
    pub identifier: SyntaxToken<'a>,
    pub colon_token: SyntaxToken<'a>,
    pub type_: TypeNode<'a>
}

impl<'a> ParameterNode<'a> {
    pub fn new(identifier: SyntaxToken<'a>, colon_token: SyntaxToken<'a>, type_: TypeNode<'a>) -> Self {
        Self {
            identifier,
            colon_token,
            type_,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeNode<'a> {
    pub identifier: SyntaxToken<'a>,
    pub brackets: Vec<SyntaxToken<'a>>,
}

impl<'a> TypeNode<'a> {
    pub fn new(identifier: SyntaxToken<'a>, brackets: Vec<SyntaxToken<'a>>) -> Self {
        Self {
            identifier,
            brackets,
        }
    }

    pub fn span(&self) -> TextSpan {
        TextSpan::bounds(self.identifier.span(), self.brackets.last().unwrap_or(&self.identifier).span())
    }
}    

#[derive(Debug, Clone)]
pub struct LiteralNodeKind<'a> {
    pub token: SyntaxToken<'a>,
    pub value: Value,
}

#[derive(Debug, Clone)]
pub struct ArrayLiteralNodeKind<'a> {
    pub lbracket: SyntaxToken<'a>,
    pub children: Vec<SyntaxNode<'a>>,
    pub comma_tokens: Vec<SyntaxToken<'a>>,
    pub rbracket: SyntaxToken<'a>,
}

#[derive(Debug, Clone)]
pub struct VariableNodeKind<'a> {
    pub token: SyntaxToken<'a>,
}

#[derive(Debug, Clone)]
pub struct BinaryNodeKind<'a> {
    pub lhs: Box<SyntaxNode<'a>>,
    pub operator_token: SyntaxToken<'a>,
    pub rhs: Box<SyntaxNode<'a>>,
}

#[derive(Debug, Clone)]
pub struct UnaryNodeKind<'a> {
    pub operator_token: SyntaxToken<'a>,
    pub operand: Box<SyntaxNode<'a>>,
}

#[derive(Debug, Clone)]
pub struct ParenthesizedNodeKind<'a> {
    pub lparen: SyntaxToken<'a>,
    pub expression: Box<SyntaxNode<'a>>,
    pub rparen: SyntaxToken<'a>,
}

#[derive(Debug, Clone)]
pub struct FunctionCallNodeKind<'a> {
    pub base: Box<SyntaxNode<'a>>,
    pub open_parenthesis_token: SyntaxToken<'a>,
    pub arguments: Vec<SyntaxNode<'a>>,
    pub comma_tokens: Vec<SyntaxToken<'a>>,
    pub close_parenthesis_token: SyntaxToken<'a>,
}

impl FunctionCallNodeKind<'_> {
    pub fn argument_span(&self) -> TextSpan {
        TextSpan::bounds(
            self.open_parenthesis_token.span(),
            self.close_parenthesis_token.span(),
        )
    }
}

#[derive(Debug, Clone)]
pub struct ArrayIndexNodeKind<'a> {
    pub base: Box<SyntaxNode<'a>>,
    pub lbracket: SyntaxToken<'a>,
    pub index: Box<SyntaxNode<'a>>,
    pub rbracket: SyntaxToken<'a>,
}

#[derive(Debug, Clone)]
pub struct FieldAccessNodeKind<'a> {
    pub base: Box<SyntaxNode<'a>>,
    pub period: SyntaxToken<'a>,
    pub field: SyntaxToken<'a>,
}

#[derive(Debug, Clone)]
pub struct BlockStatementNodeKind<'a> {
    pub lbrace: SyntaxToken<'a>,
    pub statements: Vec<SyntaxNode<'a>>,
    pub rbrace: SyntaxToken<'a>,
}

#[derive(Debug, Clone)]
pub struct ForStatementNodeKind<'a> {
    pub for_keyword: SyntaxToken<'a>,
    pub optional_index_variable: Option<SyntaxToken<'a>>,
    pub variable: SyntaxToken<'a>,
    pub in_keyword: SyntaxToken<'a>,
    pub collection: Box<SyntaxNode<'a>>,
    pub body: Box<SyntaxNode<'a>>,
}

#[derive(Debug, Clone)]
pub struct IfStatementNodeKind<'a> {
    pub if_keyword: SyntaxToken<'a>,
    pub condition: Box<SyntaxNode<'a>>,
    pub body: Box<SyntaxNode<'a>>,
    pub else_clause: Option<ElseClause<'a>>,
}

#[derive(Debug, Clone)]
pub struct ElseClause<'a> {
    pub else_keyword: SyntaxToken<'a>,
    pub body: Box<SyntaxNode<'a>>,
}

impl<'a> ElseClause<'a> {
    pub fn new(else_keyword: SyntaxToken<'a>, body: SyntaxNode<'a>) -> Self {
        Self {
            else_keyword,
            body: Box::new(body),
        }
    }
}

#[derive(Debug, Clone)]
pub struct VariableDeclarationNodeKind<'a> {
    pub let_keyword: SyntaxToken<'a>,
    pub identifier: SyntaxToken<'a>,
    pub equals_token: SyntaxToken<'a>,
    pub initializer: Box<SyntaxNode<'a>>,
    pub semicolon_token: SyntaxToken<'a>,
}

#[derive(Debug, Clone)]
pub struct ReturnStatementNodeKind<'a> {
    pub return_keyword: SyntaxToken<'a>,
    pub optional_expression: Option<Box<SyntaxNode<'a>>>,
    pub semicolon_token: SyntaxToken<'a>,
}

#[derive(Debug, Clone)]
pub struct WhileStatementNodeKind<'a> {
    pub while_keyword: SyntaxToken<'a>,
    pub condition: Box<SyntaxNode<'a>>,
    pub body: Box<SyntaxNode<'a>>,
}

#[derive(Debug, Clone)]
pub struct AssignmentNodeKind<'a> {
    pub lhs: Box<SyntaxNode<'a>>,
    pub expression: Box<SyntaxNode<'a>>,
    pub semicolon_token: SyntaxToken<'a>,
}

#[derive(Debug, Clone)]
pub struct ExpressionStatementNodeKind<'a> {
    pub expression: Box<SyntaxNode<'a>>,
    pub semicolon_token: SyntaxToken<'a>,
}
