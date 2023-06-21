use either::Either;

use crate::{
    diagnostics::DiagnosticBag,
    lexer::syntax_token::{SyntaxToken, SyntaxTokenKind},
    text::{SourceTextCollection, TextLocation},
    value::Value,
};

#[derive(Debug, Clone)]
pub struct SyntaxNode {
    pub kind: SyntaxNodeKind,
    pub location: TextLocation,
    pub is_inserted: bool,
}

impl SyntaxNode {
    pub fn binary(lhs: SyntaxNode, operator_token: SyntaxToken, rhs: SyntaxNode) -> Self {
        let span = TextLocation::bounds(lhs.location, rhs.location);
        Self {
            kind: SyntaxNodeKind::Binary(BinaryNodeKind {
                lhs: Box::new(lhs),
                operator_token,
                rhs: Box::new(rhs),
            }),
            location: span,
            is_inserted: false,
        }
    }

    pub fn error(location: TextLocation) -> Self {
        let mut result =
            Self::none_literal(SyntaxToken::error(location, SyntaxTokenKind::NoneKeyword));
        result.is_inserted = true;
        result
    }

    pub fn import_statement(
        import_keyword: SyntaxToken,
        expression: SyntaxNode,
        as_keyword: SyntaxToken,
        identifier: SyntaxToken,
        semicolon_token: SyntaxToken,
    ) -> Self {
        let span = TextLocation::bounds(import_keyword.location, semicolon_token.location);
        Self {
            location: span,
            kind: SyntaxNodeKind::ImportStatement(ImportStatementNodeKind {
                import_keyword,
                function: Box::new(expression),
                as_keyword,
                identifier,
                semicolon_token,
            }),
            is_inserted: false,
        }
    }

    pub fn function_declaration(
        optional_generic_keyword: Option<SyntaxToken>,
        func_keyword: SyntaxToken,
        identifier: SyntaxToken,
        function_type: FunctionTypeNode,
        body: SyntaxNode,
    ) -> Self {
        let span = TextLocation::bounds(
            optional_generic_keyword
                .as_ref()
                .unwrap_or(&func_keyword)
                .location,
            body.location,
        );
        Self {
            location: span,
            kind: SyntaxNodeKind::FunctionDeclaration(Box::new(FunctionDeclarationNodeKind {
                optional_generic_keyword,
                func_keyword,
                identifier,
                function_type,
                body: Box::new(body),
            })),
            is_inserted: false,
        }
    }

    pub fn struct_declaration(
        optional_abstract_keyword: Option<SyntaxToken>,
        struct_keyword: SyntaxToken,
        identifier: SyntaxToken,
        generic_parameters: Vec<SyntaxToken>,
        optional_parent: Option<SyntaxToken>,
        body: StructBodyNode,
    ) -> Self {
        let span = TextLocation::bounds(
            optional_abstract_keyword
                .as_ref()
                .unwrap_or(&struct_keyword)
                .location,
            body.location,
        );
        Self {
            location: span,
            kind: SyntaxNodeKind::StructDeclaration(StructDeclarationNodeKind {
                optional_abstract_keyword,
                struct_keyword,
                identifier,
                generic_parameters,
                optional_parent,
                body: Box::new(body),
            }),
            is_inserted: false,
        }
    }

    pub fn enum_declaration(
        enum_keyword: SyntaxToken,
        identifier: SyntaxToken,
        body: EnumBodyNode,
    ) -> Self {
        let span = TextLocation::bounds(enum_keyword.location, body.location);
        Self {
            location: span,
            kind: SyntaxNodeKind::EnumDeclaration(EnumDeclarationNodeKind {
                enum_keyword,
                identifier,
                body: Box::new(body),
            }),
            is_inserted: false,
        }
    }

    pub fn struct_field(field: ParameterNode, semicolon_token: SyntaxToken) -> Self {
        let span = TextLocation::bounds(field.location, semicolon_token.location);
        Self {
            location: span,
            kind: SyntaxNodeKind::StructField(StructFieldNodeKind {
                field,
                semicolon_token,
            }),
            is_inserted: false,
        }
    }

    pub fn literal(
        token: SyntaxToken,
        source_text_collection: &SourceTextCollection,
        diagnostic_bag: &mut DiagnosticBag,
    ) -> Self {
        let lexeme = &source_text_collection[token.location];
        let value = match &token.kind {
            SyntaxTokenKind::NumberLiteral => {
                match lexeme.parse::<u64>() {
                    Ok(value) => (value as i64).into(),
                    // TODO: Someday ParseIntError::kind() might allow to differentiate more
                    Err(_) => {
                        diagnostic_bag.report_bad_integer(
                            token.location.span.start(),
                            token.location.source_text,
                            lexeme,
                        );
                        Value::None
                    }
                }
            }
            SyntaxTokenKind::StringLiteral => lexeme[1..lexeme.len() - 1].to_owned().into(),
            error => unreachable!("Unexpected Literal SyntaxToken {:#?}!", error),
        };
        Self {
            location: token.location,
            kind: SyntaxNodeKind::Literal(LiteralNodeKind { token, value }),
            is_inserted: false,
        }
    }

    pub fn true_literal(token: SyntaxToken) -> Self {
        let value = true.into();
        Self {
            location: token.location,
            kind: SyntaxNodeKind::Literal(LiteralNodeKind { token, value }),
            is_inserted: false,
        }
    }

    pub fn false_literal(token: SyntaxToken) -> Self {
        let value = false.into();
        Self {
            location: token.location,
            kind: SyntaxNodeKind::Literal(LiteralNodeKind { token, value }),
            is_inserted: false,
        }
    }

    pub fn none_literal(token: SyntaxToken) -> Self {
        let value = Value::None;
        Self {
            location: token.location,
            kind: SyntaxNodeKind::Literal(LiteralNodeKind { token, value }),
            is_inserted: false,
        }
    }

    pub fn array_literal(
        optional_array_list_keyword: Option<SyntaxToken>,
        lbracket: SyntaxToken,
        children: Vec<SyntaxNode>,
        comma_tokens: Vec<SyntaxToken>,
        rbracket: SyntaxToken,
    ) -> Self {
        let span = TextLocation::bounds(
            optional_array_list_keyword
                .map(|t| t.location)
                .unwrap_or(lbracket.location),
            rbracket.location,
        );
        Self {
            location: span,
            kind: SyntaxNodeKind::ArrayLiteral(ArrayLiteralNodeKind {
                optional_array_list_keyword,
                lbracket,
                children,
                comma_tokens,
                rbracket,
            }),
            is_inserted: false,
        }
    }

    pub fn dictionary_literal(
        dict_keyword: SyntaxToken,
        lbracket: SyntaxToken,
        values: Vec<(SyntaxNode, SyntaxNode)>,
        rbracket: SyntaxToken,
    ) -> Self {
        let location = TextLocation::bounds(dict_keyword.location, rbracket.location);
        Self {
            kind: SyntaxNodeKind::DictionaryLiteral(DictionaryLiteralNodeKind {
                dict_keyword,
                lbracket,
                values,
                rbracket,
            }),
            location,
            is_inserted: false,
        }
    }

    pub fn repetition_node(
        base_expression: SyntaxNode,
        semicolon_token: SyntaxToken,
        repetition: SyntaxNode,
    ) -> Self {
        let span = TextLocation::bounds(base_expression.location, repetition.location);
        Self {
            location: span,
            kind: SyntaxNodeKind::RepetitionNode(RepetitionNodeNodeKind {
                base_expression: Box::new(base_expression),
                semicolon_token,
                repetition: Box::new(repetition),
            }),
            is_inserted: false,
        }
    }

    pub fn cast_expression(
        cast_keyword: SyntaxToken,
        expression: SyntaxNode,
        colon_token: SyntaxToken,
        type_: TypeNode,
    ) -> Self {
        let span = TextLocation::bounds(cast_keyword.location, type_.location());
        Self {
            location: span,
            kind: SyntaxNodeKind::CastExpression(CastExpressionNodeKind {
                cast_keyword,
                expression: Box::new(expression),
                colon_token,
                type_,
            }),
            is_inserted: false,
        }
    }

    pub fn constructor_call(
        new_keyword: SyntaxToken,
        library_name: Option<SyntaxToken>,
        type_name: SyntaxToken,
        open_parenthesis_token: SyntaxToken,
        arguments: Vec<SyntaxNode>,
        comma_tokens: Vec<SyntaxToken>,
        close_parenthesis_token: SyntaxToken,
    ) -> Self {
        let span = TextLocation::bounds(new_keyword.location, close_parenthesis_token.location);
        Self {
            location: span,
            kind: SyntaxNodeKind::ConstructorCall(ConstructorCallNodeKind {
                new_keyword,
                library_name,
                type_name,
                open_parenthesis_token,
                arguments,
                comma_tokens,
                close_parenthesis_token,
            }),
            is_inserted: false,
        }
    }

    pub fn variable(token: SyntaxToken) -> Self {
        Self {
            location: token.location,
            kind: SyntaxNodeKind::Variable(VariableNodeKind { token }),
            is_inserted: false,
        }
    }

    pub fn unary(operator_token: SyntaxToken, operand: SyntaxNode) -> Self {
        let span = TextLocation::bounds(operator_token.location, operand.location);
        Self {
            kind: SyntaxNodeKind::Unary(UnaryNodeKind {
                operator_token,
                operand: Box::new(operand),
            }),
            location: span,
            is_inserted: false,
        }
    }

    pub fn parenthesized(lparen: SyntaxToken, expression: SyntaxNode, rparen: SyntaxToken) -> Self {
        let span = TextLocation::bounds(lparen.location, rparen.location);
        Self {
            kind: SyntaxNodeKind::Parenthesized(ParenthesizedNodeKind {
                lparen,
                expression: Box::new(expression),
                rparen,
            }),
            location: span,
            is_inserted: false,
        }
    }

    pub fn function_call(
        base: SyntaxNode,
        open_parenthesis_token: SyntaxToken,
        arguments: Vec<SyntaxNode>,
        comma_tokens: Vec<SyntaxToken>,
        close_parenthesis_token: SyntaxToken,
    ) -> Self {
        let span = TextLocation::bounds(base.location, close_parenthesis_token.location);
        Self {
            kind: SyntaxNodeKind::FunctionCall(FunctionCallNodeKind {
                base: Box::new(base),
                open_parenthesis_token,
                arguments,
                comma_tokens,
                close_parenthesis_token,
            }),
            location: span,
            is_inserted: false,
        }
    }

    pub fn array_index(
        base: SyntaxNode,
        lbracket: SyntaxToken,
        index: SyntaxNode,
        rbracket: SyntaxToken,
    ) -> Self {
        let span = TextLocation::bounds(base.location, rbracket.location);
        Self {
            kind: SyntaxNodeKind::ArrayIndex(ArrayIndexNodeKind {
                base: Box::new(base),
                lbracket,
                index: Box::new(index),
                rbracket,
            }),
            location: span,
            is_inserted: false,
        }
    }

    pub fn field_access(base: SyntaxNode, period: SyntaxToken, field: SyntaxToken) -> Self {
        let span = TextLocation::bounds(base.location, field.location);
        Self {
            kind: SyntaxNodeKind::FieldAccess(FieldAccessNodeKind {
                base: Box::new(base),
                period,
                field,
            }),
            location: span,
            is_inserted: false,
        }
    }

    pub fn for_statement(
        for_keyword: SyntaxToken,
        optional_index_variable: Option<SyntaxToken>,
        variable: SyntaxToken,
        in_keyword: SyntaxToken,
        collection: SyntaxNode,
        body: SyntaxNode,
    ) -> Self {
        let span = TextLocation::bounds(for_keyword.location, body.location);
        Self {
            kind: SyntaxNodeKind::ForStatement(ForStatementNodeKind {
                for_keyword,
                optional_index_variable,
                variable,
                in_keyword,
                collection: Box::new(collection),
                body: Box::new(body),
            }),
            location: span,
            is_inserted: false,
        }
    }

    pub fn if_statement(
        if_keyword: SyntaxToken,
        condition: SyntaxNode,
        body: SyntaxNode,
        else_clause: Option<ElseClause>,
    ) -> Self {
        let span = TextLocation::bounds(if_keyword.location, body.location);
        Self {
            kind: SyntaxNodeKind::IfStatement(IfStatementNodeKind {
                if_keyword,
                condition: Box::new(condition),
                body: Box::new(body),
                else_clause,
            }),
            location: span,
            is_inserted: false,
        }
    }

    pub fn variable_declaration(
        let_keyword: SyntaxToken,
        identifier: SyntaxToken,
        optional_type_declaration: Option<TypeDeclaration>,
        equals_token: SyntaxToken,
        initializer: SyntaxNode,
        semicolon_token: SyntaxToken,
    ) -> Self {
        let span = TextLocation::bounds(let_keyword.location, semicolon_token.location);
        Self {
            kind: SyntaxNodeKind::VariableDeclaration(VariableDeclarationNodeKind {
                let_keyword,
                identifier,
                optional_type_declaration,
                equals_token,
                initializer: Box::new(initializer),
                semicolon_token,
            }),
            location: span,
            is_inserted: false,
        }
    }

    pub fn return_statement(
        return_keyword: SyntaxToken,
        optional_expression: Option<SyntaxNode>,
        semicolon_token: SyntaxToken,
    ) -> Self {
        let span = TextLocation::bounds(return_keyword.location, semicolon_token.location);
        Self {
            kind: SyntaxNodeKind::ReturnStatement(ReturnStatementNodeKind {
                return_keyword,
                optional_expression: optional_expression.map(Box::new),
                semicolon_token,
            }),
            location: span,
            is_inserted: false,
        }
    }

    pub fn while_statement(
        while_keyword: SyntaxToken,
        condition: SyntaxNode,
        body: SyntaxNode,
    ) -> Self {
        let span = TextLocation::bounds(while_keyword.location, body.location);
        Self {
            kind: SyntaxNodeKind::WhileStatement(WhileStatementNodeKind {
                while_keyword,
                condition: Box::new(condition),
                body: Box::new(body),
            }),
            location: span,
            is_inserted: false,
        }
    }

    pub fn match_statement(
        match_keyword: SyntaxToken,
        expression: SyntaxNode,
        open_brace: SyntaxToken,
        match_cases: Vec<MatchCaseNode>,
        close_brace: SyntaxToken,
    ) -> SyntaxNode {
        let location = TextLocation::bounds(match_keyword.location, close_brace.location);
        Self {
            kind: SyntaxNodeKind::MatchStatement(MatchStatementNodeKind {
                match_keyword,
                expression: Box::new(expression),
                open_brace,
                match_cases,
                close_brace,
            }),
            location,
            is_inserted: false,
        }
    }

    pub fn assignment(
        lhs: SyntaxNode,
        expression: SyntaxNode,
        semicolon_token: SyntaxToken,
    ) -> Self {
        let span = TextLocation::bounds(lhs.location, semicolon_token.location);
        Self {
            kind: SyntaxNodeKind::Assignment(AssignmentNodeKind {
                lhs: Box::new(lhs),
                expression: Box::new(expression),
                semicolon_token,
            }),
            location: span,
            is_inserted: false,
        }
    }

    pub fn block_statement(
        lbrace: SyntaxToken,
        statements: Vec<SyntaxNode>,
        rbrace: SyntaxToken,
    ) -> Self {
        let span = TextLocation::bounds(lbrace.location, rbrace.location);
        Self {
            kind: SyntaxNodeKind::BlockStatement(BlockStatementNodeKind {
                lbrace,
                statements,
                rbrace,
            }),
            location: span,
            is_inserted: false,
        }
    }

    pub fn expression_statement(expression: SyntaxNode, semicolon_token: SyntaxToken) -> Self {
        let span = TextLocation::bounds(expression.location, semicolon_token.location);
        Self {
            kind: SyntaxNodeKind::ExpressionStatement(ExpressionStatementNodeKind {
                expression: Box::new(expression),
                semicolon_token,
            }),
            location: span,
            is_inserted: false,
        }
    }

    pub fn compilation_unit(statements: Vec<SyntaxNode>, eoi: SyntaxToken) -> Self {
        let span = TextLocation::bounds(
            statements
                .first()
                .map(|s| s.location)
                .unwrap_or_else(|| TextLocation::zero_in_file(eoi.location.source_text)),
            statements
                .last()
                .map(|s| s.location)
                .unwrap_or_else(|| TextLocation::zero_in_file(eoi.location.source_text)),
        );
        Self {
            kind: SyntaxNodeKind::CompilationUnit(CompilationUnitNodeKind { statements, eoi }),
            location: span,
            is_inserted: false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum SyntaxNodeKind {
    // Top Level Statements
    CompilationUnit(CompilationUnitNodeKind),
    _ConstDeclaration(ConstDeclarationNodeKind),
    ImportStatement(ImportStatementNodeKind),
    FunctionDeclaration(Box<FunctionDeclarationNodeKind>),
    StructDeclaration(StructDeclarationNodeKind),
    EnumDeclaration(EnumDeclarationNodeKind),

    // Expressions
    Literal(LiteralNodeKind),
    ArrayLiteral(ArrayLiteralNodeKind),
    DictionaryLiteral(DictionaryLiteralNodeKind),
    RepetitionNode(RepetitionNodeNodeKind),
    CastExpression(CastExpressionNodeKind),
    ConstructorCall(ConstructorCallNodeKind),
    Variable(VariableNodeKind),
    Binary(BinaryNodeKind),
    Unary(UnaryNodeKind),
    Parenthesized(ParenthesizedNodeKind),
    FunctionCall(FunctionCallNodeKind),
    ArrayIndex(ArrayIndexNodeKind),
    FieldAccess(FieldAccessNodeKind),

    // Statements
    BlockStatement(BlockStatementNodeKind),
    ForStatement(ForStatementNodeKind),
    IfStatement(IfStatementNodeKind),
    VariableDeclaration(VariableDeclarationNodeKind),
    ReturnStatement(ReturnStatementNodeKind),
    WhileStatement(WhileStatementNodeKind),
    MatchStatement(MatchStatementNodeKind),
    Assignment(AssignmentNodeKind),
    ExpressionStatement(ExpressionStatementNodeKind),

    // Trivia
    StructField(StructFieldNodeKind),
}

impl SyntaxNodeKind {
    pub fn is_assignable(&self) -> bool {
        matches!(
            self,
            Self::Variable(_) | Self::ArrayIndex(_) | Self::FieldAccess(_)
        )
    }
}

#[derive(Debug, Clone)]
pub struct CompilationUnitNodeKind {
    pub statements: Vec<SyntaxNode>,
    pub eoi: SyntaxToken,
}

#[derive(Debug, Clone)]
pub struct ConstDeclarationNodeKind {
    pub const_keyword: SyntaxToken,
    pub identifier: SyntaxToken,
    pub equals_token: SyntaxToken,
    pub initializer: Box<SyntaxNode>,
    pub semicolon_token: SyntaxToken,
}

#[derive(Debug, Clone)]
pub struct ImportStatementNodeKind {
    pub import_keyword: SyntaxToken,
    pub function: Box<SyntaxNode>,
    pub as_keyword: SyntaxToken,
    pub identifier: SyntaxToken,
    pub semicolon_token: SyntaxToken,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclarationNodeKind {
    pub optional_generic_keyword: Option<SyntaxToken>,
    pub func_keyword: SyntaxToken,
    pub identifier: SyntaxToken,
    pub function_type: FunctionTypeNode,
    pub body: Box<SyntaxNode>,
}

#[derive(Debug, Clone)]
pub struct FunctionTypeNode {
    // NOTE: Normally the type of a function comes after the identifier, but
    // generic comes before the func keyword. So this is just a flag and not the
    // token.
    pub is_generic: bool,
    pub lparen: SyntaxToken,
    pub parameters: Vec<ParameterNode>,
    pub comma_tokens: Vec<SyntaxToken>,
    pub rparen: SyntaxToken,
    pub return_type: Option<ReturnTypeNode>,
}

impl FunctionTypeNode {
    pub fn new(
        is_generic: bool,
        lparen: SyntaxToken,
        parameters: Vec<ParameterNode>,
        comma_tokens: Vec<SyntaxToken>,
        rparen: SyntaxToken,
        return_type: Option<ReturnTypeNode>,
    ) -> Self {
        Self {
            is_generic,
            lparen,
            parameters,
            comma_tokens,
            rparen,
            return_type,
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructDeclarationNodeKind {
    pub optional_abstract_keyword: Option<SyntaxToken>,
    pub struct_keyword: SyntaxToken,
    pub identifier: SyntaxToken,
    pub generic_parameters: Vec<SyntaxToken>,
    pub optional_parent: Option<SyntaxToken>,
    pub body: Box<StructBodyNode>,
}

#[derive(Debug, Clone)]
pub struct StructBodyNode {
    pub is_generic: bool,
    pub lbrace: SyntaxToken,
    pub statements: Vec<SyntaxNode>,
    pub rbrace: SyntaxToken,
    pub location: TextLocation,
}

impl StructBodyNode {
    pub fn new(
        is_generic: bool,
        lbrace: SyntaxToken,
        statements: Vec<SyntaxNode>,
        rbrace: SyntaxToken,
    ) -> Self {
        let span = TextLocation::bounds(lbrace.location, rbrace.location);
        Self {
            is_generic,
            lbrace,
            statements,
            rbrace,
            location: span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct EnumDeclarationNodeKind {
    pub enum_keyword: SyntaxToken,
    pub identifier: SyntaxToken,
    pub body: Box<EnumBodyNode>,
}

#[derive(Debug, Clone)]
pub struct EnumBodyNode {
    pub lbrace: SyntaxToken,
    pub values: Vec<EnumValueNode>,
    pub rbrace: SyntaxToken,
    pub location: TextLocation,
}

impl EnumBodyNode {
    pub fn new(lbrace: SyntaxToken, values: Vec<EnumValueNode>, rbrace: SyntaxToken) -> Self {
        let span = TextLocation::bounds(lbrace.location, rbrace.location);
        Self {
            lbrace,
            values,
            rbrace,
            location: span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct EnumValueNode {
    pub identifier: SyntaxToken,
    // pub optional_value: Option<SyntaxToken>,
    pub location: TextLocation,
}

impl EnumValueNode {
    pub fn new(identifier: SyntaxToken) -> Self {
        let location = identifier.location;
        Self {
            identifier,
            location,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParameterNode {
    pub identifier: SyntaxToken,
    pub type_declaration: TypeDeclaration,
    pub location: TextLocation,
}

impl ParameterNode {
    pub fn new(identifier: SyntaxToken, colon_token: SyntaxToken, type_: TypeNode) -> Self {
        let span = TextLocation::bounds(identifier.location, type_.location());
        let type_declaration = TypeDeclaration { colon_token, type_ };
        Self {
            identifier,
            type_declaration,
            location: span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ReturnTypeNode {
    pub arrow_token: SyntaxToken,
    pub return_type: TypeNode,
}

#[derive(Debug, Clone)]
pub struct TypeDeclaration {
    pub colon_token: SyntaxToken,
    pub type_: TypeNode,
}

#[derive(Debug, Clone)]
pub struct TypeNode {
    pub optional_ampersand_token: Option<SyntaxToken>,
    pub library_name: Option<SyntaxToken>,
    pub type_name: SyntaxToken,
    pub generic_type_qualifier: Vec<TypeNode>,
    pub optional_question_mark: Option<SyntaxToken>,
    pub brackets: Vec<SyntaxToken>,
}

impl TypeNode {
    pub fn new(
        optional_ampersand_token: Option<SyntaxToken>,
        library_name: Option<SyntaxToken>,
        type_name: SyntaxToken,
        generic_type_qualifier: Vec<TypeNode>,
        optional_question_mark: Option<SyntaxToken>,
        brackets: Vec<SyntaxToken>,
    ) -> Self {
        Self {
            optional_ampersand_token,
            library_name,
            type_name,
            generic_type_qualifier,
            optional_question_mark,
            brackets,
        }
    }

    pub fn location(&self) -> TextLocation {
        TextLocation::bounds(
            self.optional_ampersand_token
                .as_ref()
                .map(|t| t.location)
                .unwrap_or_else(|| self.type_name.location),
            self.brackets.last().unwrap_or(&self.type_name).location,
        )
    }

    pub fn full_type_name(&self, source_text_collection: &SourceTextCollection) -> String {
        match &self.library_name {
            Some(it) => format!(
                "{}.{}",
                &source_text_collection[it.location],
                &source_text_collection[self.type_name.location]
            ),
            None => source_text_collection[self.type_name.location].to_owned(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LiteralNodeKind {
    pub token: SyntaxToken,
    pub value: Value,
}

#[derive(Debug, Clone)]
pub struct ArrayLiteralNodeKind {
    pub optional_array_list_keyword: Option<SyntaxToken>,
    pub lbracket: SyntaxToken,
    pub children: Vec<SyntaxNode>,
    pub comma_tokens: Vec<SyntaxToken>,
    pub rbracket: SyntaxToken,
}


#[derive(Debug, Clone)]
pub struct DictionaryLiteralNodeKind {
    pub dict_keyword: SyntaxToken,
    pub lbracket: SyntaxToken,
    pub values: Vec<(SyntaxNode, SyntaxNode)>,
    pub rbracket: SyntaxToken,
}

#[derive(Debug, Clone)]
pub struct RepetitionNodeNodeKind {
    pub base_expression: Box<SyntaxNode>,
    pub semicolon_token: SyntaxToken,
    pub repetition: Box<SyntaxNode>,
}

#[derive(Debug, Clone)]
pub struct CastExpressionNodeKind {
    pub cast_keyword: SyntaxToken,
    pub expression: Box<SyntaxNode>,
    pub colon_token: SyntaxToken,
    pub type_: TypeNode,
}

#[derive(Debug, Clone)]
pub struct ConstructorCallNodeKind {
    pub new_keyword: SyntaxToken,
    pub library_name: Option<SyntaxToken>,
    pub type_name: SyntaxToken,
    pub open_parenthesis_token: SyntaxToken,
    pub arguments: Vec<SyntaxNode>,
    pub comma_tokens: Vec<SyntaxToken>,
    pub close_parenthesis_token: SyntaxToken,
}

#[derive(Debug, Clone)]
pub struct VariableNodeKind {
    pub token: SyntaxToken,
}

#[derive(Debug, Clone)]
pub struct BinaryNodeKind {
    pub lhs: Box<SyntaxNode>,
    pub operator_token: SyntaxToken,
    pub rhs: Box<SyntaxNode>,
}

#[derive(Debug, Clone)]
pub struct UnaryNodeKind {
    pub operator_token: SyntaxToken,
    pub operand: Box<SyntaxNode>,
}

#[derive(Debug, Clone)]
pub struct ParenthesizedNodeKind {
    pub lparen: SyntaxToken,
    pub expression: Box<SyntaxNode>,
    pub rparen: SyntaxToken,
}

#[derive(Debug, Clone)]
pub struct FunctionCallNodeKind {
    pub base: Box<SyntaxNode>,
    pub open_parenthesis_token: SyntaxToken,
    pub arguments: Vec<SyntaxNode>,
    pub comma_tokens: Vec<SyntaxToken>,
    pub close_parenthesis_token: SyntaxToken,
}

impl FunctionCallNodeKind {
    pub fn argument_span(&self) -> TextLocation {
        TextLocation::bounds(
            self.open_parenthesis_token.location,
            self.close_parenthesis_token.location,
        )
    }
}

#[derive(Debug, Clone)]
pub struct ArrayIndexNodeKind {
    pub base: Box<SyntaxNode>,
    pub lbracket: SyntaxToken,
    pub index: Box<SyntaxNode>,
    pub rbracket: SyntaxToken,
}

#[derive(Debug, Clone)]
pub struct FieldAccessNodeKind {
    pub base: Box<SyntaxNode>,
    pub period: SyntaxToken,
    pub field: SyntaxToken,
}

#[derive(Debug, Clone)]
pub struct BlockStatementNodeKind {
    pub lbrace: SyntaxToken,
    pub statements: Vec<SyntaxNode>,
    pub rbrace: SyntaxToken,
}

#[derive(Debug, Clone)]
pub struct ForStatementNodeKind {
    pub for_keyword: SyntaxToken,
    pub optional_index_variable: Option<SyntaxToken>,
    pub variable: SyntaxToken,
    pub in_keyword: SyntaxToken,
    pub collection: Box<SyntaxNode>,
    pub body: Box<SyntaxNode>,
}

#[derive(Debug, Clone)]
pub struct IfStatementNodeKind {
    pub if_keyword: SyntaxToken,
    pub condition: Box<SyntaxNode>,
    pub body: Box<SyntaxNode>,
    pub else_clause: Option<ElseClause>,
}

#[derive(Debug, Clone)]
pub struct ElseClause {
    pub else_keyword: SyntaxToken,
    pub body: Box<SyntaxNode>,
}

impl ElseClause {
    pub fn new(else_keyword: SyntaxToken, body: SyntaxNode) -> Self {
        Self {
            else_keyword,
            body: Box::new(body),
        }
    }
}

#[derive(Debug, Clone)]
pub struct VariableDeclarationNodeKind {
    pub let_keyword: SyntaxToken,
    pub identifier: SyntaxToken,
    pub optional_type_declaration: Option<TypeDeclaration>,
    pub equals_token: SyntaxToken,
    pub initializer: Box<SyntaxNode>,
    pub semicolon_token: SyntaxToken,
}

#[derive(Debug, Clone)]
pub struct ReturnStatementNodeKind {
    pub return_keyword: SyntaxToken,
    pub optional_expression: Option<Box<SyntaxNode>>,
    pub semicolon_token: SyntaxToken,
}

#[derive(Debug, Clone)]
pub struct WhileStatementNodeKind {
    pub while_keyword: SyntaxToken,
    pub condition: Box<SyntaxNode>,
    pub body: Box<SyntaxNode>,
}

#[derive(Debug, Clone)]
pub struct MatchStatementNodeKind {
    pub match_keyword: SyntaxToken,
    pub expression: Box<SyntaxNode>,
    pub open_brace: SyntaxToken,
    pub match_cases: Vec<MatchCaseNode>,
    pub close_brace: SyntaxToken,
}

#[derive(Debug, Clone)]
pub struct MatchCaseNode {
    pub expression: Either<SyntaxToken, SyntaxNode>,
    pub fat_arrow: SyntaxToken,
    pub body: SyntaxNode,
}

impl MatchCaseNode {
    pub(crate) fn new(
        expression: Either<SyntaxToken, SyntaxNode>,
        fat_arrow: SyntaxToken,
        body: SyntaxNode,
    ) -> MatchCaseNode {
        Self {
            expression,
            fat_arrow,
            body,
        }
    }

    // pub fn location(&self) -> TextLocation {
    //     TextLocation::bounds(for_both!(&self.expression, e => e.location), self.body.location)
    // }
}

#[derive(Debug, Clone)]
pub struct AssignmentNodeKind {
    pub lhs: Box<SyntaxNode>,
    pub expression: Box<SyntaxNode>,
    pub semicolon_token: SyntaxToken,
}

#[derive(Debug, Clone)]
pub struct ExpressionStatementNodeKind {
    pub expression: Box<SyntaxNode>,
    pub semicolon_token: SyntaxToken,
}

#[derive(Debug, Clone)]
pub struct StructFieldNodeKind {
    pub field: ParameterNode,
    pub semicolon_token: SyntaxToken,
}
