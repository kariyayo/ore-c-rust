use std::collections::HashMap;
use std::sync::OnceLock;

use super::{Parser, Result, Error, TokenType};
use super::ast::{Expression};

// 式の優先順位。
// 値が大きいほど優先順位が高く、ASTの深いレベルに配置される。
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub(super) enum ExpressionPrecedence {
    Lowest,
    Assign, // =
    Equals, // ==
    LessGreater, // > または <
    Sum, // +
    Product, // *
    Prefix, // -X, !X, ++X, --X
    Postfix, // X++, X--
    Call, // myFunction(X)
    Index,
}

static PRECEDENCES: OnceLock<HashMap<TokenType, ExpressionPrecedence>> = OnceLock::new();

fn precedences() -> &'static HashMap<TokenType, ExpressionPrecedence> {
    PRECEDENCES.get_or_init(|| {
        let mut precedences: HashMap<TokenType, ExpressionPrecedence> = HashMap::with_capacity(21);
        precedences.insert(TokenType::Assign, ExpressionPrecedence::Assign);
        precedences.insert(TokenType::PlusAssign, ExpressionPrecedence::Assign);
        precedences.insert(TokenType::MinusAssign, ExpressionPrecedence::Assign);
        precedences.insert(TokenType::AsteriskAssign, ExpressionPrecedence::Assign);
        precedences.insert(TokenType::SlashAssign, ExpressionPrecedence::Assign);
        precedences.insert(TokenType::PercentAssign, ExpressionPrecedence::Assign);
        precedences.insert(TokenType::Eq, ExpressionPrecedence::Equals);
        precedences.insert(TokenType::NotEq, ExpressionPrecedence::Equals);
        precedences.insert(TokenType::Lt, ExpressionPrecedence::LessGreater);
        precedences.insert(TokenType::Gt, ExpressionPrecedence::LessGreater);
        precedences.insert(TokenType::Plus, ExpressionPrecedence::Sum);
        precedences.insert(TokenType::Minus, ExpressionPrecedence::Sum);
        precedences.insert(TokenType::Slash, ExpressionPrecedence::Product);
        precedences.insert(TokenType::Asterisk, ExpressionPrecedence::Product);
        precedences.insert(TokenType::Percent, ExpressionPrecedence::Product);
        precedences.insert(TokenType::Increment, ExpressionPrecedence::Postfix);
        precedences.insert(TokenType::Decrement, ExpressionPrecedence::Postfix);
        precedences.insert(TokenType::Lparem, ExpressionPrecedence::Call);
        precedences.insert(TokenType::Lbracket, ExpressionPrecedence::Index);
        precedences.insert(TokenType::Dot, ExpressionPrecedence::Call);
        precedences.insert(TokenType::Arrow, ExpressionPrecedence::Call);
        return precedences;
    })
}

fn is_infix_token_type(token_type: TokenType) -> bool {
    return match token_type {
        TokenType::Plus
        | TokenType::Minus
        | TokenType::Slash
        | TokenType::Asterisk
        | TokenType::Percent
        | TokenType::Eq
        | TokenType::NotEq
        | TokenType::Lt
        | TokenType::Gt
        | TokenType::Assign
        | TokenType::PlusAssign
        | TokenType::MinusAssign
        | TokenType::AsteriskAssign
        | TokenType::SlashAssign
        | TokenType::PercentAssign
        | TokenType::Lparem
        | TokenType::Lbracket
        | TokenType::Dot
        | TokenType::Arrow => {
            true
        }
        _ => {
            false
        }
    };
}

fn is_postfix_token_type(token_type: TokenType) -> bool {
    return match token_type {
        TokenType::Increment | TokenType::Decrement => {
            true
        }
        _ => false
    }
}

/// Pratt構文解析器
/// 
/// 式の解析において、構文解析関数を文法ルールに関連づけるのではなく、単一のトークンタイプに関連づける。
/// それぞれのトークンタイプに対して、中置演算子と前置演算子と、2つの構文解析関数を関連づける。
impl Parser {
    // 現在のトークンの次のトークン優先順位を返す
    fn peek_precedence(&self) -> ExpressionPrecedence {
        return *precedences().get(&self.peek_token.token_type).unwrap_or(&ExpressionPrecedence::Lowest);
    }

    // 現在のトークンの優先順位を返す
    fn cur_precedence(&self) -> ExpressionPrecedence {
        return *precedences().get(&self.cur_token.token_type).unwrap_or(&ExpressionPrecedence::Lowest);
    }

    // 式をパースする
    pub(super) fn parse_expression(&mut self, precedence: ExpressionPrecedence) -> Result<Expression> {
        // まず、前置演算子もしくはリテラルをパースする
        let prefix_result = self.prefix();
        match prefix_result {
            None => {
                return Err(Error { errors: vec![format!("[parse_expression] no prefix parse function for {:?}", self.cur_token.token_type)] });
            }
            Some(Err(e)) => {
                return Err(e);
            }
            Some(Ok(exp)) => {
                let mut result = exp;
                loop {
                    // 次のトークンがセミコロンの場合は文が終了するので、ここでparse結果を返す。
                    //
                    // もしくは、
                    //
                    // 現在の演算子の優先順位(precedence)が、次のトークンの優先順位 以上 である場合は、
                    // ここでparse結果を返す。
                    // 結果的に、優先順位が高いほどASTの深いレベルに配置されることになる。
                    //
                    // 例えば、`5 * 5 + 3` の場合、`5 * 5` が終わった時点で `*` の優先順位(`precedence`)が
                    // 次の演算子 `+` の優先順位(`self.peek_precedence`)より高いので `5 * 5` のparse結果を返す。
                    if self.peek_token.token_type == TokenType::Semicolon || precedence >= self.peek_precedence() {
                        return Ok(result);
                    }

                    if is_infix_token_type(self.peek_token.token_type) {
                        let left_exp = result;
                        self.next_token();
                        let infix_result = self.infix(left_exp);
                        result = infix_result?;
                    } else if is_postfix_token_type(self.peek_token.token_type) {
                        let left_exp = result;
                        self.next_token();
                        let postfix_result = self.parse_postfix_expression(left_exp);
                        result = postfix_result;
                    } else {
                        return Ok(result);
                    }
                }
            }
        }
    }

    fn prefix(&mut self) -> Option<Result<Expression>> {
        let current_token_type = self.cur_token.token_type;
        let result = match current_token_type {
            TokenType::Ident => {
                Some(self.parse_identifier())
            }
            TokenType::Integer => {
                Some(self.parse_integer_literal())
            }
            TokenType::Character => {
                Some(self.parse_character_literal())
            }
            TokenType::String => {
                Some(self.parse_string_literal())
            }
            TokenType::Bang
            | TokenType::Minus
            | TokenType::Increment
            | TokenType::Decrement
            | TokenType::Asterisk
            | TokenType::Ampersand => {
                Some(self.parse_prefix_expression())
            }
            TokenType::Lparem => {
                Some(self.parse_grouped_expression())
            }
            TokenType::Lbrace => {
                Some(self.parse_initializer_expression())
            }
            _ => {
                None
            }
        };
        return result;
    }

    fn infix(&mut self, left: Expression) -> Result<Expression> {
        return if self.cur_token.token_type == TokenType::Lparem {
            self.parse_function_call_expression(left)
        } else if self.cur_token.token_type == TokenType::Lbracket {
            self.parse_index_expression(left)
        } else {
            self.parse_infix_expression(left)
        };
    }

    pub(super) fn parse_identifier(&self) -> Result<Expression> {
        return Ok(Expression::Identifier { value: self.cur_token.literal.clone() });
    }

    pub(super) fn parse_integer_literal(&self) -> Result<Expression> {
        return self.cur_token.literal.parse()
            .map(|value| Expression::Int { value })
            .map_err(|_| Error { errors: vec!["[parse_integer_literal] parse int error".to_string()] });
    }

    pub(super) fn parse_character_literal(&self) -> Result<Expression> {
        return self.cur_token.literal.parse()
            .map(|value| Expression::CharacterLiteral { value })
            .map_err(|_| Error { errors: vec![format!("[parse_character_literal] parse character error. cur_token is {:?}", self.cur_token).to_string()] });
    }

    pub(super) fn parse_string_literal(&self) -> Result<Expression> {
        return self.cur_token.literal.parse()
            .map(|value| Expression::StringLiteral { value })
            .map_err(|_| Error { errors: vec!["[parse_string_literal] parse string error".to_string()] });
    }

    pub(super) fn parse_grouped_expression(&mut self) -> Result<Expression> {
        self.next_token();
        let exp = self.parse_expression(ExpressionPrecedence::Lowest);
        if self.peek_token.token_type != TokenType::Rparem {
            return Err(Error { errors: vec![format!("[parse_grouped_expression] expected next token to be Rparem, got {:?}", self.peek_token.token_type)] });
        }
        self.next_token();
        return exp;
    }

    pub(super) fn parse_initializer_expression(&mut self) -> Result<Expression> {
        self.next_token();
        let mut elements: Vec<Expression> = vec![];
        if self.cur_token.token_type == TokenType::Rbrace {
            return Ok(Expression::InitializerExpression { elements });
        }
        while self.cur_token.token_type != TokenType::Rbrace {
            let value = self.parse_expression(ExpressionPrecedence::Lowest)?;
            elements.push(value);
            self.next_token();
            if self.cur_token.token_type != TokenType::Comma {
                break;
            }
            self.next_token(); // `,` を読み飛ばす
        }
        if self.cur_token.token_type != TokenType::Rbrace {
            return Err(Error { errors: vec![format!("[parse_initializer_expression] expected next token to be Rbrace, got {:?}", self.cur_token.token_type)] });
        }
        return Ok(Expression::InitializerExpression { elements });
    }

    // !, -, ++, --, *, & の前置演算子をパースする
    pub(super) fn parse_prefix_expression(&mut self) -> Result<Expression> {
        let operator = self.cur_token.literal.clone();
        self.next_token();
        return self.parse_expression(ExpressionPrecedence::Prefix)
            .map(|right|
                Expression::PrefixExpression {
                    operator,
                    right: Box::new(right),
                }
            );
    }

    pub(super) fn parse_function_call_expression(&mut self, left: Expression) -> Result<Expression> {
        let function_name = match left {
            Expression::Identifier { value } => value,
            _ => {
                return Err(Error { errors: vec![format!("[parse_function_call_expression] expected `left` to be Expression::Identifier, got {:?}", left)] });
            }
        };
        let mut arguments: Vec<Expression> = vec![];
        self.next_token(); // `(` を読み飛ばす
        if self.cur_token.token_type != TokenType::Rparem {
            loop {
                let arg = self.parse_expression(ExpressionPrecedence::Lowest)?;
                arguments.push(arg);
                if self.peek_token.token_type != TokenType::Comma {
                    self.next_token(); // cur_token is `)`
                    break;
                }
                self.next_token(); // cur_token is `,`
                self.next_token(); // `,` を読み飛ばす
            }
        }
        if self.cur_token.token_type != TokenType::Rparem {
            return Err(Error { errors: vec![format!("[parse_function_call_expression] expected next token to be Rparem, got {:?}", self.cur_token.token_type)] });
        }
        return Ok(Expression::FunctionCallExpression { function_name, arguments });
    }

    pub(super) fn parse_index_expression(&mut self, left: Expression) -> Result<Expression> {
        self.next_token();
        let right = self.parse_expression(ExpressionPrecedence::Lowest)?;
        self.next_token();
        if self.cur_token.token_type != TokenType::Rbracket {
            return Err(Error { errors: vec![format!("[parse_index_expression] expected next token to be Rbracket, got {:?}", self.cur_token.token_type)] });
        }
        return Ok(Expression::IndexExpression {
            left: Box::new(left),
            index: Box::new(right),
        });
    }

    pub(super) fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression> {
        let operator = self.cur_token.literal.clone();
        let operator_precedence = self.cur_precedence();
        self.next_token();
        return self.parse_expression(operator_precedence)
            .map(|right|
                Expression::InfixExpression {
                    operator,
                    left: Box::new(left),
                    right: Box::new(right),
                }
            );
    }

    // ++, -- の後置演算子をパースする
    pub(super) fn parse_postfix_expression(&mut self, left: Expression) -> Expression {
        let operator = self.cur_token.literal.clone();
        return Expression::PostfixExpression {
            operator,
            left: Box::new(left),
        }
    }
}

#[cfg(test)]
mod tests {

}
