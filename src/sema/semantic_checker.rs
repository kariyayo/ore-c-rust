use std::{collections::HashMap, fmt};

use crate::parser::ast::{
    Expression, ExternalItem, ExternalItemNode, FunctionDecl, Loc, Program, Statement,
    StatementNode, SwitchLabel,
};

#[derive(Debug)]
pub struct SemanticError {
    errors: Vec<String>,
}

impl SemanticError {
    fn new(loc: &Loc, msg: String) -> SemanticError {
        SemanticError {
            errors: vec![build_error_msg(loc, msg)],
        }
    }
}

fn build_error_msg(loc: &Loc, msg: String) -> String {
    format!("error:{}:{}: {}", loc.row, loc.col, msg)
}

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for error in &self.errors {
            writeln!(f, "{}", error)?;
        }
        Ok(())
    }
}

impl core::error::Error for SemanticError {}

pub type Result<T> = std::result::Result<T, SemanticError>;

pub fn check_semantic(ast: &Program) -> Result<()> {
    let results: Vec<SemanticError> = ast
        .external_item_nodes
        .iter()
        .filter_map(|item_node| {
            if let ExternalItem::FunctionDeclNode(FunctionDecl {
                return_type_ref: _,
                name: _,
                parameters: _,
                body,
            }) = &item_node.0
            {
                Some(check_function(Context::ExternalItem(item_node), body))
            } else {
                None
            }
        })
        .flatten()
        .collect();
    if results.is_empty() {
        return Ok(());
    }
    Err(SemanticError {
        errors: results.iter().flat_map(|e| e.errors.clone()).collect(),
    })
}

fn check_function(ctx: Context, body: &Option<Box<StatementNode>>) -> Vec<SemanticError> {
    let mut results: Vec<SemanticError> = vec![];

    // check function body
    if let Some(stmt) = body {
        let mut es: Vec<SemanticError> = check_statement(&ctx, stmt)
            .into_iter()
            .filter_map(|a| a.err())
            .collect();
        results.append(&mut es);
    }
    results
}

enum Context<'a> {
    ExternalItem(&'a ExternalItemNode),
    Statement(&'a StatementNode),
}

impl Context<'_> {
    fn loc(&self) -> &Loc {
        match self {
            Context::ExternalItem(item_node) => &item_node.1,
            Context::Statement(stmt_node) => &stmt_node.1,
        }
    }
}

fn check_statement(ctx: &Context, stmt_node: &StatementNode) -> Vec<Result<()>> {
    let (stmt, _) = stmt_node;

    match stmt {
        Statement::Return(..) => {
            vec![Ok(())]
        }
        Statement::Break => {
            if let &Context::Statement((
                Statement::Switch { .. }
                | Statement::While { .. }
                | Statement::DoWhile { .. }
                | Statement::For { .. },
                _,
            )) = ctx
            {
                vec![Ok(())]
            } else {
                vec![Err(SemanticError::new(
                    ctx.loc(),
                    "invalid `break` in here".to_string(),
                ))]
            }
        }
        Statement::Continue => {
            let mut is_ok = true;
            if let &Context::Statement((stmt, ..)) = ctx {
                match stmt {
                    Statement::While { .. } | Statement::DoWhile { .. } | Statement::For { .. } => {
                        is_ok = true;
                    }
                    Statement::Switch { .. }
                    | Statement::Return(_)
                    | Statement::Break
                    | Statement::Continue
                    | Statement::TypeRef(..)
                    | Statement::VarDecl(_)
                    | Statement::Block(_)
                    | Statement::If { .. }
                    | Statement::ExpressionStatement(_) => is_ok = false,
                }
            }
            if is_ok {
                vec![Ok(())]
            } else {
                vec![Err(SemanticError::new(
                    ctx.loc(),
                    "invalid `continue` in here".to_string(),
                ))]
            }
        }
        Statement::TypeRef(..) => {
            vec![Ok(())]
        }
        Statement::VarDecl(..) => {
            vec![Ok(())]
        }
        Statement::Block(statements) => statements
            .iter()
            .flat_map(|stmt| check_statement(ctx, stmt))
            .collect(),
        Statement::If {
            condition: _,
            consequence,
            alternative,
        } => {
            let mut results: Vec<Result<()>> = vec![];
            let mut consequence_result = check_statement(ctx, consequence);
            let mut alternative_result = alternative
                .as_ref()
                .map(|stmt| check_statement(ctx, stmt))
                .unwrap_or(vec![]);
            results.append(&mut consequence_result);
            results.append(&mut alternative_result);
            results
        }
        Statement::Switch {
            condition,
            switch_block,
        } => {
            let switch_ctx = Context::Statement(stmt_node);
            let mut results: Vec<Result<()>> = vec![];

            // ラベルの重複チェック
            let mut labels_cnt: HashMap<&Expression, i32> = HashMap::new();
            let mut default_label_cnt = 0;
            for label_entry in &switch_block.label_entries {
                for label in &label_entry.labels {
                    match label {
                        SwitchLabel::Case((exp, ..)) => {
                            *labels_cnt.entry(exp).or_default() += 1;
                        }
                        SwitchLabel::Default => default_label_cnt += 1,
                    }
                }
            }
            if labels_cnt.into_values().any(|x| x > 1) {
                return vec![Err(SemanticError::new(
                    &condition.1,
                    "switch label is duplicated".to_string(),
                ))];
            }
            if default_label_cnt > 1 {
                return vec![Err(SemanticError::new(
                    &condition.1,
                    "switch default label is duplicated".to_string(),
                ))];
            }

            for stmt in &switch_block.body {
                results.append(&mut check_statement(&switch_ctx, stmt));
            }
            results
        }
        Statement::While { condition: _, body } => {
            let while_ctx = Context::Statement(stmt_node);
            let mut results: Vec<Result<()>> = vec![];
            results.append(&mut check_statement(&while_ctx, body));
            results
        }
        Statement::DoWhile { body, condition: _ } => {
            let dowhile_ctx = Context::Statement(stmt_node);
            let mut results: Vec<Result<()>> = vec![];
            results.append(&mut check_statement(&dowhile_ctx, body));
            results
        }
        Statement::For {
            init: _,
            condition: _,
            post: _,
            body,
        } => {
            let for_ctx = Context::Statement(stmt_node);
            let mut results: Vec<Result<()>> = vec![];
            results.append(&mut check_statement(&for_ctx, body));
            results
        }
        Statement::ExpressionStatement(..) => {
            vec![Ok(())]
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{lexer::Lexer, parser::Parser};

    #[test]
    fn test_invalid_switch_statement() {
        // given
        let input = "
int main() {
    switch (a) {
        case 1:
            x;
            break;
        case 2:
            y;
            break;
        case 1:
            aaa;
            break;
        default:
            bbb;
    }

    switch (x) {
        case 1:
        default:
            bbb;
        default:
            ccc;
            break;
    }
}
";
        let mut p = Parser::new(Lexer::new(input));
        let ast = p.parse_program();

        // when
        let result = check_semantic(&ast);

        // then
        if let Some(SemanticError { errors }) = result.err() {
            assert_eq!(errors.len(), 2);
            assert_eq!(
                true,
                errors[0].starts_with("error:3:13: switch label is duplicated"),
                "actual message: `{}`",
                errors[0]
            );
            assert_eq!(
                true,
                errors[1].starts_with("error:17:13: switch default label is duplicated"),
                "actual message: `{}`",
                errors[1]
            );
        } else {
            assert!(false);
        }
    }
}
