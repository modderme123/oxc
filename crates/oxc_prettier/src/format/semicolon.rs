use oxc_ast::AstKind;

use crate::{array, doc::Doc, ss, Prettier};

pub(super) fn should_print_leading_semicolon<'a>(p: &mut Prettier<'a>, doc: Doc<'a>) -> Doc<'a> {
    if p.options.semi {
        return doc;
    }

    let AstKind::ExpressionStatement(stmt) = p.current_kind() else { return doc };

    if !matches!(
        p.parent_kind(),
        AstKind::Program(_) | AstKind::BlockStatement(_) | AstKind::StaticBlock(_)
    ) {
        return doc;
    }

    p.nodes.push(AstKind::from_expression(&stmt.expression));
    let result = expression_needs_asi_protection(p);
    p.nodes.pop();
    if result {
        array![p, ss!(";"), doc]
    } else {
        doc
    }
}

pub(super) fn expression_needs_asi_protection(p: &mut Prettier<'_>) -> bool {
    let kind = p.current_kind();
    match kind {
        AstKind::ArrayExpression(_)
        | AstKind::ArrayPattern(_)
        | AstKind::TemplateLiteral(_)
        | AstKind::RegExpLiteral(_) => return true,
        AstKind::UnaryExpression(e) if e.operator.is_arithmetic() => return true,
        _ => {}
    }

    if p.need_parens(kind) {
        return true;
    }

    if !Prettier::has_naked_left_side(kind) {
        return false;
    }

    let lhs = Prettier::get_left_side_path_name(kind);
    p.nodes.push(lhs);
    let result = expression_needs_asi_protection(p);
    p.nodes.pop();
    result
}
