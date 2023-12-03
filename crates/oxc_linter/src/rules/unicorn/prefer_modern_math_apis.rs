use oxc_ast::{
    ast::{Argument, BinaryExpression, Expression},
    AstKind,
};
use oxc_diagnostics::{
    miette::{self, Diagnostic},
    thiserror::{self, Error},
};
use oxc_macros::declare_oxc_lint;
use oxc_span::{GetSpan, Span};
use oxc_syntax::operator::BinaryOperator;

use crate::{ast_util::is_method_call, context::LintContext, rule::Rule, AstNode};

#[derive(Debug, Error, Diagnostic)]
#[error("eslint-plugin-unicorn(prefer-modern-math-apis):")]
#[diagnostic(severity(warning), help(""))]
struct PreferModernMathApisDiagnostic(#[label] pub Span);

#[derive(Debug, Default, Clone)]
pub struct PreferModernMathApis;

declare_oxc_lint!(
    /// ### What it does
    ///
    ///
    /// ### Why is this bad?
    ///
    ///
    /// ### Example
    /// ```javascript
    /// ```
    PreferModernMathApis,
    correctness
);

impl Rule for PreferModernMathApis {
    fn run<'a>(&self, node: &AstNode<'a>, ctx: &LintContext<'a>) {
        // there are two main cases to check:
        // Bin expression:
        //     `Math.log(x) * Math.LOG10E`
        //     `Math.LOG10E * Math.log(x)`
        //     `Math.log(x) / Math.LN10`
        //     `Math.log(x) * Math.LOG2E`
        //     `Math.log(x) / Math.LN2`
        //     `Math.log(x) * Math.LOG10E`
        //     `Math.log(x) * Math.LOG10E`
        //
        // Call expressions:
        //     ONLY `Math.sqrt`
        //     where the contents are a bin expression (a * a + b * b) OR (a ** 2 + b ** 2)
        //
        //
        match node.kind() {
            AstKind::BinaryExpression(bin_expr) => {
                check_prefer_log(bin_expr, ctx);
            }

            AstKind::CallExpression(call_expr) => {
                if !is_method_call(call_expr, None, Some(&["sqrt"]), Some(1), Some(1)) {
                    return;
                };

                let Expression::MemberExpression(member_expr) = &call_expr.callee else {
                    return;
                };

                if !member_expr.object().is_specific_id("Math") {
                    return;
                };

                let Argument::Expression(arg) = &call_expr.arguments[0] else { return };

                let Expression::BinaryExpression(bin_expr) = arg else {
                    return;
                };

                // if !matches!(bin_expr.operator, BinaryOperator::Addition) {
                //     return;
                // }

                let a = !is_pythagorean_theorem_calculation(bin_expr);
                let b = !is_pythagorean_theorem_calculation_part(arg);

                dbg!(a, b);
                if a && b {
                    return;
                }

                ctx.diagnostic(PreferModernMathApisDiagnostic(call_expr.span));
            }
            _ => {}
        }
    }
}

fn check_prefer_log<'a, 'b>(expr: &'b BinaryExpression<'a>, ctx: &LintContext<'a>) {
    match expr.operator {
        BinaryOperator::Multiplication => {
            check_multiplication(&expr.left, &expr.right, ctx);
            check_multiplication(&expr.right, &expr.left, ctx);
        }
        BinaryOperator::Division => {
            let Expression::CallExpression(call_expr) = &expr.left else {
                return;
            };

            if !is_method_call(call_expr, None, Some(&["log"]), Some(1), Some(1)) {
                return;
            };

            if matches!(call_expr.arguments[0], Argument::SpreadElement(_)) {
                return;
            }

            let Expression::MemberExpression(member_expr) = &call_expr.callee else {
                return;
            };

            if !member_expr.object().is_specific_id("Math") {
                return;
            };

            let Expression::MemberExpression(member_expr) = &expr.right else {
                return;
            };

            if !matches!(
                member_expr.static_property_name(),
                Some("LN10" | "LN2" | "LOG10E" | "LOG2E")
            ) {
                return;
            };

            if !member_expr.object().is_specific_id("Math") {
                return;
            };

            ctx.diagnostic(PreferModernMathApisDiagnostic(expr.span));
        }
        _ => {}
    }
}

fn check_multiplication<'a, 'b>(
    left: &'b Expression<'a>,
    right: &'b Expression<'a>,
    ctx: &LintContext<'a>,
) {
    let Expression::CallExpression(call_expr) = left else {
        return;
    };

    if !is_method_call(call_expr, None, Some(&["log"]), Some(1), Some(1)) {
        return;
    };

    if matches!(call_expr.arguments[0], Argument::SpreadElement(_)) {
        return;
    }

    let Expression::MemberExpression(member_expr) = &call_expr.callee else {
        return;
    };

    if !member_expr.object().is_specific_id("Math") {
        return;
    };

    let Expression::MemberExpression(member_expr) = right else {
        return;
    };

    if !matches!(member_expr.static_property_name(), Some("LN10" | "LN2" | "LOG10E" | "LOG2E")) {
        return;
    };

    if !member_expr.object().is_specific_id("Math") {
        return;
    };

    ctx.diagnostic(PreferModernMathApisDiagnostic(member_expr.span()));
}

// PART + PART
// a ** 2
// Math.pow(a, 2)
fn is_pythagorean_theorem_calculation(expression: &BinaryExpression) -> bool {
    println!("1");
    if !matches!(expression.operator, BinaryOperator::Addition) {
        return false;
    }
    println!("2");

    is_pythagorean_theorem_calculation_part(&expression.left.without_parenthesized())
        && is_pythagorean_theorem_calculation_part(&expression.right.without_parenthesized())
}
fn is_pythagorean_theorem_calculation_part(expression: &Expression) -> bool {
    match expression {
        Expression::BinaryExpression(bin_expr) => match bin_expr.operator {
            BinaryOperator::Addition => is_pythagorean_theorem_calculation(bin_expr),
            BinaryOperator::Multiplication => true,
            BinaryOperator::Exponential => bin_expr.right.without_parenthesized().is_number(2_f64),

            _ => false,
        },
        Expression::CallExpression(call_expr) => {
            if !is_method_call(call_expr, None, Some(&["pow"]), Some(2), Some(2)) {
                return false;
            }

            if matches!(call_expr.arguments[0], Argument::SpreadElement(_)) {
                return false;
            }

            let Expression::MemberExpression(member_expr) = &call_expr.callee else {
                return false;
            };

            if !member_expr.object().is_specific_id("Math") {
                return false;
            };

            true
        }
        _ => false,
    }
}

#[test]
fn test() {
    use crate::tester::Tester;

    let pass = vec![
        // Prefer `Math.sqrt(x)`
        r"Math.notSqrt(a ** 2 + b ** 2)",
        r"NotMath.sqrt(a ** 2 + b ** 2)",
        r"Math.sqrt(a ** 2 - b ** 2)",
        r"Math.sqrt(a ** 2 + 2 ** b)",
        // r"Math.sqrt(a * c + b * c)",
        r"Math.sqrt((++a) * (++a))",
        // r"Math.sqrt(Math.pow(a, 2) + Math.pow(b, 2))",
        // Prefer `Math.log10(x)`
        r"Math.log(x) * Math.log(x)",
        r"Math.LOG10E * Math.LOG10E",
        r"Math.log(x) * Math[LOG10E]",
        r"Math.log(x) * LOG10E",
        r"Math[log](x) * Math.LOG10E",
        r"foo.Math.log(x) * Math.LOG10E",
        r"Math.log(x) * foo.Math.LOG10E",
        r"Math.log(x) * Math.NOT_LOG10E",
        r"Math.log(x) * Math?.LOG10E",
        r"Math?.log(x) * Math.LOG10E",
        r"log(x) * Math.LOG10E",
        r"new Math.log(x) * Math.LOG10E",
        r"Math.not_log(x) + Math.LOG10E",
        r"Math.log(x)[Math.LOG10E]",
        r"Math.log() * Math.LOG10E",
        r"Math.log(x, extraArgument) * Math.LOG10E",
        r"Math.log(...x) * Math.LOG10E",
        r"Math.LN10 / Math.LN10",
        r"Math.log(x) /Math[LN10]",
        r"Math.log(x) / LN10",
        r"Math[log](x) / Math.LN10",
        r"foo.Math.log(x) / Math.LN10",
        r"Math.log(x) / foo.Math.LN10",
        r"Math.log(x) / Math.log(x)",
        r"Math.log(x) / Math.NOT_LN10",
        r"Math.log(x) / Math?.LN10",
        r"Math?.log(x) / Math.LN10",
        r"log(x) / Math.LN10",
        r"new Math.log(x) / Math.LN10",
        r"Math.not_log(x) + Math.LN10",
        r"Math.log(x)[Math.LN10]",
        r"Math.log() / Math.LN10",
        r"Math.log(x, extraArgument) / Math.LN10",
        r"Math.log(...x) / Math.LN10",
        r"Math.log(x) * Math.log(x)",
        // Prefer `Math.log2(x)`
        r"Math.LOG2E * Math.LOG2E",
        r"Math.log(x) * Math[LOG2E]",
        r"Math.log(x) * LOG2E",
        r"Math[log](x) * Math.LOG2E",
        r"foo.Math.log(x) * Math.LOG2E",
        r"Math.log(x) * foo.Math.LOG2E",
        r"Math.log(x) * Math.NOT_LOG2E",
        r"Math.log(x) * Math?.LOG2E",
        r"Math?.log(x) * Math.LOG2E",
        r"log(x) * Math.LOG2E",
        r"new Math.log(x) * Math.LOG2E",
        r"Math.not_log(x) + Math.LOG2E",
        r"Math.log(x)[Math.LOG2E]",
        r"Math.log() * Math.LOG2E",
        r"Math.log(x, extraArgument) * Math.LOG2E",
        r"Math.log(...x) * Math.LOG2E",
        r"Math.LN2 / Math.LN2",
        r"Math.log(x) /Math[LN2]",
        r"Math.log(x) / LN2",
        r"Math[log](x) / Math.LN2",
        r"foo.Math.log(x) / Math.LN2",
        r"Math.log(x) / foo.Math.LN2",
        r"Math.log(x) / Math.log(x)",
        r"Math.log(x) / Math.NOT_LN2",
        r"Math.log(x) / Math?.LN2",
        r"Math?.log(x) / Math.LN2",
        r"log(x) / Math.LN2",
        r"new Math.log(x) / Math.LN2",
        r"Math.not_log(x) + Math.LN2",
        r"Math.log(x)[Math.LN2]",
        r"Math.log() / Math.LN2",
        r"Math.log(x, extraArgument) / Math.LN2",
        r"Math.log(...x) / Math.LN2",
    ];

    let fail = vec![
        // Prefer `Math.sqrt(x)`
        r#"Math.sqrt(a * a + b * b)"#,
        r"Math.sqrt(a ** 2 + b ** 2)",
        r#"Math.sqrt(a * a + b ** 2)"#,
        r#"Math.sqrt(a * a + b * b + c * c)"#,
        r#"Math.sqrt(a ** 2 + b ** 2 + c ** 2)"#,
        r#"Math.sqrt(a * a)"#,
        r#"Math.sqrt(a ** 2)"#,
        r#"Math.sqrt(a * a,)"#,
        r#"Math.sqrt(a ** 2,)"#,
        // r#"Math.sqrt((a, b) ** 2)"#,
        r#"Math.sqrt((++a) ** 2)"#,
        r#"Math.sqrt(a * a + b * b,)"#,
        r#"Math.sqrt(a ** 2 + b ** 2,)"#,
        r#"Math.sqrt((( a ** 2 )) + (( b ** 2 + c ** 2 )) + (( d )) * (( d )) + (( e )) ** (( 2 )))"#,
        // Prefer `Math.log10(x)`
        r"Math.log(x) * Math.LOG10E",
        r"Math.LOG10E * Math.log(x)",
        r"Math.log(x) / Math.LN10",
        r"Math.log((( 0,x ))) * Math.LOG10E",
        r"Math.LOG10E * Math.log((( 0,x )))",
        r"Math.log((( 0,x ))) / Math.LN10",
        r"
			function foo(x) {
				return (
					Math.log(x)
						/ Math.LN10
				);
			}
			",
        // Prefer `Math.log2(x)`
        r"Math.log(x) * Math.LOG2E",
        r"Math.LOG2E * Math.log(x)",
        r"Math.log(x) / Math.LN2",
        r"Math.log((( 0,x ))) * Math.LOG2E",
        r"Math.LOG2E * Math.log((( 0,x )))",
        r"Math.log((( 0,x ))) / Math.LN2",
        r"
			function foo(x) {
				return (
					Math.log(x)
						/ Math.LN2
				);
			}
		",
    ];

    Tester::new_without_config(PreferModernMathApis::NAME, pass, fail).test_and_snapshot();
}
