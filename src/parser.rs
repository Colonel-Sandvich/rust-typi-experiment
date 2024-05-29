use oxc::allocator::Allocator;
use oxc::ast::ast::Program;
use oxc::parser::{Parser, ParserReturn};
use oxc::span::SourceType;

pub fn parse_typescript<'a>(
    allocator: &'a Allocator,
    code: &'a str,
) -> Result<ParserReturn<'a>, String> {
    let source_type = SourceType::default().with_typescript(true); // Assuming the source type can be inferred
    let parser = Parser::new(allocator, code, source_type);
    let ret = parser.parse();

    if ret.panicked {
        for error in ret.errors {
            let error = error.with_source_code(code.to_string());
            println!("{error:?}");
            println!("Parsed with Errors.");
        }
        Err("Failed to parse".to_string())
    } else {
        Ok(ret)
    }
}

pub fn print_program(program: &Program) {
    println!("AST:");
    println!(
        "{}",
        serde_json::to_string_pretty(program)
            .map_err(|e| e.to_string())
            .unwrap()
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_js_to_ast() {
        let allocator = Allocator::default();
        let code = r#"
/**
 * Paste or drop some JavaScript here and explore
 * the syntax tree created by chosen parser.
 * You can use all the cool new features from ES6
 * and even more. Enjoy!
 */

let tips = [
    "Click on any AST node with a '+' to expand it",
    
    "Hovering over a node highlights the \
    corresponding location in the source code",
    
    "Shift click on an AST node to expand the whole subtree"
];

function printTips() {
    tips.forEach((tip, i) => console.log(`Tip ${i}:` + tip));
}
        "#;

        match parse_typescript(&allocator, code) {
            Ok(json) => {
                println!("AST:");
                println!(
                    "{}",
                    serde_json::to_string_pretty(&json.program)
                        .map_err(|e| e.to_string())
                        .unwrap()
                );
            }
            Err(e) => panic!("Failed to parse JS code: {}", e),
        };
    }
}
