/// Convert to snake case, assuming pascal case.
///
/// If `s` is already in snake case, should yield the same result.
pub(crate) fn pascal_to_snake_case(s: &str) -> String {
    let mut snake = String::new();
    for ch in s.chars() {
        if ch.is_ascii_uppercase() && !snake.is_empty() {
            snake.push('_');
        }
        snake.push(ch.to_ascii_lowercase());
    }
    snake
}

// Convert to pascal case, assuming snake case.
//
// If `s` is already in pascal case, should yield the same result.
pub(crate) fn snake_to_pascal_case(s: &str) -> String {
    let mut pascal = String::new();
    let mut capitalize = true;
    for ch in s.chars() {
        if ch == '_' {
            capitalize = true;
        } else if capitalize {
            pascal.push(ch.to_ascii_uppercase());
            capitalize = false;
        } else {
            pascal.push(ch);
        }
    }
    pascal
}
