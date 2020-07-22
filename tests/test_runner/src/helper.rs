

// the kitchen sink util file that every project needs

pub fn indent(input: &str) -> String {
    let indent = " ".repeat(8);

    format!("{}{}", indent,  input.replace("\n", &format!("\n{}", indent)))
}