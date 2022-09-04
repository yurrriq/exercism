pub fn build_proverb(list: &[&str]) -> String {
    let num_inputs = list.len();
    if num_inputs == 0 {
        return String::new();
    }

    let mut proverb_lines = Vec::new();

    for i in 0..num_inputs - 1 {
        proverb_lines.push(format!(
            "For want of a {0} the {1} was lost.",
            list[i],
            list[i + 1]
        ));
    }

    proverb_lines.push(format!("And all for the want of a {0}.", list[0]));
    proverb_lines.join("\n")
}
