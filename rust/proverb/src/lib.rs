pub fn build_proverb(list: &[&str]) -> String {
    let mut proverb = list
        .windows(2)
        .map(|w| format!("For want of a {0} the {1} was lost.", w[0], w[1]))
        .collect::<Vec<String>>();

    if !list.is_empty() {
        proverb.push(format!("And all for the want of a {}.", list[0]));
    }
    proverb.join("\n")
}
