use crate::warnings;

#[test]
fn main() {
    let w = warnings("../..".to_string());
    dbg!(&w.get("src/main.rs").unwrap().len());
}
