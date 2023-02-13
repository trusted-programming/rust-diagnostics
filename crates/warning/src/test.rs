use crate::warnings;

#[test]
fn main() {
    let w = warnings("../..");
    dbg!(&w.get("src/main.rs").unwrap().len());
}
