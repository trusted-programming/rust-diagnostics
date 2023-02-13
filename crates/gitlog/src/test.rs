use crate::gitlog;

#[test]
fn main() {
    let map = gitlog("../..");
    println!("{map:?}");
}
