use crate::gitlog;

#[test]
fn main() {
    let (url, hashes) = gitlog("../..");
    println!("{url}, {hashes:?}");
}
