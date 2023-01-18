
fn main() {
    let s = std::fs::read_to_string("Cargo.toml").unwrap();
    println!("The configuration file is: {s}");
}
