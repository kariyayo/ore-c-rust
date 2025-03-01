use ore_c_rust::repl;

pub fn main() -> Result<(), std::io::Error> {
    let user = whoami::username();
    println!("Hello, {}!", user);
    repl::start()
}
