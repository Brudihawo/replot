use replot::repl::Repl;

fn main() {
    let mut repl = Repl::new();
    loop {
        Repl::print_prompt();
        repl.parse_and_process()
    }
}
