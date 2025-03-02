use colored::*;
use std::fs;

pub fn print_error(title: &str, details: &[(&str, String)], filename: &str, line: usize, column: usize) {
    let absolute_path = match fs::canonicalize(filename) {
        Ok(path) => path.display().to_string(),
        Err(_) => filename.to_string(),
    };

    eprintln!("{}", format!("error: {}", title).red().bold());
    
    eprintln!(" --> {}:{}:{}", absolute_path, line, column);
    eprintln!("  |");
    
    for (label, value) in details {
        eprintln!("{} {}", label.green().bold(), value);
    }

    eprintln!("  |");
    eprintln!("  |                 ^");
    eprintln!();
}