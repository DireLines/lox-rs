#![allow(dead_code)] // <- TODO - remove
mod evaluator;
mod parser;
mod scanner;
pub use parser::run;
