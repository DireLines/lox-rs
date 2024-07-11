#![allow(dead_code)] // <- TODO - remove
mod evaluator;
mod parser;
mod scanner;
pub use evaluator::{interpret, EnvStack};
pub use parser::{parse_str_with, Program};
