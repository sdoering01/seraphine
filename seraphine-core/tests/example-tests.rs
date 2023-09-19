use std::fs::{read_dir, read_to_string};

use seraphine_core::{parser::parse, tokenizer::tokenize};

#[test]
fn test_that_examples_parse() {
    for example_file in read_dir("../examples").unwrap() {
        let example_file = example_file.unwrap();
        let example_code = read_to_string(example_file.path()).unwrap();
        let tokens = tokenize(&example_code).unwrap();
        parse(&tokens).unwrap();
    }
}
