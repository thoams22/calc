<<<<<<< HEAD
mod ast;
mod lexer;
mod parser;
mod utils;
=======
pub fn add(left: usize, right: usize) -> usize {
    left + right
}

pub fn sub(left: usize, right: usize) -> usize {
    left - right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
>>>>>>> 3deb1343c06f47d681b44fb3c4d7df0a3f194217
