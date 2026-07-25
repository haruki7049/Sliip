//! # lll parser constants

pub mod keywords {
    pub const DEFINE: &str = "define";
    pub const LAMBDA: &str = "lambda";

    #[cfg(test)]
    mod tests {
        use crate::evaluator::parser::constants::keywords;

        #[test]
        fn define() {
            assert_eq!("define", keywords::DEFINE);
        }

        #[test]
        fn lambda() {
            assert_eq!("lambda", keywords::LAMBDA);
        }
    }
}
