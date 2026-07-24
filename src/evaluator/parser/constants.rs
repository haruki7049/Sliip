//! # lll parser constants

use std::sync::LazyLock;

pub static reserved: LazyLock<Vec<&str>> = LazyLock::new(|| vec!["define"]);
