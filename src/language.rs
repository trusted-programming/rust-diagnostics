use anyhow::{anyhow, bail, Error, Result};
use std::fmt::{Display, Formatter};
use std::str::FromStr;

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum Language {
    Rust,
}

impl Language {
    pub fn all() -> Vec<Language> {
        vec![Language::Rust]
    }

    pub fn language(&self) -> tree_sitter::Language {
        unsafe {
            match self {
                Language::Rust => tree_sitter_rust(),
            }
        }
    }

    pub fn parse_query(&self, raw: &str) -> Result<tree_sitter::Query> {
        tree_sitter::Query::new(self.language(), raw).map_err(|err| anyhow!("{}", err))
    }
}

impl FromStr for Language {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        match s {
            "rust" => Ok(Language::Rust),
            _ => bail!(
                "unknown language {}. Try one of: {}",
                s,
                Language::all()
                    .into_iter()
                    .map(|l| l.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

impl Display for Language {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Language::Rust => f.write_str("rust"),
        }
    }
}

extern "C" {
    fn tree_sitter_rust() -> tree_sitter::Language;
}
