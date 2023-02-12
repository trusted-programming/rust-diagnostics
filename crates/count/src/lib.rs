use std::{collections::BTreeMap, process::{Command, Stdio}};

#[cfg(test)]
pub mod test;

use warning::Warning;
use persistence::load_map;

pub fn counting() 
{
    let map: BTreeMap<String, Vec<Warning>> = load_map();
    let mut s: usize = 0;
    map.iter().for_each(|(k, v)| {
        println!("==================================== {k}: {}", v.len());
        s += v.len()
    });
    println!("==================================== {s}");
}
