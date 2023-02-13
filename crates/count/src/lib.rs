use std::{collections::BTreeMap, process::{Command, Stdio}};

#[cfg(test)]
pub mod test;

use warning::Warning;
use persistence::load_map;
use persistence::load_loc_map;

/// counting warnings and LOC per revision of a project
pub fn counting() 
{
    let map: BTreeMap<String, Vec<Warning>> = load_map();
    let loc_map: BTreeMap<String, usize> = load_loc_map();
    let mut s: usize = 0;
    map.iter().for_each(|(k, v)| {
        let segs = k.split("->").collect::<Vec<&str>>();
        let k0 = format!("{}->{}", segs[0], segs[1]);
        let k1 = format!("{}->{}->loc", segs[0], segs[1]);
        let loc = loc_map.get(&k1).unwrap();
        println!("{k0}: {}, {}", v.len(), loc);
        s += v.len()
    });
}
