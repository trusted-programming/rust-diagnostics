use std::collections::BTreeMap;

#[cfg(test)]
pub mod test;

use gitlog::Log;
use warning::Warning;
use persistence::load_logs;
use persistence::load_warnings;
use persistence::load_loc_map;

/// counting warnings and LOC per revision of a project
pub fn counting() 
{
    let logs: BTreeMap<String, Log> = load_logs(); 
    let map: BTreeMap<String, Vec<Warning>> = load_warnings();
    let loc_map: BTreeMap<String, usize> = load_loc_map();
    let mut s: usize = 0;
    println!("timestamp,no. of warnings,LOC");
    map.iter().for_each(|(k, v)| {
        let segs = k.split("->").collect::<Vec<&str>>();
        let k0 = format!("{}->{}", segs[0], segs[1]);
        let k1 = format!("{}->{}->loc", segs[0], segs[1]);
        let loc = loc_map.get(&k1).unwrap();
        let log = logs.get(&k0).unwrap();
        println!("{},{},{}", log.timestamp, v.len(), loc);
        s = s.saturating_add(v.len())
    });
}
