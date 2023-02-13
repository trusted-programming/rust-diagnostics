use crate::save_value;
use crate::save_map;
use crate::save_table;
use crate::update_set;
use gitlog::gitlog;
use gitlog::checkout;
use warning::warnings;
use warning::loc;

#[test]
fn main() {
    let (url, hashes) = gitlog("/tmp/diag");
    let _ = save_table(url.clone(), hashes.clone());
    let _ = update_set("projects".to_string(), url.clone());
    let mut i: i32 = 0;
    for h in hashes {
        i += 1;
        checkout("/tmp/diag".to_string(), h.clone());
        let w = warnings("/tmp/diag".to_string());
        let _ = save_map(format!("{url}->{i:08}"), w);
        let loc = loc("/tmp/diag".to_string());
        let _ = save_value(format!("{url}->{i:08}->loc"), format!("{loc}"));
    }
}
