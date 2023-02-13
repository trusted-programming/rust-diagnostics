use crate::save_value;
use crate::save_map;
use crate::update_set;
use gitlog::gitlog;
use gitlog::checkout;
use warning::warnings;
use warning::loc;

#[test]
fn main() {
    let folder = "/tmp/diag";
    if let Ok(new) = update_set("projects".to_string(), folder.to_string()) {
        if new {
            let (url, n, logs) = gitlog(folder);
            let _ = save_value(folder.to_string(), url.clone());
            let _ = save_value(format!("{url}->revisions"), format!("{n}"));
            let _ = save_map(url.clone(), logs.clone());
            for (i, l) in &logs {
                checkout(folder, l.hash.clone());
                let w = warnings(folder);
                let _ = save_map(format!("{url}->{i:08}->warning"), w);
                let loc = loc(folder);
                let _ = save_value(format!("{url}->{i:08}->loc"), format!("{loc}"));
            }
        }
    }
}
