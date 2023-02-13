use gitlog::gitlog;
use gitlog::checkout;
use persistence::save_map;
use persistence::save_value;
use persistence::update_set;
use warning::warnings;
use warning::loc;

use structopt::StructOpt;

#[derive(StructOpt, Debug, Clone)]
struct Args {
    #[structopt(name = "folder", long)]
    /// the folder of the repository, default to "/tmp/diag"
    folder: Option<String>,
}


pub fn main() {
    let args = Args::from_args();
    let mut folder = "/tmp/diag".to_string();
    if let Some(f) = args.folder {
        folder = f;
    }
    let folder = folder.as_str();
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
