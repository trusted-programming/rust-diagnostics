#[cfg(test)]
pub mod test;

use std::collections::BTreeMap;

use git2::Repository;
use serde::{Serialize, Deserialize};

#[non_exhaustive]
#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct Log {
    pub url: String,
    pub hash: String,
    pub timestamp: i64,
}
/// 
/// Obtain the commit logs from a git repository at $path,
/// which is the folder where the git repository has been checked out. 
///
/// Return the URL of the remote repository, the number of revisions, and the table of the commit
/// revisions in the order of time (oldest first)
///
/// Upon an error, return empty URL and Hashes
///
pub fn gitlog(path: &str) -> (String, usize, BTreeMap<String, Log>) {
    let mut url = "".to_string();
    let mut logs = BTreeMap::new();
    let mut i: usize = 0;
    if let Ok(repo) = Repository::open(path) {
        if let Ok(remote) = repo.find_remote("origin") {
            if let Some(u) = remote.url() {
                url = u.to_string();
                if let Ok(mut revwalk) = repo.revwalk() {
                    revwalk
                        .set_sorting(git2::Sort::REVERSE | git2::Sort::TIME)
                        .ok();
                    revwalk.push_head().ok();
                    for id in revwalk {
                        if let Ok(id) = id {
                            if let Ok(commit) = repo.find_commit(id) {
                                let hash = format!("{}", commit.id());
                                let timestamp = commit.author().when().seconds();
                                i = i.saturating_add(1);
                                let log = Log {url: url.clone(), hash: hash.clone(), timestamp };
                                logs.insert(format!("{i:08}"), log);
                            } else {
                                println!("================== Cannot get the commit {id:?}");
                            }
                        } else {
                            println!("================== Cannot get the oid {id:?} as a commit id");
                        }
                    }
                } else {
                    println!("================== Cannot walk the repository");
                }
            } else {
                println!("================== Cannot find the url");
            }
        } else {
            println!("================== Cannot find the origin");
        }
    } else {
        println!("================== Cannot open the repository at {path}");
    }
    (url, i, logs)
}

/// functionality is the same as running the following commands
/// ```bash
/// cd $folder
/// git checkout $commit_id
/// ```
pub fn checkout(folder: &str, commit_id: String) {
    if let Ok(repo) = git2::Repository::open(folder) {
        if let Ok(oid) = git2::Oid::from_str(&commit_id) {
            if let Ok(commit) = repo.find_commit(oid) {
                repo.reset(
                    commit.as_object(),
                    git2::ResetType::Hard,
                    Some(
                        git2::build::CheckoutBuilder::new()
                            .force()
                            .remove_untracked(true),
                    ),
                )
                .ok();
            }
        }
    }
}
