#[cfg(test)]
pub mod test;

use git2::Repository;

/// path is the folder where git repository has been checked out return the URL of the remote
/// repository and the hashes of the commit history in the order of time (oldest first)
///
/// Upon an error, return empty URL and Hashes
pub fn gitlog(path: &str) -> (String, Vec<String>) {
    let mut url = "".to_string();
    let mut hashes = Vec::new();
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
                                hashes.push(hash);
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
    (url, hashes)
}

/// functionality is the same as running the following commands
/// ```bash
/// cd $folder
/// git checkout $commit_id
/// ```
pub fn checkout(folder: String, commit_id: String) {
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
