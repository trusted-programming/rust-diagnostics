main=$(git branch -r | grep HEAD | cut -d/ -f3)
git checkout $main
git log -p Cargo.toml | grep "^-version = .*" -B13 | egrep -h "commit|version" | awk '
/commit/ {
	commit=$2
}
/version/ {
	tags[$3] = commit
}
END {
	for (tag in tags) {
		system("git checkout " tags[tag] "^");
		system("git tag -f " tag)
	}
}
'
git checkout $main
git for-each-ref --sort=creatordate --format '%(objectname) %(refname:short) %(creatordate)' refs/tags
