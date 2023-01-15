cd abc
git log
init=$(git log | grep commit | tail -1 | cut -d" " -f2)
update=$(git log | grep commit | head -1 | cut -d" " -f2)
echo $init
echo $update
git checkout $init
git diff --minimal -U0 HEAD..$update
../target/debug/rust-diagnostics --patch $update --confirm --pair --function
git checkout $update
cd -
