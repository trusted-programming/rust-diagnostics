cargo install --path .
redis-cli flushall
mkdir -p plain/
export NAMESPACE=plain/
tree-marker
save
ls -l $NAMESPACE.tar.gz
