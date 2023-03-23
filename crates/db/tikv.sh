wget http://download.pingcap.org/tidb-latest-linux-amd64.tar.gz
sudo mkdir -p /opt/server
sudo chown -R $(id -nu) /opt/server
tar xvfz tidb-latest-linux-amd64.tar.gz -C /opt/server
cd /opt/server/tidb-v5.0.1-linux-amd64/
./bin/pd-server --data-dir=pd --log-file=pd.log &
./bin/tikv-server --pd="127.0.0.1:2379" --data-dir=tikv --log-file=tikv.log &
