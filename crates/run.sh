redis-cli flushdb
persistence --folder $1
count > counts.csv
gnuplot counts.gnuplot
open counts.png
