# https://raymii.org/s/tutorials/GNUplot_tips_for_nice_looking_charts_from_a_CSV_file.html
set datafile separator ','
#set xdata time
set timefmt "%Y-%m-%d %H:%M:%S"
#set format x "%Y-%m-%d %H:%M:%S"
# set xrange ["2022-09-21 16:00":"2023-01-22 16:00"]
set key autotitle columnhead # use the first line as title
set xlabel "revision history of the git repository"
set ylabel "number of warnings"
set y2tics # enable second axis
set ytics nomirror # dont show the tics on that side
set y2label "warnings per KLOC" # label for second axis
set terminal png size 2000,1500 enhanced font "Helvetica,20"
set output 'counts.png'
plot "counts.csv" using 1:2 with lines, '' using 1:($2*1000/$3) with lines axis x1y2
