# Parameters
id = "2015-04-27"
appid = "app0"
opt = "O2"

# Generic settings
set terminal png size 1024,480
set xlabel "number of calls to Data.Text.toCaseFold"
set ylabel "build time in seconds"
set y2label "binary size in MB"
set ytics nomirror
set y2tics
set key outside

# line styles
set style line 1 lt 5 lw 2 pt 5 ps 1 lc rgb "red"
set style line 2 lt 5 lw 2 pt 5 ps 1 lc rgb "blue"
set style line 3 lt 5 lw 2 pt 5 ps 1 lc rgb "green"

set style line 4 lt 2 lw 1 pt 2 ps 1 lc rgb "red"
set style line 5 lt 2 lw 1 pt 2 ps 1 lc rgb "blue"
set style line 6 lt 2 lw 1 pt 2 ps 1 lc rgb "green"

# programs
app0 = "main :: IO ()\nmain =\n    -- repeated n times:\n    print (toCaseFold \"a\")"
app1 = "main :: IO ()\nmain = print\n    -- repeated n times:\n    $ toCaseFold . take 1\n    $ \"abc\""

# functions
outfile(a,o,x) = "images/results-".a."-".o.".".x.".png"
infile(a,o,x) = "data/results-".a."-".o.".".x.".txt"
title(a,o) = "Build of ".a." with -".o

set output outfile(appid,opt,id)
set title title(appid,opt)
set label app0 left at screen 0.82, screen 0.5
plot \
      infile(appid,opt,id) index 0 using 1:2 ls 1 with linespoints title "time ghc-7.6.3" axes x1y1 \
    , infile(appid,opt,id) index 0 using 1:3 ls 4 with linespoints title "size ghc-7.6.3" axes x1y2 \
    , infile(appid,opt,id) index 1 using 1:2 ls 2 with linespoints title "time ghc-7.8.3" axes x1y1 \
    , infile(appid,opt,id) index 1 using 1:3 ls 5 with linespoints title "size ghc-7.8.3" axes x1y2 \
    , infile(appid,opt,id) index 2 using 1:2 ls 3 with linespoints title "time ghc-7.10.1" axes x1y1 \
    , infile(appid,opt,id) index 2 using 1:3 ls 6 with linespoints title "size ghc-7.10.1" axes x1y2

