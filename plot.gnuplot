set terminal png
set xlabel "number of nested calls to Data.Text.toCaseFold"
set ylabel "build time in seconds"

# app1 -O0
# set title "Build of app1 with -O0"
# set label "main :: IO ()\nmain = print\n    -- repeated n times:\n    $ toCaseFold . take 1\n    $ \"abc\"" at 2,1
# plot "data/results-app1-O0.txt" index 0 with linespoints title "ghc-7.6.3" \
#      , "data/results-app1-O0.txt" index 1 with linespoints title "ghc-7.8.3" \
#      , "data/results-app1-O0.txt" index 2 with linespoints title "ghc-7.10.1"

# app1 -O
# set title "Build of app1 with -O"
# set label "main :: IO ()\nmain = print\n    -- repeated n times:\n    $ toCaseFold . take 1\n    $ \"abc\"" at 1.5,700
# plot "data/results-app1-O.txt" index 0 with linespoints title "ghc-7.6.3" \
#      , "data/results-app1-O.txt" index 1 with linespoints title "ghc-7.8.3" \
#      , "data/results-app1-O.txt" index 2 with linespoints title "ghc-7.10.1"

# app1 -O2
set title "Build of app1 with -O2"
set label "main :: IO ()\nmain = print\n    -- repeated n times:\n    $ toCaseFold . take 1\n    $ \"abc\"" at 1.5,600
plot "data/results-app1-O2.txt" index 0 with linespoints title "ghc-7.6.3" \
     , "data/results-app1-O2.txt" index 1 with linespoints title "ghc-7.8.3" \
     , "data/results-app1-O2.txt" index 2 with linespoints title "ghc-7.10.1"
