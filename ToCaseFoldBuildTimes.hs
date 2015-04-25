{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | Plot output with gnuplot as follows:
--
-- > set xlabel "number of calls to Data.Text.toCaseFold"
-- > set ylabel "build time in seconds"
-- > set title "Build with -O"
-- > set label "main ∷ IO ()\nmain =\n    -- repeated n times:\n    print (toCaseFold \"a\")" at 2,30
-- > plot "results.txt" index 0 with lines title "ghc-7.6.3", "results.txt" index 1 with lines title "ghc-7.8.3", "results.txt" index 2 with lines title "ghc-7.10.1"
--
module Main
( main
) where

import Data.Monoid
import Data.String
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Distribution.Simple.Utils
import Distribution.Verbosity

import Numeric.Natural

import System.Clock
import System.FilePath
import System.Environment
import System.Process

-- -------------------------------------------------------------------------- --
-- static parameters

optimizationFlag ∷ IsString a ⇒ a
optimizationFlag="-O2"

defaultVersions ∷ IsString a ⇒ [a]
defaultVersions = ["7.6.3", "7.8.3", "7.10.1"]

sizeList ∷ [Natural]
-- sizeList = [1..10] <> [20]
sizeList = [1..4]

app
    ∷ Natural
        -- ^ number of times 'T.toFoldCase' is called
    → T.Text
app = app1

-- -------------------------------------------------------------------------- --
-- main

main ∷ IO ()
main = do
    versions ← getArgs >>= \case
        [] → return defaultVersions
        x → return $ T.pack <$> x
    mapM_ (benchGhcVersion sizeList) versions

benchGhcVersion
    ∷ [Natural]
        -- ^ list with test cases (number of times 'T.toFoldCase' is called)
    → T.Text
        -- ^ compiler version
    → IO ()
benchGhcVersion l ver = do
    T.putStrLn $ "# ghc-" <> ver
    mapM_ (\i → bench i ver >>= printTime i) l
    T.putStrLn "\n"
  where
    printTime i t = T.putStrLn $  sshow i <> " " <> sshow (fromIntegral (timeSpecAsNanoSecs t) / 1e9 ∷ Double)

bench
    ∷ Natural
        -- ^ size
    → T.Text
        -- ^ compiler version
    → IO TimeSpec
bench size ver = withTempDirectory normal "." "ghc-bench" $ \dir → do
    let fileName = dir </> "Main.hs"
    T.writeFile fileName $ app size
    compile ver fileName

compile
    ∷ T.Text
        -- ^ compiler version
    → FilePath
        -- ^ Filename
    → IO TimeSpec
compile ver fileName = fmap fst . time $
    callCommand $ "ghc-" <> T.unpack ver <> " -fforce-recomp " <> optimizationFlag <> " " <> fileName <> " >/dev/null"

-- |
-- > main = do
-- >    -- repeat the following statement n times
-- >    print (toCaseFold "a")
--
app0
    ∷ Natural
        -- ^ number of times 'T.toFoldCase' is called
    → T.Text
app0 n = T.unlines $
    [ "{-# LANGUAGE OverloadedStrings #-}"
    , "module Main"
    , "( main"
    , ") where"
    , ""
    , "import qualified Data.Text as T"
    , ""
    , "main :: IO ()"
    , "main = do"
    ]
    <> replicate (fromIntegral n) "    print (toCaseFold \"a\")"
    -- <> replicate (fromIntegral n) "    print (toLower \"a\")"

-- |
-- > import qualified Data.Text as T
-- >
-- > main = print
-- >     -- repeat following statement n times
-- >     $ T.toCaseFold . T.take 1
-- >     $ "abc"
--
app1
    ∷ Natural
        -- ^ number of times 'T.toFoldCase' is called
    → T.Text
app1 n = T.unlines $
    [ "{-# LANGUAGE OverloadedStrings #-}"
    , "module Main"
    , "( main"
    , ") where"
    , ""
    , "import qualified Data.Text as T"
    , ""
    , "main :: IO ()"
    , "main = print"
    ]
    <> replicate (fromIntegral n) "    $ T.toCaseFold . T.take 1"
    <> [ "    $ \"abc\"" ]

-- -------------------------------------------------------------------------- --
-- Utils

sshow ∷ (Show a, IsString b) ⇒ a → b
sshow = fromString . show

time
    ∷ IO a
    → IO (TimeSpec, a)
time action = do
  start ← getTime Monotonic
  !result ← action
  end ← getTime Monotonic
  let !delta = diffTimeSpec start end
  return (delta, result)

