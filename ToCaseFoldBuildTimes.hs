{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import qualified Options.Applicative as O

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class

import Data.Monoid.Unicode
import Data.String
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Typeable

import Distribution.Simple.Utils
import Distribution.Verbosity

import Numeric.Natural

import Prelude.Unicode

import System.Clock
import System.FilePath
import System.Posix.Files
import System.Process

import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Language as P (haskellDef)

-- -------------------------------------------------------------------------- --
-- static parameters

defaultVersions ∷ [GhcVersion]
defaultVersions = [GHC_7_6_3, GHC_7_8_3, GHC_7_10_1]

sizeList ∷ [Natural]
sizeList = [1..10] ⊕ [20]
-- sizeList = [1..6]

-- -------------------------------------------------------------------------- --
-- Supported GHC Versions

data GhcVersion
    = GHC_7_6_3
    | GHC_7_10_1
    | GHC_7_8_3
    deriving (Show, Read, Eq, Ord, Bounded, Enum, Typeable)

readGhcVersion
    ∷ (IsString e, MonadError e m, IsString a, Eq a, Show a, Monoid e)
    ⇒ a
    → m GhcVersion
readGhcVersion "7.6.3" = return GHC_7_6_3
readGhcVersion "7.8.3" = return GHC_7_8_3
readGhcVersion "7.10.1" = return GHC_7_10_1
readGhcVersion x = throwError $ "unsupported GHC Version: " ⊕ sshow x

ghcVersionText
    ∷ IsString a
    ⇒ GhcVersion
    → a
ghcVersionText GHC_7_6_3 = "7.6.3"
ghcVersionText GHC_7_8_3 = "7.8.3"
ghcVersionText GHC_7_10_1 = "7.10.1"

pGhcVersion ∷ O.Parser GhcVersion
pGhcVersion = O.option (O.eitherReader readGhcVersion)
    $ O.long "ghc-version"
    ⊕ O.short 'g'
    ⊕ O.metavar "7.6.3|7.8.3|7.10.1"
    ⊕ O.help "ghc version that is included in the tests"

-- -------------------------------------------------------------------------- --
-- Optimization Flag

data Optimization
    = O0
    | O1
    | O2
    deriving (Show, Read, Eq, Ord, Bounded, Enum, Typeable)

readOptimization
    ∷ (IsString e, MonadError e m, IsString a, Eq a, Show a, Monoid e)
    ⇒ a
    → m Optimization
readOptimization "O0" = return O0
readOptimization "O" = return O1
readOptimization "O2" = return O2
readOptimization x = throwError $ "unsupported optimization level: " ⊕ sshow x

optimizationText
    ∷ IsString a
    ⇒ Optimization
    → a
optimizationText O0 = "O0"
optimizationText O1 = "O"
optimizationText O2 = "O2"

pOptimization ∷ O.Parser Optimization
pOptimization = O.option (O.eitherReader readOptimization)
    $ O.long "optimization"
    ⊕ O.short 'o'
    ⊕ O.metavar "O0|O|O2"
    ⊕ O.help "optimization level that is used in the tests"
    ⊕ O.value O1

-- -------------------------------------------------------------------------- --
-- Test Applications

data TestApp
    = App0
    | App1
    deriving (Show, Read, Eq, Ord, Bounded, Enum, Typeable)

readTestApp
    ∷ (IsString e, MonadError e m, IsString a, Eq a, Show a, Monoid e)
    ⇒ a
    → m TestApp
readTestApp "app0" = return App0
readTestApp "app1" = return App1
readTestApp x = throwError $ "unknown test application: " ⊕ sshow x

testAppText
    ∷ IsString a
    ⇒ TestApp
    → a
testAppText App0 = "app0"
testAppText App1 = "app1"

pTestApp ∷ O.Parser TestApp
pTestApp = O.option (O.eitherReader readTestApp)
    $ O.long "test-app"
    ⊕ O.short 'a'
    ⊕ O.metavar "app0|app1"
    ⊕ O.help "test application"

-- -------------------------------------------------------------------------- --
-- Parameter

data Params = Params
    { _paramTestApp ∷ !TestApp
    , _paramGhcVersions ∷ ![GhcVersion]
    , _paramOptimization ∷ Optimization
    , _paramSizes ∷ ![Natural]
    }
    deriving (Show, Read, Eq, Ord, Typeable)

pParams ∷ O.Parser Params
pParams = Params
    <$> pTestApp
    <*> many pGhcVersion
    <*> pOptimization
    <*> O.option (O.eitherReader pSizes)
        ( O.long "sizes"
        ⊕ O.short 's'
        ⊕ O.metavar "RANGE(,RANGE)*"
        ⊕ O.help "list of sizes of the test application"
        ⊕ O.value [1]
        )

pSizes ∷ String → Either String [Natural]
pSizes s = either (Left ∘ show) Right $ P.parse ranges "sizes option" s
  where
    ranges = concat <$> P.sepBy1 numberOrRange (P.char ',')
    numberOrRange = P.try range <|> (:[]) <$> natural
    range = (\a b → [a..b]) <$> natural <*> (P.string ".." *> natural)

    natural = fromIntegral <$> P.natural (P.makeTokenParser P.haskellDef)

-- -------------------------------------------------------------------------- --
-- main

main ∷ IO ()
main = do
    params ← O.execParser $ O.info pParams
        $ O.briefDesc
        ⊕ O.progDesc ""
    benchGhcVersion $ params
        { _paramGhcVersions = if null (_paramGhcVersions params)
            then defaultVersions
            else _paramGhcVersions params
        }

benchGhcVersion
    ∷ Params
    → IO ()
benchGhcVersion Params{..} = do
    forM_ _paramGhcVersions $ \ver → do
        T.putStrLn $ "# ghc-" ⊕ ghcVersionText ver
        forM_ _paramSizes $ \size → do
            bench _paramTestApp _paramOptimization size ver >>= uncurry (printResults size)
        T.putStrLn "\n"
  where
    printResults i t s = T.putStrLn ∘ T.intercalate " " $
        [ sshow i
        , sshow (fromIntegral (timeSpecAsNanoSecs t) / 1e9 ∷ Double)
        , sshow (fromIntegral s / 1e6 ∷ Double)
        ]

bench
    ∷ TestApp
    → Optimization
    → Natural
        -- ^ size
    → GhcVersion
        -- ^ compiler version
    → IO (TimeSpec, Natural)
bench app opt size ver = withTempDirectory normal "." "ghc-bench" $ \dir → do
    let baseName = dir </> "Main"
        fileName = baseName <.> "hs"
        binName = baseName
    T.writeFile fileName $ getApp app size
    t ← compile opt ver baseName
    s ← (fromIntegral ∘ fileSize) <$> getFileStatus binName
    return (t, s)

compile
    ∷ Optimization
    → GhcVersion
        -- ^ compiler version
    → FilePath
        -- ^ File name /without extension/
    → IO TimeSpec
compile opt ver baseName = do
    fmap fst ∘ time $
        callCommand
            $ "ghc-" ⊕ ghcVersionText ver
            ⊕ " -fforce-recomp"
            ⊕ " -" ⊕ optimizationText opt
            ⊕ " " ⊕ hsName
            ⊕ " >/dev/null"
  where
    hsName = baseName <.> "hs"

getApp ∷ TestApp → Natural → T.Text
getApp App0 = app0
getApp App1 = app1

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
    , "{-# LANGUAGE UnicodeSyntax #-}"
    , "module Main"
    , "( main"
    , ") where"
    , ""
    , "import qualified Data.Text as T"
    , ""
    , "main ∷ IO ()"
    , "main = do"
    ]
    ⊕ replicate (fromIntegral n) "    print (T.toCaseFold \"a\")"
    -- ⊕ replicate (fromIntegral n) "    print (toLower \"a\")"

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
    , "{-# LANGUAGE UnicodeSyntax #-}"
    , "module Main"
    , "( main"
    , ") where"
    , ""
    , "import qualified Data.Text as T"
    , ""
    , "main ∷ IO ()"
    , "main = print"
    ]
    ⊕ replicate (fromIntegral n) "    $ T.toCaseFold . T.take 1"
    ⊕ [ "    $ \"abc\"" ]

-- -------------------------------------------------------------------------- --
-- Utils

sshow ∷ (Show a, IsString b) ⇒ a → b
sshow = fromString ∘ show

time
    ∷ IO a
    → IO (TimeSpec, a)
time action = do
  start ← getTime Monotonic
  !result ← action
  end ← getTime Monotonic
  let !delta = diffTimeSpec start end
  return (delta, result)

