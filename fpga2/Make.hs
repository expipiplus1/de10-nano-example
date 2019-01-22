#!/usr/bin/env runhaskell

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ViewPatterns           #-}
{-# LANGUAGE TupleSections           #-}
{-# LANGUAGE ApplicativeDo           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Make
  ( main
  ) where

-- import Data.Attoparsec.Text
import           Clash.Driver.Types
import qualified Data.IntMap as Map
import Data.IntMap (IntMap)
import Text.Earley hiding (rule)
import Text.Earley.Grammar (Prod(..))
import           Clash.Promoted.Nat
import Control.Error.Util (hush)
import Data.Foldable
import Clash.Promoted.Nat.Literals
import           Clash.Promoted.Symbol
import           Data.Bifunctor
import           Data.Biapplicative
import           Control.Applicative
import           Control.Arrow
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Maybe
import           Data.Binary hiding (get, put)
import           Data.Char
import           Data.Hashable
import           Data.Kind
import           Data.List
import           Data.Maybe
import           Data.Singletons.Prelude.List hiding (Any)
import           Data.String
import qualified Data.Text                    as T
import           Data.Typeable
import           Development.Shake
import           Development.Shake.FilePath
import           Development.Shake.Util
import           GHC.TypeLits
import Data.Traversable

build, src, includeDir, rbfFile, dtboFile :: FilePath
type Build = "build"
build = symbolVal (Proxy @Build)
src = "src"
includeDir = src
rbfFile = "output_files" </> "test.rbf"
dtboFile = build </> "test.dtbo"

qsysHPSSource :: String
qsysHPSSource = "top"

clashModules :: [(String, String)]
clashModules = [("Blink", "Blink"), ("Slave", "simple_axi3_slave"), ("UART", "uart_test")]
clashLanguage :: String
clashLanguage = "verilog"

projectName :: String
projectName = "test"

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = build } $ do
  want [rbfFile, dtboFile]

  phony "clean" $ do
    putNormal $ "Cleaning files in " <> show build
    removeFilesAfter build ["//*"]

  clashRules
  quartusRules
  qsysRules
  sopcRules

-- | Find all the hand written Verilog files as well as the verilog files
-- compiled with Clash.
gatherVerilog :: Action [FilePath]
gatherVerilog = do
  nativeVerilogSources <- getDirectoryFiles "" [src <> "//*.v"]
  haskellSources       <- getDirectoryFiles "" [src <> "//*.hs"]
  let haskellBaseNames = takeBaseName <$> haskellSources
      listingFiles     = [ build </> bn <.> "listing" | bn <- haskellBaseNames ]
  need listingFiles
  haskellHDLFiles <- fmap read <$> traverse readFile' listingFiles
  pure $ nativeVerilogSources <> concat haskellHDLFiles

qsysRules :: Rules ()
qsysRules = do
  rule $ \(m :: M (1 / "synthesis" / 1 <> ".qip")) -> do
    let n = ref d1 m
    need [n <.> "qsys"]
    -- This returns a non zero code on success...
    Exit e <- command [] "qsys-generate" ["--synthesis=verilog", n <.> "qsys"]
    putNormal $ "qsys-generate finished with code " ++ show e
    pure ()

  "*.sopcinfo" %> \sopcinfo -> do
    let n = takeBaseName sopcinfo
    need [n </> "synthesis" </> n <.> "qip"]

newtype QuartusQuery = QuartusQuery [String]
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

type instance RuleResult QuartusQuery = [FilePath]

quartusRules :: Rules ()
quartusRules = do
  -- Query global assignments in quartus
  rec
    quartusQuery <- addOracleCache $ \(QuartusQuery qs) ->
      withTempDir $ \dir -> do
        need [projectName <.> "qsf"]
        let -- These qip files provide additional global assignments, it's
            -- important to have these available before performing the proper
            -- query
            bootSources = ["QIP_FILE"]
        when (qs /= bootSources) $ do
          boots <- quartusQuery (QuartusQuery bootSources)
          need boots
        let tcl = dir </> "get-source.tcl"
        liftIO $ writeFile tcl $ unlines
          [ "project_open test"
          , "foreach source_type [ list " ++ unwords qs ++ "] {"
          , "  foreach_in_collection foo [get_all_assignments -type global -name $source_type] {"
          , "      set name   [get_assignment_info $foo -name]"
          , "      set value  [get_assignment_info $foo -value]"
          , "      puts $value"
          , "  }"
          , "}"
          ]
        Stdout out <- command [] "quartus_sh" ["--no_banner", "-t", tcl]
        let filterWarnings =
              filter (\l -> not $ any (`isInfixOf` l) ["Info", "Warning"])
            getWarnings = filter ("Warning" `isInfixOf`)
        traverse_ putNormal (getWarnings . lines $ out)
        pure (filterWarnings . lines $ out)

  phonys $ \n -> do
    guard ("_FILE" `isSuffixOf` n)
    pure $ liftIO . print =<< quartusQuery (QuartusQuery [n])

  phony "timing" $ need ["output_files" </> projectName <.> "sta.rpt"]

  -- Update the initial on-chip memory according to 
  phony "update-memory" $ do
    command_ [] "quartus_cdb" ["--update_mif", projectName]
    command_ [] "quartus_asm" [projectName]

  "output_files" </> "*.map.rpt" %> \mapReport -> do
    let Just n = stripExtension "map.rpt" . takeFileName $ mapReport
    qsysSources <- quartusQuery $ QuartusQuery ["QSYS_FILE"]
    sources     <- quartusQuery $ QuartusQuery
      [ "QIP_FILE"
      , "VERILOG_FILE"
      , "VHDL_FILE"
      , "SYSTEMVERILOG_FILE"
      , "SOURCE_FILE"
      ]
    [part] <- quartusQuery $ QuartusQuery ["DEVICE"]
    let qips =
          (\bn -> bn </> "synthesis" </> bn <.> "qip")
            .   takeBaseName
            <$> qsysSources

    need (sources ++ qips)
    command_
      []
      "quartus_map"
      [ "--part"
      , part
      , "--read_settings_file=on"
      , "--write_settings_file=off"
      , n
      ]

  "output_files" </> "*.merge.rpt" %> \mergeReport -> do
    let Just n = stripExtension "merge.rpt" . takeFileName $ mergeReport
    need [mergeReport -<..> "map.rpt"]
    command_ [] "quartus_cdb" ["--merge", n]

  "output_files" </> "*.fit.rpt" %> \fitReport -> do
    let Just n = stripExtension "fit.rpt" . takeFileName $ fitReport
    need [fitReport -<..> "merge.rpt"]
    [part] <- quartusQuery $ QuartusQuery ["DEVICE"]
    need =<< quartusQuery (QuartusQuery ["SDC_FILE"])
    command_
      []
      "quartus_fit"
      [ "--part"
      , part
      , "--read_settings_file=on"
      , "--write_settings_file=off"
      , n
      ]

  "output_files" </> "*.sta.rpt" %> \staReport -> do
    let Just n = stripExtension ".sta.rpt" . takeFileName $ staReport
    need [staReport -<.> "fit.rpt"]
    command_ [] "quartus_sta" [n]

  "output_files" </> "*.sof" %> \sof -> do
    let Just n = stripExtension "sof" . takeFileName $ sof
    need [sof -<.> "fit.rpt"]
    command_ [] "quartus_asm" [n]

  "output_files" </> "*.rbf" %> \rbf -> do
    let sof = rbf -<.> "sof"
    need [sof]
    command_ []
             "quartus_cpf"
             ["--convert", "--option", "bitstream_compression=on", sof, rbf]

  "clash.qip" %> \qip -> do
    let manifests =
          [ (build </> clashLanguage </> m </> n </> n <.> "manifest", m)
          | (m, n) <- clashModules
          ]
    need (fst <$> manifests)
    clashHDL <- fmap concat . for manifests $ \(man, module_) -> do
      Manifest {..} <- read <$> readFile' man
      let clashTypes = fmap toLower module_ <> "_types"
      hasTypes <- doesFileExist clashTypes
      let clashSrcs = [ clashTypes | hasTypes ] ++ fmap T.unpack componentNames
          entity    = last clashSrcs
      pure
        [ build </> clashLanguage </> module_ </> entity </> c <.> "v"
        | c <- clashSrcs
        ]
    writeFile' qip $ unlines
      (   ("set_global_assignment -library \"top\" -name VERILOG_FILE " ++)
      <$> clashHDL
      )

sopcRules :: Rules ()
sopcRules = do
  build </> "*.dtbo" %> \dtbo -> do
    let dts = dtbo -<.> "dts"
    need [dts]
    command_
      []
      "dtc"
      [ "-Wno-interrupts_property"
      , "--symbols"
      , "--out-format"
      , "dtb"
      , "--out"
      , dtbo
      , dts
      ]

  build </> "*.dts" %> \dts -> do
    let n = takeBaseName dts
        sopcinfo    = qsysHPSSource <.> "sopcinfo"
    need [sopcinfo]
    command_
      []
      "sopc2dts"
      [ "--input"
      , sopcinfo
      , "--output"
      , dts
      , "--type"
      , "dts"
      , "--overlay-target"
      , "/soc/base-fpga-region"
      , "--pov"
      , "hps_0_bridges"
      , "--pov-type"
      , "overlay"
      , "--no-timestamp"
      , "--firmware-name"
      , n <.> "rbf"
      , "--verbose"
      , "--extra-components"
      , "sopc_components_extra.xml"
      ]
    command_
      []
      "sed"
      [ "-i"
      , unwords
        [ "s|/dts-v1/ /plugin/|/dts-v1/;\\n/plugin/|;"
        , "s|hps_0_arm_gic_0|intc|g"
        ]
      , dts
      ]

clashRules :: Rules ()
clashRules = do
  build </> "*.dep" %> \out -> do
    let bn      = takeBaseName out
        hs      = src </> bn <.> "hs"
        include = "-i" <> includeDir
    command_ []
             "clash"
             [include, "-M", "-dep-suffix", "", "-dep-makefile", out, hs]

  rule $ \(m :: M (Build / "verilog" / 1 / 2 / 2 <> ".manifest")) -> do
    let bn      = ref d1 m
        include = "-i" <> includeDir
        hdl     = "verilog"
        hs      = src </> bn <.> "hs"
        dep     = build </> bn <.> "dep"
    need [dep]
    needSomeMakefileDepends (".hs" `isSuffixOf`) dep
    command_ []
             "clash"
             [include, "-odir", build, "-outputdir", build, "--" <> hdl, hs]

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

(-<..>) :: FilePath -> String -> String
f -<..> s = (<.> s) . dropExtension . dropExtension $ f

-- | Depend on some specific inputs in a makefile
needSomeMakefileDepends :: (FilePath -> Bool) -> FilePath -> Action ()
needSomeMakefileDepends p makefile = do
  contents <- readFile' makefile
  let deps = snd =<< parseMakefile contents
  needed (filter p deps)

(.:) :: (b -> c) -> (a -> d -> b) -> a -> d -> c
(.:) = (.).(.)

rule :: CanMatch d => (Match d -> Action ()) -> Rules ()
rule = ((listToMaybe . matchAll . T.pack) ??>)

(??>) :: (FilePath -> Maybe a) -> (a -> Action ()) -> Rules ()
match ??> act = (isJust . match) ?> (act . fromJust . match)


----------------------------------------------------------------
-- Fancy path matching
--
-- Construct matches from 'Symbol's, the constructor of 'Pat' or the type
-- synonyms: '(:/)', '(:.)' and 'Ref'
--
-- Create matches with 'matchAll'
--
-- Consume matches with 'ref'
--
----------------------------------------------------------------

data Pat where
  (:<>) :: a -> b -> Pat
  Any   :: Pat
  Sub   :: Nat -> Pat -> Pat

type a / b = a <> "/" <> b
type a <> b = a :<> b

type family HasRef (r :: Nat) (m :: Pat) :: Constraint where
  HasRef r m = RefErr r m (r `Elem` Refs m)

type family RefErr (r :: Nat) (m :: Pat) (b :: Bool) :: Constraint where
  RefErr r m False =
    TypeError (ShowType r :<>: Text " is not a reference in " :<>: ShowType m)
  RefErr r m True = ()

type M = Match
data Match d where
  Match :: IntMap T.Text -> Match d

ref :: HasRef r d => SNat r -> Match d -> String
ref n (Match rs) =
  let i = snatToNum n
  in  fromMaybe
        (  error
        $  "Impossible: unable to find reference for "
        <> show i
        <> " in match!"
        )
        (T.unpack <$> Map.lookup i rs)

matchAll :: forall d a . CanMatch d => T.Text -> [Match d]
matchAll s =
  [ Match g
  | p <- fst (fullParses (parser (pure (getParser (Proxy @d)))) s)
  , Just g <- pure $ goodRefs p
  ]

-- | Are there any duplicate references?
goodRefs :: [(Int, T.Text)] -> Maybe (IntMap T.Text)
goodRefs =
  sequence . Map.fromListWith (\x y -> guard (x == y) >> x) . fmap (fmap Just)

type P r = Prod r T.Text Char [(Int, T.Text)]

class CanMatch p where
  type Refs p :: [Nat]
  getParser :: Proxy p -> P r

instance CanMatch Any where
  type Refs Any = '[]
  getParser _ = [] <$ some (satisfy (const True))

instance KnownSymbol s => CanMatch s where
  type Refs s = '[]
  getParser _ =
    let s = symbolVal (Proxy @s)
    in [] <$ list s

instance CanMatch (Sub n Any) => CanMatch n where
  type Refs n = Refs (Sub n Any)
  getParser _ = getParser (Proxy @(Sub n Any))

instance (CanMatch a, CanMatch b) => CanMatch (a :<> b) where
  type Refs (a :<> b) = Union (Refs a) (Refs b)
  getParser _ = do
    r1 <- getParser (Proxy @a)
    r2 <- getParser (Proxy @b)
    pure (r1 ++ r2)

instance (KnownNat n, CanMatch p) => CanMatch (Sub n p) where
  type Refs (Sub n p) = Union '[n] (Refs p)
  getParser _ = do
    (fst -> t) <- withToks (getParser (Proxy @p))
    pure [(snatToNum (SNat @n), T.pack t)]

withToks :: Prod r e t a -> Prod r e t ([t], a)
withToks = \case
  Pure a -> Pure ([], a)
  Terminal p c ->
    Terminal (\t -> (t, ) <$> p t) (biliftA2 (flip (:)) id <$> withToks c)
  -- NonTerminal :: !(r e t a) -> !(Prod r e t (a -> b)) -> Prod r e t b
  NonTerminal _ _ -> error "withToks: NonTerminal"
  Alts as c ->
    -- Alts (withToks <$> as) (((\(ts, f) (t, x) -> (t ++ ts, f x))) <$> withToks c)
    Alts (withToks <$> as) (biliftA2 (flip (++)) id <$> withToks c)
  Many a c -> Many
    (withToks a)
    ((\(t, f) (unzip -> (ts, xs)) -> (concat (ts ++ [t]), f xs)) <$> withToks c)
  Named p n -> Named (withToks p) n
