#!/usr/bin/env runhaskell

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Make
  ( main
  ) where

import           Control.DeepSeq
import           Control.Monad
import           Data.Binary
import           Data.Hashable
import           Data.List
import           Data.Typeable
import           Development.Shake
import           Development.Shake.FilePath
import           Development.Shake.Util

build, src, includeDir, rbfFile, dtboFile :: FilePath
build = "build"
src = "src"
includeDir = src
rbfFile = "output_files" </> "test.rbf"
dtboFile = build </> "test.dtbo"

qsysHPSSource :: String
qsysHPSSource = "top"

projectName :: String
projectName = "test"

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = build } $ do
  want [rbfFile, dtboFile]

  phony "timing" $ need ["output_files" </> projectName <.> "sta.rpt"]

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
  "*/synthesis/*.qip" %> \qip -> do
    let n = takeBaseName qip
    need [n <.> "qsys"]
    command_ [] "qsys-generate" ["--synthesis=verilog", n <.> "qsys"]

  "*.sopcinfo" %> \sopcinfo -> do
    let n = takeBaseName sopcinfo
    need [n </> "synthesis" </> n <.> "qip"]

newtype QuartusQuery = QuartusQuery [String]
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

type instance RuleResult QuartusQuery = [FilePath]

quartusRules :: Rules ()
quartusRules = do
  -- Query global assignments in quartus
  quartusQuery <- addOracleCache $ \(QuartusQuery qs) -> withTempDir $ \d -> do
    need [projectName <.> "qsf"]
    let tcl = d </> "get-source.tcl"
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
    let filterInfo = filter (not . ("Info" `isInfixOf`))
    pure (filterInfo . lines $ out)

  "output_files" </> "*.map.rpt" %> \mapReport -> do
    let Just projectName = stripExtension "map.rpt" . takeFileName $ mapReport
    sources <- quartusQuery $ QuartusQuery
      [ "QIP_FILE"
      , "VERILOG_FILE"
      , "VHDL_FILE"
      , "SYSTEMVERILOG_FILE"
      , "SOURCE_FILE"
      ]
    [part] <- quartusQuery $ QuartusQuery ["DEVICE"]

    need sources
    command_
      []
      "quartus_map"
      [ "--part"
      , part
      , "--read_settings_file=on"
      , "--write_settings_file=off"
      , projectName
      ]

  "output_files" </> "*.merge.rpt" %> \mergeReport -> do
    let Just projectName =
          stripExtension "merge.rpt" . takeFileName $ mergeReport
    need [mergeReport -<..> "map.rpt"]
    command_ [] "quartus_cdb" ["--merge", projectName]

  "output_files" </> "*.fit.rpt" %> \fitReport -> do
    let Just projectName = stripExtension "fit.rpt" . takeFileName $ fitReport
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
      , projectName
      ]

  "output_files" </> "*.sta.rpt" %> \staReport -> do
    let Just projectName = stripExtension ".sta.rpt" . takeFileName $ staReport
    need [staReport -<.> "fit.rpt"]
    command_ [] "quartus_sta" [projectName]

  "output_files" </> "*.sof" %> \sof -> do
    let Just projectName = stripExtension "sof" . takeFileName $ sof
    need [sof -<.> "fit.rpt"]
    command_ [] "quartus_asm" [projectName]

  "output_files" </> "*.rbf" %> \rbf -> do
    let sof = rbf -<.> "sof"
    need [sof]
    command_ []
             "quartus_cpf"
             ["--convert", "--option", "bitstream_compression=on", sof, rbf]

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
    let projectName = takeBaseName dts
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
      , projectName <.> "rbf"
      , "--verbose"
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
    cmd_ "clash" include "-M" "-dep-suffix" [""] "-dep-makefile" out hs

  -- Writes a list of HDL files, Clash can compile several files
  -- at once, amortizing the overhead of reading primitives
  batch
    maxBound
    ((build </> "*.listing") %>)
    (\out -> do
      let bn  = takeBaseName out
          dep = build </> bn <.> "dep"
      needSomeMakefileDepends (".hs" `isSuffixOf`) dep
      pure out
    )
    (\outs -> do
      let bns = takeBaseName <$> outs
      vss <- compileWithClash bns
      zipWithM_ writeFile' outs (show <$> vss)
    )

-- | Given the name of some Haskell modules, compile them with Clash and write
-- the output Verilog files to a ".listing" file in the build directory.
--
-- TODO: Ignore testbench hdl files
compileWithClash :: [String] -> Action [[FilePath]]
compileWithClash basenames = do
  let hss     = (\b -> src </> b <.> "hs") <$> basenames
      include = "-i" <> includeDir
      hdl     = "verilog"
  cmd_ "clash" include "-odir" build "-outputdir" build ("--" <> hdl) hss
  vs <- traverse (\b -> getDirectoryFiles "" [build </> hdl </> b <> "//*.v"])
                 basenames
  pure vs

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
