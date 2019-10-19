--------------------------------------------------------------------------------
-- Command-line utility for Tabula                                            --
-- Copyright 2018 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Config (
    AppConfig(..),

    parseCmdLineArgs
) where

--------------------------------------------------------------------------------

import qualified Data.ByteString.Internal as BS
import Data.Monoid

import Options.Applicative

import Warwick.Tabula.Types

--------------------------------------------------------------------------------

-- | Represents command-line c
data AppConfig
    = Help
    | DownloadSubmissions {
        cfgModuleCode   :: ModuleCode,
        cfgAcademicYear :: String,
        cfgUnpack       :: Bool,
        cfgOnlyPDF      :: Bool
    }
    | Tutees {}

--------------------------------------------------------------------------------

mc :: ReadM ModuleCode
mc = ModuleCode . BS.packChars <$> str

downloadSubmissionsP :: Parser AppConfig
downloadSubmissionsP =
    DownloadSubmissions <$> argument mc ( metavar "MODULE"
                                       <> help "The module code (e.g. cs141)"
                                        )
                        <*> strOption (long "year" <> help "The academic year")
                        <*> switch ( long "unpack"
                                  <> help "Unpack submissions automatically."
                                   )
                        <*> switch (long "only-pdf"
                                  <> help "Only download PDFs")

tuteesP :: Parser AppConfig
tuteesP = pure Tutees

appConfigP :: Parser AppConfig
appConfigP =
    subparser (command "download" (info downloadSubmissionsP (progDesc "Download coursework submissions."))
            <> command "tutees" (info tuteesP (progDesc "View information about tutees"))
            <> command "help" (info (pure Help) (progDesc "Lists commands supported by this program")))

opts :: ParserInfo AppConfig
opts = info (appConfigP <**> helper) idm

parseCmdLineArgs :: IO AppConfig
parseCmdLineArgs = execParser opts

--------------------------------------------------------------------------------
