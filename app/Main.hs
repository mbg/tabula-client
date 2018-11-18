--------------------------------------------------------------------------------
-- Haskell bindings for the Tabula API                                        --
-- Copyright 2018 Michael B. Gale (m.gale@warwick.ac.uk)                      --
--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import Prelude hiding (unzip)

import Text.Read (readMaybe)

import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class

import Data.Conduit
import Data.Conduit.Binary hiding (mapM_)
import Data.ByteString.Internal as BS
import qualified Data.HashMap.Lazy as HM
import Data.Time
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Encoding

import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Client.Conduit as C
import Network.HTTP.Conduit
import Network.HTTP.Simple

import Servant.API.BasicAuth
import Servant.Client

import Warwick.Tabula
import Warwick.Tabula.Coursework
import Warwick.Tabula.Attachment
import Warwick.Tabula.Relationship

import Warwick.Tabula.Config

import System.IO
import System.Directory
import System.FilePath

import qualified Shelly as S

import Config

--------------------------------------------------------------------------------

untar :: FilePath -> FilePath -> IO ()
untar zf dir = S.shelly $ S.silently $ S.errExit False $ do
    r <- S.run "tar" ["-xf", T.pack zf, "-C", T.pack dir]
    e <- S.lastExitCode
    d <- S.lastStderr

    S.liftIO $ do
        if e == 0 then do
            --setSGR [SetColor Foreground Dull Green]
            putStr "Unpacked."
            --setSGR [Reset]
        else do
            --setSGR [SetColor Foreground Vivid Red]
            putStr $ "Unpack failed (" ++ show e ++ ")."
            --setSGR [Reset]

unzip :: FilePath -> FilePath -> IO ()
unzip zf dir = S.shelly $ S.silently $ S.errExit False $ do
    r <- S.run "unzip" [T.pack zf, "-d", T.pack dir]
    e <- S.lastExitCode
    d <- S.lastStderr

    S.liftIO $ do
        if e == 0 then do
            --setSGR [SetColor Foreground Dull Green]
            putStr "Unzipped."
            --setSGR [Reset]
        else do
            --setSGR [SetColor Foreground Vivid Red]
            putStr $ "Unzip failed (" ++ show e ++ ")."
            --setSGR [Reset]

unpackSubmission :: FilePath -> FilePath -> IO ()
unpackSubmission dir file = do
    case takeExtension file of
            ".zip" -> do
                --putStr $ "Unzipping " ++ file ++ "... "
                unzip file dir
            ".gz" -> do
                --putStr $ "Unpacking " ++ file ++ "... "
                untar file dir
            ext -> putStr $ "Unpack not supported."

--------------------------------------------------------------------------------

-- | An error message which is displayed when the configuration is invalid.
configError :: [String]
configError =
    [ "There is no configuration file or the configuration is invalid."
    , ""
    , "The tabula.json file should contain the following:"
    , "{"
    , "    \"username\": \"YOUR_USERNAME\","
    , "    \"password\": \"YOUR_PASSWORD\""
    , "}"
    ]

-- | Pretty-prints an assignment for the assignment selection menu.
ppAssignment :: (Assignment, Int) -> IO ()
ppAssignment (Assignment {..}, idx) = do
    putStr (show idx)
    putStr ". "
    putStr assignmentName
    when assignmentArchived $ putStr " (Archived)"
    putStr " - "
    putStr (show assignmentSubmissions)
    putStrLn " submission(s)"

-- | Prompts the user to select a coursework within the range.
promptID :: Int -> IO Int
promptID len = do
    putStr "Select coursework [0.."
    putStr (show $ len-1)
    putStr "]: "

    r <- readMaybe <$> getLine

    case r of
        Nothing -> do
            putStrLn "Not a valid integer."
            promptID len
        Just idx | idx >= len -> do
                    putStrLn "Out of range."
                    promptID len
                 | otherwise ->
                    return idx

-- | Downloads all coursework submissions for an assignment.
downloadSubmissions :: ModuleCode -> Assignment -> Bool -> Tabula ()
downloadSubmissions mc cwk up = do
    let aid = assignmentID cwk
        anm = assignmentName cwk
        dir = "./submissions-" ++ show aid

    -- get a list of all submissions for the assignment
    TabulaOK {..} <- listSubmissions mc aid

    -- create a directory for the submissions, if there isn't one yet
    liftIO $ do
        putStrLn $ "Downloading submissions for " ++ anm ++ "..."

        -- create the directory if it doesn't exist
        unlessM (doesDirectoryExist dir) $ createDirectory dir

    -- download every submission
    forM_ (HM.toList tabulaData) $ \(sid, subm) -> do
            case subm of
                Nothing  -> liftIO $ putStrLn $ sid ++ " has not submitted anything."
                Just sub -> forM_ (submissionAttachments sub) $ \att -> do
                    let
                        subDir = dir </> sid
                        subFile = subDir </> attachmentFilename att
                    liftIO $ do
                        putStr $ "Downloading submission for " ++ sid ++ " ... "
                        unlessM (doesDirectoryExist subDir) $ createDirectory subDir
                    downloadSubmission sid mc (assignmentID cwk) sub (attachmentFilename att) subFile
                    liftIO $ do
                        putStr "Done. "
                        when up $ unpackSubmission subDir subFile
                        putStrLn ""

selectAssignment :: TabulaConfig -> ModuleCode -> Bool -> IO ()
selectAssignment cfg mc up = do
    r <- withTabula Live cfg $ do
        TabulaOK {..} <- listAssignments mc (Just "18/19")

        case tabulaData of
            []  -> liftIO $ putStrLn "There are no assignments for this module."
            [x] -> downloadSubmissions mc x up
            xs  -> do
                idx <- liftIO $ do
                    mapM_ ppAssignment $ zip tabulaData [0..]
                    promptID (length tabulaData)

                downloadSubmissions mc (xs !! idx) up

    -- handle errors
    case r of
        Left err -> putStrLn $ "Communication error:\n" ++ show err
        Right r -> putStrLn "Program ran successfully."

-- | The main entry point to this program.
main :: IO ()
main = do
    -- disable buffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin LineBuffering

    -- read the configuration (username and password) from a file
    mcfg <- readTabulaConfig "tabula.json"
    case mcfg of
        Nothing  -> mapM_ putStrLn configError
        Just cfg -> do
            -- read the command-line options
            opts <- parseCmdLineArgs

            --putStr "Module code: "
            --mc <- getLine
            --putStr "Academic year (yy/yy): "
            --yr <- return "18/19"

            case opts of
                Help -> putStrLn "help"
                DownloadSubmissions {..} ->
                    selectAssignment cfg cfgModuleCode cfgUnpack
                Tutees -> do
                    putStr "User ID: "
                    uid <- getLine
                    
                    r <- withTabula Live cfg $ do


                        TabulaOK {..} <- listRelationships uid

                        return tabulaData
                    print r



--------------------------------------------------------------------------------
