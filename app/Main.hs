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
import qualified Data.ByteString as BS (length)
import Data.ByteString.Internal as BS
import qualified Data.HashMap.Lazy as HM
import Data.Time
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Encoding
import Data.Maybe (fromJust)

import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Client.Conduit as C
import Network.HTTP.Conduit
import Network.HTTP.Simple

import Servant.API.BasicAuth
import Servant.Client

import Warwick.Config
import Warwick.Tabula
import Warwick.Tabula.Coursework
import Warwick.Tabula.Attachment
import Warwick.Tabula.Member
import Warwick.Tabula.Relationship
import Warwick.Tabula.StudentAssignment

import Warwick.Tabula.Config

import System.IO
import System.Directory
import System.FilePath
import System.Console.AsciiProgress

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
downloadSubmissions :: ModuleCode -> Assignment -> Bool -> Bool -> Warwick ()
downloadSubmissions mc cwk up pdf = do
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
    forM_ (HM.toList tabulaData) $ \(sid, subm) ->
            case subm of
                Nothing  -> liftIO $ putStrLn $ sid ++ " has not submitted anything."
                Just sub -> forM_ (submissionAttachments sub) $ \att -> do
                    let
                        subDir = dir </> sid
                        subFile = subDir </> attachmentFilename att
                        subExt = takeExtension (attachmentFilename att)
                    if ((subExt /= ".pdf") && pdf) then do
                        liftIO $ putStrLn ("Skipping submission for " ++ sid ++ " (" ++ attachmentFilename att ++ " is not a PDF) ... ")
                    else do
                        liftIO $ do
                            putStrLn $ "Downloading submission for " ++ sid ++ " ... "
                            unlessM (doesDirectoryExist subDir) $ createDirectory subDir
                        downloadSubmissionWithCallbacks
                            sid
                            mc
                            (assignmentID cwk)
                            (submissionID sub)
                            (attachmentFilename att)
                            subFile
                            Callbacks {
                                onWrapper = displayConsoleRegions,
                                onLength = \l -> newProgressBar def {
                                                pgTotal = fromIntegral l,
                                                pgWidth = 100,
                                                pgOnCompletion = Just "Downloaded."
                                           },
                                onUpdate = \pb bs -> tickN pb (BS.length bs),
                                onComplete = complete
                            }
                        liftIO $ do
                            --putStr "Done. "
                            when up $ unpackSubmission subDir subFile
                            --putStrLn ""

selectAssignment :: APIConfig -> ModuleCode -> String -> Bool -> Bool -> IO ()
selectAssignment cfg mc ay up pdf = do
    r <- withTabula Live cfg $ do
        TabulaOK {..} <- listAssignments mc (Just ay)

        case tabulaData of
            []  -> liftIO $ putStrLn "There are no assignments for this module."
            [x] -> downloadSubmissions mc x up pdf
            xs  -> do
                idx <- liftIO $ do
                    mapM_ ppAssignment $ zip tabulaData [0..]
                    promptID (length tabulaData)

                downloadSubmissions mc (xs !! idx) up pdf

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
    mcfg <- readAPIConfig "tabula.json"
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
                    selectAssignment cfg cfgModuleCode cfgAcademicYear cfgUnpack cfgOnlyPDF
                Tutees -> do
                    putStr "User ID: "
                    uid <- getLine

                    r <- withTabula Live cfg $ do
                        TabulaOK {..} <- listRelationships uid

                        rs <- forM (relationshipsOfType "personalTutor" tabulaData) $ \r -> do
                            forM (relationshipStudents r) $ \e -> do
                                TabulaOK {..} <- retrieveMember (relationshipEntryUniversityID e) ["member.fullName"]
                                liftIO $ putStrLn (fromJust (memberFullName tabulaData) ++ "(" ++ relationshipEntryUniversityID e ++ ")")
                                TabulaAssignmentOK {..} <- personAssignments (relationshipEntryUniversityID e) (Just "18/19")
                                let hls = concernAssignments "18/19" $ historicAssignments tabulaAssignmentData
                                    els = concernAssignments "18/19" $ enrolledAssignments tabulaAssignmentData

                                return (memberFullName tabulaData, length els, length hls)
                        liftIO $ mapM_ print rs
                        return ()

                    print r

concernAssignments :: String -> [StudentAssignment] -> [StudentAssignment]
concernAssignments ay = filter $ \StudentAssignment{..} -> case studentAssignmentSubmission of
    -- if there is no submission, would it be late now?
    Nothing -> studentAssignmentLate && studentAssignmentAcademicYear == ay
    -- otherwise, if there is a submission, was the submission late?
    Just (StudentAssignmentSubmission {..}) ->
        studentAssignmentSubmissionLate && studentAssignmentAcademicYear == ay

--------------------------------------------------------------------------------
