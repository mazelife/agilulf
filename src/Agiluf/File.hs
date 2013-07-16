module File where


import Control.Applicative
import Data.Configurator
import Data.Configurator.Types as C
import qualified Data.ByteString as DB
import qualified Data.ByteString.Char8 as CH
import qualified Data.ByteString.Lazy as LZ
import Data.Text (pack)
import System.Cmd
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.Environment (getArgs)
import System.Exit
import System.FilePath (joinPath)
import Text.Hastache
import Text.Hastache.Context


import Config (post_publish_command)
import Definition



{--

    Useful utilities for doing filesystems tasks and checks.

--}


-- | Verify the file structure of the specified (in command-line arg) blog directory
-- | and retrieve create a Configurator object containing blog settings.
getConfig :: FilePath -> IO (String, C.Config)
getConfig blog_directory = do
    _checkDirPath "" "blog directory"
    checkFilePath blog_directory "config" "configuration"
    _checkDirPath "entries" "blog entries directory"
    _checkDirPath "templates" "templates directory"
    conf <- readConfigFile
    return (blog_directory, conf)
    where _checkDirPath = checkDirPath blog_directory
          config_path = joinPath [blog_directory, "config"]
          readConfigFile = load [(Required config_path)]


-- | Does the specified directory exist within the blog root directory?
-- | Error-out if it does not with a message saying it's missing.
checkDirPath :: FilePath -> FilePath -> String -> IO Bool
checkDirPath root dir name = do
    present <- doesDirectoryExist path
    if present then return True else error ("No " ++ name ++ " found at " ++ path ++ ".")
    where path = joinPath [root, dir]


-- | Does the specified file exist within the blog root directory?
-- | Error-out if it does not with a message saying it's missing.
checkFilePath :: FilePath -> FilePath -> String -> IO Bool
checkFilePath root dir name = do
    present <- doesFileExist path
    if present then return True else error ("No " ++ name ++ " file found at " ++ path ++ ".")
    where path = joinPath [root, dir]



-- | Get the filepath to the blog from the command line arguments.
getBlogPath :: IO FilePath
getBlogPath = do
    args <- getArgs
    if null args
        then error "You must provide a filepath to the blog directory."
        else return $ head args


-- | FIXME: Unix/Mac only.
copyDirectory :: FilePath -> FilePath -> IO ExitCode
copyDirectory src dest = system $ "cp -r " ++ src ++ " " ++ dest


-- | Create initial output directories.
createOutputDirectories :: IO Blog -> IO ()
createOutputDirectories blog = do
    blogConf <- blog
    let output_directory = outputDirectory blogConf
    createDirectoryIfMissing False output_directory
    createDirectoryIfMissing False (joinPath [output_directory, "tags"])


-- | Copy static files over to the output directory
copyStaticFiles :: IO Blog -> IO ExitCode
copyStaticFiles blog = do
    blogConf <- blog
    copyDirectory (staticDirectory blogConf) (outputDirectory blogConf)


postPublishHook :: IO (String, C.Config) -> IO Blog -> IO ()
postPublishHook conf blog = conf >>= getCommand >>= system >> return ()

getCommand :: (String, C.Config) -> IO String
getCommand (base_directory, conf) = liftA addEnv (lookupDefault post_publish_command conf (pack "post_publish_command"))
    where addEnv command = "( export ROOT=\"" ++ base_directory ++ "\"; " ++ command ++ " )"
