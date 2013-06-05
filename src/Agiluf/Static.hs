module Static where


import System.Cmd
import System.Directory (createDirectoryIfMissing)
import System.Exit
import System.FilePath (joinPath)


import Definition


-- | FIXME: Unix/Mac only.
copyDirectory :: FilePath -> FilePath -> IO ExitCode
copyDirectory src dest = system $ "cp -r " ++ src ++ " " ++ dest


-- | Create initial output directories.
createOutputDirectories :: FilePath -> IO ()
createOutputDirectories output_directory = do
    createDirectoryIfMissing False output_directory
    createDirectoryIfMissing False (joinPath [output_directory, "tags"])


-- | Copy statoc files over to the output directory
copyStaticFiles :: FilePath -> FilePath -> IO ExitCode
copyStaticFiles static_directory output_directory = copyDirectory static_directory output_directory
