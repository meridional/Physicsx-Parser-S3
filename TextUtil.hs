module TextUtil (rtfToTxt, rtfToTxt') where


import System.Process (system)
import System.Exit (ExitCode)
import System.FilePath.Posix

rtfToTxt :: FilePath -> FilePath -> IO ExitCode
rtfToTxt x y = system $ convertCMD x y
  where
    convertCMD from to = "textutil -convert txt \"" ++ from 
      ++ "\" -output \"" ++ to ++ "\""

rtfToTxt' :: FilePath -> IO ExitCode
rtfToTxt' x = rtfToTxt x (replaceExtension x "txt")
