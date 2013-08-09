module Main where

import System.Environment
import S3
import Chapter.Files
import Chapter.Types
import LocalFiles
import System.FilePath.Posix
import Control.Monad (void)

main :: IO ()
main = do
  args <- getArgs
  parseAndUpload args

parseAndUpload :: [String] -> IO ()
parseAndUpload [s] = do
      chapFiles <- parseAndGroupFiles s
      let fm = concatMap chapterFiles2S3FileMapping chapFiles
      void $ batchUpload fm 
parseAndUpload ["-q",s] = do
      chapFiles <- parseQuizFiles s
      let fm = concatMap chapterFiles2S3FileMapping chapFiles
      void $ batchUpload fm 
parseAndUpload _ = putStrLn quickHelp

quickHelp :: String
quickHelp = "Usage: ./physicsx-s3 [Content-directory]\n" ++
  "\'-q [Content-directory]\' to only parse quiz files"
      
chapterFiles2S3FileMapping :: ChapterFiles -> [S3FileMapping]
chapterFiles2S3FileMapping (ChapterFiles m qf vf) =
  let prefix = chap2String m ++ "/"
      fs = qf ++ vf
      makeFileMapping f = (prefix ++ takeFileName f, f)
  in map makeFileMapping fs
