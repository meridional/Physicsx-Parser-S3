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
parseAndUpload s
  | null s = putStrLn "Usage: ./physicsx-s3 [Content-directory]"
  | otherwise = do
      let d = head s
      chapFiles <- parseAndGroupFiles d
      let fm = concatMap chapterFiles2S3FileMapping chapFiles
      void $ batchUpload fm 
      

chapterFiles2S3FileMapping :: ChapterFiles -> [S3FileMapping]
chapterFiles2S3FileMapping (ChapterFiles m qf vf) =
  let prefix = chap2String m ++ "/"
      fs = qf ++ vf
      makeFileMapping f = (prefix ++ takeFileName f, f)
  in map makeFileMapping fs
