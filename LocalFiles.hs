module LocalFiles (parseAndGroupFiles, parseQuizFiles) where

import Distribution.Simple.Utils
import Chapter.Files
import Chapter.Types
import System.FilePath.Posix
import qualified Data.IntMap as Map
import TextUtil
import Control.Monad
import Quiz.Parser
--import Quiz.Types
import Data.Aeson
--import System.IO
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char (isDigit)

parseQuizFiles :: FilePath -> IO [ChapterFiles]
parseQuizFiles d = do
  let d' = addTrailingPathSeparator d
  paths <- fmap (map (d' ++)) (getDirectoryContentsRecursive d)
  rush' paths


parseAndGroupFiles :: FilePath -> IO [ChapterFiles]
parseAndGroupFiles d = do
  let d' = addTrailingPathSeparator d
  paths <- fmap (map (d' ++)) ( getDirectoryContentsRecursive d )
  rush paths

rush' :: [FilePath] -> IO [ChapterFiles]
rush' fp = do
  let qp = filter isQuiz fp
      qpJSON = map (`replaceExtension` "json") qp
  rtfToJSON fp
  let onlyQuizFiles = foldr insertFold Map.empty qpJSON 
      insertFold p m = let (n,cname) = numberAndName p
                           chap = Chapter n cname
                       in Map.insert n (ChapterFiles chap [p] []) m
  return $ map snd (Map.toList onlyQuizFiles)

rtfToJSON :: [FilePath] -> IO ()
rtfToJSON fp = do
  let qp = filter isQuiz fp
      qpTxt = map (`replaceExtension` "txt") qp
      qpJSON = map (`replaceExtension` "json") qp
  zipWithM_ rtfToTxt qp qpTxt
  zipWithM_ convertJSON qpTxt qpJSON

rush :: [FilePath] -> IO [ChapterFiles]
rush fp = do
  let qp = filter isQuiz fp
      qpJSON = map (`replaceExtension` "json") qp
      vp = filter isVideo fp
  print vp
  rtfToJSON fp
  let onlyQuizFiles = foldr insertFold Map.empty qpJSON 
      insertFold p m = let (n,cname) = numberAndName p
                           chap = Chapter n cname
                       in Map.insert n (ChapterFiles chap [p] []) m
      chapFiles = foldr insertVideo onlyQuizFiles vp
      insertVideo p = Map.update (addVid p) (numberOfVideo p)
      addVid p (ChapterFiles c q v) = Just $ ChapterFiles c q (p:v)
  return $ map snd (Map.toList chapFiles)

numberAndName :: FilePath -> (Int, String)
numberAndName f = (numberOfQuiz f, (tail . dropWhile isDigit . dropExtension . takeFileName) f)

numberOfQuiz :: FilePath -> Int
numberOfQuiz = read . takeWhile isDigit . takeFileName

numberOfVideo :: FilePath -> Int
numberOfVideo = 
  read . takeWhile isDigit . dropWhile (not . isDigit) . takeFileName

convertJSON :: FilePath -> FilePath -> IO ()
convertJSON from to = do
  let n = numberOfQuiz from
  c <- readFile from
  putStrLn $ "coverting " ++ from
  --print c
  let quiz = parse n c
      qjson = encode quiz
  BS.writeFile to qjson

isQuiz :: FilePath -> Bool
isQuiz fp = takeExtension fp == ".rtf" && head fn `elem` ['0'..'9']
  where fn = takeFileName fp

isVideo :: FilePath -> Bool
isVideo fp = (ext `elem` [".mov", ".m4v"]) && head fn == 'V'
  where fn = takeFileName fp
        ext = takeExtension fn
