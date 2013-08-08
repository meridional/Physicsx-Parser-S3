{-# LANGUAGE NoMonomorphismRestriction #-}
module Quiz.Parser (parse) where

import Quiz.Types
--import Data.List.Split (splitOn)
import Data.Char (ord, isDigit)
--import System.IO

parse :: Int -> String -> Quiz
parse quizNum quizContent = Quiz t quizNum qs
  where t:ls = map strip . filter isNotEmpty . lines $ quizContent
        isNotEmpty = any (`notElem` " \t")
        qs = groupByQuestion ls

groupByQuestion :: [String] -> [Question]
groupByQuestion [] = []
groupByQuestion (h:rest)
  | (head . clean) h `notElem` ['0'..'9'] = []
  | otherwise = Question n b cs sol diff : groupByQuestion r
    where (nStr, bStr) = splitOn "." h
          n = read nStr
          b = clean bStr
          (cs, ss:ds:r) = groupByChoice rest
          sol = (clean . snd . splitOn ":")  ss
          diff = case clean . snd . splitOn ":" $ ds of
                      [] -> 1
                      s -> if isDigit (head s) then read s else 1

groupByChoice :: [String] -> ([Choice], [String])
groupByChoice = f [] 
  where f cs (h:rest)
          | head h == 'A' = (cs', rest)
          | otherwise = f (cs ++ [n]) rest
            where n = clean . snd . splitOn "." $ h
                  ans = (ord . head . clean . snd . splitOn ":") h
                    - ord 'a' + 1
                  op = map (\x -> if x == ans 
                                     then Correct 
                                     else Incorrect) [1..]
                  cs' = zipWith ($) op cs 


-- strip, get rid of trailing and leading spaces
strip :: String -> String
strip = reverse . clean . reverse . clean

clean :: String -> String
clean = dropWhile (`elem` " \t") 

splitOn :: String -> String -> (String, String)
splitOn _ [] = ([], [])
splitOn p (h:r)
  | h `elem` p = ([], r)
  | otherwise = (h:b, e)
      where (b,e) = splitOn p r
