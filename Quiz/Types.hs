{-# LANGUAGE OverloadedStrings #-}
module Quiz.Types where
import Data.Aeson

data Quiz = Quiz {
  name :: String,
  quizNumber :: Int,
  questions :: [Question]
  } deriving (Show)

data Question = Question {
  questionNumber :: Int,
  body :: String,
  choices :: [Choice],
  solution :: String,
  difficulty :: Int
  } deriving (Show)

data Choice = Correct String | Incorrect String deriving (Show)

-- JSON support
instance ToJSON Quiz where
  toJSON (Quiz n num q) = object ["name" .= n, "quiz_no" .= num, 
    "questions" .= q]

instance ToJSON Question where
  toJSON (Question num b cs sol diff) = object ["number" .= num,
    "body" .= b, "choices" .= cs, "sol" .= sol, "diff" .= diff]
  
instance ToJSON Choice where
  toJSON (Correct s) = object ["true" .= True, "text" .= s]
  toJSON (Incorrect s) = object ["true" .= False, "text" .= s]
