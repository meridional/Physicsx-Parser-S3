{-# LANGUAGE OverloadedStrings #-}
module Quiz.JSON (toJSON) where

import Data.Aeson
import Quiz.Types

instance ToJSON Quiz where
  toJSON (Quiz n num q) = object ["name" .= n, "quiz_no" .= num, 
    "questions" .= q]

instance ToJSON Question where
  toJSON (Question num b cs sol diff) = object ["number" .= num,
    "body" .= b, "choices" .= cs, "sol" .= sol, "diff" .= diff]
  
instance ToJSON Choice where
  toJSON (Correct s) = object ["true" .= True, "text" .= s]
  toJSON (Incorrect s) = object ["true" .= False, "text" .= s]
