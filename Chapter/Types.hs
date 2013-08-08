module Chapter.Types where

data Chapter = Chapter {
  chapterNumber :: Int,
  chapterName :: String
  }

chap2String :: Chapter -> String
chap2String (Chapter n name) = show n ++ "." ++ name
