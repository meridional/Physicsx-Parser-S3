module Chapter.Files where

import Chapter.Types

data ChapterFiles = ChapterFiles {
  meta :: Chapter,
  quizFiles :: [FilePath],
  videoFiles :: [FilePath]
  }
