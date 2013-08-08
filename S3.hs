module S3 (batchUpload, S3FileMapping, awsConn) where

import Network.AWS.S3Object
import Network.AWS.AWSConnection
import Data.ByteString.Lazy (hGetContents)
import Network.AWS.AWSResult
import System.IO hiding (hGetContents)
import Data.Maybe

awsConn :: IO AWSConnection
awsConn = do
    accessID:secretKey:_ <- fmap lines (readFile "S3.config")
    return $ amazonS3Connection  accessID secretKey

-- mapping from s3 file name to local file path
type S3FileMapping = (String, FilePath)

batchUpload :: [S3FileMapping] -> IO [AWSResult ()]
batchUpload l = do
  conn <- fmap fromJust amazonS3ConnectionFromEnv
  mapM (\x -> do obj <- makeObject x 
                 explain x
                 sendObject conn obj) l


explain :: S3FileMapping -> IO ()
explain (s, _) = putStrLn $ "Uploading " ++ s ++ " ..."

makeObject :: S3FileMapping -> IO S3Object
makeObject (name, path) = do
  h <- openFile path ReadMode
  content <- hGetContents h
  bucketName <- readFile "S3.config"
  return $ S3Object bucketName name "" [] content
