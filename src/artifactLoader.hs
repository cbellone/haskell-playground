import System.Environment
import System.IO
import System.Directory
import System.Time

data Package = Package { packageName :: String
		       , fileSize :: Int
		       , url :: String
		       , timestamp :: Int
		       } deriving (Show)

data Version = Version { number :: String
		       , files :: [Package]
		       } deriving (Show)

data Resource = Resource { resourceName :: String
			 , versions :: [Version]
} deriving (Show)
			 
data PathDescriptor = PathDescriptor { pathName :: String
				     , lastModification :: String
				     , size :: Int
} deriving (Show)


main = do
  args <- getArgs
  let path = args !! 0
  fileDescriptor <- loadAllDescriptors path
  return ()

loadAllDescriptors :: String -> IO [PathDescriptor]
loadAllDescriptors path = do
  files <- getDirectoryContents path
  mapM toDescriptor files
  
toDescriptor :: String -> IO PathDescriptor
toDescriptor filePath = do
  lastModification <- getLastModification filePath
  return $ PathDescriptor filePath lastModification 0

getLastModification :: String -> IO String
getLastModification filePath = do
  exists <- doesFileExist filePath
  if exists then
     getFileLastModification filePath
  else return "0"

getFileLastModification :: String -> IO String
getFileLastModification filePath = do
  lastModification <- getModificationTime filePath
  return $ show lastModification