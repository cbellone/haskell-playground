import System.Environment
import System.IO
import qualified System.Directory as Directory

data Package = Package { packageName :: String
		       , size :: Int
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
				     , children :: [PathDescriptor]
				     , lastModification :: Int
} deriving (Show)


main = do
  args <- getArgs
  let path = args !! 0
  traverseDirectory path


getRegularFiles :: [String] -> [String]
getRegularFiles files =
  filter isRegularFile files
    
getDirectories :: [String] -> [String]
getDirectories files =
  filter isDirectory files
  
isDirectory :: String -> Bool
isDirectory [] = False
isDirectory (_:"/") = True
isDirectory _ = False
  
isRegularFile :: String -> Bool
isRegularFile path = 
  not $ isDirectory path

traverseDirectory :: String -> IO [Resource]
traverseDirectory path = do
  children <- Directory.getDirectoryContents path
  return ((map toResource $ getRegularFiles children) ++ map traverseDirectory $ getDirectories children)
  
toResource :: String -> Resource
toResource path = Resource { resourceName = path, versions = []}