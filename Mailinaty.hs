module Main where
import Network.HTTP
import Text.HTML.TagSoup
import Control.Monad ((>=>), replicateM)
import Data.List
import System.Random
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)


alphaNum = ['0' .. '9'] ++ ['a' .. 'z'] -- mailinator ignores case

addressLength = 20

getBody :: String -> IO String
getBody = simpleHTTP . getRequest >=> getResponseBody

genAddr :: String-> Int -> IO String
genAddr alphabet len = fmap (alphabet!!) `fmap` replicateM len (randomRIO (0, pred $ length alphabet))

alternateAddr :: [Tag String] -> Maybe String
alternateAddr page = fromTagText `fmap` find (\t -> isTagText t && isPrefixOf "M8R-" (fromTagText t)) page

main = do
    localPart <- genAddr alphaNum addressLength
    pageTags <- fmap parseTags $ getBody $ "http://mailinator.com/maildir.jsp?email=" ++ localPart

    case alternateAddr pageTags of
         Nothing             -> do
             hPutStrLn stderr "failed to parse inbox page"
             exitFailure
         Just alternateAddr' -> mapM_ (putStrLn . (++"@mailinator.com")) [localPart, alternateAddr']
