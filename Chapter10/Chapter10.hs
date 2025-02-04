import Data.IORef
import qualified Data.Text as Text

characterCounter :: FilePath -> IO (Text.Text -> Int)
characterCounter filePath = do
  haystack <- TextIO.readFile filePath
  pure $ \needle ->
    Text.count needle haystack
    + Text.count needle (Text.pack filePath)

someExample :: FilePath -> IO (IORef Int)
someExample path = do
  countRef <- newIORef 0
  let
    somePath = complicatedPathFinding path
  counter <- characterCounter somePath
  writeIORef countRef (counter " ")
  pure countRef
  where
    -- You can use any function you'd like here
    complicatedPathFinding = id
