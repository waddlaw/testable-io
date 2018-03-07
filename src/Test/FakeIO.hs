module Test.FakeIO (execFakeIO, evalFakeIO, runFakeIO) where

import           Control.DeepSeq   (deepseq)
import           Control.Exception (bracket)
import           GHC.IO.Handle     (hDuplicate, hDuplicateTo)
import           System.Directory  (getTemporaryDirectory, removeFile)
import           System.IO         (Handle, SeekMode (AbsoluteSeek), hClose,
                                    hFlush, hGetBuffering, hGetContents,
                                    hPutStr, hSeek, hSetBuffering, openTempFile,
                                    stdin, stdout)

evalFakeIO :: IO a -> String -> IO a
evalFakeIO act inp = fst <$> runFakeIO act inp

execFakeIO :: IO a -> String -> IO String
execFakeIO act inp = snd <$> runFakeIO act inp

-- | Perform an action with explicit input\/output connected to
-- @`stdin`@\/@`stdout`@
runFakeIO
  :: IO a            -- ^ Action
  -> String          -- ^ Input to send to @stdin@
  -> IO (a, String)  -- ^ Result from @action@ and @stdout@
runFakeIO act inp = do
  tmpDir <- getTemporaryDirectory
  withTempFile tmpDir "fakeInput" $ \(_inpFile, inpH) ->
    withTempFile tmpDir "fakeOutput" $ \(_outFile, outH) ->
      withRedirect outH stdout $ withRedirect inpH stdin $ do
        hPutStr inpH inp
        hSeek inpH AbsoluteSeek 0
        res <- act
        hFlush stdout
        hSeek outH AbsoluteSeek 0
        str <- hGetContents outH
        str `deepseq` return (res, str)

-- | Perform an action with a redirected handle
withRedirect
  :: Handle  -- ^ Shadowing handle
  -> Handle  -- ^ Shadowed handle
  -> IO a    -- ^ Action in which the redirect takes place
  -> IO a
withRedirect new old act = bracket
  (do
    buffering <- hGetBuffering old
    dupH      <- hDuplicate old
    hDuplicateTo new old
    return (dupH, buffering)
  )
  (\(dupH, buffering) -> do
    hDuplicateTo  dupH old
    hSetBuffering old  buffering
    hClose dupH
  )
  (const act)

-- | Perform an action that with access to a temporary file. The file is removed
-- after the action is completed.
withTempFile
  :: FilePath                      -- ^ Path to directory for temporary file
  -> String                        -- ^ Base name for temporary file
  -> ((FilePath, Handle) -> IO a)  -- ^ Action
  -> IO a
withTempFile tmpDir base =
  bracket (openTempFile tmpDir base) (\(file, h) -> hClose h >> removeFile file)
