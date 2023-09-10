appendFile :: FilePath -> String -> IO ()
fixIO :: (a -> IO a) -> IO a
getChar :: IO Char
getContents :: IO String
getContents' :: IO String
getLine :: IO String
hPrint :: Show a => Handle -> a -> IO ()
hReady :: Handle -> IO Bool
interact :: (String -> String) -> IO ()
localeEncoding :: TextEncoding
openBinaryTempFile :: FilePath -> String -> IO (FilePath, Handle)
openBinaryTempFileWithDefaultPermissions :: FilePath -> String -> IO (FilePath, Handle)
openTempFile :: FilePath -> String -> IO (FilePath, Handle)
openTempFileWithDefaultPermissions :: FilePath -> String -> IO (FilePath, Handle)
print :: Show a => a -> IO ()
putChar :: Char -> IO ()
putStr :: String -> IO ()
putStrLn :: String -> IO ()
readFile :: FilePath -> IO String
readFile' :: FilePath -> IO String
readIO :: Read a => String -> IO a
readLn :: Read a => IO a
writeFile :: FilePath -> String -> IO ()
type BufferMode :: *
data BufferMode
  | NoBuffering
  | LineBuffering
  | BlockBuffering (Maybe Int)
type FilePath :: *
type FilePath = String
type Handle :: *
data Handle
  | GHC.IO.Handle.Types.FileHandle FilePath {-# UNPACK #-}(GHC.MVar.MVar GHC.IO.Handle.Types.Handle__)
  | GHC.IO.Handle.Types.DuplexHandle FilePath {-# UNPACK #-}(GHC.MVar.MVar GHC.IO.Handle.Types.Handle__) {-# UNPACK #-}(GHC.MVar.MVar GHC.IO.Handle.Types.Handle__)
type HandlePosn :: *
data HandlePosn = GHC.IO.Handle.HandlePosn Handle GHC.IO.Handle.HandlePosition
type IO :: * -> *
newtype IO a = GHC.Types.IO (GHC.Prim.State# GHC.Prim.RealWorld -> (# GHC.Prim.State# GHC.Prim.RealWorld, a #))
type IOMode :: *
data IOMode
  | ReadMode
  | WriteMode
  | AppendMode
  | ReadWriteMode
type Newline :: *
data Newline
  | LF
  | CRLF
type NewlineMode :: *
data NewlineMode = NewlineMode
  inputNL :: Newline
  outputNL :: Newline
type SeekMode :: *
data SeekMode
  | AbsoluteSeek
  | RelativeSeek
  | SeekFromEnd
type TextEncoding :: *
data TextEncoding = forall dstate estate. GHC.IO.Encoding.Types.TextEncoding
  GHC.IO.Encoding.Types.textEncodingName :: String
  GHC.IO.Encoding.Types.mkTextDecoder :: IO (GHC.IO.Encoding.Types.TextDecoder dstate)
  GHC.IO.Encoding.Types.mkTextEncoder :: IO (GHC.IO.Encoding.Types.TextEncoder estate)
char8 :: TextEncoding
hClose :: Handle -> IO ()
hFileSize :: Handle -> IO Integer
hFlush :: Handle -> IO ()
hGetBuf :: Handle -> GHC.Ptr.Ptr a -> Int -> IO Int
hGetBufNonBlocking :: Handle -> GHC.Ptr.Ptr a -> Int -> IO Int
hGetBufSome :: Handle -> GHC.Ptr.Ptr a -> Int -> IO Int
hGetBuffering :: Handle -> IO BufferMode
hGetChar :: Handle -> IO Char
hGetContents :: Handle -> IO String
hGetContents' :: Handle -> IO String
hGetEcho :: Handle -> IO Bool
hGetEncoding :: Handle -> IO (Maybe TextEncoding)
hGetLine :: Handle -> IO String
hGetPosn :: Handle -> IO HandlePosn
hIsClosed :: Handle -> IO Bool
hIsEOF :: Handle -> IO Bool
hIsOpen :: Handle -> IO Bool
hIsReadable :: Handle -> IO Bool
hIsSeekable :: Handle -> IO Bool
hIsTerminalDevice :: Handle -> IO Bool
hIsWritable :: Handle -> IO Bool
hLookAhead :: Handle -> IO Char
hPutBuf :: Handle -> GHC.Ptr.Ptr a -> Int -> IO ()
hPutBufNonBlocking :: Handle -> GHC.Ptr.Ptr a -> Int -> IO Int
hPutChar :: Handle -> Char -> IO ()
hPutStr :: Handle -> String -> IO ()
hPutStrLn :: Handle -> String -> IO ()
hSeek :: Handle -> SeekMode -> Integer -> IO ()
hSetBinaryMode :: Handle -> Bool -> IO ()
hSetBuffering :: Handle -> BufferMode -> IO ()
hSetEcho :: Handle -> Bool -> IO ()
hSetEncoding :: Handle -> TextEncoding -> IO ()
hSetFileSize :: Handle -> Integer -> IO ()
hSetNewlineMode :: Handle -> NewlineMode -> IO ()
hSetPosn :: HandlePosn -> IO ()
hShow :: Handle -> IO String
hTell :: Handle -> IO Integer
hWaitForInput :: Handle -> Int -> IO Bool
isEOF :: IO Bool
latin1 :: TextEncoding
mkTextEncoding :: String -> IO TextEncoding
nativeNewline :: Newline
nativeNewlineMode :: NewlineMode
noNewlineTranslation :: NewlineMode
openBinaryFile :: FilePath -> IOMode -> IO Handle
openFile :: FilePath -> IOMode -> IO Handle
stderr :: Handle
stdin :: Handle
stdout :: Handle
universalNewlineMode :: NewlineMode
utf16 :: TextEncoding
utf16be :: TextEncoding
utf16le :: TextEncoding
utf32 :: TextEncoding
utf32be :: TextEncoding
utf32le :: TextEncoding
utf8 :: TextEncoding
utf8_bom :: TextEncoding
withBinaryFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
