module Main (main) where

import Data.List.Split (splitOn)
import System.Console.ANSI
import System.IO
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (mzero, liftM, forever, when)
import Control.Monad.Maybe
import Control.Monad.Trans (lift)
import Data.Char (chr)
import Foreign.C.Types (CChar)
--import Control.Concurrent.STM
import System.Console.Haskeline
import System.Console.Haskeline.IO
import Client

ifReady :: Handle -> (Handle -> IO Char) -> IO (Maybe Char)
ifReady hnd = do
   ready <- hReady hnd
   if ready then fmap Just (getHiddenChar) else return Nothing

inputChar :: String -> MaybeT (InputT IO) Char
inputChar prompt = do
   m <- lift $ getInputChar prompt
   case m of
      Just x -> return x
      Nothing -> mzero

moveAbout :: Int -> Int -> GameClientHandle -> String -> IO ()
moveAbout pY pX gc username = do
 threadDelay 50000
 c <- ifReady stdin
 case c of
    'w' -> updatePlayer pY pX (pY - 1) pX gc username
    's' -> updatePlayer pY pX (pY + 1) pX gc username 
    'a' -> updatePlayer pY pX pY (pX - 1) gc username
    'd' -> updatePlayer pY pX pY (pX + 1) gc username 
    'p' -> return ()
    _ -> moveAbout pY pX gc username

getHiddenChar = liftM (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
   c_getch :: IO CChar

readServer :: GameClientHandle -> String -> IO ()
readServer gc username = forever $ do 
   response <- hGetLine (gcHandle gc)
   hFlush (gcHandle gc)
   let response' = splitOn " " response
   if (head response') /= username 
     then handleResponse response' 
     else putStrLn ""

handleResponse :: [String] -> IO () 
handleResponse (x:xs) = moveOpponent xs 

moveOpponent :: [String] -> IO ()
moveOpponent positions = do
   setSGR [Reset]
   setCursorPosition (read (positions !! 0)) (read (positions !! 1))
   putStrLn ":"
   setSGR [SetColor Foreground Vivid Red]
   setCursorPosition (read $ positions !! 2) (read $ positions !! 3)
   putStrLn "@"
   

updatePlayer oY oX nY nX gc username = do
   sayPlayer gc (username ++ " " ++ show oY ++ " " ++ show oX ++ " " ++ show nY ++ " " ++ show nX)
   setSGR [Reset]
   setCursorPosition oY oX
   putStrLn ":"
   setSGR [SetColor Foreground Vivid Red]
   setCursorPosition nY nX 
   putStrLn "@" -- place a character in curses's virtual screen
   moveAbout nY nX gc username

main :: IO ()
main = forever $ do
    setTitle "Sugar Rush: TEH GAM!!!!!"
    hSetBuffering stdout NoBuffering 
    putStr "Please enter your damn name: "
    userName <- getLine
    gc <- opengame "beast.xen.prgmr.com" "10514" (show userName)
    hSetBuffering (gcHandle gc) NoBuffering
    hSetEcho stdin False
    initScreen
    hideCursor
    hSetBuffering stdin NoBuffering
    forkIO $ moveAbout 5 5 gc userName
    readServer gc userName

initScreen :: IO ()
initScreen = do
    setCursorPosition 0 0
    putStrLn "###############################################"
    putStrLn "#:::::::::::::::::::::::::::::::::::::::::::::#"
    putStrLn "#:::::::::::::::::::::::::::::::::::::::::::::#"
    putStrLn "#:::::::::::::::::::::::::::::::::::::::::::::#"
    putStrLn "#:::::::::::::::::::::::::::::::::::::::::::::#"
    putStrLn "#:::::::::::::::::::::::::::::::::::::::::::::#"
    putStrLn "#:::::::::::::::::::::::::::::::::::::::::::::#"
    putStrLn "#:::::::::::::::::::::::::::::::::::::::::::::#"
    putStrLn "#:::::::::::::::::::::::::::::::::::::::::::::#"
    putStrLn "#:::::::::::::::::::::::::::::::::::::::::::::#"
    putStrLn "#:::::::::::::::::::::::::::::::::::::::::::::#"
    putStrLn "#:::::::::::::::::::::::::::::::::::::::::::::#"
    putStrLn "#:::::::::::::::::::::::::::::::::::::::::::::#"
    putStrLn "#:::::::::::::::::::::::::::::::::::::::::::::#"
    putStrLn "#:::::::::::::::::::::::::::::::::::::::::::::#"
    putStrLn "#:::::::::::::::::::::::::::::::::::::::::::::#"
    putStrLn "#:::::::::::::::::::::::::::::::::::::::::::::#"
    putStrLn "#:::::::::::::::::::::::::::::::::::::::::::::#"
    putStrLn "###############################################"
