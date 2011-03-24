module Main (main) where

import System.Console.ANSI
import System.IO
import Control.Concurrent
import Control.Monad
import Data.Char
import Foreign.C.Types
import System.Console.Haskeline

moveAbout pY pX = do
 setSGR [SetColor Foreground Vivid Red]
 setCursorPosition pY pX 
 putStrLn "@" -- place a character in curses's virtual screen
 c <- getHiddenChar 
 case c of
   'w' -> updatePlayer pY pX (pY - 1) pX
   's' -> updatePlayer pY pX (pY + 1) pX
   'a' -> updatePlayer pY pX pY (pX - 1)
   'd' -> updatePlayer pY pX pY (pX + 1)
   'p' -> return ()
   _ -> moveAbout pY pX

getHiddenChar = liftM (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
   c_getch :: IO CChar


updatePlayer oY oX nY nX = do
   setSGR [Reset]
   setCursorPosition oY oX
   putStrLn ":"
   moveAbout nY nX

main :: IO ()
main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering 
    hSetBuffering stdout NoBuffering
    initScreen
    hideCursor
    moveAbout 5 5

initScreen :: IO ()
initScreen = do
    setTitle "Sugar Rush: TEH GAM!!!!!"
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
