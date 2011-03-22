module Main (main) where

import Server

main :: IO ()
main = do
    serveGame "10514" plainHandler

