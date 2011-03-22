module MainServer (main) where

import Server

main :: IO ()
main = serveGame "10514" plainHandler

