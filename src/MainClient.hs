module MainClient (main) where

import UI.HSCurses.Curses

castEnum = toEnum . fromEnum

moveAbout pY pX = do
 erase -- clear curses's virtual screen but don't force a redraw
 mvAddCh pY pX (castEnum '@') -- place a character in curses's virtual screen
 refresh -- copy the virtual screen to the terminal
 c <- getCh
 case c of
   KeyUp -> moveAbout (pY - 1) pX
   KeyDown -> moveAbout (pY + 1) pX
   KeyLeft -> moveAbout pY (pX - 1)
   KeyRight -> moveAbout pY (pX + 1)
   _ -> return ()

main :: IO ()
main = do
    initCurses
    keypad stdScr True
    echo False
    cursSet CursorInvisible
    (sizeY, sizeX) <- scrSize
    moveAbout (sizeY `div` 2) (sizeX `div` 2)
    endWin

