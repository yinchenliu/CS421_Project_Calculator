module Main where

import System.IO
import Eval
import Parser

cls:: IO()
cls = putStr "\ESC[2J" --clears the screen

type Pos = (Int,Int)

writeAt:: Pos->String->IO()
writeAt p s = do
        goto p
        putStr s 

goto:: Pos->IO()
goto (x,y) = do
        putStr ("\ESC["++ show y ++";"++ show x ++ "H")

box :: [String]
box = ["+-----------------+",
       "|                 |",
       "+---+---+---+---+ |",
       "| q | c | d | = | |",
       "+---+---+---+---+ |",
       "| 1 | 2 | 3 | + | |",
       "+---+---+---+---+ |",
       "| 4 | 5 | 6 | - | |",
       "+---+---+---+---+ |",
       "| 7 | 8 | 9 | * | |",
       "+---+---+---+---+ |",
       "| 0 | ( | ) | / | |",
       "+-----------------+"]

buttons :: String
buttons = standard ++ extra
  where
    standard = "qcd=123+456-789*0()/."
    extra = "QCD \ESC\BS\DEL\n"

showbox:: IO()
showbox = sequence_[writeAt (1,y) b| (y,b)<-zip [1..] box]

display:: String-> IO()
display xs = do writeAt (3,2) (replicate 13 ' ')
                writeAt (3,2) (reverse (take 13 (reverse xs)))

getCh:: IO Char
getCh = do
        hSetEcho stdin False
        x<-getChar
        hSetEcho stdin True
        return x

--wrong input would trigger beep sound
beep :: IO()
beep = putStr "\BEL"

-- calc

calc:: String -> IO()
calc xs = do display xs
             c <-getCh
             if elem c buttons then 
                process c xs 
             else 
                do  beep
                    calc xs

process:: Char-> String-> IO()
process c xs | elem c "qQ\ESC" = quit
             | elem c "dD\BS\DEL" =  delete xs
             | elem c "=\n" = eval1 xs -- when press "=" or enter, evaluate the expression
             | elem c "cC" = clear
             | otherwise = press c xs

quit:: IO()
quit = goto (1,14)

--remove one character from the input
delete :: String-> IO()
delete [] = calc []
delete xs = calc (init xs)

--eval1 is like eval, but eval1 is for the calculator interface calculator
eval1 :: String-> IO()
eval1 xs = case run expr xs of 
            [(n,[])]->calc (show n)
            _ -> do calc xs

clear :: IO()
clear = calc []

-- add the addtional char into the string
press:: Char-> String-> IO()
press c xs = calc (xs++[c])


start :: IO()   
start = do  cls
            showbox
            clear

main:: IO()
main = start