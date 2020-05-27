import Data.Set as Set
import Data.List as List
import Data.Map.Strict as Map
import Data.Char
import Data.List.Split

-- Direction a car move towards
data Direction = North | South | East | West | NoWhere deriving (Show,Eq)

-- A move is a list of CarType and Directions that the Car
-- is moved towards , and how many tiles
type Positions = Int
type Move = (CarType,Direction,Positions)
type CarType = Char
type CarSize = Int
data Orientation = UpDir | RightDir deriving (Show,Eq)

printElements :: [String] -> IO()
printElements [] = return ()
printElements (x:xs) = do putStrLn x
                          printElements xs

main :: IO()
main = initGame

--initGame "aaabcd\neffbcd\ne.==cd\ngg....\n.ih.jj\n.ih.ll\n"
--initGame "aaabcd\neffbcd\ne.==cd\nggh...\n.ih.jj\n.i..ll\n"
initGame :: IO ()
initGame = do 
          putStrLn "Game starting...\nSelect difficulty: easy, medium, hard, nightmare"
          category <- getLine
          if category == "easy" then printElements (splitEvery 12 (insertBackslash 1 ' ' (clean "aa...b\nc..d.b\nc==d.b\nc..d..\ne...ff\ne.ggg.\n")))
          else if category == "medium" then printElements (splitEvery 12 (insertBackslash 1 ' ' (clean "aabbcd\ne.ffcd\negh==d\n.ghiii\n..h...\njj....\n")))
          else if category == "hard" then printElements (splitEvery 12 (insertBackslash 1 ' ' (clean "..abb.\n.ca...\ndc==e.\ndff.e.\nggg.h.\nii..h.\n")))
          else if category == "nightmare" then printElements (splitEvery 12 (insertBackslash 1 ' ' (clean "abb.c.\nade.cf\nade==f\ngggh.f\n..ihjj\nkkill.\n")))
          else initGame

          if category == "easy" then beginGame  "aa...b\nc..d.b\nc==d.b\nc..d..\ne...ff\ne.ggg.\n"
          else if category == "medium" then beginGame "aabbcd\ne.ffcd\negh==d\n.ghiii\n..h...\njj....\n"
          else if category == "hard" then beginGame "..abb.\n.ca...\ndc==e.\ndff.e.\nggg.h.\nii..h.\n"
          else beginGame "abb.c.\nade.cf\nade==f\ngggh.f\n..ihjj\nkkill.\n"
          return()

beginGame:: String -> IO()
beginGame initBoard = do         --(j,W,1)
                        putStrLn "Tip mutare: (Car,Direction,Positions)"
                        moveUser <- getLine
                        let (newBoard, isPossibleToMakeMove, finished) = play initBoard (getMove moveUser)
                        --print newBoard
                        --print isPossibleToMakeMove
                        --print finished
                        printElements (splitEvery 12 (insertBackslash 1 ' ' (clean newBoard)))
                        --let finished = False
                        if finished == True then putStrLn "You've finished the game!"
                        else beginGame (insertBackslash 6 '\n' newBoard)

play :: String -> Move -> (String, Bool, Bool)
play board move =  if ((findOri board (getMoveTy move)) == UpDir) && ((getMoveDir move) == North)  then checkMoveNorth board move --am nevoie de \n
                   else if ((findOri board (getMoveTy move)) == UpDir) && ((getMoveDir move) == South) then checkMoveSouth board move
                   else if ((findOri board (getMoveTy move)) == RightDir) && ((getMoveDir move) == West)  then checkMoveWest board move
                   else if ((findOri board (getMoveTy move)) == RightDir) && ((getMoveDir move) == East)  then checkMoveEast board move
                   else (board, False, True)                          

getMove :: String -> Move
getMove ['(',x,',', y,',',z,')'] = if (y == 'N') then (x, North, digitToInt z)
                                 else if (y == 'S') then (x, South, digitToInt z)
                                 else if (y == 'E') then (x, East, digitToInt z)
                                 else if (y == 'W') then (x, West, digitToInt z)
                                 else ('.', NoWhere, 0)



--initGame "aaab..\neffbc.\ne.==c.\nggh...\n.ih..d\n.ikk.d\n" -> ('d',North,1) -> "aaab..effbc.e.==c.ggh..d.ih..d.ikk.." -> ('d',North,1) -> isPossMove = False
--                                                                 checkMoveNorth "aaab..effbc.e.==c.ggh..d.ih..d.ikk.." ('d',North,1) -> isPossMove = True
--(d,N,1)  (d,N,2)  (d,N,3)  (d,N,4)
--initGame "aaabmm\neffbc.\ne.==c.\nggh..d\n.ih..d\n.ikk.d\n"

-----           N O R T H   M O V E            -----


getVertNorth::Int->Int->Int->[Int] -- [6,12,18,24,30,36]
getVertNorth i len wid = if i `mod` 6 == 0 then [(i `mod` 6)+6 + (j*len) | j <- [0..(wid - 1)]]
                         else [(i `mod` 6) + (j*len) | j <- [0..(wid - 1)]]

--findIndNorth "aaab..\neffbc.\ne.==c.\nggh..d\n.ih.jj\n.ikkll\n" ('d',North,1) : 3
--findIndNorth "aaab..\neffbc.\ne.==c.\nggh..d\n.ih.jj\n.ikkll\n" ('c',North,2) : 1
findIndNorth :: String -> Move -> Int--  ==  24                                  [6,12,18,24,30,36]  
findIndNorth board move = (List.findIndices(==index (clean board) (getMoveTy move)) (getVertNorth (index (clean board) (getMoveTy move)) 6 6))!!0


--indexListVertNorth "aaab..\neffbc.\ne.==c.\nggh..d\n.ih.jj\n.ikkll\n" ('d',North,1) 1 : pt o mutare verific : 18
--indexListVertNorth "aaab..\neffbc.\ne.==c.\nggh..d\n.ih.jj\n.ikkll\n" ('d',North,2) 2 : pt o mutare verific : 12
--indexListVertNorth "aaab..\neffbc.\ne.==c.\nggh..d\n.ih.jj\n.ikkll\n" ('d',North,3) 3 : pt o mutare verific : 6
indexListVertNorth:: String -> Move -> Int-> Int
                                   --[6,12,18,24,30,36]                                      !! 
indexListVertNorth board move pos = (getVertNorth (index (clean board) (getMoveTy move)) 6 6)!!((findIndNorth board move) - pos)

--((List.findIndices(==index (clean board) (getMoveTy move)) (getVertNorth (index (clean board) (getMoveTy move)) 6 6))!!0) - (getMovePos move) >= 0)
checkMoveNorth :: String -> Move -> (String, Bool, Bool)
checkMoveNorth board move = if ((getCarLen board (getMoveTy move)) + (getMovePos move) <= 6) then
                                if (getMovePos move) == 1 then 
                                   --indexListVertNorth (clean "aaab..\neffbc.\ne.==c.\nggh..d\n.ih.jj\n.ikkll\n") ('d',North,1) 1 : 18
                                   if ((clean board)!!((indexListVertNorth (clean board) move (getMovePos move))-1) == '.') then moveNorth (clean board) move (createNewCarPosListNorth (clean board) move (getMovePos move)) (createEmptySlotListNorth (clean board) move (getMovePos move))
                                   else (clean board, False, False) --nu pot face mutarea data
                                else if (getMovePos move) == 2 then 
                                   if ((clean board)!!((indexListVertNorth (clean board) move 1)-1) == '.') && ((clean board)!!((indexListVertNorth (clean board) move 2)-1) == '.') then moveNorth (clean board) move (createNewCarPosListNorth (clean board) move (getMovePos move)) (createEmptySlotListNorth (clean board) move (getMovePos move))
                                   else (clean board, False, False) --nu pot face mutarea data
                                else if (getMovePos move) == 3 then 
                                   if ((clean board)!!((indexListVertNorth (clean board) move 1)-1) == '.') && ((clean board)!!((indexListVertNorth (clean board) move 2)-1) == '.') && ((clean board)!!((indexListVertNorth (clean board) move 3)-1) == '.') then moveNorth (clean board) move (createNewCarPosListNorth (clean board) move (getMovePos move)) (createEmptySlotListNorth (clean board) move (getMovePos move))
                                   else (clean board, False, False) --nu pot face mutarea data
                                else if (getMovePos move) == 4 then 
                                   if ((clean board)!!((indexListVertNorth (clean board) move 1)-1) == '.') && ((clean board)!!((indexListVertNorth (clean board) move 2)-1) == '.') && ((clean board)!!((indexListVertNorth (clean board) move 3)-1) == '.') && ((clean board)!!((indexListVertNorth (clean board) move 4)-1) == '.') then moveNorth (clean board) move (createNewCarPosListNorth (clean board) move (getMovePos move)) (createEmptySlotListNorth (clean board) move (getMovePos move))
                                   else (clean board, False, False) --nu pot face mutarea data
                                else (clean board, False, False) --nu pot face mutarea data      
                             else (clean board, False, False) --nu pot face mutarea data


--createNewCarPosListNorth "aaab.deffbcde.==cdggh....ih.jj.ikkll" ('d',North,1) 1 : [6,12,18]
createNewCarPosListNorth :: String -> Move -> Int -> [Int]
createNewCarPosListNorth board move pos = List.take (getCarLen (clean board) (getMoveTy move)) (List.drop (((List.findIndices(==index (clean board) (getMoveTy move)) (getVertNorth (index (clean board) (getMoveTy move)) 6 6))!!0) - pos) (getVertNorth (index (clean board) (getMoveTy move)) 6 6))


--createEmptySlotListNorth "aaab.deffbcde.==cdggh....ih.jj.ikkll" ('d',North,1) 1 : [24]
createEmptySlotListNorth :: String -> Move -> Int -> [Int]
createEmptySlotListNorth board move pos = List.take pos (List.drop (getCarLen (clean board) (getMoveTy move)) (List.drop (((List.findIndices(==index (clean board) (getMoveTy move)) (getVertNorth (index (clean board) (getMoveTy move)) 6 6))!!0) - pos) (getVertNorth (index (clean board) (getMoveTy move)) 6 6)))

moveNorth :: String -> Move -> [Int] -> [Int] -> (String, Bool, Bool)
moveNorth board move listNewCar listEmptySlot= insertEmptySlotNorth (changeCharAtGivenPositions (clean board) listNewCar (getMoveTy move)) listEmptySlot

insertEmptySlotNorth :: String -> [Int] -> (String, Bool, Bool)
insertEmptySlotNorth board listEmptySlot = (changeCharAtGivenPositions (clean board) listEmptySlot '.', True, False)



--initGame "aaabcd\neffbcd\ne.==.d\nggh...\n.ih...\n.ikk..\n"
--(c,S,1)  (c,S,2)  (c,S,3)  (c,S,4)

-----        S O U T H     M O V E   -----

getVertSouth::Int->Int->Int->[Int]
getVertSouth i len wid = if i > 6 && i < 12 then [i + (j*len) | j <- [0..(wid - 2)]]
                         else if i > 12 && i < 18 then [i + (j*len) | j <- [0..(wid - 3)]]
                         else if i > 18 && i < 24 then [i + (j*len) | j <- [0..(wid - 4)]]
                         else if i > 24 && i < 30 then [i + (j*len) | j <- [0..(wid - 5)]]
                         else if i > 30 && i < 36 then [i + (j*len) | j <- [0..(wid - 6)]]
                         else [i + (j*len) | j <- [0..(wid - 1)]]

--findIndSouth (clean "aaabcd\neffbcd\ne.==.d\nggh...\n.ih...\n.ikk..\n") ('c',South,1) : leng+pos =3 -ok
findIndSouth :: String -> Move -> Int
findIndSouth board move = (getCarLen (clean board) (getMoveTy move))                         

--indexListVertSouth (clean "aaabcd\neffbcd\ne.==.d\nggh...\n.ih...\n.ikk..\n") ('c',South,1) 1 : 17
--indexListVertSouth (clean "aaabcd\neffbcd\ne.==.d\nggh...\n.ih...\n.ikk..\n") ('c',South,2) 2 : 23
--indexListVertSouth (clean "aaabcd\neffbcd\ne.==.d\nggh...\n.ih...\n.ikk..\n") ('c',South,3) 3 : 29
--indexListVertSouth (clean "aaabcd\neffbcd\ne.==.d\nggh...\n.ih...\n.ikk..\n") ('c',South,4) 4 : 35
indexListVertSouth :: String -> Move -> Int -> Int
--                                   [5,11,17,23,29,35] !! 
indexListVertSouth board move pos = (getVertSouth (index (clean board) (getMoveTy move)) 6 6)!!((findIndSouth board move) + pos - 1)

--(clean "aaabcd\neffbcd\ne.==.d\nggh...\n.ih...\n.ikk..\n")!!((indexListVertSouth(clean "aaabcd\neffbcd\ne.==.d\nggh...\n.ih...\n.ikk..\n") ('c',South,1) 1) -1) : '.'
--(clean "aaabcd\neffbcd\ne.==.d\nggh...\n.ih...\n.ikk..\n")!!((indexListVertSouth(clean "aaabcd\neffbcd\ne.==.d\nggh...\n.ih...\n.ikk..\n") ('c',South,2) 2) -1) : '.'
--(clean "aaabcd\neffbcd\ne.==.d\nggh...\n.ih...\n.ikk..\n")!!((indexListVertSouth(clean "aaabcd\neffbcd\ne.==.d\nggh...\n.ih...\n.ikk..\n") ('c',South,3) 3) -1) : '.'
--(clean "aaabcd\neffbcd\ne.==.d\nggh...\n.ih...\n.ikk..\n")!!((indexListVertSouth(clean "aaabcd\neffbcd\ne.==.d\nggh...\n.ih...\n.ikk..\n") ('c',South,4) 4) -1) : '.'
checkMoveSouth :: String -> Move -> (String, Bool, Bool) --(board, False, False)
checkMoveSouth board move = if (((getCarLen board (getMoveTy move)) + (getMovePos move) <= 6) ) then
                               if (getMovePos move) == 1 then 
                                   if ((clean board)!!((indexListVertSouth(clean board) move 1) -1)   == '.') then moveSouth (clean board) move (createNewCarPosListSouth (clean board) move (getMovePos move)) (createEmptySlotListSouth (clean board) move (getMovePos move))
                                   else (clean board, False, False) --nu pot face mutarea data
                                else if (getMovePos move) == 2 then
                                    if ((clean board)!!((indexListVertSouth(clean board) move 1) -1)   == '.') && ((clean board)!!((indexListVertSouth(clean board) move 2) -1 ) == '.') then moveSouth (clean board) move (createNewCarPosListSouth (clean board) move (getMovePos move)) (createEmptySlotListSouth (clean board) move (getMovePos move))
                                    else (clean board, False, False) --nu pot face mutarea data
                                else if (getMovePos move) == 3 then
                                    if ((clean board)!!((indexListVertSouth(clean board) move 1) -1)   == '.') && ((clean board)!!((indexListVertSouth(clean board) move 2) -1 ) == '.') && ((clean board)!!((indexListVertSouth(clean board) move 3) -1) == '.') then moveSouth (clean board) move (createNewCarPosListSouth (clean board) move (getMovePos move)) (createEmptySlotListSouth (clean board) move (getMovePos move))
                                    else (clean board, False, False) --nu pot face mutarea data
                                else if (getMovePos move) == 4 then
                                    if ((clean board)!!((indexListVertSouth(clean board) move 1) -1)   == '.') && ((clean board)!!((indexListVertSouth(clean board) move 2) -1 ) == '.') && ((clean board)!!((indexListVertSouth(clean board) move 3) -1) == '.') && ((clean board)!!((indexListVertSouth(clean board) move 4) -1) == '.') then moveSouth (clean board) move (createNewCarPosListSouth (clean board) move (getMovePos move)) (createEmptySlotListSouth (clean board) move (getMovePos move))
                                    else (clean board, False, False) --nu pot face mutarea data
                                else (clean board, False, False) --nu pot face mutarea data
                            else (clean board, True, False) --nu pot face mutarea data


moveSouth :: String -> Move -> [Int] -> [Int] -> (String, Bool, Bool)
moveSouth board move listNewCar listEmptySlot = insertEmptySlotSouth (changeCharAtGivenPositions (clean board) listNewCar (getMoveTy move)) listEmptySlot

-- changeCharAtGivenPositions (clean "0aaab.deffbcde.==cdggh....ih.jj.ikkll") [11] '.' : "0aaab.deffb.de.==cdggh....ih.jj.ikkll"
insertEmptySlotSouth :: String -> [Int] -> (String, Bool, Bool)
insertEmptySlotSouth board listEmptySlot = (changeCharAtGivenPositions (clean board) listEmptySlot '.', True, False)

--createNewCarPosListSouth "aaab.deffbcde.==cdggh....ih.jj.ikkll" ('c',South,1) 1 : [17,23] -ok
createNewCarPosListSouth :: String -> Move -> Int -> [Int]
createNewCarPosListSouth board move pos = List.take (getCarLen board (getMoveTy move)) (List.drop pos (getVertSouth (index (clean board) (getMoveTy move)) 6 6)) 

--createEmptySlotListSouth "aaab.deffbcde.==cdggh....ih.jj.ikkll" ('d',South,1) 2 : [6,12] -ok 
createEmptySlotListSouth :: String -> Move -> Int -> [Int]
createEmptySlotListSouth board move pos = List.take pos (getVertSouth (index (clean board) (getMoveTy move)) 6 6)



--initGame "aaabcd\n.ffbcd\n...==d\nggh..d\n.ih.jj\n.ikkll"
--(=,West,1)/2/3/4

-----                    W E S T     M O V E                  -----

getHor::Int->Int->Int->[Int]
getHor i len wid = if i >=31 && i<=36 then [((6-1)*len) + j + 1| j <- [0..(len -1 )]]
                   else if i>=25 && i<=30 then [((5-1)*len) + j + 1| j <- [0..(len -1 )]]
                   else if i>=19 && i<=24 then [((4-1)*len) + j + 1| j <- [0..(len -1 )]]
                   else if i>=13 && i<=18 then [((3-1)*len) + j + 1| j <- [0..(len -1 )]]
                   else if i>=7 && i<12 then [((2-1)*len) + j + 1| j <- [0..(len -1 )]]
                   else [ j + 1 | j <- [0..(len -1 )]]

findIndWest :: String -> Move -> Int
findIndWest board move = (List.findIndices(==index (clean board) (getMoveTy move)) (getHor (index (clean board) (getMoveTy move)) 6 6))!!0

--indexNextPosWest "aaabcd\n.ffbcd\n...==d\nggh..d\n.ih.jj\n.ikkll" ('=',West,1) 1 : 15 -ok
--indexNextPosWest "aaabcd\n.ffbcd\n...==d\nggh..d\n.ih.jj\n.ikkll" ('=',West,2) 2 : 14 -ok
--indexNextPosWest "aaabcd\n.ffbcd\n...==d\nggh..d\n.ih.jj\n.ikkll" ('=',West,3) 3 : 13 -ok
--indexNextPosWest "aaabcd\n.ffbcd\n...==d\nggh..d\n.ih.jj\n.ikkll" ('=',West,4) 4 : negative index -ok
indexNextPosWest:: String -> Move -> Int-> Int                                      
indexNextPosWest board move pos= (getHor (index (clean board) (getMoveTy move)) 6 6)!!((findIndWest board move) - pos)


--(length(List.drop (getCarLen "aaab..effbcde.==cdggh..d.ih.jj.ikkll"  (getMoveTy ('=',West,1)) ) (getHor (index "aaab..effbcde.==cdggh..d.ih.jj.ikkll"  (getMoveTy ('=',West,1))) 6 6))) + (getMovePos ('=',West,1))
--(length(List.drop (getCarLen board (getMoveTy move)) (getHor (index (clean board) (getMoveTy move)) 6 6)) + (getMovePos move) <= 6)
checkMoveWest :: String -> Move -> (String, Bool, Bool)
checkMoveWest board move = if (((getCarLen board (getMoveTy move)) + (getMovePos move) <= 6)) then
                               if (getMovePos move) == 1 then 
                                   if ((clean board)!!((indexNextPosWest (clean board) move 1) -1) == '.') then moveWest board move (createNewCarPosListWest (clean board) move (getMovePos move)) (createEmptySlotListWest (clean board) move (getMovePos move))
                                   else (board, False, False) --nu pot face mutarea data
                                else if (getMovePos move) == 2 then 
                                   if ((clean board)!!((indexNextPosWest (clean board) move 1) -1) == '.') && ((clean board)!!((indexNextPosWest (clean board) move 2) -1) == '.') then moveWest board move (createNewCarPosListWest (clean board) move (getMovePos move)) (createEmptySlotListWest (clean board) move (getMovePos move))
                                   else (board, False, False) --nu pot face mutarea data
                                else if (getMovePos move) == 3 then
                                   if ((clean board)!!((indexNextPosWest (clean board) move 1) -1) == '.') && ((clean board)!!((indexNextPosWest (clean board) move 2) -1) == '.') && ((clean board)!!((indexNextPosWest (clean board) move 3) -1) == '.') then moveWest board move (createNewCarPosListWest (clean board) move (getMovePos move)) (createEmptySlotListWest (clean board) move (getMovePos move))
                                   else (board, False, False) --nu pot face mutarea data
                                else if (getMovePos move) == 4 then 
                                   if ((clean board)!!((indexNextPosWest (clean board) move 1) -1) == '.') && ((clean board)!!((indexNextPosWest (clean board) move 2) -1) == '.') && ((clean board)!!((indexNextPosWest (clean board) move 3) -1) == '.') && ((clean board)!!((indexNextPosWest (clean board) move 4) -1) == '.') then moveWest board move (createNewCarPosListWest (clean board) move (getMovePos move)) (createEmptySlotListWest (clean board) move (getMovePos move))
                                   else (board, False, False) --nu pot face mutarea data
                                else (board, False, False) --nu pot face mutarea data
                            else (board, False, False) --nu pot face mutarea data


createNewCarPosListWest :: String -> Move -> Int -> [Int]
createNewCarPosListWest board move pos = List.take (getCarLen board (getMoveTy move)) (List.drop (((List.findIndices(==index (clean board) (getMoveTy move)) (getHor (index (clean board) (getMoveTy move)) 6 6))!!0) - pos) (getHor (index (clean board) (getMoveTy move)) 6 6))

--createEmptySlotListWest "aaab..effbcde.==cdggh..d.ih.jj.ikkll" ('d',West,1) 2 : [6,12] -ok 
createEmptySlotListWest :: String -> Move -> Int -> [Int]
createEmptySlotListWest board move pos = List.take pos (List.drop (getCarLen board (getMoveTy move)) (List.drop (((List.findIndices(==index (clean board) (getMoveTy move)) (getHor (index (clean board) (getMoveTy move)) 6 6))!!0) - pos) (getHor (index (clean board) (getMoveTy move)) 6 6)))

moveWest :: String -> Move -> [Int] -> [Int] -> (String, Bool, Bool)
moveWest board move listNewCar listEmptySlot= insertEmptySlotWest (changeCharAtGivenPositions (clean board) listNewCar (getMoveTy move)) listEmptySlot

-- changeCharAtGivenPositions (clean "0aaab.deffbcde.==cdggh....ih.jj.ikkll") [11] '.' : "0aaab.deffb.de.==cdggh....ih.jj.ikkll"
insertEmptySlotWest :: String -> [Int] -> (String, Bool, Bool)
insertEmptySlotWest board listEmptySlot = checkIfFinished (changeCharAtGivenPositions board listEmptySlot '.')   


--initGame "aaabcd\neffbcd\ne.==cd\ngg....\n.i..jj\n.ikkll\n"

-----                    E A S T      M O V E                 -----

findIndEast :: String -> Move -> Int
findIndEast board move = (List.findIndices(==index (clean board) (getMoveTy move)) (getHor (index (clean board) (getMoveTy move)) 6 6))!!0

--indexNextPosEast "aaabcd\neffbcd\ne.==cd\ngg....\n.i..jj\n.ikkll\n" ('g',East,1) 1 : 21
--indexNextPosEast "aaabcd\neffbcd\ne.==cd\ngg....\n.i..jj\n.ikkll\n" ('g',East,2) 2 : 22
--indexNextPosEast "aaabcd\neffbcd\ne.==cd\ngg....\n.i..jj\n.ikkll\n" ('g',East,3) 3 : 23
--indexNextPosEast "aaabcd\neffbcd\ne.==cd\ngg....\n.i..jj\n.ikkll\n" ('g',East,4) 4 : 24
indexNextPosEast :: String -> Move -> Int -> Int
--                                   [5,11,17,23,29,35] !! 
indexNextPosEast board move pos = (getHor (index (clean board) (getMoveTy move)) 6 6)!!((findIndEast board move) + (getCarLen board (getMoveTy move)) + pos - 1)


checkMoveEast :: String -> Move -> (String, Bool, Bool)
checkMoveEast board move = if (((getCarLen board (getMoveTy move)) + (getMovePos move) <= 6)) then
                               if (getMovePos move) == 1 then 
                                   if ((clean board)!!((indexNextPosEast (clean board) move 1) -1) == '.') then moveEast board move (createNewCarPosListEast (clean board) move (getMovePos move)) (createEmptySlotListEast (clean board) move (getMovePos move))
                                   else (board, False, False) --nu pot face mutarea data
                                else if (getMovePos move) == 2 then 
                                   if ((clean board)!!((indexNextPosEast (clean board) move 1) -1) == '.') && ((clean board)!!((indexNextPosEast (clean board) move 2) -1) == '.') then moveEast board move (createNewCarPosListEast (clean board) move (getMovePos move)) (createEmptySlotListEast (clean board) move (getMovePos move))
                                   else (board, False, False) --nu pot face mutarea data
                                else if (getMovePos move) == 3 then
                                   if ((clean board)!!((indexNextPosEast (clean board) move 1) -1) == '.') && ((clean board)!!((indexNextPosEast (clean board) move 2) -1) == '.') && ((clean board)!!((indexNextPosEast (clean board) move 3) -1) == '.') then moveEast board move (createNewCarPosListEast (clean board) move (getMovePos move)) (createEmptySlotListEast (clean board) move (getMovePos move))
                                   else (board, False, False) --nu pot face mutarea data
                                else if (getMovePos move) == 4 then 
                                   if ((clean board)!!((indexNextPosEast (clean board) move 1) -1) == '.') && ((clean board)!!((indexNextPosEast (clean board) move 2) -1) == '.') && ((clean board)!!((indexNextPosEast (clean board) move 3) -1) == '.') && ((clean board)!!((indexNextPosEast (clean board) move 4) -1) == '.') then moveEast board move (createNewCarPosListEast (clean board) move (getMovePos move)) (createEmptySlotListEast (clean board) move (getMovePos move))
                                   else (board, False, False) --nu pot face mutarea data
                                else (board, False, False) --nu pot face mutarea data
                            else (board, False, False) --nu pot face mutarea data



createNewCarPosListEast :: String -> Move -> Int -> [Int]
createNewCarPosListEast board move pos = List.take (getCarLen board (getMoveTy move)) (List.drop ((findIndEast (clean board) move) + (getMovePos move)) (getHor (index (clean board) (getMoveTy move)) 6 6))

--createEmptySlotListEast "aaabcd\neffbcd\ne.==cd\ngg....\n.i..jj\n.ikkll\n" ('g',East,3) 3 : [19,20,21]
createEmptySlotListEast :: String -> Move -> Int -> [Int]
createEmptySlotListEast board move pos = List.take pos (List.drop (findIndEast (clean board) move) (getHor (index (clean board) (getMoveTy move)) 6 6))

moveEast :: String -> Move -> [Int] -> [Int] -> (String, Bool, Bool)
moveEast board move listNewCar listEmptySlot= insertEmptySlotWest (changeCharAtGivenPositions (clean board) listNewCar (getMoveTy move)) listEmptySlot

-- changeCharAtGivenPositions (clean "0aaab.deffbcde.==cdggh....ih.jj.ikkll") [11] '.' : "0aaab.deffb.de.==cdggh....ih.jj.ikkll"
insertEmptySlotEast :: String -> [Int] -> (String, Bool, Bool)
insertEmptySlotEast board listEmptySlot = checkIfFinished (changeCharAtGivenPositions board listEmptySlot '.')


-----     F I N I S H      F C T                 -----

--findIndicesWantedCar "aaabcd\n.ffbcd\n...==d\nggh..d\n.ih.jj\n.ikkll" ('=',West,0) : [17,18]
findIndicesWantedCar :: String -> Move -> [Int]
findIndicesWantedCar board move = lastN (getCarLen board (getMoveTy move)) (getHor (index (clean board) (getMoveTy move)) 6 6) 

checkIfFinished :: String -> (String, Bool, Bool)
checkIfFinished board = if ((clean board)!!(((findIndicesWantedCar board ('=',West,0))!!0) -1) == '=') && ((clean board)!!(((findIndicesWantedCar board ('=',West,0))!!1) -1) == '=') 
                           then (board, True, True)
                        else (board, True, False)

-- aici am modificat deoarece boardul incepe de la 0 --> take(n), drop(n+1) erau
replaceItemInList :: Int -> [a] -> a -> [a]
replaceItemInList n sir val = List.take(n-1) sir ++ [val] ++ List.drop (n) sir


changeCharAtGivenPositions :: String -> [Int] -> Char -> String
changeCharAtGivenPositions string [] _ = string
changeCharAtGivenPositions string (x:xs) char = replaceItemInList x (changeCharAtGivenPositions string xs char) char

lastN :: Int -> [a] -> [a]
lastN n xs = List.drop (length xs - n) xs

insertBackslash :: Int -> a -> [a] -> [a]
insertBackslash n y xs = countdown n xs where
   countdown 0 xs = y:countdown n xs -- reset to original n
   countdown _ [] = []
   countdown m (x:xs) = x:countdown (m-1) xs

-- Gets Length of Car by counting how many occurences there are
--getCarLen "aaabcd\neffbcd\ne.==cd\nggh...\n.ih.jj\n.ikkll\n" 'a' : 3
getCarLen::String->CarType->CarSize
getCarLen str tp = length $ List.filter (\x -> x == tp) str

-- Give a String Create List of String split at \n
--listify "aaabcd\neffbcd\ne.==cd\nggh...\n.ih.jj\n.ikkll\n" : ["aaabcd","effbcd","e.==cd","ggh...",".ih.jj",".ikkll"]
listify::String->[String]
listify str = words str'
                where str' = replaceStr str ' ' '\n'

-- Returns string without the '\n'
--clean "aaabcd\neffbcd\ne.==cd\nggh...\n.ih.jj\n.ikkll\n" : "aaabcdeffbcde.==cdggh....ih.jj.ikkll"
clean::String->String
clean str = List.foldr (++) [] str'
                where
                    str' = listify str

--findOri "aaabcd\neffbcd\ne.==cd\nggh...\n.ih.jj\n.ikkll\n" 'a' : RightDir
findOri::String->CarType->Orientation
findOri str ctype = if length matches == 1
                    then RightDir else UpDir
                        where
                            str' = listify str
                            matches = [x | x <- str' , ctype `List.elem` x ]

fst3::(a,b,c)->a
fst3 (x,_,_) = x
snd3::(a,b,c)->b
snd3 (_,x,_) = x
trd3::(a,b,c)->c
trd3 (_,_,x) = x

index::String->Char->Int
index [] _ = 1
index (a:str) c = if a == c
    then 1
    else 1 + (index str c)

getMoveTy :: Move -> CarType
getMoveTy x = fst3 x
getMoveDir :: Move -> Direction
getMoveDir x = snd3 x
getMovePos :: Move -> Positions
getMovePos x = trd3 x

-- Replace Char b in string with char c
-- replaceStr "aaabcd\neffbcd\ne.==cd\nggh...\n.ih.jj\n.ikkll\n" 'b' 'c'
replaceStr::String->Char->Char->String
replaceStr [] _ _ = []
replaceStr (a:str) c b = if (a == b) then c : (replaceStr str c b)
    else a:(replaceStr str c b)