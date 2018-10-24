import Solver

import Data.Bool
import Data.List
import Data.Maybe

import Text.Printf
import System.Environment
import System.CPUTime

import Debug.Trace

-- import Control.Parallel.Strategies

-- main = 

-- main = do
--     t1 <- getCPUTime
--     (zed zed06)
--     t2 <- getCPUTime
--     let t = fromIntegral (t2-t1) * 1e012 :: Double
--     printf "\nTime: %02.fs\n" t

-- main = do 
--     args <- getArgs
--     n_queens (read $ head args)


-- Timings done using compiled binary on Intel i7-7700 (4GHz).
-- compile command: $ ghc puzzles.hs -O2 -fexcess-precision -optc-O3 -optc-ffast-math

-- zed06 0.2s
-- zed07 8s
-- zed08 385s 

-- n_queens 8 => 0.5s   (all 92 solutions)
-- n_queens 14 => 3s    (first solution)
-- n_queens 16 => 21s

-- sudoku08 => 0.2s
-- sudoku12 => 0.2s
-- sudoku16 => 0.8s
-- sudoku17 => 165s


-- main = zed zed08
main = zed zed07

---------------------------------------------------------------------------------------------------
-- Solver API Usage
---------------------------------------------------------------------------------------------------

-- Solver.solve [Constraint] [[1..n],[1..n],[1..n]] => [[a,b,c,d,]]

-- Constraint is one of:
-- Matches (predicate on list of values) Selector
-- Reducer ([[Int]] -> [[Int]] reducer function) Selector

-- Selector is one of:
-- Select [(dim 1 predicate), (dim 2 predicate)..]
-- Points [(x,y)..]

---------------------------------------------------------------------------------------------------
-- Zed Puzzles
---------------------------------------------------------------------------------------------------

zed04 = ([1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1]) :: ([Int],[Int],[Int],[Int])
zed05 = ([1,2,3,4,2],[5,1,3,2,2],[2,2,1,3,3],[2,4,2,2,1]) :: ([Int],[Int],[Int],[Int])
zed06 = ([3,3,2,4,2,1],[1,4,3,4,2,2],[2,1,2,4,2,3],[2,2,2,1,3,4]) :: ([Int],[Int],[Int],[Int])
zed07 = ([3,1,2,3,3,4,2],[3,1,3,4,2,2,4],[3,3,2,3,1,2,4],[3,4,2,1,3,3,2]) :: ([Int],[Int],[Int],[Int])
zed08 = ([3,4,3,2,2,1,4,3],[2,4,4,1,5,2,3,2],[3,1,2,2,4,3,2,3],[3,2,3,3,3,1,4,5]) :: ([Int],[Int],[Int],[Int])

zed :: ([Int],[Int],[Int],[Int]) -> IO ()
zed clues = show_solution $ take 1 $ Solver.solve (zed_constraints clues') [[1..n],[1..n],[1..n]] where
    clues' = format_zed_clues clues
    n = length (fst clues')
    show_solution [] = putStrLn "No solution found"
    show_solution [solution] = show_zed_solution solution
    show_solution solutions = mapM_ (\s -> putStrLn "-------------------" >> show_zed_solution s) solutions >> putStrLn ((show (length solutions)) ++ " solutions")

format_zed_clues :: ([Int], [Int], [Int], [Int]) -> ([(Int, Int)], [(Int, Int)])
format_zed_clues (t, r, b, l) = (zip t (reverse b), zip (reverse l) r)

zed_constraints :: ([(Int, Int)], [(Int, Int)]) -> [Constraint]
zed_constraints clues = let
    n = length (fst clues)
    count_two_way list = (zed_count list, zed_count . reverse $ list)
    (==?) _ (0,0) = True           -- Using ==? allows to check opposite clues at the same time.
    (==?) (_,a) (0,b) = a == b
    (==?) (a,_) (b,0) = a == b
    (==?) a b = a == b
    in (   [Matches ((==?((fst clues) !! (x-1))) . count_two_way) (Select [(==x), truth]) | x <- [1..n]]
        ++ [Matches ((==?((snd clues) !! (y-1))) . count_two_way) (Select [truth, (==y)]) | y <- [1..n]]
        ++ [Matches (has_n_unique_elements n) (Select [(==x), truth]) | x <- [1..n]]
        ++ [Matches (has_n_unique_elements n) (Select [truth, (==y)]) | y <- [1..n]] )

zed_count = fst . foldl' (\(c,m) n -> (bool c (c+1) (n > m), max n m) ) (0,0)

show_zed_solution :: [[Int]] -> IO ()
show_zed_solution grid = putStrLn header >> putStrLn (grid_to_string grid) >> putStrLn "" >> putStrLn footer where 
    grid_to_string = concat . (map (("\n"++) . line_to_string))
    header = ("   "++) $ concat $ map ((++" ") . show . zed_count) (transpose grid)
    footer = ("   "++) $ concat $ map ((++" ") . show . zed_count . reverse) (transpose grid)
    clue_str_left line = (show (zed_count line)) ++ "  "
    clue_str_right line = " " ++ (show (zed_count (reverse line)))
    line_to_string line = (clue_str_left line) ++ (concat . (map ((++" ") . show)) $ line) ++ (clue_str_right line)


---------------------------------------------------------------------------------------------------
-- Sudoku Puzzles
---------------------------------------------------------------------------------------------------

sudoku0 = replicate 81 '0'  -- Can be used for generating puzzles.
sudoku01 = "040000179002008054006005008080070910050090030019060040300400700570100200928000060"
sudoku02 = "206000049037009000100700006000580900705000804009062000900004001000300490410000208"
sudoku05 = "105000370000000200097300010000053102300801004201470000070008640008000000012000807"
sudoku08 = "670008010020060000000030000201000006480001700000000009004500000000000300003400802"
sudoku12 = "000003017015009008060000000100007000009000200000500004000000020500600340340200000"
sudoku16 = "100007090030020008009600500005300900010080002600004000300000010040000007007000300"
sudoku17 = "000000000000003085001020000000507000004000100090000000500000073002010000000040009"

-- Solutions:     sudoku01                sudoku02                sudoku05                sudoku08
--   8 4 5 | 6 3 2 | 1 7 9   2 5 6 | 8 3 1 | 7 4 9   1 2 5 | 6 4 9 | 3 7 8   6 7 5 | 9 4 8 | 2 1 3
--   7 3 2 | 9 1 8 | 6 5 4   8 3 7 | 6 4 9 | 5 1 2   8 3 4 | 7 1 5 | 2 9 6   3 2 8 | 1 6 5 | 9 7 4
--   1 9 6 | 7 4 5 | 3 2 8   1 9 4 | 7 2 5 | 3 8 6   6 9 7 | 3 8 2 | 4 1 5   1 4 9 | 7 3 2 | 5 6 8
--   ---------------------   ---------------------   ---------------------   ---------------------
--   6 8 3 | 5 7 4 | 9 1 2   6 4 1 | 5 8 7 | 9 2 3   7 4 6 | 9 5 3 | 1 8 2   2 9 1 | 3 5 7 | 4 8 6
--   4 5 7 | 2 9 1 | 8 3 6   7 2 5 | 1 9 3 | 8 6 4   3 5 9 | 8 2 1 | 7 6 4   4 8 6 | 2 9 1 | 7 3 5
--   2 1 9 | 8 6 3 | 5 4 7   3 8 9 | 4 6 2 | 1 7 5   2 8 1 | 4 7 6 | 9 5 3   5 3 7 | 6 8 4 | 1 2 9
--   ---------------------   ---------------------   ---------------------   ---------------------
--   3 6 1 | 4 2 9 | 7 8 5   9 7 8 | 2 5 4 | 6 3 1   5 7 3 | 2 9 8 | 6 4 1   8 1 4 | 5 2 3 | 6 9 7
--   5 7 4 | 1 8 6 | 2 9 3   5 6 2 | 3 1 8 | 4 9 7   4 6 8 | 1 3 7 | 5 2 9   9 5 2 | 8 7 6 | 3 4 1
--   9 2 8 | 3 5 7 | 4 6 1   4 1 3 | 9 7 6 | 2 5 8   9 1 2 | 5 6 4 | 8 3 7   7 6 3 | 4 1 9 | 8 5 2

sudoku :: String -> IO ()
sudoku clues = show_solution $ take 1 $ solve (sudoku_constraints clues') [[1..9],[1..9],[1..9]] where
    clues' = format_sudoku_clues clues
    show_solution [] = putStrLn "No solution found"
    show_solution [solution] = show_sudoku_solution solution

format_sudoku_clues :: String -> [[Int]]
format_sudoku_clues = (unlist 9) . (map (\c -> read [c] :: Int)) . (filter (`elem` ['0'..'9']))

show_sudoku_solution :: [[Int]] -> IO()
show_sudoku_solution grid = putStrLn grid_string where
    [r1, r2, r3] = unlist 3 grid
    grid_string = 
        (concat $ map (print_line) r1) ++ dash_line ++ (concat $ map (print_line) r2) ++ dash_line ++ (concat $ map (print_line) r3)

    dash_line = (replicate 21 '-') ++ "\n"
    show_int_list = (intercalate " ") . (map show)

    print_line [] = "----"
    print_line line = let [c1,c2,c3] = unlist 3 line
        in (show_int_list c1) ++ " | " ++ (show_int_list c2) ++ " | " ++ (show_int_list c3) ++ "\n"

sudoku_constraints :: [[Int]] -> [Constraint]
sudoku_constraints clues = let
    cell_clue i j = [clues !! (j-1) !! (i-1)]
    box_selectors = cross_with (\a b -> Select [(`elem` a),(`elem` b)]) [[1,2,3],[4,5,6],[7,8,9]] [[1,2,3],[4,5,6],[7,8,9]]
    in (   [Matches (==(cell_clue x y)) (Select [(==x), (==y)]) | x <- [1..9], y <- [1..9], (cell_clue x y) /= [0]]
        ++ [Reducer (permutation_reducer) (Select [(==x), truth]) | x <- [1..9]]  -- Column permuation
        ++ [Reducer (permutation_reducer) (Select [truth, (==y)]) | y <- [1..9]]  -- Row permutation
        ++ (map (Reducer (permutation_reducer)) box_selectors) )                        -- 3x3 box permutation


---------------------------------------------------------------------------------------------------
-- Kenken Puzzles
---------------------------------------------------------------------------------------------------

kenken01 = unlines ["6","11 + A1 A2","2 / B1 C1","20 * D1 D2","6 * E1 F1 F2 F3","3 - B2 C2","3 / E2 E3","240 * A3 A4 B3 B4","6 * C3 D3","6 * C4 C5","7 + D4 D5 E5","30 * E4 F4","6 * A5 B5 ","9 + F5 F6","8 + A6 B6 C6","2 / D6 E6"]
kenken02 = unlines ["6","13 + A1 A2 B1 B2","180 * C1 D1 D2 E1","9 + F1 F2 F3","2 = C2","20 * E2 E3","15 + A3 A4 A5","6 * B3 C3","11 + C4 D3 D4 ","3 = B4","9 + D5 E4 E5 F4","2 / B5 C5 ","18 + D6 E6 F5 F6","8 + A6 B6 C6"]
kenken09 = unlines ["9","12 * A1 A2","60 * B1 B2 C1","4 / D1 E1","189 * F1 F2 F3","3 / G1 H1","432 * I1 I2 I3 H2 H3","2 / C2 C3","6 = D2","4 - E2 E3","2 - G2 G3","2 / A3 B3","11 + D3 D4","12 + A4 B4 C4","6 = E4","11 + F4 F5 F6","1 - G4 H4","15 + I4 I5 I6","10 + A5 B5","17 + C5 C6","40 * D5 D6 D7","2 / E5 E6","42 * G5 H5","2 - A6 B6","4 / G6 H6","45 * A7 B7","1 - C7 C8","10 + E7 E8","21 + F7 F8 F9 G9 H9","3 - G7 G8","12 + H7 H8 I7","13 + A8 A9","10 + B8 B9 C9","243 * D8 D9 E9","3 / I8 I9"]

-- Solutions:    kenken01       kenken02      kenken09
--               5 6 3 4 1 2    1 4 3 5 2 6   4 6 5 2 8 7 3 9 1
--               6 1 4 5 2 3    3 5 2 6 4 1   3 2 4 6 5 9 7 1 8
--               4 5 2 3 6 1    4 6 1 3 5 2   8 4 2 7 1 3 5 6 9
--               3 4 1 2 5 6    5 3 6 2 1 4   2 7 3 4 6 1 9 8 5
--               2 3 6 1 4 5    6 2 4 1 3 5   1 9 8 5 2 4 6 7 3
--               1 2 5 6 3 4    2 1 5 4 6 3   5 3 9 1 4 6 8 2 7
--                                            9 5 6 8 7 2 1 3 4
--                                            6 1 7 9 3 8 4 5 2
--                                            7 8 1 3 9 5 2 4 6

kenken :: String -> IO ()
kenken clues = show_solution $ take 1 $ solve (kenken_constraints clues') [[1..n],[1..n],[1..n]] where
    clues' = format_kenken_clues clues
    (n, _) = clues'
    show_solution [] = putStrLn "No solution found"
    show_solution [solution] = mapM_ (putStrLn . show_int_list) solution
    show_int_list = (intercalate " ") . (map show)

format_kenken_clues :: String -> (Int, [(Int, String, [[Int]])])
format_kenken_clues clues = (n, clues') where
    (n_str:clue_strs) = lines clues
    clues' = map (parse . words) clue_strs
    n = read n_str
    letter_lut a = (fromJust $ findIndex (==a) ['A'..'Z']) + 1
    parse (val:op:cells) = (read val :: Int, op, map parse_cell cells)
    parse_cell (letter:number) = [letter_lut letter, read number :: Int]

kenken_constraints :: (Int, [(Int, String, [[Int]])]) -> [Constraint]
kenken_constraints (n, clues) = let
    clue_constraint (target, op, cells) = Matches (pred target op) (Points cells)
    pred t "*" = (==t) . product
    pred t "+" = (==t) . sum
    pred t "=" = (==[t])
    -- For division and subtraction, any combination can match the target.
    pred t "/" = or . (map ((==(fromIntegral t)) . (foldl1' (/)) . (map fromIntegral))) . nub . permutations
    pred t "-" = or . (map ((==(fromIntegral t)) . (foldl1' (-)))) . nub . permutations
    in (   (map clue_constraint clues)  -- Clue constraints
        ++ [Reducer (permutation_reducer) (Select [(==x), truth]) | x <- [1..n]]   -- Column permuation
        ++ [Reducer (permutation_reducer) (Select [truth, (==y)]) | y <- [1..n]] ) -- Row permuation


---------------------------------------------------------------------------------------------------
-- N Queens Problem
---------------------------------------------------------------------------------------------------

n_queens n | n > 8 = n_queens' n 1
n_queens n = n_queens' n 92

n_queens' n i = show_solution $ take i $ solve (n_queens_constraints n) [[1..n],[1..n],[0,1]] where
    show_solution [] = putStrLn "No solution"
    show_solution solutions = mapM_ show_one solutions >> putStrLn ((show (length solutions)) ++ " solutions")
    show_one option = mapM_ (putStrLn . show_int_list) option >> putStrLn (replicate (n*2) '-')
    show_int_list = (intercalate " ") . (map show)

n_queens_constraints n = let
    points_tr_tl_bl = [(i,1) | i <- [1..n]] ++ [(1,j) | j <- [2..n]]
    dn_diagonals = filter ((>1) . length) $ [takeWhile (all (<=n)) $ iterate (map (+1)) [x,y] | (x,y) <- points_tr_tl_bl]
    points_tl_bl_br = [(1,j) | j <- [1..n]] ++ [(i,n) | i <- [2..n]]
    up_diagonals = filter ((>1) . length) $ [takeWhile (all (\x -> x > 0 && x <= n)) $ iterate (\[i,j] -> [i+1,j-1]) [x,y] | (x,y) <- points_tl_bl_br]
    
    selects = map Points (dn_diagonals ++ up_diagonals)
    in (    (map (Matches ((<=1) . sum)) selects)    -- Each diagonal has at most 1 queen.
            -- Using findIndices allows to break early as opposed to `(==1) . sum`. This seems slightly more efficient.
         ++ [Matches ((==1) . length . (take 2) . (findIndices (==1))) (Select [(==x), truth]) | x <- [1..n]]   -- Each col has only 1 queen.
         ++ [Matches ((==1) . length . (take 2) . (findIndices (==1))) (Select [truth, (==y)]) | y <- [1..n]] ) -- Each row has only 1 queen.


---------------------------------------------------------------------------------------------------
-- Kakuro Puzzles
---------------------------------------------------------------------------------------------------

kakuro01 = unlines ["x/x x/x 15/x 3/x 15/x x/x","x/x 4/6 x x x x/x","x/13 x x x x 3/x","x/3 x x 4/4 x x","x/x x/11 x x x x","x/x x/7 x x x x/x"]
-- (x/x , x/x , 15/x, 3/x, 15/x, x/x),     Solution:  0 0 0 0 0 0
-- (x/x , 4/6 , x   , x  , x   , x/x),                0 0 3 2 1 0
-- (x/13, x   , x   , x  , x   , 3/x),                0 3 5 1 4 0
-- (x/3 , x   , x   , 4/4, x   , x),                  0 1 2 0 3 1
-- (x/x , x/11, x   , x  , x   , x),                  0 0 1 3 5 2
-- (x/x , x/7 , x   , x  , x   , x/x)                 0 0 4 1 2 0

kakuro :: String -> IO ()
kakuro clues = show_solution $ take 1 $ solve (kakuro_constraints clues') [[1..width],[1..height],[0..(max width height)]] where
    clues' = (map words) . lines $ clues
    width = length (head clues')
    height = length clues'
    digit_width = if (max width height) > 9 then 2 else 1
    show_solution [] = putStrLn "No solution found"
    show_solution [solution] = mapM_ (putStrLn . show_int_list) solution
    show_int i = (replicate n ' ') ++ (show i) where n = digit_width - (length (show i))
    show_int_list = (intercalate " ") . (map show_int)

kakuro_constraints :: [[String]] -> [Constraint]
kakuro_constraints clues = let
    width = length (head clues)
    height = length clues

    row_constraints = concat . (zipWith parse_row [1..]) $ clues
    col_constraints = concat . (zipWith parse_col [1..]) . transpose $ clues

    value_constraints = [Matches (if (clues !! y !! x) == "x" then (/=[0]) else (==[0])) (Points [[x+1,y+1]]) | x <- [0..(width-1)], y <- [0..(height-1)]]
    splitOn ch xs = (,) (takeWhile (/=ch) xs) (tail $ dropWhile (/=ch) xs)

    next_visible row = (zipWith (\i v -> length $ takeWhile (=="x") (drop (i+1) row) ) [0..] row)

    h_constraint x y t l = Matches (\vec -> (t == sum vec) && has_n_unique_elements l vec) (Points [[i, y] | i <- [(x+1)..(x+l)]])
    v_constraint x y t l = Matches (\vec -> (t == sum vec) && has_n_unique_elements l vec) (Points [[x, j] | j <- [(y+1)..(y+l)]])

    parse_row y row = concat $ zipWith constraint [(x, y) | x <- [1..(length row)]] (zipWith (parse_cell) row (next_visible row)) where
        constraint (_,_) (0,0) = []
        constraint (x,y) (t, l) = [h_constraint x y t l]
        
        parse_cell "x" _ = (0,0)
        parse_cell str l = case splitOn '/' str of
            (_, "x") -> (0,0)
            (_, n) -> (read n, l)

    parse_col x col = concat $ zipWith constraint [(x, y) | y <- [1..(length col)]] (zipWith (parse_cell) col (next_visible col)) where
        constraint (_,_) (0,0) = []
        constraint (x,y) (t, l) = [v_constraint x y t l]
        
        parse_cell "x" _ = (0,0)
        parse_cell str l = case splitOn '/' str of
            ("x", _) -> (0,0)
            (n, _) -> (read n, l)

    in ( value_constraints ++ row_constraints ++ col_constraints )