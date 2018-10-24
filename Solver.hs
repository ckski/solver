module Solver ( Constraint (..), Selector (..), solve, truth, has_n_unique_elements, permutation_reducer, unlist, cross_with) where
    import Data.Bool; import Data.List; import Data.Maybe
    
    data Selector = Points [[Int]] | Select [Int -> Bool]
    data Constraint = Matches { predicate :: [Int] -> Bool, selector :: Selector } 
                    | Reducer { reducer :: [[Int]] -> [[Int]], selector :: Selector }

    truth = \_ -> True

    has_n_unique_elements n = (==n) . length . nub  -- True if the list has n unique elements.

    -- A reducer takes a list of options and returns the reduced list of options.
    -- The permutation reducer removes options that would break the permutation constraint.
    permutation_reducer options = reduced_cols where
        bitmapped = map (\x -> map (fromEnum . (`elem` x)) [1..(length options)]) options

        -- Tests if the options in the sequence are the only allowable ones for the set.
        -- [[1,0,0,0]] => True, [[1,1,0,0],[1,1,0,0]] => True
        test_seqs (seq1:seqs) | (sum seq1) == s = all (==seq1) seqs where s = 1 + length seqs; n = length seq1
        test_seqs _ = False

        subs = tail $ subsequences [0..(n-1)] where n = length options -- Subsequences of the options to search.

        -- Returns a list of sequences which can be used in reduction.
        test_subsequences bitmap = catMaybes $ map (\seqs -> bool Nothing (Just seqs) (test_seqs (map (bitmap!!) seqs))) $ subs

        reduced_cols = foldl' apply_reduction options (test_subsequences bitmapped)

        apply_reduction options seq = zipWith (\i list -> if i `elem` seq then vals else list\\vals) [0..] options
            where vals = options !! (head seq)

    unlist :: Int -> [a] -> [[a]]   -- Turns a flat list into nested lists.
    unlist _ [] = []
    unlist n l = [take n l] ++ (unlist n (drop n l))

    cross_with :: (a -> b -> c) -> [a] -> [b] -> [c]
    cross_with f a b = [f a' b' | a' <- a, b' <- b]

    groupOn f = groupBy (\a b -> f a == f b)


    combos :: [[a]] -> [[a]]  -- Returns the combinations of choosing 1 element from each list.
    combos [] = []
    combos [a] = map (\x -> [x]) a
    combos [a,b] = cross_with (\a b -> [a, b]) a b
    combos (x:xs) = cross_with (:) x (combos xs)

    generate_field :: [[a]] -> [[a]]  -- Turns a field ( [[1..3],[1..3]] ) into a list of points ( [[1,1],[1,2],..])
    generate_field dims = gen' dims where
        gen' [] = [[]]
        gen' [d] = [d]
        gen' [d1, d2] = cross_with (\a b -> [a, b]) d1 d2
        gen' [d1, d2, d3] = cross_with (:) d1 (gen' [d2, d3])
        gen' [d1, d2, d3, d4] = cross_with (:) d1 (gen' [d2, d3, d4])

    select :: Selector -> [[Int]] -> [[Int]]  -- Apply selector to points. e.g. select [even] [[x,y]] => [[x,y] : x is even]
    select (Points points) = filter (\p -> any (`isPrefixOf` p) points)
    select (Select filters) = filter (test_each_selector) where
        test_each_selector :: [Int] -> Bool
        test_each_selector val = and $ map ($val) selector_functions
        selector_functions :: [[Int] -> Bool]
        selector_functions = zipWith (\n p -> (p . (!!(n-1)))) [1..] filters

    -- Returns all possible solutions. If you want only one solution, use `take 1 $ solve ..`.
    solve :: [Constraint] -> [[Int]] -> [[[Int]]]
    solve constraints field = fix_transpose $ solve' (generate_field field) [] where
        fix_transpose results = fmap (transpose . (unlist num_cols)) results

        num_cols = (product $ map length (init . init $ field))
        num_values = product $ map (toInteger . length) (init field)

        solve' solver_step acc = let

            pop = if null acc then [] else solve' (head acc) (tail acc)  -- Pop next option off stack.
            push = solve' with_target (without_target:acc)               -- Solve with, push without to stack.
            continue = solve' new_points acc                             -- Repeat applying the constraints.
            return = [concat value_options]

            new_points = apply_constraints constraints solver_step
            double_check = apply_constraints constraints new_points

            value_options = (map . map) last $ groupOn init new_points
            value_positions = nub $ (map init) new_points

            target = fromJust $ findIndex (==t_val) lengths
                where lengths = map length value_options; t_val = head $ filter (>1) lengths          

            with_target = new_points \\ [(value_positions !! target) ++ [v] | v <- last field, v /= head (value_options !! target)]
            without_target = new_points \\ [(value_positions !! target) ++ [head (value_options !! target)]]

            in (if (toInteger $ length value_options) < num_values then -- If fewer options than needed, fail.
                    pop
                else if all (==1) (map length value_options) then  -- One option left for each value.
                    if (length double_check) == 0 then pop else return ++ pop
                else if length new_points == length solver_step then
                    push   -- No progress was made so go to guessing strategy.
                else
                    continue
            )

    apply_constraints constraints = apply' where
        apply' points = foldl' (update_points) points constraints where  -- Apply each constraint in sequence.
            update_points points constraint = let
                selected_points = select (selector $ constraint) points -- List of points
                -- list of points to list of value options, [[1,1],[1,2],[2,3]] => [[1,2],[3]]
                options = (map . map) last $ groupOn init selected_points
                positions = nub $ map init selected_points   -- [[1,1],[1,2],[2,3]] => [[1],[2]]
                
                new_options = apply_reducer constraint options positions

                in ( points \\ (selected_points \\ new_options) )  -- Remove options that did not pass constraint. 

    -- Applying a `Matches` constraint is reducing by filtering the combinations with the `Matches` predicate.
    apply_reducer (Matches predicate _) options positions = to_point_list . (filter predicate) . combos $ options
        where to_point_list = nub . concat . (map (zipWith (\p v -> p++[v]) positions))

    apply_reducer (Reducer reduce _) options positions = to_point_list . reduce $ options
        where to_point_list = concat . (zipWith (\p -> map (\v -> p++[v])) positions)