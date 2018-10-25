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
    generate_field [d1, d2] = cross_with (\a b -> [a, b]) d1 d2
    generate_field [d1, d2, d3] = cross_with (:) d1 (generate_field [d2, d3])

    select :: Selector -> [[Int]] -> [[Int]]  -- Apply selector to points. e.g. select [even] [[x,y]] => [[x,y] : x is even]
    select (Points points) = filter (\p -> any (`isPrefixOf` p) points)
    select (Select filters) = filter (test_each_selector) where
        test_each_selector val = and $ map ($val) selector_functions
        selector_functions = zipWith (\n p -> (p . (!!(n-1)))) [1..] filters

    -- Returns all possible solutions. If you want only one solution, use `take 1 $ solve ..`.
    solve :: [Constraint] -> [[Int]] -> [[[Int]]]
    solve constraints field = post_process $ solve' (generate_field field) [] where
        post_process results = fmap (transpose . (unlist num_cols)) $ filter (/=[]) $ results

        num_cols = product $ map length (init . init $ field)
        num_values = product $ map length (init field)
        length_constraint = Reducer (\vec -> if length vec >= num_values then vec else []) (Select [])

        solve' solver_step acc = let
            pop = if null acc then [] else solve' (head acc) (tail acc)  -- Pop next option off stack.

            new_points = apply_constraints (length_constraint:constraints) solver_step
            options = (map . map) last $ groupOn init new_points
            positions = nub $ (map init) new_points

            target = fromJust $ findIndex (==t_val) lengths
                where lengths = map length options; t_val = head $ filter (>1) lengths          
            with_target = new_points \\ [(positions !! target) ++ [v] | v <- last field, v /= head (options !! target)]
            without_target = new_points \\ [(positions !! target) ++ [head (options !! target)]]

            in ( if all (==1) (map length options) then [concat options] ++ pop  -- One option left for each value.
                 else solve' with_target (without_target:acc) )         -- Split into two branches, push one branch to acc.

    apply_constraints constraints = apply' where
        -- Apply each constraint in sequence, then repeat if there's fewer options until no progress is made.
        apply' points = if length points /= length new_points then apply' new_points else new_points where
            new_points = foldl' (update_points) points constraints
            
            update_points points constraint = let
                selected_points = select (selector $ constraint) points
                options = (map . map) last $ groupOn init selected_points -- [[1,1],[1,2],[2,3]] => [[1,2],[3]]
                positions = nub $ map init selected_points   -- [[1,1],[1,2],[2,3]] => [[1],[2]]
                
                new_options = apply_reducer constraint options positions
                in ( points \\ (selected_points \\ new_options)  )

    -- Applying a `Matches` constraint is reducing by filtering the combinations with the `Matches` predicate.
    apply_reducer (Matches predicate _) options positions = to_point_list . (filter predicate) . combos $ options
        where to_point_list = nub . concat . (map (zipWith (\p v -> p++[v]) positions))

    apply_reducer (Reducer reduce _) options positions = to_point_list . reduce $ options
        where to_point_list = concat . (zipWith (\p -> map (\v -> p++[v])) positions)