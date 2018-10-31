import itertools
from functools import reduce
import operator

# Selector = ('Points', [[Int]]) | ('Select', [fn])
# Constraint = ('Matches', predicate_fn, Selector) | ('Reducer', reducer_fn, Selector)

truth = lambda _: True

def has_n_unique_elements(n): return lambda l: len(list(set(l))) == n

def unlist(n, l): return [l[i:(i+n)] for i in range(0,len(l),n)]
flatten = lambda l: [item for sublist in l for item in sublist]

def nub(l):
    used = []
    return [x for x in l if x not in used and (used.append(x) or True)]

def transpose(l): return list(map(list, zip(*l)))

def list_subtract(a, b):
    if len(b) == 0: return a

    if type(b[0]) == type([]):
        bs = {tuple(item) for item in b}
        return [item for item in a if tuple(item) not in bs]
    else:
        return [item for item in a if item not in set(b)]

def zipWith(f, a, b): return list(map(lambda x: f(*x), zip(a, b)))

def cross_with(f, a, b): return list(map(lambda x: f(*x), itertools.product(a,b)))

def subsequences(l): return flatten([itertools.combinations(l, n) for n in range(1, len(l)+1)])

def permutation_reducer(options):
    def test_seqs(l):
        if sum(l[0]) == 1 + len(l[1:]):
            return all(map(lambda x: x==l[0], l[1:]))
        return False

    bitmapped = list(map(lambda x: list(map(lambda y: y in x, range(1,len(options)+1))), options))
    
    subs = subsequences(range(0,len(options)))
    subs = filter(lambda seqs: test_seqs(list(map(lambda i: bitmapped[i], seqs))), subs)

    for seq in subs:
        vals = options[seq[0]]
        options = list(map(lambda il: il[0] in seq and vals or list_subtract(il[1], vals), enumerate(options)))

    return options

def prod(l): return reduce(operator.mul, l)

def combos(l):
    if l == []:
        return []
    elif len(l) == 1:
        return list(map(lambda x: [x], l[0]))
    elif len(l) == 2:
        return cross_with(lambda a,b: [a,b], l[0], l[1])
    else:
        return cross_with(lambda a,b: [a]+b, l[0], combos(l[1:]))

def isPrefixOf(a,b): return b[:len(a)] == a

def select(selector, l):
    (t, p) = selector
    if t == 'Points':
        return list(filter(lambda x: any(map(lambda y: isPrefixOf(y, x), p)), l))
    else: # t == 'Select'
        return list(filter(lambda x: all(zipWith(lambda f,a: f(a), p, x)), l))


def split_points(points):
    options = []
    positions = []
    for (p, o) in itertools.groupby(points, lambda x: x[:-1]):
        options.append(list(map(lambda x: x[-1], o)))
        positions.append(p)
    return (options, positions)

def solve(constraints, field):
    num_values = prod(map(len, field[:-1]))
    length_constraint = ('Reducer', lambda vec: len(vec) >= num_values and vec or [], ('Select', []))

    def post_process(vals): return transpose(unlist(len(field[1]), vals))

    def solver():
        acc = [combos(field)]

        while True:
            if len(acc) == 0: break

            points = acc.pop()

            points = apply_constraints([length_constraint]+constraints, points)
            (options, positions) = split_points(points)

            if all(map(lambda x: len(x)==1, options)):
                yield post_process(list(map(lambda x: x[0], options)))
            else:
                t_val = next(filter(lambda x: x > 1, map(len, options)))
                target = list(map(len, options)).index(t_val)

                p1 = [positions[target] + [options[target][0]]]
                p2 = [positions[target] + [v] for v in field[-1] if v != options[target][0]]
                without_target = list_subtract(points, p1) # [item for item in points if item not in set(p1)]
                with_target = list_subtract(points, p2) # [item for item in points if item not in set(p2)]

                acc.append(without_target)
                acc.append(with_target)

    return filter(lambda x: x != [], solver())

def apply_constraints(constraints, points):
    def apply(points):
        for c in constraints:
            selected_points = select(c[2], points)
            (options, positions) = split_points(selected_points)

            new_options = apply_reducer(c, options, positions)
            discard = list_subtract(selected_points, new_options) # [item for item in selected_points if item not in set(new_options)]
            points = list_subtract(points, discard) # [item for item in new_points if item not in set(discard)]
        return points

    while True:
        len1 = len(points)
        points = apply(points)
        if len(points) == len1:
            return points

def apply_reducer(constraint, options, positions):
    if constraint[0] == 'Matches':
        a = filter(constraint[1], combos(options))
        return nub(flatten(map(lambda x: zipWith(lambda p,v: p+[v], positions, x), a)))
    else: # == 'Reducer'
        a = constraint[1](options)
        return flatten(zipWith(lambda p,z: list(map(lambda v: p+[v], z)), positions, a))


sudoku01 = "040000179002008054006005008080070910050090030019060040300400700570100200928000060"
def format_sudoku_clues(s): return unlist(9, list(map(int, filter(lambda x: x in "0123456789", s))))

def sudoku_constraints(clues):
    def cell_clue(i,j): return [clues[j-1][i-1]]

    box_selectors = cross_with(lambda a,b: ('Select', [lambda x,a=a: x in a, lambda y,b=b: y in b]), [[1,2,3],[4,5,6],[7,8,9]], [[1,2,3],[4,5,6],[7,8,9]])

    return list(map(lambda s: ('Reducer', permutation_reducer, s), box_selectors)) \
        +[('Matches', lambda vec,x=x,y=y: vec==cell_clue(x,y), ('Points', [[x,y]])) for (x,y) in itertools.product(range(1,10), range(1,10)) if cell_clue(x,y) != [0]] \
        +flatten([[('Reducer', permutation_reducer, ('Select', [lambda x,i=i: x==i, truth])), \
                    ('Reducer', permutation_reducer, ('Select', [truth, lambda y,i=i: y==i]))] for i in range(1,10) ])

def sudoku(clues):
    solution = next(solve(sudoku_constraints(format_sudoku_clues(clues)), [range(1,10),range(1,10),range(1,10)]))
    # show_solution [] = putStrLn "No solution found"
    # show_solution [solution] = show_sudoku_solution solution

    def show_sudoku_solution(grid): 
        dash_line = '-'*21 + "\n"

        def print_line(line):
            if line == []: return "----"
            [c1,c2,c3] = unlist(3, line)
            return ' '.join(map(str, c1)) + ' | ' + ' '.join(map(str, c2)) + ' | ' + ' '.join(map(str, c3)) + '\n'

        [r1, r2, r3] = unlist(3, grid)
        grid_string = ''.join(map(print_line, r1)) + dash_line + ''.join(map(print_line, r2)) + dash_line + ''.join(map(print_line, r3))
        
        print(grid_string)
        
    show_sudoku_solution(solution)
