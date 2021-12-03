module HaskellAsEnglish where

    -- literals
    empty_list = []
    an_infinite_list_starting_at_zero = [0..]

    -- types
    type ListOfTextSnippets = [String]
    type ListOfCharacters = [Char]
    type WholeNumber = Int
    
    -- functions
    incremented :: Int -> Int
    incremented n = n + 1
    
    -- function aliases
    (·) f b = f $ b
    every = map
    as_follows = map
    every_time_it_holds_that :: Foldable t => t Bool -> Bool
    every_time_it_holds_that = and
    first_character_of = head
    first_line_of_text_of = head
    and_also :: Bool -> Bool -> Bool
    and_also = (&&)
    equalling :: (Eq a) => a -> a -> Bool
    equalling = (==)
    equal :: (Eq a) => a -> a -> Bool
    equal = (==)
    is :: (Eq a) => a -> a -> Bool
    is = (==)
    is_equal_to :: (Eq a) => a -> a -> Bool
    is_equal_to = (==)
    select = filter
    elements_not_at_the_front_of = tail
    element_not_at_the_front_of = tail
    placed_in_front_of = (:)
    size_of :: Foldable t => t a -> Int
    size_of = length
    in_front_of = (:)
    is_less_than :: (Ord a) => a -> a -> Bool
    is_less_than = (<)
    using = zipWith
    multiplication :: Int -> Int -> Int
    multiplication = (*)
    reversing = reverse
    to_the_power_of :: (Integral b, Num a) => a -> b -> a
    to_the_power_of = (^)
    length_of :: Foldable t => t a -> Int
    length_of = length
    at = (!!)
    
    -- Identity functions
    the = id
    none = id
    above = id
    for = id
    result = id
    true = id
    placing = id
    but = id
    use = id
    first = id
    doing = id
    version = id
    when = id
    you = id
    compute = id
    output = id
    with = id
    numbers = id
    criterium = id
    two = id
    following = id
    test = id
    must = id
    be = id
    inside = id
    each = id
    entries = id
    given = id
    that = id
    are = id
    _of = id
    _in = id
    _take = id
    _all = id
    _and = id
    _as = id
    over = id
    on = id
    set = id
    being = id
    to = id
    calling = id
    which = id
    means = id
    converting = id
    conversion = id
    an = id
    we = id
    shall = id
    obtained = id
    by = id
    get = id
    values = id
    value = id
    reach = id
    from = id
    most = id
    common = id
    bits = id
    checking = id
    index = id
    starting = id