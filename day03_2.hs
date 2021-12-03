import HaskellAsEnglish

count_the_first_characters_of :: [[Char]] -> Char -> Int
count_the_first_characters_of diagnostic_report the_character = when·you·_take·the·length_of only_the_ones_with_the_character
  where
    list_of_first_characters = when·you·compute·every first_character_of·the diagnostic_report
    only_the_ones_with_the_character = when·you·filter (with·the·criterium·equalling the_character) (on·the list_of_first_characters)

all_empty_in :: [[Char]] -> Bool
all_empty_in diagnostic_report = true·when·the·sum (_of·every length (_of·the·entries·_in·the·diagnostic_report)) `is` 0

all_are_the_same_in diagnostic_report | given·that·the·length_of (the·diagnostic_report) `is` 0 = True
all_are_the_same_in diagnostic_report | given·that·the·length_of (the·diagnostic_report) `is` 1 = True
all_are_the_same_in diagnostic_report | given·that·none·_of·the·above·are·True                  = when·we·must·compute·the_result
  where
    the_second_line_of_text_of a_list = the·first_line_of_text_of (elements_not_at_the_front_of a_list)
    the_result = the·first_line_of_text_of (the·diagnostic_report) `is_equal_to` the_second_line_of_text_of (the·diagnostic_report) `and_also` all_are_the_same_in (element_not_at_the_front_of (the·diagnostic_report))


shrinked :: [[Char]] -> [[Char]]
shrinked diagnostic_report =  when·you·_take·every element_not_at_the_front_of (the·inside·_of·each·_of·the·entries·_in·the diagnostic_report)

count_of_ones_of  diagnostic_report = when·you·count_the_first_characters_of (the·diagnostic_report) (with·'1')
count_of_zeros_of diagnostic_report = when·you·count_the_first_characters_of (the·diagnostic_report) (with·'0')

most_common_bits_in :: [[Char]] -> [Char]
most_common_bits_in diagnostic_report | given·that·all_empty_in diagnostic_report                                                                        = empty_list
most_common_bits_in diagnostic_report | given·that·the·count_of_ones_of (the diagnostic_report) `is_less_than` count_of_zeros_of (the diagnostic_report) = '0' `placed_in_front_of` (most_common_bits_in (the·shrinked diagnostic_report))
                                      | given·that·none·_of·the·above·are·True                                                                           = '1' `placed_in_front_of` (most_common_bits_in (the·shrinked diagnostic_report))

least_common_bits_from :: [Char] -> [Char]
least_common_bits_from most_common_bits | given·that·the·size_of most_common_bits `is` 0                    = empty_list
least_common_bits_from most_common_bits | given·that·the·first_character_of (the·most_common_bits) `is` '1' = '0' `placed_in_front_of` (least_common_bits_from·elements_not_at_the_front_of most_common_bits)
least_common_bits_from most_common_bits                                                                     = '1' `placed_in_front_of` (least_common_bits_from·elements_not_at_the_front_of most_common_bits)

sieveWith :: Int -> [[Char]] -> Bool -> [Char]
sieveWith index diagnostic_report want_to_check_with_most_common_bits | given·that·the·length_of (the·diagnostic_report) `is` 0 = empty_list
sieveWith index diagnostic_report want_to_check_with_most_common_bits | given·that·the·length_of (the·diagnostic_report) `is` 1 = the·first_line_of_text_of (the·diagnostic_report)
sieveWith index diagnostic_report want_to_check_with_most_common_bits | given·that·all_are_the_same_in (the·diagnostic_report)  = the·first_line_of_text_of (the·diagnostic_report)
sieveWith index diagnostic_report want_to_check_with_most_common_bits | given·that·none·_of·the·above·are·True                  = sieveWith (the·incremented index) (_and·the_filtered_diagnostic_report) (_and·the·value·_of·want_to_check_with_most_common_bits)
    where the_filtered_diagnostic_report = when·you·filter (\the_bits -> (for·which·the·value·_of·the_bits `at` index) `is` (the·value·_of·the_model `at` index)) (over·the·diagnostic_report)
          the_model = if we·want_to_check_with_most_common_bits then we·use·the·most_common_bits_in (the diagnostic_report) else we·use·the (least_common_bits_from (the·most_common_bits_in (the diagnostic_report)))

list_of_powers_of_two_with_size the_amount = obtained·by·reversing the_finite_list_of_powers
    where
        the_finite_list_of_powers = when·we·take the_amount (_of·numbers·from the_infinite_list_of_powers)
        the_infinite_list_of_powers = obtained·by·the_generator_with 0
        the_generator_with the_number = doing·the·following ((placing·(2 `to_the_power_of` the_number)) `in_front_of` (the_generator_with (an·incremented (version·_of·the_number))))

convert_to_decimal text = when·you·compute·the·sum (_of·the converted_bits)
    where
        convert character = when·you·read [which·means·the·conversion·character] :: Int
        the_converted_inputs = when·you·compute·every (output·_of·convert) (over·the text)
        converted_bits = using multiplication (over·the_converted_inputs) (_and·the list_of_powers_of_two_with_size (the·size_of the_converted_inputs))
                            

parse :: String -> [String]
parse input_read_from_the_file = when·you·_take·_all·the·lines (_of·the·input_read_from_the_file)

get_answer :: String -> Int
get_answer input = when·you·_take·the·product (_of·the_sub_results)
    where
        the_sub_results = as_follows convert_to_decimal the_sieved_results
        the_sieved_results = as_follows (sieveWith (index 0) (_and·the parsed_input)) [with·checking·most·common·bits·_as True, _and·checking·most·common·bits·_as False]
        parsed_input = when·we·parse (the·input)




-- IO stuff sucks so imma keep it short
day3 :: IO()
day3 = do
    inp <- readFile "./inputs/day03.txt"
    putStrLn $ show $ get_answer inp
