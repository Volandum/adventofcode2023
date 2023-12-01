input_lines = readLines('./inputs/1.txt')

two_or_more_digits_regex = '[a-z]*([0-9])[0-9a-z]*([0-9])[a-z]*'
one_digit_regex = '[a-z]*([0-9])[a-z]*'

calibration_values_str = ifelse(
  grepl(two_or_more_digits_regex, input_lines),
  gsub(two_or_more_digits_regex,
       '\\1\\2', input_lines, perl = TRUE),
  gsub(one_digit_regex,
       '\\1\\1', input_lines, perl = TRUE))

sum(as.numeric(calibration_values_str)) #54968

digits = c('one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine')
digits_regex = paste0(c(digits, as.character(0:9)), collapse = '|')

first_digits = gsub(
  paste0('.*?(', digits_regex, ').*'),
  '\\1', input_lines, perl = TRUE
) 
# *? says take the smallest number needed to get a match 
# (whereas * will normally take the largest number)

last_digits = gsub(
  paste0('.*(', digits_regex, ').*'),
  '\\1', input_lines, perl = TRUE
)

first_digits_numerical_char = ifelse(first_digits %in% digits,
                                     as.character(match(first_digits, digits)),
                                     first_digits)

last_digits_numerical_char = ifelse(last_digits %in% digits,
                                    as.character(match(last_digits, digits)),
                                    last_digits)

sum(as.numeric(paste0(first_digits_numerical_char, last_digits_numerical_char))) #54094
