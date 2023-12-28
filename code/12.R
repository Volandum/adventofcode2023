input_lines = readLines('./inputs/12.txt')

input_df = data.frame(
  string = split_vectorised(input_lines, ' ', 1),
  positions = split_vectorised(input_lines, ' ', 2)
)

get_number_of_questions = function(string){
  sum(strsplit(string, '')[[1]] == '?')
}

# table(Vectorize(get_number_of_questions)(input_df$string))
# Goes up to 18 so brute force possible but might take a little while

potential_string = input_df$string[500]
potential_conditions = input_df$positions[500]

conditions_to_pattern = function(pattern){
  pattern %>% strsplit(',') %>% el %>%
    paste0('#{', ., '}') %>%
    paste0(collapse = '\\.+') %>%
    paste0('^\\.*', ., '\\.*$')
}

conditions_to_possible_pattern = function(pattern){
  pattern %>% strsplit(',') %>% el %>%
    paste0('[#?]{', ., '}') %>%
    paste0(collapse = '[\\.?]+') %>%
    paste0('^[\\.?]*', ., '[\\.?]*$')
}


get_options_recursive = function(string, question_marks, formal_pattern, possible_pattern){
  if(!grepl(possible_pattern, string)){
    return(0)
  }
  if(question_marks == 0){
    if(grepl(formal_pattern, string)){
      return(1)
    } else {
      return(0)
    }
  }
  hash_option_string = sub('?', '#', string, fixed = TRUE)
  options_for_hash_option_string = get_options_recursive(
    hash_option_string, question_marks - 1, formal_pattern, possible_pattern
  )
  dot_option_string = sub('?', '.', string, fixed = TRUE)
  options_for_dot_option_string = get_options_recursive(
    dot_option_string, question_marks - 1, formal_pattern, possible_pattern
  )
  return(options_for_dot_option_string + options_for_hash_option_string)
}

get_options = function(string, conditions){
  formal_pattern = conditions_to_pattern(conditions)
  possible_pattern = conditions_to_possible_pattern(conditions)
  question_marks = get_number_of_questions(string)
  return(get_options_recursive(string, question_marks, formal_pattern, possible_pattern))
}

input_df$options = Vectorize(get_options)(input_df$string, input_df$positions)

sum(input_df$options) #7173

get_options_unfolded = function(string, conditions){
  unfolded_string = paste0(rep(string, 5), collapse = '?')
  unfolded_conditions = paste0(rep(conditions, 5), collapse = ',')
  print(unfolded_string)
  print(unfolded_conditions)
  get_options(unfolded_string, unfolded_conditions)
}
get_options_unfolded('?###????????', '3,2,1') # takes a while to get the right answer

unfolded_options = rep(-1, length(input_df))
for(index in 1:length(input_df)){
  print(index)
  start_time = Sys.time()
  unfolded_options[index] = get_options_unfolded(input_df$string[index], input_df$positions[index])
  end_time = Sys.time()
  print(end_time - start_time)
} # Doesn't run fast enough, the problem with this algorithm is that it confirms every positive possibility, so if there are a lot then it can take ages

# Try an algorithm that uses the conditions list rather than the string, and uses caching/memoisation?

