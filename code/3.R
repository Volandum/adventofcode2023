input_lines = readLines('./inputs/3.txt')

get_numbers = function(line){
  matches = gregexec('\\d+', line, perl = T)[[1]]
  match_locations = t(matches)
  match_lengths = t(attr(matches, 'match.length'))
  match_count = length(matches)
  matched_strings = sapply(1:match_count,
                           function(index){
                             substring(line, match_locations[index],
                                       match_locations[index] + match_lengths[index] - 1)
                           })
  output = data.frame(
    match_locations = match_locations, match_lengths = match_lengths, matched_strings = matched_strings
  )
  return(output)
}

get_symbols = function(line){
  matches = gregexec('[^.\\d]', line, perl = T)[[1]]
  match_locations = t(matches)
  match_count = length(matches)
  matched_strings = sapply(1:match_count,
                           function(index){
                             substring(line, match_locations[index],
                                       match_locations[index])
                           })
  output = data.frame(
    match_locations = match_locations, matched_strings = matched_strings
  )
  if(match_count == 1){
    if(match_locations == -1){
    return(data.frame())
  }}
  return(output)
}

numbers = process_lines_into_df(input_lines, get_numbers)
symbols = process_lines_into_df(input_lines, get_symbols)

number_symbol_adjacency = 
  sqldf("
  select numbers.matched_strings as number_str,
  numbers.match_locations as number_start, numbers.line_id as number_line,
  symbols.line_id as symbol_line, symbols.match_locations as symbol_location,
  symbols.matched_strings as symbol
  from numbers
  inner join symbols
  on symbols.line_id between numbers.line_id - 1 and numbers.line_id + 1
  and symbols.match_locations between numbers.match_locations - 1 and numbers.match_locations + numbers.match_lengths
        ")

number_symbol_adjacency %>% group_by(number_line, number_start) %>%
  summarise(n = n()) %>% ungroup() %>% count(n) #each number shows up only once

number_symbol_adjacency %>% pull(number_str) %>% as.numeric %>% sum #556057

gears = number_symbol_adjacency %>% filter(symbol == '*') %>%
  group_by(symbol_line, symbol_location) %>%
  summarise(number_numbers = n(), 
            least_number = min(as.numeric(number_str)), 
            greatest_number = max(as.numeric(number_str)),
            .groups = 'drop')

gears %>% filter(number_numbers == 2) %>%
  mutate(ratio = least_number * greatest_number) %>%
  pull(ratio) %>% sum #82824352
