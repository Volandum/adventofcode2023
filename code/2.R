input_lines = readLines('./inputs/2.txt')

get_iteration_df = function(iteration){
  # e.g. 10 red, 2 blue, 5 green
  iteration_elements = strsplit(iteration, ', ')[[1]]
  numbers = as.numeric(split_vectorised(iteration_elements, ' ', 1))
  colours = split_vectorised(iteration_elements, ' ', 2)
  return(data.frame(numbers = numbers, colours = colours))
}

get_game_df = function(line){
  first_split = strsplit(line, ': ')[[1]]
  gameid = first_split[1]
  second_split = strsplit(first_split[2], '; ')[[1]]
  dfs = sapply(1:length(second_split),
               function(index){
                 df = get_iteration_df(second_split[index])
                 df$iteration = index
                 return(df)
               },
               simplify = FALSE)
  df = do.call(rbind, dfs)
  df$gameid = as.numeric(strsplit(gameid, ' ')[[1]][2])
  return(df)
}

input_dfs = sapply(input_lines, get_game_df, simplify = FALSE)
input_df = do.call(rbind, input_dfs)

impossible_games = input_df %>%
  filter((colours == 'red' & numbers > 12) |
           colours == 'green' & numbers > 13 |
           colours == 'blue' & numbers > 14) %>%
  pull(gameid) %>% unique

sum(setdiff(1:100, impossible_games)) # 2061

mins = input_df %>% group_by(gameid, colours) %>% summarise(min_number = max(numbers)) %>% ungroup()

powers = mins %>% group_by(gameid) %>% filter(n() == 3) %>% summarise(power = prod(min_number)) %>%
  ungroup()

sum(powers$power) #72596
