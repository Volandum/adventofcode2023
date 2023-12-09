input_lines = readLines('./inputs/6.txt')
times = as.numeric(strsplit(input_lines[1], ' +', perl = TRUE)[[1]][2:5])
distances = as.numeric(strsplit(input_lines[2], ' +', perl = TRUE)[[1]][2:5])

ways_to_exceed = function(time, distance){
  sum((0:time) * (time:0) > distance)
}

margins_of_error = Vectorize(ways_to_exceed)(times, distances)
  
prod(margins_of_error) #512295

true_time = strsplit(input_lines[1], ' +', perl = TRUE)[[1]][2:5] %>%
  paste0(collapse = '') %>% as.numeric

true_distance = strsplit(input_lines[2], ' +', perl = TRUE)[[1]][2:5] %>%
  paste0(collapse = '') %>% as.numeric

# find minimum x s.t. x(true_time - x) - true_distance > 0

polyroot(c(-true_distance, true_time, -1))

#roots are approximately 5138491, 41669375
#the function is negative at 5138491 so the first option is 5138492 and the last option is true_time - 5138492

true_time - 5138492 - 5138492 + 1 #36530883
