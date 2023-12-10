input_lines = readLines('./inputs/8.txt')

instructions = input_lines[1]

network_connections = input_lines[3:length(input_lines)]
network_connections_df = data.frame(
  node = split_vectorised(network_connections, '[ =(),]+', 1),
  left = split_vectorised(network_connections, '[ =(),]+', 2),
  right = split_vectorised(network_connections, '[ =(),]+', 3)
)

process = list()

for(index in 1:nrow(network_connections_df)){
  process[[network_connections_df$node[index]]] = 
    c(network_connections_df$left[index], network_connections_df$right[index])
}

split_instructions = strsplit(instructions, '')[[1]]

steps_taken = 0
current_position = 'AAA'

for(iterations in 1:100){
  done = FALSE
  for(instruction in split_instructions){
    print(current_position)
    if(current_position == 'ZZZ'){
      done = TRUE
      break
    }
    next_positions = process[[current_position]]
    if(instruction == 'L'){
      current_position = next_positions[1]
    } else {
      current_position = next_positions[2]
    }
    steps_taken = steps_taken + 1
  }
  if(done){
    break
  }
}

#14893

A_nodes = network_connections_df$node[substr(network_connections_df$node, 3, 3) == 'A']
Z_nodes = network_connections_df$node[substr(network_connections_df$node, 3, 3) == 'Z']


# Get the 1st and 2nd times each A-node-starting gets to each Z-node

get_first_and_second_time = function(start, end){
  steps_taken = 0
  current_position = start
  reached_times = 0
  first_reached_time = -1
  second_reached_time = -1
  for(iterations in 1:500){
    for(instruction in split_instructions){
      if(current_position == end){
        reached_times = reached_times + 1
        if(reached_times == 2){
          second_reached_time = steps_taken
          break
        } else {
          first_reached_time = steps_taken
        }
      }
      next_positions = process[[current_position]]
      if(instruction == 'L'){
        current_position = next_positions[1]
      } else {
        current_position = next_positions[2]
      }
      steps_taken = steps_taken + 1
    }
    if(reached_times == 2){
      break
    }
  }
  return(list(first_time = first_reached_time, second_time = second_reached_time))
}

pairs_of_nodes = data.frame(first_node = A_nodes) %>%
  cross_join(data.frame(last_node = Z_nodes)) %>%
  mutate(first_time = NA_integer_, second_time = NA_integer_)

for(row in 1:nrow(pairs_of_nodes)){
  first_and_second_time = get_first_and_second_time(pairs_of_nodes$first_node[row],
                                                    pairs_of_nodes$last_node[row])
  pairs_of_nodes$first_time[row] = first_and_second_time$first_time
  pairs_of_nodes$second_time[row] = first_and_second_time$second_time
}

A_Z_pairings = pairs_of_nodes[pairs_of_nodes$first_time != -1,]

(A_Z_pairings$second_time - A_Z_pairings$first_time)/length(split_instructions) # all integers thank goodness

(A_Z_pairings$first_time) %% length(split_instructions) # all 0

A_Z_pairings$first_time / length(split_instructions)

A_Z_pairings$second_time - A_Z_pairings$first_time * 2 # Okay this is much easier than it would be otherwise

# And A_Z_pairings$first_time / length(split_instructions) are all prime

as.bigz(prod(A_Z_pairings$first_time / length(split_instructions))) * length(split_instructions)
#10241191004509
