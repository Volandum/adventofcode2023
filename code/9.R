input_lines = readLines('./inputs/9.txt')

lines = list()
for(index in 1:length(input_lines)){
  lines[[index]] = as.numeric(strsplit(input_lines[index], ' ')[[1]])
}

process_line = function(line){
  diffs = list()
  current_diffs = line
  order = 0
  for(difference_time in 1:20){
    if(all(current_diffs == 0)){
      break
    }
    order = order + 1
    current_diffs = diff(current_diffs)
    diffs[[order]] = current_diffs
  }
  new_diff = 0
  for(working_order in order:1){
    new_diff = diffs[[working_order]][length(diffs[[working_order]])] + new_diff
  }
  return(line[length(line)] + new_diff)
}

processed_lines = sapply(lines, process_line)

sum(processed_lines) #1953784198

process_line_backwards = function(line){
  diffs = list()
  current_diffs = line
  order = 0
  for(difference_time in 1:20){
    if(all(current_diffs == 0)){
      break
    }
    order = order + 1
    current_diffs = diff(current_diffs)
    diffs[[order]] = current_diffs
  }
  new_diff = 0
  for(working_order in order:1){
    new_diff = diffs[[working_order]][1] - new_diff
  }
  return(line[1] - new_diff)
}

processed_lines_backwards = sapply(lines, process_line_backwards)

sum(processed_lines_backwards) #957
