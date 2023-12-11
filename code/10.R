input_lines = readLines('./inputs/10.txt')

height = length(input_lines)
width = nchar(input_lines[1])

split_input_lines = strsplit(input_lines, '')
# a point is represented by x-coordinate + y-coordinate * (width + 1) 
# (so they start at 142 and go to 140 * 142)
# x goes right, y goes down

x_y_to_point = function(x,y){
  return(x + y * (width + 1))
}

point_to_x_y = function(point){
  return(c(point %% (width + 1), point %/% (width + 1)))
}

#Implement dijkstra's algorithm

s_row = which(sapply(split_input_lines,
                     function(x){
                       any(x == 'S')
                     }))
s_column = which(split_input_lines[[s_row]] == 'S')

down_tiles = c('S', '|', '7', 'F')
right_tiles = c('S', '-', 'L', 'F')
left_tiles = c('S', '-', '7', 'J')
up_tiles = c('S', '|', 'L', 'J')

distance_to = rep(-1, (height + 1) * (width + 1))
points_to_handle = x_y_to_point(s_column, s_row)
distance_to[points_to_handle] = 0
while(length(points_to_handle) > 0){
  new_points_to_handle = integer(0)
  for(point in points_to_handle){
    coords = point_to_x_y(point)
    x = coords[1]
    y = coords[2]
    character = split_input_lines[[y]][x]
    current_distance = distance_to[point]
    # down
    if(y < height & character %in% down_tiles){
      if(split_input_lines[[y + 1]][x] %in% up_tiles){
        new_point = x_y_to_point(x, y + 1)
        if(distance_to[new_point] == -1){
          new_points_to_handle = c(new_points_to_handle, new_point)
          distance_to[new_point] = current_distance + 1
        }
      }
    }
    # up
    if(y > 1 & character %in% up_tiles){
      if(split_input_lines[[y - 1]][x] %in% down_tiles){
        new_point = x_y_to_point(x, y - 1)
        if(distance_to[new_point] == -1){
          new_points_to_handle = c(new_points_to_handle, new_point)
          distance_to[new_point] = current_distance + 1
        }
      }
    }
    # left
    if(x > 1 & character %in% left_tiles){
      if(split_input_lines[[y]][x - 1] %in% right_tiles){
        new_point = x_y_to_point(x - 1, y)
        if(distance_to[new_point] == -1){
          new_points_to_handle = c(new_points_to_handle, new_point)
          distance_to[new_point] = current_distance + 1
        }
      }
    }
    # right
    if(x < width & character %in% right_tiles){
      if(split_input_lines[[y]][x + 1] %in% left_tiles){
        new_point = x_y_to_point(x + 1, y)
        if(distance_to[new_point] == -1){
          new_points_to_handle = c(new_points_to_handle, new_point)
          distance_to[new_point] = current_distance + 1
        }
      }
    }
  }
  points_to_handle = new_points_to_handle
}

max(distance_to) #7086

# I think you are enclosed by the loop if you're a tile that's not in the loop but 
# to your left you have an odd number of contiguous loop elements

loop_elements = which(distance_to >= 0)

get_interior_indexes_for_row = function(row){
  loop_elements_in_row = loop_elements[loop_elements > (width + 1) * row & loop_elements < (width + 1) * (row + 1)]
  x_coords = loop_elements_in_row - ((width + 1) * row)
  row_letters = split_input_lines[[row]]
  relevant_row_letters = x_coords[row_letters[x_coords] %in% c('|', 'S')]
  down_row_letters = x_coords[row_letters[x_coords] %in% c('L', '7')]
  up_row_letters = x_coords[row_letters[x_coords] %in% c('F', 'J')]
  loop_elements_passed = cumsum(1:width %in% relevant_row_letters) + 
    (cumsum(1:width %in% down_row_letters) / 2) - (cumsum(1:width %in% up_row_letters) / 2)
  potential_interior_indexes = which(loop_elements_passed %% 2 == 1)
  interior_indexes = setdiff(potential_interior_indexes, x_coords)
}

interior_indexes = lapply(1:height, get_interior_indexes_for_row)

sum(unlist(lapply(interior_indexes, length)))
