input_lines = readLines('./inputs/24test.txt')

input_df = data.frame(
  id = 1:length(input_lines),
  px = as.numeric(split_vectorised(input_lines, '[ ,@]+', 1)),
  py = as.numeric(split_vectorised(input_lines, '[ ,@]+', 2)),
  pz = as.numeric(split_vectorised(input_lines, '[ ,@]+', 3)),
  vx = as.numeric(split_vectorised(input_lines, '[ ,@]+', 4)),
  vy = as.numeric(split_vectorised(input_lines, '[ ,@]+', 5)),
  vz = as.numeric(split_vectorised(input_lines, '[ ,@]+', 6))
)

intersect_ray_2d = function(px1, py1, vx1, vy1,
                            px2, py2, vx2, vy2){
  # px1 + vx1t1 - px2 - vx2t2 = 0
  # py2 + vy2t1 - py2 - vy2t2 = 0
  # (vx1 vx2) * (-t1) = (px1 - px2)
  # (vy1 vy2) * ( t2) = (py1 - py2)
  #
  if(vx1 / vy1 == vx2 / vy2){
    return (NULL) # parallel
  }
  velocity_matrix = matrix(c(vx1, vx2, vy1, vy2), nrow = 2, byrow = TRUE)
  relative_position_matrix = matrix(c(px1 - px2, py1 - py2), nrow = 2, byrow = TRUE)
  solution = solve(velocity_matrix, relative_position_matrix)
  t1 = -solution[1]
  t2 = solution[2]
  new_p1 = c(px1, py1) + c(vx1, vy1) * t1
  new_p2 = c(px2, py2) + c(vx2, vy2) * t2
  return(list(t1 = t1, t2 = t2, new_p1 = new_p1, new_p2 = new_p2))
}

test_pairs = input_df %>% cross_join(input_df, suffix = c('1', '2')) %>%
  filter(id1 < id2) %>% mutate(t1 = NA_real_, t2 = NA_real_, new_x = NA_real_, new_y = NA_real_)

for(row_index in 1:nrow(test_pairs)){
  intersection = with(test_pairs[row_index,], intersect_ray_2d(px1, py1, vx1, vy1,
                                                              px2, py2, vx2, vy2))
  if(!is.null(intersection)){
    test_pairs$t1[row_index] = intersection$t1
    test_pairs$t2[row_index] = intersection$t2
    test_pairs$new_x[row_index] = intersection$new_p1[1]
    test_pairs$new_y[row_index] = intersection$new_p1[2]
  }
}

min_coord = 200000000000000
max_coord = 400000000000000

test_pairs %>% filter(t1 >= 0, t2 >= 0, 
                      between(new_x, min_coord, max_coord),
                      between(new_y, min_coord, max_coord)) %>% nrow() #14672


# OK, we have some stuff to solve
# 6 unknowns + 1/hailstone considered (position + velocity of rock, time of hailstone)
# 3 equations/hailstone considered (x, y, z)
# considering 3 hailstones gets us 9 unknowns and 9 equations

# equations look like px' * 1 + vx' * t1 - px1 - vx1 * t1 = 0

# if I just do 2 dimensions
# 4 unknowns + 1/hailstone considered
# 2 equations/hailstone considered
# 4 hailstones for 8 unknowns and 8 equations

# px' + vx' * t1 - vx1 * t1 = px1
# py' + vy' * t1 - vy1 * t1 = py1
# px' + vx' * t2 - vx2 * t2 = px2

# so vx' * (t2 - t1) + vx1 * t1 - vx2 * t2 = px2 - px1
#and vy' * (t2 - t1) + vy1 * t1 - vy2 * t2 = px2 - px1