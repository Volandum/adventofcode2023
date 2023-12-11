input_lines = readLines('./inputs/11.txt')
height = length(input_lines)
width = nchar(input_lines[1])

split_input_lines = strsplit(input_lines, '')

star_df = data.frame()

for(row in 1:height){
  stars = which(split_input_lines[[row]] == '#')
  if(length(stars) > 0){
    new_star_df = data.frame(row = row, column = stars)
    star_df = rbind(star_df, new_star_df)
  }
}

columns_without_stars = which(!(1:width %in% star_df$column))
rows_without_stars = which(!(1:height %in% star_df$row))

expansion_column_map = data.frame(original_column = 1:width,
                           new_column = 1:width + cumsum(1:width %in% columns_without_stars))
expansion_row_map = data.frame(original_row = 1:height,
                                  new_row = 1:height + cumsum(1:height %in% rows_without_stars))

expanded_df = sqldf(
  "select new_row, new_column from star_df
  inner join expansion_column_map on row = original_row
  inner join expansion_row_map on column = original_column"
)

pairs_of_stars = expanded_df %>% cross_join(expanded_df)

pairs_of_stars %>% mutate(distance = abs(new_row.x - new_row.y) + 
                            abs(new_column.x - new_column.y)) %>%
  pull(distance) %>% sum

# 19278320 is too high - I double counted

19278320/2 #9639160

expansion_column_map_pt2 = data.frame(original_column = 1:width,
                                  new_column = 1:width + cumsum(1:width %in% columns_without_stars) * 999999)
expansion_row_map_pt2 = data.frame(original_row = 1:height,
                               new_row = 1:height + cumsum(1:height %in% rows_without_stars) * 999999)

expanded_df_pt2 = sqldf(
  "select new_row, new_column from star_df
  inner join expansion_column_map_pt2 on row = original_row
  inner join expansion_row_map_pt2 on column = original_column"
)

pairs_of_stars_pt2 = expanded_df_pt2 %>% cross_join(expanded_df_pt2)

double_distance = pairs_of_stars_pt2 %>% mutate(distance = abs(new_row.x - new_row.y) + 
                            abs(new_column.x - new_column.y)) %>%
  pull(distance) %>% as.bigz %>% sum

double_distance/2
# 752936133304
