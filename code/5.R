input_lines = readLines('./inputs/5.txt')
input_length = length(input_lines)

splitting_lines = which(input_lines == '')

seeds_line = input_lines[1]

seeds = as.numeric(strsplit(seeds_line, ' ')[[1]][-1])

maps = list()
for (index in 1:length(splitting_lines)){
  if(index == length(splitting_lines)){
    end_of_range = input_length
  } else {
    end_of_range = splitting_lines[index + 1] - 1
  }
  start_of_range = splitting_lines[index] + 1
  relevant_lines= input_lines[start_of_range:end_of_range]
  map_lines = relevant_lines[2:length(relevant_lines)]
  map_source = split_vectorised(relevant_lines[1], '[- ]')[1]
  map_sink = split_vectorised(relevant_lines[1], '[- ]')[3]
  map_df = data.frame(
    destination_start = as.numeric(split_vectorised(map_lines, ' ', 1)),
    source_start = as.numeric(split_vectorised(map_lines, ' ', 2)),
    range_length = as.numeric(split_vectorised(map_lines, ' ', 3))
  )
  maps[[index]] = list(map_source = map_source, map_sink = map_sink, map_df = map_df)
}

apply_map_to_numbers = function(numbers, map){
  numbers_df = data.frame(number = numbers)
  map_df = map$map_df
  new_numbers_df = 
    sqldf('select 
    case when range_length is not null then number - source_start + destination_start
    else number end as new_number
    from numbers_df left outer join map_df
    on numbers_df.number between map_df.source_start and map_df.source_start + map_df.range_length - 1
          ')
  new_numbers = new_numbers_df$new_number
  return(new_numbers)
}

current_numbers = seeds
for(map_index in 1:7){
  current_numbers = apply_map_to_numbers(current_numbers, maps[[map_index]])
}
min(current_numbers) #165788812

range_starts = seeds[(1:10) * 2 - 1]
range_lengths = seeds[(1:10)*2]

range_df = data.frame(range_starts = range_starts, range_ends = range_starts + range_lengths - 1)

apply_range_map_to_ranges = function(range_df, map){
  map_df = map$map_df
  
  full_map_df = 
    sqldf("
  with source_starts as (
    select source_start, source_start + range_length - 1 as source_end, 
    destination_start - source_start as translation,
    'actual_start' as type from map_df
  union all 
    select source_start + range_length as source_start,
    null as source_end, 0 as translation, 'gap_or_after' as type from map_df
  union all
    select null as source_start, min(source_start) - 1 as source_end, 0 as translation, 'before' as type from map_df
  ),
  fill_in_source_ends as (
    select 
    source_start, 
    coalesce(source_end, 
      lead(source_start) over(order by source_start, case when source_end is null then 0 else 1 end) - 1)
    as source_end, translation, type
    from source_starts order by source_start, case when source_end is null then 0 else 1 end
  )
  select * from fill_in_source_ends where source_start is null or source_end is null or source_start <= source_end
        ")

  mapped_range_df = 
    sqldf("
  with intersection as (
    select *, 
    max(range_starts, coalesce(source_start, range_starts)) as intersection_start,
    min(range_ends, coalesce(source_end, range_ends)) as intersection_end
    from range_df inner join full_map_df on
    (range_starts <= source_end and range_starts >= source_start) or
    (source_start >= range_starts and source_start <= range_ends) or
    (source_start is null and range_starts <= source_end) or
    (source_end is null and range_ends >= source_start)
  )
  select intersection_start + translation as range_starts,
  intersection_end + translation as range_ends
  from intersection
        ")

  return(mapped_range_df)
  }

working_range_df = range_df
for(map_index in 1:7){
  working_range_df = apply_range_map_to_ranges(working_range_df, maps[[map_index]])
  print(nrow(working_range_df))
}
min(working_range_df$range_starts) #1928058
