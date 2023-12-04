input_lines = readLines('./inputs/4.txt')

card_data = data.frame(
  cardid = split_vectorised(input_lines, ':', 1),
  winning_numbers_str = trimws(split_vectorised(split_vectorised(input_lines, ':', 2), '\\|', 1)),
  numbers_you_have_str = trimws(split_vectorised(split_vectorised(input_lines, ':', 2), '\\|', 2))
)

str_to_numbers = function(input_string){
  as.numeric(strsplit(input_string, ' +')[[1]])
}

get_value = function(winning_numbers, numbers_you_have){
  winning_numbers_you_have = numbers_you_have[numbers_you_have %in% winning_numbers]
  if (length(winning_numbers_you_have) == 0){
    return(0)
  } else(
    return(2 ^ (length(winning_numbers_you_have) - 1))
  )
}

card_data$values = sapply(1:nrow(card_data),
                          function(index){
                            get_value(str_to_numbers(card_data$winning_numbers_str[index]),
                                      str_to_numbers(card_data$numbers_you_have_str[index]))
                          })
sum(card_data$values) #27059

get_number_of_matches = function(winning_numbers, numbers_you_have){
  winning_numbers_you_have = numbers_you_have[numbers_you_have %in% winning_numbers]
  return(length(winning_numbers_you_have))
}

card_data$matches = sapply(1:nrow(card_data),
                           function(index){
                             get_number_of_matches(str_to_numbers(card_data$winning_numbers_str[index]),
                                       str_to_numbers(card_data$numbers_you_have_str[index]))
                           })

instances = rep(0, 400)
instances[1:length(input_lines)] = 1

for(card_index in 1:length(input_lines)){
  matches = card_data$matches[card_index]
  number_to_add = instances[card_index]
  if(matches > 0){
    instances[card_index + (1:matches)] = instances[card_index + (1:matches)] + number_to_add
  }
}

sum(instances) #5744979

