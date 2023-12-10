input_lines = readLines('./inputs/7.txt')

input_df = data.frame(
  hand = split_vectorised(input_lines, ' ', 1),
  bid = as.numeric(split_vectorised(input_lines, ' ', 2))
)

input_df$first_card = substr(input_df$hand, 1, 1)
input_df$first_card_value = match(input_df$first_card,
                                  c('A', 'K', 'Q', 'J', 'T',
                                    '9', '8', '7', '6', '5', '4', '3', '2'))

# types: 1: 5 of a kind, 2: 4 of a kind, 3: full house (3 + 2), 4: three of a kind
# 5: two pair, 6: one pair, 7: high card

classify_hand = function(hand){
  cards = strsplit(hand, '')[[1]]
  card_table = table(cards)
  card_table_breakdown = sort(unname(card_table), decreasing = TRUE)
  if(card_table_breakdown[1] == 5){
    return(1)
  }
  if(card_table_breakdown[1] == 4){
    return(2)
  }
  if(card_table_breakdown[1] == 3){
    if(card_table_breakdown[2] == 2){
      return(3)
    } else {
      return(4)
    }
  }
  if(card_table_breakdown[1] == 2){
    if(card_table_breakdown[2] == 2){
      return(5)
    } else {
      return(6)
    }
  }
  return(7)
}

input_df$hand_type = Vectorize(classify_hand)(input_df$hand)

hand_to_sort_order = function(hand){
  cards = strsplit(hand, '')[[1]]
  mapped_cards = letters[match(cards,
                       c('A', 'K', 'Q', 'J', 'T',
                         '9', '8', '7', '6', '5', '4', '3', '2'))]
  return(paste0(mapped_cards, collapse = ''))
}

input_df$hand_sort_order = Vectorize(hand_to_sort_order)(input_df$hand)

sorted_df = input_df %>% arrange(hand_type, hand_sort_order) %>% mutate(rank = n() + 1 - row_number())

sum(sorted_df$rank * sorted_df$bid) #250602641


classify_hand_joker = function(hand){
  cards = strsplit(hand, '')[[1]]
  jokers = sum(cards == 'J')
  card_table = table(cards[cards != 'J'])
  card_table_breakdown = sort(unname(card_table), decreasing = TRUE)
  top_card_count = card_table_breakdown[1] + jokers
  second_card_count = card_table_breakdown[2]
  if(is.na(top_card_count)){
    top_card_count = jokers
    second_card_count = 0
  }
  if(top_card_count == 5){
    return(1)
  }
  if(top_card_count == 4){
    return(2)
  }
  if(top_card_count == 3){
    if(second_card_count == 2){
      return(3)
    } else {
      return(4)
    }
  }
  if(top_card_count == 2){
    if(second_card_count == 2){
      return(5)
    } else {
      return(6)
    }
  }
  return(7)
}

hand_to_sort_order_joker = function(hand){
  cards = strsplit(hand, '')[[1]]
  mapped_cards = letters[match(cards,
                               c('A', 'K', 'Q', 'T',
                                 '9', '8', '7', '6', '5', '4', '3', '2',
                                 'J'))]
  return(paste0(mapped_cards, collapse = ''))
}

input_df$hand_type_joker = Vectorize(classify_hand_joker)(input_df$hand)
input_df$hand_sort_order_joker = Vectorize(hand_to_sort_order_joker)(input_df$hand)

sorted_df_joker = input_df %>% arrange(hand_type_joker, hand_sort_order_joker) %>% 
  mutate(rank = n() + 1 - row_number())

sum(sorted_df_joker$rank * sorted_df_joker$bid) #251037509
