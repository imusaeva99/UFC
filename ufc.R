library("tidyverse")

fighters <- read_csv("all_fighters.csv")

fights <- read_csv("all_fights-2.csv")

winners <- fights %>% 
  separate(col = 'Fighter', into = c('First', 'Last'), sep = ' ') %>% 
  left_join(fighters, by = c('First', 'Last'))

#write_csv(winners, 'winners.csv')

winners <- winners %>% 
  separate('Ht.', into = c('H1', 'H2'), convert = TRUE) %>% 
  mutate('Height, cm' = as.integer(H1 * 30.48 + H2 * 2.54))

winners <- winners %>% 
  separate('Wt.', into = 'Weight, lbs', sep = ' ')

winners_selected_cols <- winners %>% 
  select('First', 'Last', 'Weight class', 'Method', 'Round', 'Time',
         'Weight, lbs', 'Height, cm', 'Stance', 'W', 'L', 'D')

write_csv(winners, 'winners_new.csv')
write_csv(winners_selected_cols, 'winners_selected_cols.csv')

winners_method <- winners %>% 
  mutate(`Method short` = case_when(str_sub(Method, start=1, end=2) == 'KO' ~ 'KO',
                                    str_sub(Method, start=1, end=3) == 'SUB' ~ 'SUB',
                                    str_sub(Method, start=1, end=2) == 'DQ' ~ 'DQ',
                                    str_sub(Method, start=1, end=3) == 'CNC' ~ 'CNC',
                                    str_sub(Method, start=3, end=5) == 'DEC' ~ 'DEC',
                                    str_sub(Method, start=1, end=2) == 'Ov' ~ 'Overturned',
                                    str_sub(Method, start=1, end=2) == 'Ot' ~ 'Other')) %>% 
  mutate(Gender = case_when(str_sub(`Weight class`, start=1, end=5) == 'Women' ~ 'F',
                            str_sub(`Weight class`, start=1, end=5) != 'Women' ~ 'M')) %>% 
  select(First, Last, Gender, `Method short`, Round, Time, `Weight, lbs`, `Height, cm`, W, L, D)

write_csv(winners_method, 'winners_method.csv')

winners_method %>% 
  group_by(Gender) %>% 
  count(`Method short`) %>% 
  write_csv('method_counts.csv')

events <- read_csv("events_data.csv")

events %>% 
  mutate(Country = sapply(str_split(Location, ", "), tail, n=1)) %>% 
  count(Country, sort=TRUE)

winners %>% 
  mutate(`Method short` = case_when(str_sub(Method, start=1, end=2) == 'KO' ~ 'KO',
                                    str_sub(Method, start=1, end=3) == 'SUB' ~ 'SUB',
                                    str_sub(Method, start=1, end=2) == 'DQ' ~ 'DQ',
                                    str_sub(Method, start=1, end=3) == 'CNC' ~ 'CNC',
                                    str_sub(Method, start=3, end=5) == 'DEC' ~ 'DEC',
                                    str_sub(Method, start=1, end=2) == 'Ov' ~ 'Overturned',
                                    str_sub(Method, start=1, end=2) == 'Ot' ~ 'Other')) %>% 
  mutate(`Method details` = case_when(str_sub(Method, start=3, end=5) == 'DEC' ~ str_replace(Method, '-DEC', ''),
                                      str_sub(Method, start=1, end=2) == 'Ov' ~ 'Overturned',
                                      str_sub(Method, start=1, end=2) == 'Ot' ~ 'Other',
                                      str_sub(Method, start=1, end=2) == 'DQ' ~ 'DQ',
                                      str_sub(Method, start=1, end=3) == 'CNC' ~ 'CNC',
                                      str_sub(Method, start=1, end=2) == 'KO' ~ str_replace(Method, 'KO/TKO ', ''),
                                      str_sub(Method, start=1, end=3) == 'SUB' ~ str_replace(Method, 'SUB ', ''))) %>% 
  mutate(Gender = case_when(str_sub(`Weight class`, start=1, end=5) == 'Women' ~ 'F',
                            str_sub(`Weight class`, start=1, end=5) != 'Women' ~ 'M')) %>% 
  select(First, Last, Gender, `Method short`, `Method details`, Round, Time, `Weight, lbs`, `Height, cm`, W, L, D) %>% 
  filter(`Method short` != 'Other') %>% 
  filter(`Method short` != 'Overturned') %>% 
  filter(`Method short` != 'CNC') %>% 
  filter(`Method short` != 'DQ') %>% 
  group_by(Gender) %>% 
  count(`Method short`, `Method details`) %>% 
  write_csv('method_details_2.csv')





