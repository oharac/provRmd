temp <- read_csv('spp_table2.csv')

temp <- temp %>%
  filter(n_spp < 50)

write_csv(temp, 'spp_table3.csv')
