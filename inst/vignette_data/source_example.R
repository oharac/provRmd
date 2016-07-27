iris_temp <- read_csv('iris_table2.csv')

iris_temp <- iris_temp %>%
  filter(Aspect.Ratio > 5)

write_csv(iris_temp, 'iris_table3.csv')
