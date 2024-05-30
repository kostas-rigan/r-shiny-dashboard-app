library(ggplot2)
library(dplyr)
library(forcats)
sales <- read.csv('sales_data.csv')

grouped_by <- sales %>% group_by(is_gift) %>% summarise(n = n())
sales %>% 
  ggplot(aes(y = fct_reorder(as.factor(is_gift), revenue, .fun = sum,), x = revenue)) +
  geom_col()
