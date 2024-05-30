library(ggplot2)
library(dplyr)
library(lubridate)
library(forcats)

data <- read.csv('sales_data.csv')
data
colnames(data)

grouped <- data %>%
  group_by(is_gift) %>%
  summarise(rev = sum(revenue))
grouped
ggplot(grouped, aes(x = as.factor(is_gift), y = rev)) + geom_col()

grouped <- data %>%
  group_by(as.Date(order_date)) %>%
  summarise(rev = sum(revenue))

colnames(grouped)[1] <- 'order_date'

ggplot(grouped, aes(x = order_date, y = rev)) + geom_line()

total_revenue <- sum(data$revenue)
plot_data <- data.frame(total_revenue)

euro <- scales::label_currency(
  largest_with_fractional = 1000000,
  accuracy = 2,
  prefix = '',
  suffix = '\U20AC',
  big.mark = '.',
  decimal.mark = ','
)

grouped <- data %>%
  group_by(category) %>%
  summarise(rev = sum(revenue))

grouped <- grouped[!is.na(grouped[, 1]), ]

ggplot(grouped, aes(x = rev, y = fct_reorder(factor(category), rev, na.rm = F))) + geom_col()

unique(grouped$category)

world <- map_data('world')
head(world)

greece <- filter(world, region == 'Greece')
ggplot(greece, aes(x = long, y = lat, group = group, fill = subregion)) + geom_polygon() + coord_quickmap()
ggplot(world, aes(x = long, y = lat, group = group)) + 
  geom_polygon() + 
  coord_quickmap() +
  theme_void()

by.country <- group_by(data, '')
merge(world, data, by.x = 'region', by.y = 'country')
