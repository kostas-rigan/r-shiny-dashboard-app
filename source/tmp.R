library(ggplot2)
library(dplyr)
library(lubridate)

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
  prefix = '',
  suffix = 'U+20AC',
  big.mark = '.',
  decimal.mark = ','
)

ggplot(plot_data, aes(x = 1, y = 1)) +
  geom_text(aes(label = labels(total_revenue, )), 
            size = 30, 
            vjust = 0.5, 
            hjust = 0.5) +
  theme_void()
