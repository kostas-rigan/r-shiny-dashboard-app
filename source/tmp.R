library(ggplot2)
library(dplyr)
library(lubridate)
library(forcats)

data <- as_tibble(read.csv('sales_data.csv'))
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

by.country <- group_by(data, country) %>%
  summarise(revenue = sum(revenue),
            cost = sum(commissions),
            profit = sum(profit),
            quantity = sum(quantity),
            orders = n())
by.country
world.wo.antarctica <- world %>% 
  filter(region != 'Antarctica')
country.agg <- merge(world.wo.antarctica, by.country, by.x = 'region', by.y = 'country', all.x=T)
country.agg

ggplot(country.agg, aes(x = long, y = lat, group = group)) + 
  geom_polygon() + 
  coord_quickmap() +
  theme_void()

arrange(country.agg, region, group, order) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = revenue)) + 
  geom_polygon() + 
  coord_quickmap() +
  theme_void() +
  theme(legend.position = 'none')


date_data <- data[, c('order_date', 'revenue', 'commissions', 'profit')]
date_data$day_of_week <-wday(date_data$order_date, label = T, abbr = F)
date_data$month <- month(as.Date(date_data$order_date), label = T, abbr = F)
date_data$hour_of_day <- ymd_hms(date_data$order_date) %>%
  hour()

f1 <- match.fun('sum')
f2 <- match.fun('mean')
date_data
grouped_date <- date_data %>%
  group_by(hour_of_day) %>%
  summarise(revenue = f2(revenue), n = n())
ggplot(grouped_date, aes(x = hour_of_day, y = n)) + geom_col()

data
col = 'price'
data[, col]

match.fun('wday')

group_and_summarize <- function(dataframe, by_variable, input_agg_func, metric) {
  agg_func <- match.fun(input_agg_func)

  if (input_agg_func == 'n') {
    grouped <- group_by(dataframe, !!by_variable) %>%
      summarise(agg_metric = agg_func())
  } else {
    grouped <- group_by(dataframe, !!by_variable) %>%
      summarise(agg_metric = agg_func(!!metric))
  }
  
  return (grouped)
}

data
group_and_summarize(data, quo(country), 'sum', quo(profit))
