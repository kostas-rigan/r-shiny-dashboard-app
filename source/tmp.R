library(ggplot2)
library(dplyr)
library(lubridate)
library(forcats)
library(fastDummies)
library(factoextra)

data <- as_tibble(read.csv('sales_data.csv'))
data
colnames(data)

data
dummy <- dummy_columns(data, select_columns = 'category', remove_selected_columns = T)
dummy <- select(dummy, order_id, starts_with('category'))
orders <- dummy %>% group_by(order_id) %>% summarize_all(sum)

dummy <- dummy_columns(data, select_columns = 'category', remove_selected_columns = T)
dummy <- select(dummy, user_id, starts_with('category'))
users <- dummy %>% group_by(user_id) %>% summarize_all(sum)



users <- replace(users, is.na(users), 0)
users[, -1] <- scale(users[, -1])
users[, -1]
results<-kmeans(users[, -1], 3)
results$cluster

lower_k <- 1
higher_k <- 10

sample <- slice_sample(users, prop = 0.1)

elb <- data.frame(matrix(nrow = 0, ncol = 2))


for (k in lower_k : higher_k) {
  results <- kmeans(sample[, -1], k)
  elb <- rbind(elb, c(k, results$tot.withinss))
}
colnames(elb) <- c('k', 'twss')
elb
ggplot(elb, aes(x = k, y = twss)) + geom_line() + geom_point()

require(cluster)
sil <- silhouette(results$cluster, dist(sample[, -1]))
fviz_silhouette(sil)
sil

lst = list()
for (i in 1:10) {
  lst <- append(lst, i)
}
lst
