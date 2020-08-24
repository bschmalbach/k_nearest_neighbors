library(class)

df <- iris

#scale
df <- dplyr::mutate(df, Sepal.Length=scale(Sepal.Length), Sepal.Width=scale(Sepal.Width), Petal.Length=scale(Petal.Length), Petal.Width=scale(Petal.Width))

#randomly split data 70/30
split <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7, 0.3))
df_train <- df[split,]
df_test <- df[!split,]

res <- knn(df_train[1:4], df_test[1:4], df_train$Species, k=1)
df_test$pred <- res
df_test$true <- F
df_test$true[df_test$pred == df_test$Species] <- T
table(df_test$true) # error rate of 3.6%


rates<-0

for (i in 1:50) {
  df_test$pred <- knn(df_train[1:4], df_test[1:4], df_train$Species, k=i)
  df_test$true <- F
  df_test$true[df_test$pred == df_test$Species] <- T
  rates[i] <- table(df_test$true)[1]/sum(table(df_test$true))
}

plot(rates, type = "l") # still low error rate (less than 5%) with k=4
