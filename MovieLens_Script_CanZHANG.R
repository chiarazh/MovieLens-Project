
#### Create edx set, validation set (final hold-out test set)####

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# # https://grouplens.org/datasets/movielens/10m/
# # http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                  col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
    semi_join(edx, by = "movieId") %>%
    semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)
save(edx, validation, file = "movielens_dataset_start.RData")


####Load data####
getwd()
setwd("C:/Users/Me/Desktop/Chiara")
load("movielens_dataset_start.RData")

save.image(file = "movielens_dataset_start.RData")

####Load libraries####
library(tidyverse)
library(dplyr)
library(caret)
library(stringr)
library(ranger)
library(recosystem)
library(magrittr)
library(data.table)

####Data exploration####
str(edx)
summary(edx)
head(edx)

#check number of unique users and number of unique movies
library(dplyr)
edx %>% summarise(n_unique_user = n_distinct(userId),
                  n_uniqe_movie = n_distinct(movieId))

#check the rating variable
rating_frequency <- edx %>%
  group_by(rating) %>%
  summarise(count_rating = n()) %>%
  arrange(desc(count_rating)) %T>% 
    knitr::kable()
rating_frequency

#####Movie and ratings####

#check number of ratings per movie
ratings_per_movie <- edx %>%
  group_by(movieId, title) %>%
  summarise(count_ratings_per_movie = n()) %>% 
  arrange(desc(count_ratings_per_movie)) %T>% 
    knitr::kable()

ratings_per_movie %>% ggplot(aes(count_ratings_per_movie)) +
  geom_histogram(color = "black", fill = "sky blue", bins = 30) +
  scale_x_log10()+
  ggtitle(" Number of ratings per movie")

#check rating of each movie
final_rating_of_movies <- edx %>%
    group_by(movieId, title) %>%
    summarise(final_rating = mean(rating), count_rating = n()) %>% 
    arrange(desc(final_rating)) %T>% 
    knitr::kable()

final_rating_of_movies[1:10,] 
final_rating_of_movies[10667:10677,]

#plot rating of each movie against number of ratings they receive
final_rating_of_movies %>% 
    ggplot(aes(final_rating, count_rating, group = final_rating)) +
    geom_boxplot(color = "black", fill = "sky blue") +
    scale_y_log10()

#####User and ratings####

#check average rating that a user gives
average_rating_of_user <- edx %>% 
  group_by(userId) %>% 
  summarise(avg_rating_of_user = mean(rating)) %>% 
  ggplot(aes(avg_rating_of_user)) + 
  geom_histogram(color = "black", fill = "sky blue", bins = 30) +
  ggtitle("average rating of each user")

average_rating_of_user

#check number of ratings per user
ratings_per_user <- edx %>% 
  group_by(userId) %>% 
  summarise(count_ratings_per_user = n()) %>% 
  arrange(desc(count_ratings_per_user)) %>%
  ggplot(aes(count_ratings_per_user)) +
  geom_histogram(color = "black", fill = "sky blue", bins = 30) +
  scale_x_log10()+
  ggtitle(" Number of ratings per user")

ratings_per_user

#Compute the number of ratings per movie and plot it again the release year
#not very useful
edx %>% group_by(movieId) %>% 
    summarise(n_per_movie = n(), year = as.character(year)) %>% 
    ggplot(aes(year, n_per_movie)) +
    geom_boxplot(color = "black", fill = "sky blue") +
    ggtitle(" Number of ratings per movie vs year")

#####Genre and ratings####

#split the "genres" variable and count number of ratings per genre
edx_split_genre <- edx %>%
    separate_rows(genres, sep = "\\|")

ratings_per_genre <- edx_split_genre %>% 
  group_by(genres)%>% 
  summarise(count_ratings_per_genre = n()) %>% 
  arrange(desc(count_ratings_per_genre)) %T>% 
    knitr::kable()

ratings_per_genre

#calculate the average rating each genre received
avg_rating_per_genre <- edx_split_genre %>% 
    group_by(genres) %>% 
    summarise(avg_rating = mean(rating)) %>% 
    left_join(ratings_per_genre, by = "genres") %>% 
    arrange(desc(avg_rating)) %T>% 
    knitr::kable()

avg_rating_per_genre %>% 
    ggplot(aes( count_ratings_per_genre, avg_rating, label = genres)) +
    geom_point() +
    geom_text(position = "nudge", vjust = -0.5) +
    geom_smooth()

#####Year and ratings####

#create a new variable "year" by getting the release year from the "title" variable
library(stringr)
edx_split_year <- edx %>% 
    mutate(year = as.numeric(str_sub(edx$title,-5, -2)))

#calculate the number of ratings and average rating for each year
edx_year_vs_rating <- edx_split_year %>% 
    group_by(year) %>% 
    mutate(count_rating_year = n(),
           avg_rating_year = mean(rating)) %>% 
    select(year, count_rating_year, avg_rating_year) %>% 
    unique()

#plot average rating of each year against year, 
#and set size to represent the number of ratings of each year
plot_year_rating <- edx_year_vs_rating %>% 
    ggplot(aes(year, avg_rating_year, size = count_rating_year)) +
    geom_point()
plot_year_rating

####Generate training and test sets####
library(caret)
set.seed(2, sample.kind = "Rounding")
test_index <- createDataPartition(edx$rating, times = 1, p = 0.2, list = FALSE)
test_set_temp <- edx[test_index, ]
train_set <- edx[-test_index, ]

# make sure userId and movieId in test set are also in train set
test_set <- test_set_temp %>% 
  semi_join(train_set, by = "movieId") %>% 
  semi_join(train_set, by = "userId")

# add rows removed from test set back into train set
removed_rows <- anti_join(test_set_temp, test_set)
train_set <- rbind(train_set, removed_rows)

save(edx, validation, train_set, test_set, file = "movielens_dataset_start.RData")

####Create the loss function####
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

save(edx, validation, train_set, test_set, RMSE, file = "movielens_dataset_start.RData")

####Building Models####

#####The 1st model:Baseline####
#predict the average rating of all movies as expected rating
mu_hat <- mean(train_set$rating)
y_hat_1 <- mu_hat
rmse_1 <- RMSE(test_set$rating, y_hat_1)
rmse_1
rmse_results <- data.frame(Method = "Baseline(average of all ratings)", RMSE = rmse_1)

#####The 2nd model: Baseline + movie effect####
movie_bias <- train_set %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu_hat))

min(movie_bias$b_i)
max(movie_bias$b_i)

b_i_test_set <- test_set %>%
  left_join(movie_bias, by = "movieId") %>% 
  pull(b_i)

y_hat_2 <- mu_hat + b_i_test_set

rmse_2 <- RMSE(test_set$rating, y_hat_2)
rmse_result_2 <- data.frame(Method = "Baseline + movie effect", RMSE = rmse_2)
rmse_results <- rbind(rmse_results, rmse_result_2)
rmse_results

#####The 3rd model: Baseline + movie effect + user effect####
user_bias <- train_set %>%
  left_join(movie_bias, by = "movieId") %>% 
  group_by(userId) %>% 
  summarise(b_u = mean(rating - mu_hat - b_i))

b_u_test_set <- test_set %>%
  left_join(user_bias, by = "userId") %>% 
  pull(b_u)

y_hat_3 <- mu_hat + b_i_test_set + b_u_test_set

rmse_3 <- RMSE(test_set$rating, y_hat_3)
rmse_result_3 <- data.frame(Method = "Baseline + movie effect + user effect", RMSE = rmse_3)
rmse_results <- rbind(rmse_results, rmse_result_3)
rmse_results

#####The 4th model: Baseline + regularized movie effect + regularized user effect####
#use cross-validation to choose lambda
lambdas <- seq(0,10,0.25)

rmses_lambdas <- sapply(lambdas, function(x){
    movie_bias_regularized <- train_set %>% 
        group_by(movieId) %>% 
        summarise(b_i_reg = sum(rating - mu_hat)/(n()+x))
    user_bias_regularized <- train_set %>% 
        left_join(movie_bias_regularized, by = "movieId") %>% 
        group_by(userId) %>% 
        summarise(b_u_reg = sum(rating - mu_hat - b_i_reg)/(n()+x))
    y_hat_reg <- test_set %>% 
        left_join(movie_bias_regularized, by = "movieId") %>%
        left_join(user_bias_regularized, by = "userId") %>%
        mutate(y_hat = mu_hat + b_i_reg + b_u_reg) %>% 
        pull(y_hat)
    rmse_reg <- RMSE(test_set$rating, y_hat_reg)
    rmse_reg
})

qplot(lambdas,rmses_lambdas)

best_lambda <- lambdas[which.min(rmses_lambdas)]
best_lambda

rmse_4 <- rmses_lambdas[which.min(rmses_lambdas)]
rmse_result_4 <- data.frame(Method = "Baseline + regularized movie effect + regularized user effect", RMSE = rmse_4)
rmse_results <- rbind(rmse_results, rmse_result_4)
rmse_results

#####The 5th model: Baseline + movie effect + user effect + random forest(ranger)####
##Calculate the residual of train set by removing baseline, movie effect and user effect
residual_train <- train_set %>% 
    left_join(movie_bias, by = "movieId") %>% 
    mutate(resi_1 = rating - b_i) %>% 
    left_join(user_bias, by = "userId") %>% 
    mutate(residual = resi_1 - b_u - mu_hat) %>%
    select(userId, movieId, rating, title, genres, residual)

##Split year and genre
residual_train_split <- residual_train %>% 
  mutate(year = as.numeric(str_sub(title,-5, -2))) %>% 
  separate_rows(genres, sep = "\\|") %>%  
  mutate(genre_count = 1L) %>% 
  pivot_wider(names_from = genres, values_from = genre_count, values_fill = 0L)
head(residual_train_split)

##make sure the order of rows of the data set is the same after splitting
sum(residual_train_split$movieId != train_set$movieId)
sum(residual_train_split$userId != train_set$userId)

##write a function to format the data set for ranger by:
###1.removing userId, movieId, rating, title, 
###2.replacing minus signs in column names to underscore
###3.replacing "(no genres listed)" with "NoGenre"
format_ranger <- function(x){
  x <- x[,!(colnames(x) %in% c("userId", "movieId", "rating", "title", "timestamp"))]
  colnames(x) <- gsub("\\-", "", colnames(x))
  colnames(x) <- gsub("^[(]no\\sgenres\\slisted[)]$", "NoGenre", colnames(x))
  x
}

##train a model with ranger
library(ranger)
fit_ranger <- ranger(residual ~ ., data = residual_train_split_ranger)

save(edx, validation, train_set, test_set, residual_train_split, format_ranger, fit_ranger, file = "movielens_dataset_start.RData")

##format the test set
test_split <- test_set %>% 
    mutate(year = as.numeric(str_sub(title, -5, -2))) %>% 
    separate_rows(genres, sep = "\\|") %>%  
    mutate(genre_count = c(1)) %>% 
    pivot_wider(names_from = genres, values_from = genre_count, values_fill = 0)

##check the order of rows are the same in the formatted test set and the original test set
sum(test_split$movieId != test_set$movieId)
sum(test_split$userId != test_set$userId)

##format the test set into ranger format
test_split_ranger <- test_split %>% format_ranger()

## with the fitted model, predict the residual of test set
pred_ranger <-predict(fit_ranger, data = test_split_ranger)
y_hat_residual <- pred_ranger$predictions

y_hat_ranger <- test_split %>% 
    left_join(movie_bias, by = "movieId") %>% 
    left_join(user_bias, by = "userId") %>% 
    mutate(y_hat = mu_hat + b_i + b_u + y_hat_residual) %>% 
    pull(y_hat)

##calculate the rmse 
rmse_ranger <- RMSE(test_set$rating, y_hat_ranger)
rmse_ranger

rmse_result_ranger <- data.frame(Method = "Baseline + regularized movie effect + regularized user effect + random forest (ranger)", RMSE = rmse_ranger)
rmse_results <- rbind(rmse_results, rmse_result_ranger)
rmse_results

  
#####The 6th model: matrix factorization(recosystem)####
library(recosystem)
##transform the train and test datasets to sparse matrix triplet form.
train_fac <- with(train_set, data_memory(user_index = userId,
                                         item_index = movieId,
                                         rating = rating))


test_fac <- with(test_set, data_memory(user_index = userId,
                                       item_index = movieId,
                                       rating = rating))
## create a model object by calling Reco()
set.seed(345, sample.kind = "Rounding")
r <- Reco()

##tuning
opts <- r$tune(train_fac, opt = list(dim = c(10L, 20L),
                                     lrate = c(0.01, 0.1),
                                     costp_l2 = c(0.01, 0.1),
                                     costq_l2 = c(0.01, 0.1),
                                     nthread = 4,
                                     niter = 50))

## train the model by calling the $train() method
r$train(train_fac, opts = c(opts$min, nthread = 4, niter = 50))

##use the $predict() method to compute predicted values
##and generate prediction to a R vector
y_hat_fac <- r$predict(test_fac, out_memory())

##calculate RMSE
rmse_fac <- RMSE(test_set$rating, y_hat_fac)
rmse_fac
rmse_result_fac <- data.frame(Method = "Matrix factorization (recosystem)", RMSE = rmse_fac)
rmse_results <- rbind(rmse_results, rmse_result_fac)
rmse_results


####Final validation####

##transform the edx and validation datasets to sparse matrix triplet form.
edx_fac <- with(edx, data_memory(user_index = userId,
                                 item_index = movieId,
                                 rating = rating))
validation_fac <- with(validation, data_memory(user_index = userId,
                                               item_index = movieId,
                                               rating = rating))

##create a model object by calling Reco()
set.seed(3456, sample.kind = "Rounding")    
r_edx <- Reco()

##tune the parameters
opts_edx <- r$tune(edx_fac, opt = list(dim = c(10L, 20L),
                                       lrate = c(0.01, 0.1),
                                       costp_l2 = c(0.01, 0.1),
                                       costq_l2 = c(0.01, 0.1),
                                       nthread = 6,
                                       niter = 40))

## train the model by calling the $train() method
r_edx$train(edx_fac, opts = c(opts_edx$min, nthread = 6, niter = 40))

##use the $predict() method to compute predicted values
##and generate prediction to a R vector
y_hat_edx_fac <- r_edx$predict(validation_fac, out_memory())
head(y_hat_edx_fac)

##calculate the final RMSE
rmse_final <- RMSE(validation$rating, y_hat_edx_fac)
rmse_final
