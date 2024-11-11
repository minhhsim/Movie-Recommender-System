library(tidyverse)
library(caret)
library(data.table)

setwd("C:\\Users\\User\\Desktop\\ml-20m")

ratings <- fread(text = gsub("::", "\t", readLines("ratings.csv")),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines("movies.csv"), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") 
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


# Splitting edx data set in train set and test set ----
edx_test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.01, list = FALSE)

train_set <- edx[-edx_test_index, ]
test_set <- edx[edx_test_index, ]


# Creating base model which predicts average of all ratings for each movie ----
avg_rating <- mean(train_set$rating)

base_model_predictions <- rep(avg_rating, nrow(test_set))

base_model_rmse <- RMSE(test_set$rating, base_model_predictions, na.rm=TRUE)

rmse_results <- tibble(Method = "Base Model", RMSE = base_model_rmse)

print(paste("Base RMSE:", rmse_results))


# Regularization (Lambda: 3) ----

lambda <- 3 # trying tuning parameter value = 3

# Regularization on movie specific effects
regularized_movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(regularized_movie_avg = sum(rating - avg_rating)/(n() + lambda))

# Regularization on user specific effects
regularized_user_avgs <- train_set %>% 
  left_join(regularized_movie_avgs, by="movieId") %>%
  group_by(userId) %>%
  summarize(regularized_user_avg = sum(rating - regularized_movie_avg - avg_rating)/(n() + lambda))

reg_predictions <- test_set %>% 
  left_join(regularized_movie_avgs, by = "movieId") %>%
  left_join(regularized_user_avgs, by = "userId") %>%
  mutate(prediction = avg_rating + regularized_movie_avg + regularized_user_avg) %>% .$prediction

reg_rmse <- RMSE(reg_predictions, test_set$rating, na.rm=TRUE)

reg_rmse_results <- bind_rows(reg_rmse_results,
                              tibble(Method="Regularized Model (lambda = 3)",  
                                     RMSE = reg_rmse ))

# Find the row corresponding to the last model (Regularized Model)
last_model_row <- reg_rmse_results %>% filter(Method == "Regularized Model (lambda = 3)")

# Print the RMSE of the last model
cat("Regularization RMSE:", last_model_row$RMSE)
#print(paste("Regularization RMSE:", reg_rmse_results))


# Regularization (Hyperparameter) ----

lambdas <- seq(0, 10, 0.2)

RMSEs <- sapply(lambdas, function(l){
  
  regularized_movie_avgs <- train_set %>% 
    group_by(movieId) %>% 
    summarize(regularized_movie_avg = sum(rating - avg_rating)/(n() + l))
  
  regularized_user_avgs <- train_set %>% 
    left_join(regularized_movie_avgs, by="movieId") %>%
    group_by(userId) %>%
    summarize(regularized_user_avg = sum(rating - regularized_movie_avg - avg_rating)/(n() + l))
  
  optReg_predictions <- test_set %>% 
    left_join(regularized_movie_avgs, by = "movieId") %>%
    left_join(regularized_user_avgs, by = "userId") %>%
    mutate(prediction = avg_rating + regularized_movie_avg + regularized_user_avg) %>% .$prediction
  
  return(RMSE(optReg_predictions, test_set$rating, na.rm = TRUE))
})

qplot(lambdas, RMSEs, xlab = "Lambda", ylab = "RMSE")

min_lambda <- lambdas[which.min(RMSEs)]

regOpt_rmse_results <- bind_rows(rmse_results,
                          tibble(Method="Regularized Model (using optimum lambda)",  
                                     RMSE = min(RMSEs)))
regOpt_rmse_results %>% knitr::kable()

# Find the row corresponding to the last model (Regularized Model)
last_model_row_reg <- regOpt_rmse_results %>% filter(Method == "Regularized Model (using optimum lambda)")

# Print the RMSE of the last model
cat("Regularization RMSE (using optimum lambda):", last_model_row_reg$RMSE)
#print(paste("Regularization (with Optimum Lambda) RMSE:", regOpt_rmse_results))

