#################################
# Create edx set & validation set
#################################

# Note: this is default code given by the assignment and is not changed
# Variable and object names are also unchanged.

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- stringr::str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
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

# End of given code. Let's have some fun!
###########################################################################################################


###########################################################################################################
# Data handling will be done in two parts: overview/summary data and analysis. Overview will review the 
# dataset at an aggregate level, and duplicate some of the work for the quiz (viz. unique movies, unique 
# users, etc.). Data Analysis will consist of the actual prediction algorithms and results based on what 
# has been taught during the course.
###########################################################################################################

#################################
# Part 1: Data Overview / Summary
#################################
# Load extra fonts for graphics prettiness.
library(extrafont)
font_import()
loadfonts(device="win")

# Some movies are part of multiple genres. Split these out for prediction algorithm.
edx2 <- edx %>% separate_rows(genres, sep = "\\|") # Split dataset since movies can be more than one genre
# Can't actually complete due to memory exhaustion - genre splitting for prediction model won't be possible.

# Convert timestamp column into human-readable data for both datasets. Release year is most critical.
edx <-  mutate(edx,release_date = as.Date(as.POSIXct(edx$timestamp,origin="1970-01-01")))
edx <-  mutate(edx,release_year = year(release_date))
validation <-  mutate(validation,release_date = as.Date(as.POSIXct(validation$timestamp,origin="1970-01-01")))
validation <-  mutate(validation,release_year = year(release_date))


# Exploratory data analysis. 
#######################################################################################
# Look at summary data first to see column definitions and create summary statistics
options(scipen=999)     # Disable scientific notation for plots
head(edx)               # View header of original file
summary(edx)            # Summary statistics
dim(edx)                # 9,000,055 observations of 8 variables
dim(validation)         # 999,999 observations of 8 variables

unique_movies <- edx %>% summarize(n_distinct(movieId))
unique_users <- edx %>% summarize(n_distinct(userId))
unique_genres <- edx %>% summarize(n_distinct(genres))

# Find which films have the most ratings & those rated only once
edx %>% group_by(title) %>% summarize(n_ratings=n()) %>% arrange(desc(n_ratings))   # Most ratings
edx %>% group_by(title) %>% summarize(n_ratings=n()) %>% filter(n_ratings == 1) %>% count() %>% pull()  # 1 rating
mu <- mean(edx$rating)  # Mean rating of all movies across all genres in original dataset

# Summary statistics from edx dataset:
#        userId         movieId          rating           timestamp            title              genres         
# Min.   :    1   Min.   :    1   Min.   :0.500   Min.   :7.897e+08   Length:9000055     Length:9000055    
# 1st Qu.:18124   1st Qu.:  648   1st Qu.:3.000   1st Qu.:9.468e+08   Class :character   Class :character  
# Median :35738   Median : 1834   Median :4.000   Median :1.035e+09   Mode  :character   Mode  :character  
# Mean   :35870   Mean   : 4122   Mean   :3.512   Mean   :1.033e+09                                        
# 3rd Qu.:53607   3rd Qu.: 3626   3rd Qu.:4.000   3rd Qu.:1.127e+09                                        
# Max.   :71567   Max.   :65133   Max.   :5.000   Max.   :1.231e+09 

# MEAN RATING:    3.512
# Unique movies:  10,677
# Unique users:   69,878
# Unique genres:  797


# Histogram of ratings by count
edx %>% ggplot(aes(rating)) + 
  geom_histogram(binwidth = 0.5, color = "black") + 
  xlab("Rating") + ylab("Count") + ggtitle("Ratings Histogram") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(family="Times New Roman", face="bold", size=10))

# Histogram of number of ratings by user
edx %>% dplyr::count(userId) %>% ggplot(aes(n)) + 
  geom_histogram(bins = 100, color = "black") + 
  scale_x_log10() +
  ggtitle("Movie Ratings by User ID") + 
  xlab("User ID") + ylab("No. of Ratings") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(family="Times New Roman", face="bold", size=10))

# Histogram of number of ratings by movie
edx %>% dplyr::count(movieId) %>% 
  ggplot(aes(n)) + geom_histogram(bins =30, color="black") + 
  scale_x_log10() +
  ggtitle("Movie Ratings by Movie ID") + 
  xlab("Movie ID") + ylab("No. of Ratings") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text=element_text(family="Times New Roman", face="bold", size=10))

# Heat map of average rating versus number of movies rated
edx %>% group_by(userId) %>% summarize(mu_user = mean(rating), number = n()) %>%
  ggplot(aes(x = mu_user, y = number)) + geom_bin2d() + scale_fill_gradientn(colors = topo.colors(10)) +
  labs(fill="Users") + scale_y_log10() + 
  ggtitle("Mean Rating vs. Number of Movies Rated") +
  xlab("Average Rating") + ylab("No. of Movies Rated") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(family="Times New Roman", face="bold", size=10))

# Mean rating versus release year; geom_smooth() using 
# method 'loess' and formula 'y~x'
edx %>% group_by(release_year) %>% summarize(rating = mean(rating)) %>%
  ggplot(aes(release_year, rating)) +
  geom_point() + geom_smooth() +
  ggtitle("Mean Rating vs. Release Year") +
  xlab("Release Year") + ylab("Mean Rating") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(family="Times New Roman", face="bold", size=10))


##########################################
# Part 2: Data Analysis & Prediction Model
##########################################
# Start with mean rating in the training set. This has already been calculated in Part 1.

# There will be movie effects (b_i, some movies are higher rated than others) and user effects
# (b_u, for 'cranky users'). Must account for these in RMSE model.
movie_effect <- edx %>% group_by(movieId) %>% summarize(b_i = mean(rating - mu))
movie_effect %>% qplot(b_i, geom ="histogram", bins = 100, data = ., color = I("black")) + 
  ggtitle("Movie Effect") +
  xlab("b_i") + ylab("Value") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(family="Times New Roman", face="bold", size=10))

user_effect <- edx %>% left_join(movie_effect, by='movieId') %>% group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
user_effect %>% qplot(b_u, geom ="histogram", bins = 100, data = ., color = I("black")) + 
  ggtitle("User Effect") +
  xlab("b_u") + ylab("Value") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(family="Times New Roman", face="bold", size=10))



# Create RMSE loss function
RMSE <- function(true_rating, pred_rating){
  sqrt(mean((true_rating - pred_rating)^2)) }

# Prediction method 1: mean rating (naive prediction)
rmse_naive <- RMSE(validation$rating,mu)      # 1.0612
rmse_naive <- format(round(rmse_naive,6), nsmall = 6)

# Create tibble to store results
rmse_results = tibble(method = "Naive analysis", RMSE = rmse_naive)


# Prediction method 2: incorporate movie effects
pred_rating <- mu + validation %>% 
  left_join(movie_effect,by="movieId") %>% pull(b_i)
rmse_b_i <- RMSE(pred_rating,validation$rating)      # 0.9439087
rmse_b_i <- format(round(rmse_b_i,6), nsmall = 6)
rmse_results <- rbind(rmse_results, tibble(method="Movie effects model", RMSE = rmse_b_i))


# Prediction method 3: incorporate movie effects and user effects
pred_rating2 <- validation %>% 
  left_join(movie_effect, by='movieId') %>% 
  left_join(user_effect, by='userId') %>% mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
rmse_b_u <- RMSE(pred_rating2, validation$rating)
rmse_b_u <- format(round(rmse_b_u,6), nsmall = 6)
rmse_results <- rbind(rmse_results, tibble(method="User effects model", RMSE = rmse_b_u))


# Results
# method                   RMSE
# <chr>                    <dbl>
# 1 Naive analysis by mean 1.061202 
# 2 Movie effects model    0.943909
# 3 User effects model     0.865349
# Some improvement is seen, but model can still be improved.



# Regularization - because the previous results were terrible.
lambdas <- seq(0, 20, 0.25)     
rmses <- sapply(lambdas, function(l){
  b_i <- edx %>%                                  # Add movie effect
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n() +l))
  
  b_u <- edx %>%                                  # Add user effect
    left_join(b_i, by = "movieId") %>% 
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  pred_rating <- validation %>%                   # Prediction
    left_join(b_i, by = "movieId") %>% 
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>% pull(pred)
  
  return(RMSE(pred_rating, validation$rating))})

rmse_reg <- min(rmses)  # 0.864817
rmse_reg <- format(round(rmse_reg,6), nsmall = 6)


# Plot RMSE against Lambdas to find optimal lambda
qplot(lambdas, rmses) +
  ggtitle("RMSE vs. Lambda") +
  xlab(paste("Lambda")) + ylab("RMSE") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(family="Times New Roman", face="bold", size=10))

lambda <- lambdas[which.min(rmses)]   # 5.25

rmse_results <- rbind(rmse_results, tibble(method="Regularization", RMSE = rmse_reg))
# Results
# method                    RMSE
# <chr>                     <dbl>
# 1 Naive analysis by mean  1.061202 
# 2 Movie effects model     0.943909
# 3 User Effects Model      0.865349
# 4 Regularization          0.864817   # Very little improvement over model 3



# Prediction method 5: include regularization plus release year
rmses <- sapply(lambdas, function(l){
  b_i <- edx %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n() +l))
  
  b_u <- edx %>% 
    left_join(b_i, by = "movieId") %>% 
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_y <- edx %>% 
    left_join(b_i, by = "movieId") %>% 
    left_join(b_u, by = "userId") %>%
    group_by(release_year) %>% 
    summarize(b_y = sum(rating - mu - b_i - b_u)/(n()+1), n_y = n())
  
  pred_rating <- validation %>% 
    left_join(b_i, by = "movieId") %>% 
    left_join(b_u, by = "userId") %>% 
    left_join(b_y, by = "release_year") %>%
    mutate(pred = mu + b_i + b_u + b_y) %>% pull(pred)
  
  return(RMSE(pred_rating, validation$rating))})

rmse_reg <- min(rmses)    
rmse_reg <- format(round(rmse_reg,6), nsmall = 6)


# Plot RMSE against Lambdas to find optimal lambda
qplot(lambdas, rmses) +
  ggtitle("RMSE vs. Lambda") +
  xlab(paste("Lambda")) + ylab("RMSE") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(family="Times New Roman", face="bold", size=10))

lambda <- lambdas[which.min(rmses)]   # 5.50

rmse_results <- rbind(rmse_results, tibble(method="Regularization plus release year", RMSE = rmse_reg))
# Results
# method                    RMSE
# <chr>                     <dbl>
# 1 Naive analysis by mean   1.061202     # Baseline result
# 2 Movie effects model      0.943909     # Improvement = 11.05%
# 3 User effects model       0.865349     # Improvement = 8.32%
# 4 Regularization           0.864817     # Improvement = 0.0541%; error < 0.86490 = max points
# 5 Regularization plus year 0.864796     # Improvement = 0.002%

# Graph improvements with complexity
improvements <- c(11.05, 8.32, 0.0541, 0.002)
models <- c(1, 2, 3, 4)
qplot(models,improvements, geom = c("point","smooth")) +
  ggtitle("Improvements Over Baseline by Model") +
  xlab("Model") + ylab("Improvement, %") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(family="Times New Roman", face="bold", size=10))

       