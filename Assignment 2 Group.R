# Feedback:
# Solid extensive EDA section. Very nice EDA plots. Explored and compared linear regression, random forests and gbm and evaluated performance using RMSE metric. Justified the reason why they should use those methods. Below 1 for rmse. Tuned the hyperparameters. Experimented with feature engineering. Used the CRISP-DM framework. Easy to read well presented report for a business audience. Code is in separate file. Evidence of collaborative work.


install.packages(c("randomForest","Amelia","Metrics","mlbench","caret","reshape","xgboost",
                   "ordinal","corrplot","leaps","glmnet","gbm"))
library(tidyverse) 
library(lubridate) 
library(glmnet)
library(randomForest) 
library(xgboost)
library(Amelia) 
library(dplyr)
library(Metrics)
library(mlbench)
library(caret)
library(reshape)
library(plyr)
library(ordinal)
library(corrplot)
library(leaps)
library(plyr)
library(gbm)


train <- read_rds("AT2_train_STUDENT.rds")
test <- read_rds("AT2_test_STUDENT.rds")

#------------------------DATA CLEAN------------------------
str(train)
#check for NA's in the data
apply(train, 2, function(x) any(is.na(x)))
na_count <-sapply(train, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

# Remove the IMDB and URLfrom train set
train <- select(train, -c(video_release_date, imdb_url))
test <- select(test, -c(video_release_date, imdb_url))

# Change NA's to mean value for Train

train$item_imdb_rating_of_ten <- ifelse(is.na(train$item_imdb_rating_of_ten),
                                        mean(train$item_imdb_rating_of_ten, na.rm=TRUE),
                                        train$item_imdb_rating_of_ten)

train$item_imdb_length  <- ifelse(is.na(train$item_imdb_length ),
                                  mean(train$item_imdb_length ,
                                       na.rm=TRUE), train$item_imdb_length )

train$item_imdb_count_ratings  <- ifelse(is.na(train$item_imdb_count_ratings ),
                                         mean(train$item_imdb_count_ratings , na.rm=TRUE),
                                         train$item_imdb_count_ratings )

train$item_imdb_staff_votes   <- ifelse(is.na(train$item_imdb_staff_votes ),
                                         mean(train$item_imdb_staff_votes , na.rm=TRUE),
                                         train$item_imdb_staff_votes )

train$item_imdb_staff_average  <- ifelse(is.na(train$item_imdb_staff_average ),
                                         mean(train$item_imdb_staff_average , na.rm=TRUE),
                                         train$item_imdb_staff_average )

train$item_imdb_top_1000_voters_votes  <- ifelse(is.na(train$item_imdb_top_1000_voters_votes ),
                                         mean(train$item_imdb_top_1000_voters_votes , na.rm=TRUE),
                                         train$item_imdb_top_1000_voters_votes )

train$item_imdb_top_1000_voters_average  <- ifelse(is.na(train$item_imdb_top_1000_voters_average ),
                                         mean(train$item_imdb_top_1000_voters_average , na.rm=TRUE),
                                         train$item_imdb_top_1000_voters_average )

train$user_gender_item_imdb_mean_rating  <- ifelse(is.na(train$user_gender_item_imdb_mean_rating ),
                                                   mean(train$user_gender_item_imdb_mean_rating , na.rm=TRUE),
                                                   train$user_gender_item_imdb_mean_rating )

train$user_gender_item_imdb_votes  <- ifelse(is.na(train$user_gender_item_imdb_votes ),
                                                   mean(train$user_gender_item_imdb_votes , na.rm=TRUE),
                                                   train$user_gender_item_imdb_votes )

train$user_age_band_item_imdb_mean_rating  <- ifelse(is.na(train$user_age_band_item_imdb_mean_rating ),
                                                   mean(train$user_age_band_item_imdb_mean_rating , na.rm=TRUE),
                                                   train$user_age_band_item_imdb_mean_rating )

train$user_gender_age_band_item_imdb_votes  <- ifelse(is.na(train$user_gender_age_band_item_imdb_votes ),
                                                     mean(train$user_gender_age_band_item_imdb_votes ,
                                                          na.rm=TRUE),
                                                     train$user_gender_age_band_item_imdb_votes )

train$user_gender_age_band_item_imdb_mean_rating  <- ifelse(is.na(train$user_gender_age_band_item_imdb_mean_rating ),
                                                     mean(train$user_gender_age_band_item_imdb_mean_rating ,
                                                          na.rm=TRUE),
                                                     train$user_gender_age_band_item_imdb_mean_rating )

train$user_age_band_item_imdb_votes  <- ifelse(is.na(train$user_age_band_item_imdb_votes ),
                                                            mean(train$user_age_band_item_imdb_votes ,
                                                                 na.rm=TRUE),
                                                            train$user_age_band_item_imdb_votes )

# Change NA's to mean value for Test

test$item_imdb_rating_of_ten <- ifelse(is.na(test$item_imdb_rating_of_ten),
                                        mean(test$item_imdb_rating_of_ten, na.rm=TRUE),
                                       test$item_imdb_rating_of_ten)

test$item_imdb_length  <- ifelse(is.na(test$item_imdb_length ),
                                  mean(test$item_imdb_length ,
                                       na.rm=TRUE), test$item_imdb_length )

test$item_imdb_count_ratings  <- ifelse(is.na(test$item_imdb_count_ratings ),
                                         mean(test$item_imdb_count_ratings , na.rm=TRUE),
                                        test$item_imdb_count_ratings )

test$item_imdb_staff_votes   <- ifelse(is.na(test$item_imdb_staff_votes ),
                                        mean(test$item_imdb_staff_votes , na.rm=TRUE),
                                       test$item_imdb_staff_votes )

test$item_imdb_staff_average  <- ifelse(is.na(test$item_imdb_staff_average ),
                                         mean(test$item_imdb_staff_average , na.rm=TRUE),
                                        test$item_imdb_staff_average )

test$item_imdb_top_1000_voters_votes  <- ifelse(is.na(test$item_imdb_top_1000_voters_votes ),
                                                 mean(test$item_imdb_top_1000_voters_votes , na.rm=TRUE),
                                                test$item_imdb_top_1000_voters_votes )

test$item_imdb_top_1000_voters_average  <- ifelse(is.na(test$item_imdb_top_1000_voters_average ),
                                                   mean(test$item_imdb_top_1000_voters_average , na.rm=TRUE),
                                                  test$item_imdb_top_1000_voters_average )

test$user_gender_item_imdb_mean_rating  <- ifelse(is.na(test$user_gender_item_imdb_mean_rating ),
                                                   mean(test$user_gender_item_imdb_mean_rating , na.rm=TRUE),
                                                  test$user_gender_item_imdb_mean_rating )

test$user_gender_item_imdb_votes  <- ifelse(is.na(test$user_gender_item_imdb_votes ),
                                             mean(test$user_gender_item_imdb_votes , na.rm=TRUE),
                                             test$user_gender_item_imdb_votes )

test$user_age_band_item_imdb_mean_rating  <- ifelse(is.na(test$user_age_band_item_imdb_mean_rating ),
                                                     mean(test$user_age_band_item_imdb_mean_rating , na.rm=TRUE),
                                                    test$user_age_band_item_imdb_mean_rating )

test$user_gender_age_band_item_imdb_votes  <- ifelse(is.na(test$user_gender_age_band_item_imdb_votes ),
                                                      mean(test$user_gender_age_band_item_imdb_votes ,
                                                           na.rm=TRUE),
                                                     test$user_gender_age_band_item_imdb_votes )

test$user_gender_age_band_item_imdb_mean_rating  <- ifelse(is.na(test$user_gender_age_band_item_imdb_mean_rating ),
                                                            mean(test$user_gender_age_band_item_imdb_mean_rating ,
                                                                 na.rm=TRUE),
                                                           test$user_gender_age_band_item_imdb_mean_rating)

test$user_age_band_item_imdb_votes  <- ifelse(is.na(test$user_age_band_item_imdb_votes ),
                                               mean(test$user_age_band_item_imdb_votes ,
                                                    na.rm=TRUE),
                                              test$user_age_band_item_imdb_votes )

test$user_age_band_item_mean_rating  <- ifelse(is.na(test$user_age_band_item_mean_rating ),
                                              mean(test$user_age_band_item_mean_rating ,
                                                   na.rm=TRUE),
                                              test$user_age_band_item_mean_rating )

test$user_gender_item_mean_rating  <- ifelse(is.na(test$user_gender_item_mean_rating ),
                                               mean(test$user_gender_item_mean_rating ,
                                                    na.rm=TRUE),
                                               test$user_gender_item_mean_rating )





#------------------------EDA------------------------

# Number of unique movies and users in the edx dataset 
train %>%
  summarize(n_users = n_distinct(user_id), 
            n_movies = n_distinct(item_id))


# Ratings distribution
# Change rating variable to numeric for the plot
train$rating <- as.numeric(train$rating)
train %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.25, color = "black") +
  scale_x_discrete(limits = c(seq(0,5,1))) +
  scale_y_continuous(breaks = c(seq(0, 3000000, 500000))) +
  ggtitle("Rating distribution")
# Change rating variable back to factor
train$rating <- as.factor(train$rating)

# Ratings distribution for imdb /2
train %>%
  ggplot(aes(item_imdb_rating_of_ten/2)) +
  geom_histogram(binwidth = 0.25, color = "black") +
  scale_x_discrete(limits = c(seq(1,10,0.5))) +
  scale_y_continuous(breaks = c(seq(0, 3000000, 500000))) +
  ggtitle("Rating distribution")

# Plot number of ratings per movie
glimpse(train)
train$item_id <- as.numeric(train$item_id)

train %>%
  count(item_id)%>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  xlab("Number of ratings") +
  ylab("Number of movies") +
  ggtitle("Number of ratings per movie")

# Plot number of ratings given by users
train %>%
  count(item_id) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  xlab("Number of ratings") + 
  ylab("Number of users") +
  ggtitle("Number of ratings given by users")


# i take the original column "genre" from the edx set , whatever combination appears in this column .
# i compute the average and standard error for each "genre". i Plot these as error bar plots for genres with more than 100000 ratings.

# Change different genre columns in one column
train_genre <- train %>% 
  select(11:29)

get_bools = function(n=80523){
  return( sample(x = c(TRUE,FALSE), size = n, replace = TRUE) )
}

genre <- train_genre %>% apply(1, function(x){ return(names(x[x])) }) %>%
  lapply(paste, collapse='|') %>% unlist
train$genre <- genre


# Change different genre columns in one column for test

train_genre <- test %>% 
  select(10:28)

get_bools = function(n=80523){
  return( sample(x = c(TRUE,FALSE), size = n, replace = TRUE) )
}

genre <- train_genre %>% apply(1, function(x){ return(names(x[x])) }) %>%
  lapply(paste, collapse='|') %>% unlist
test$genre <- genre


# Generate plot for mean rating of genre combination.
train$rating <- as.double(train$rating)
train %>% group_by(genre) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genre = reorder(genre, avg)) %>%
  ggplot(aes(x = genre, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Error bar plots by genres" , caption = "source data : edx set") +
  theme(
    panel.background = element_rect(fill = "lightblue",
                                    colour = "lightblue",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
  )

top_genr <- train %>% separate_rows(genre, sep = "\\|") %>%
  group_by(genre) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


# Adjust current feature variable structues 

# Convert structure of genres to binary 1 or 0. 
# train[,14:31] <- lapply(train[,14:31], as.factor)


str(train)

#-------------------------------Feature Engineering TRAIN------------------------------

#Converting Rating, Timestamp and Age into numeric variables
train$rating <- as.factor(train$rating)
train$timestamp <- as.numeric(train$timestamp)
train$age <- as.numeric(train$age)

#Make Gender and Occupation factors
train$gender <- as.factor(train$gender)
train$occupation <- as.factor(train$occupation)

#Convert Genre columns into factors
#train[,11:30] <- lapply(train[,11:30], as.factor)

str(train)

#Standardize the item_mean_rating for each user
train <- ddply(train, "user_id", transform, std.ratings = scale(as.numeric(item_mean_rating)))
#Create a new view variable which deems whether a user will like or dislike a movie
train <- transform(train, view = ifelse(std.ratings>0, 1, -1))
train$view <- factor(train$view, levels=c(1,-1), labels = c("LIKE","DISLIKE"))

#Standardize the ratings for each user
#train <- ddply(train, "user_id", transform, std.ratings = scale(as.numeric(rating)))

train$timestamp <- as.Date(as.POSIXct(train$timestamp, origin="1970-01-01"))

#new feature - amount of time since release date and rating
train <- mutate(train, timeSinceRelease = as.double(timestamp-release_date))

#New features - Month and day of the week reviewed
train$monthReviewed <- as.factor(month(train$timestamp))
train$dayReviewed <- as.factor(wday(train$timestamp))

# Data from IMDB
train$item_imdb_rating_of_ten  <- as.numeric(train$item_imdb_rating_of_ten )
train$movie_title <- as.character(train$movie_title)

#Runtimes expressed as (X min) - extract the numeric time
train$item_imdb_length <- sapply(train$item_imdb_length,
                                 function(x) {as.numeric(str_split(x, " ")[[1]][1])})

# Clustering
sapply(train, class)

train[,11:29] <- lapply(train[,11:29], as.numeric)
set.seed(1)

library(dplyr)
detach(package:plyr)

train$user_id <- as.factor(train$user_id)
train$std.imdbRating <- scale(train$item_imdb_rating_of_ten)

clustering_data <- train %>% group_by(user_id) %>% summarise(
  nCrime = sum(crime==1),
  pCrime = nCrime/length(crime),
  rCrime = ifelse(is.na(mean(std.ratings[crime==1]))|is.nan(mean(std.ratings[crime==1])), 0,
                  mean(std.ratings[crime==1])),
  iCrime = ifelse(is.na(mean(std.imdbRating[crime==1]))|is.nan(mean(std.imdbRating[crime==1])), 0,
                  mean(std.imdbRating[crime==1])),
  dCrime = rCrime - iCrime,
  
  nAction = sum(action==1),
  pAction = nAction/length(action),
  rAction = ifelse(is.na(mean(std.ratings[action==1]))|is.nan(mean(std.ratings[action==1])), 0,
                   mean(std.ratings[action==1])),
  iAction = ifelse(is.na(mean(std.imdbRating[action==1]))|is.nan(mean(std.imdbRating[action==1])), 0,
                   mean(std.imdbRating[action==1])),
  dAction = rAction - iAction,
  
  nComedy = sum(comedy==1),
  pComedy = nComedy/length(comedy),
  rComedy = ifelse(is.na(mean(std.ratings[comedy==1]))|is.nan(mean(std.ratings[comedy==1])), 0,
                   mean(std.ratings[comedy==1])),
  iComedy = ifelse(is.na(mean(std.imdbRating[comedy==1]))|is.nan(mean(std.imdbRating[comedy==1])), 0,
                   mean(std.imdbRating[comedy==1])),
  dComedy = rComedy - iComedy,
  
  nAnimation= sum(animation==1),
  pAnimation = nAnimation/length(animation),
  rAnimation = ifelse(is.na(mean(std.ratings[animation==1]))|is.nan(mean(std.ratings[animation==1])),
                      0,
                      mean(std.ratings[animation==1])),
  iAnimation = ifelse(is.na(mean(std.imdbRating[animation==1]))|
                        is.nan(mean(std.imdbRating[animation==1])), 0,
                      mean(std.imdbRating[animation==1])),
  dAnimation = rAnimation - iAnimation,
  
  nHorror = sum(horror==1),
  pHorror = nHorror/length(horror),
  rHorror = ifelse(is.na(mean(std.ratings[horror==1]))|is.nan(mean(std.ratings[horror==1])), 0,
                   mean(std.ratings[horror==1])),
  iHorror = ifelse(is.na(mean(std.imdbRating[horror==1]))|is.nan(mean(std.imdbRating[horror==1])), 0,
                   mean(std.imdbRating[horror==1])),
  dHorror = rHorror - iHorror,
  
  nUnk = sum(unknown==1),
  pUnk = nUnk/length(unknown),
  rUnk= ifelse(is.na(mean(std.ratings[unknown==1]))|is.nan(mean(std.ratings[unknown==1])), 0,
               mean(std.ratings[unknown==1])),
  iUnk = ifelse(is.na(mean(std.imdbRating[unknown==1]))|is.nan(mean(std.imdbRating[unknown==1])), 0,
                mean(std.imdbRating[unknown==1])),
  dUnk = rUnk - iUnk,
  
  nDrama = sum(drama==1),
  pDrama = nDrama/length(drama),
  rDrama = ifelse(is.na(mean(std.ratings[drama==1]))|is.nan(mean(std.ratings[drama==1])), 0,
                  mean(std.ratings[drama==1])),
  iDrama = ifelse(is.na(mean(std.imdbRating[drama==1]))|is.nan(mean(std.imdbRating[drama==1])), 0,
                  mean(std.imdbRating[drama==1])),
  dDrama = rDrama - iDrama,
  
  nAdventure = sum(adventure==1),
  pAdventure = nAdventure/length(adventure),
  rAdventure = ifelse(is.na(mean(std.ratings[adventure==1]))|is.nan(mean(std.ratings[adventure==1])),
                      0,
                      mean(std.ratings[adventure==1])),
  iAdventure = ifelse(is.na(mean(std.imdbRating[adventure==1]))|
                        is.nan(mean(std.imdbRating[adventure==1])), 0,
                      mean(std.imdbRating[adventure==1])),
  dAdventure = rAdventure - iAdventure,
  
  nScifi = sum(sci_fi==1),
  pScifi = nScifi/length(sci_fi),
  rScifi = ifelse(is.na(mean(std.ratings[sci_fi==1]))|is.nan(mean(std.ratings[sci_fi==1])), 0,
                  mean(std.ratings[sci_fi==1])),
  iScifi = ifelse(is.na(mean(std.imdbRating[sci_fi==1]))|is.nan(mean(std.imdbRating[sci_fi==1])), 0,
                  mean(std.imdbRating[sci_fi==1])),
  dScifi = rScifi - iScifi,
  
  nThrill = sum(thriller==1),
  pThrill = nThrill/length(thriller),
  rThrill= ifelse(is.na(mean(std.ratings[thriller==1]))|is.nan(mean(std.ratings[thriller==1])), 0,
                  mean(std.ratings[thriller==1])),
  iThrill = ifelse(is.na(mean(std.imdbRating[thriller==1]))|
                     is.nan(mean(std.imdbRating[thriller==1])), 0,
                   mean(std.imdbRating[thriller==1])),
  dThrill = rThrill - iThrill,
  
  nFantasy = sum(fantasy==1),
  pFantasy = nFantasy/length(fantasy),
  rFantasy = ifelse(is.na(mean(std.ratings[fantasy==1]))|is.nan(mean(std.ratings[fantasy==1])), 0,
                    mean(std.ratings[fantasy==1])),
  iFantasy = ifelse(is.na(mean(std.imdbRating[fantasy==1]))|
                      is.nan(mean(std.imdbRating[fantasy==1])), 0,
                    mean(std.imdbRating[fantasy==1])),
  dFantasy = rFantasy - iFantasy,
  
  nChild = sum(childrens==1),
  pChild = nChild/length(childrens),
  rChild = ifelse(is.na(mean(std.ratings[childrens==1]))|is.nan(mean(std.ratings[childrens==1])), 0,
                  mean(std.ratings[childrens==1])),
  iChild = ifelse(is.na(mean(std.imdbRating[childrens==1]))|
                    is.nan(mean(std.imdbRating[childrens==1])), 0,
                  mean(std.imdbRating[childrens==1])),
  dChild = rChild - iChild,
  
  nNoir= sum(film_noir==1),
  pNoir = nNoir/length(film_noir),
  rNoir = ifelse(is.na(mean(std.ratings[film_noir==1]))|is.nan(mean(std.ratings[film_noir==1])), 0,
                 mean(std.ratings[film_noir==1])),
  iNoir = ifelse(is.na(mean(std.imdbRating[film_noir==1]))|
                   is.nan(mean(std.imdbRating[film_noir==1])), 0,
                 mean(std.imdbRating[film_noir==1])),
  dNoir = rNoir - iNoir,
  
  nWestern = sum(western==1),
  pWestern = nWestern/length(western),
  rWestern = ifelse(is.na(mean(std.ratings[western==1]))|is.nan(mean(std.ratings[western==1])), 0,
                    mean(std.ratings[western==1])),
  iWestern = ifelse(is.na(mean(std.imdbRating[western==1]))|
                      is.nan(mean(std.imdbRating[western==1])), 0,
                    mean(std.imdbRating[western==1])),
  dWestern = rWestern - iWestern,
  
  nMystery= sum(mystery==1),
  pMystery = nMystery/length(mystery),
  rMystery= ifelse(is.na(mean(std.ratings[mystery==1]))|is.nan(mean(std.ratings[mystery==1])), 0, 
                   mean(std.ratings[mystery==1])),
  iMystery = ifelse(is.na(mean(std.imdbRating[mystery==1]))|is.nan(mean(std.imdbRating[mystery==1])),
                    0,
                    mean(std.imdbRating[mystery==1])),
  dMystery = rMystery - iMystery,
  
  nDoc = sum(documentary==1),
  pDoc = nDoc/length(documentary),
  rDoc = ifelse(is.na(mean(std.ratings[documentary==1]))|is.nan(mean(std.ratings[documentary==1])), 0,
                mean(std.ratings[documentary==1])),
  iDoc = ifelse(is.na(mean(std.imdbRating[documentary==1]))|
                  is.nan(mean(std.imdbRating[documentary==1])), 0,
                mean(std.imdbRating[documentary==1])),
  dDoc = rDoc - iDoc,
  
  nWar = sum(war==1),
  pWar = nWar/length(war),
  rWar = ifelse(is.na(mean(std.ratings[war==1]))|is.nan(mean(std.ratings[war==1])), 0, 
                mean(std.ratings[war==1])),
  iWar = ifelse(is.na(mean(std.imdbRating[war==1]))|is.nan(mean(std.imdbRating[war==1])), 0,
                mean(std.imdbRating[war==1])),
  dWar = rWar - iWar,
  
  nRom = sum(romance==1),
  pRom = nRom/length(romance),
  rRom= ifelse(is.na(mean(std.ratings[romance==1]))|is.nan(mean(std.ratings[romance==1])), 0,
               mean(std.ratings[romance==1])),
  iRom = ifelse(is.na(mean(std.imdbRating[romance==1]))|is.nan(mean(std.imdbRating[romance==1])), 0,
                mean(std.imdbRating[romance==1])),
  dRom = rRom - iRom,
  
  nMusical = sum(musical==1),
  pMusical = nMusical/length(musical),
  rMusical = ifelse(is.na(mean(std.ratings[musical==1]))|is.nan(mean(std.ratings[musical==1])), 0,
                    mean(std.ratings[musical==1])),
  iMusical = ifelse(is.na(mean(std.imdbRating[musical==1]))|
                      is.nan(mean(std.imdbRating[musical==1])), 0,
                    mean(std.imdbRating[musical==1])),
  dMusical = rMusical - iMusical
)

clusters <- kmeans(clustering_data[,c(seq(3,length(clustering_data),5),seq(6,length(clustering_data),5))], 100,
                   nstart = 20)
clusters_clean <- data.frame(user_id = clustering_data$user_id, cluster = clusters$cluster)
clusters_clean$cluster <- as.factor(clusters_clean$cluster)
train <- merge(train, clusters_clean)

# Generate variable for imdb Rating + Votes
# R = average for the movie (mean) = (Rating)
# v = number of votes for the movie = (votes)
# m = minimum votes required to be listed in the Top 250
# C = the mean vote across the whole report
weighted_rating <- function(R, v, m, C) {
  return (v/(v+m))*R + (m/(v+m))*C
}
# For IMDB general
train <- train %>%
  mutate(wr_imdb_rating_of_ten = weighted_rating(item_imdb_rating_of_ten, item_imdb_count_ratings,
                                                 70000, mean(item_imdb_rating_of_ten))) %>%
  arrange(desc(wr_imdb_rating_of_ten))

# For IMDB staff 
train <- train %>%
  mutate(wr_imdb_staff = weighted_rating(item_imdb_staff_average, item_imdb_staff_votes,
                                         20, mean(item_imdb_staff_average))) %>%
  arrange(desc(wr_imdb_staff))

# For IMDB top 1000 voters

train <- train %>%
  mutate(wr_imdb_1000 = weighted_rating(item_imdb_top_1000_voters_average, item_imdb_top_1000_voters_votes,
                                        400, mean(item_imdb_top_1000_voters_average))) %>%
  arrange(desc(wr_imdb_1000))

# Create a demographic variable for item mean rating.
demo_var <- train %>% group_by(age_band, occupation, gender) %>% summarise(demo_var = mean(item_mean_rating))

train <- left_join(train, demo_var, by = c("age_band", "occupation", "gender"))

# Find out how many genres each movie has. Set it as a factor. 
train$genre_sums <- rowSums(train[11:29])
train$genre_sums <- as.numeric(train$genre_sums)

# Create a variable that gives more weight to movies with more votes combined from imdb staff, compared with their imdb relevant staff rating
train <- train %>% group_by(item_id, item_imdb_staff_average, item_imdb_staff_votes)  %>% 
  mutate(weight = ((item_imdb_staff_votes * item_imdb_staff_average)/(105)))

str(train)
#------------------------------------Testset----------------------------------

#Converting Rating, Timestamp and Age into numeric variables

test$timestamp <- as.numeric(test$timestamp)
test$age <- as.numeric(test$age)

#Make Gender and Occupation factors
test$gender <- as.factor(test$gender)
test$occupation <- as.factor(test$occupation)

#Convert Genre columns into factors
#train[,11:30] <- lapply(train[,11:30], as.factor)

str(test)

#Standardize the item_mean_rating for each user
test <- ddply(test, "user_id", transform, std.ratings = scale(as.numeric(item_mean_rating)))
#Create a new view variable which deems whether a user will like or dislike a movie
test <- transform(test, view = ifelse(std.ratings>0, 1, -1))
test$view <- factor(test$view, levels=c(1,-1), labels = c("LIKE","DISLIKE"))

#Standardize the ratings for each user
#train <- ddply(train, "user_id", transform, std.ratings = scale(as.numeric(rating)))

test$timestamp <- as.Date(as.POSIXct(test$timestamp, origin="1970-01-01"))

#new feature - amount of time since release date and rating
test <- mutate(test, timeSinceRelease = as.double(timestamp-release_date))

#New features - Month and day of the week reviewed
test$monthReviewed <- as.factor(month(test$timestamp))
test$dayReviewed <- as.factor(wday(test$timestamp))

# Data from IMDB
test$item_imdb_rating_of_ten  <- as.numeric(test$item_imdb_rating_of_ten )
test$movie_title <- as.character(test$movie_title)

#Runtimes expressed as (X min) - extract the numeric time
test$item_imdb_length <- sapply(test$item_imdb_length,
                                 function(x) {as.numeric(str_split(x, " ")[[1]][1])})

# Clustering
sapply(test, class)

test[,10:28] <- lapply(test[,10:28], as.numeric)
set.seed(1)

library(dplyr)
detach(package:plyr)

test$user_id <- as.factor(test$user_id)
test$std.imdbRating <- scale(test$item_imdb_rating_of_ten)

clustering_data <- test %>% group_by(user_id) %>% summarise(
  nCrime = sum(crime==1),
  pCrime = nCrime/length(crime),
  rCrime = ifelse(is.na(mean(std.ratings[crime==1]))|is.nan(mean(std.ratings[crime==1])), 0,
                  mean(std.ratings[crime==1])),
  iCrime = ifelse(is.na(mean(std.imdbRating[crime==1]))|is.nan(mean(std.imdbRating[crime==1])), 0,
                  mean(std.imdbRating[crime==1])),
  dCrime = rCrime - iCrime,
  
  nAction = sum(action==1),
  pAction = nAction/length(action),
  rAction = ifelse(is.na(mean(std.ratings[action==1]))|is.nan(mean(std.ratings[action==1])), 0,
                   mean(std.ratings[action==1])),
  iAction = ifelse(is.na(mean(std.imdbRating[action==1]))|is.nan(mean(std.imdbRating[action==1])), 0,
                   mean(std.imdbRating[action==1])),
  dAction = rAction - iAction,
  
  nComedy = sum(comedy==1),
  pComedy = nComedy/length(comedy),
  rComedy = ifelse(is.na(mean(std.ratings[comedy==1]))|is.nan(mean(std.ratings[comedy==1])), 0,
                   mean(std.ratings[comedy==1])),
  iComedy = ifelse(is.na(mean(std.imdbRating[comedy==1]))|is.nan(mean(std.imdbRating[comedy==1])), 0,
                   mean(std.imdbRating[comedy==1])),
  dComedy = rComedy - iComedy,
  
  nAnimation= sum(animation==1),
  pAnimation = nAnimation/length(animation),
  rAnimation = ifelse(is.na(mean(std.ratings[animation==1]))|is.nan(mean(std.ratings[animation==1])),
                      0,
                      mean(std.ratings[animation==1])),
  iAnimation = ifelse(is.na(mean(std.imdbRating[animation==1]))|
                        is.nan(mean(std.imdbRating[animation==1])), 0,
                      mean(std.imdbRating[animation==1])),
  dAnimation = rAnimation - iAnimation,
  
  nHorror = sum(horror==1),
  pHorror = nHorror/length(horror),
  rHorror = ifelse(is.na(mean(std.ratings[horror==1]))|is.nan(mean(std.ratings[horror==1])), 0,
                   mean(std.ratings[horror==1])),
  iHorror = ifelse(is.na(mean(std.imdbRating[horror==1]))|is.nan(mean(std.imdbRating[horror==1])), 0,
                   mean(std.imdbRating[horror==1])),
  dHorror = rHorror - iHorror,
  
  nUnk = sum(unknown==1),
  pUnk = nUnk/length(unknown),
  rUnk= ifelse(is.na(mean(std.ratings[unknown==1]))|is.nan(mean(std.ratings[unknown==1])), 0,
               mean(std.ratings[unknown==1])),
  iUnk = ifelse(is.na(mean(std.imdbRating[unknown==1]))|is.nan(mean(std.imdbRating[unknown==1])), 0,
                mean(std.imdbRating[unknown==1])),
  dUnk = rUnk - iUnk,
  
  nDrama = sum(drama==1),
  pDrama = nDrama/length(drama),
  rDrama = ifelse(is.na(mean(std.ratings[drama==1]))|is.nan(mean(std.ratings[drama==1])), 0,
                  mean(std.ratings[drama==1])),
  iDrama = ifelse(is.na(mean(std.imdbRating[drama==1]))|is.nan(mean(std.imdbRating[drama==1])), 0,
                  mean(std.imdbRating[drama==1])),
  dDrama = rDrama - iDrama,
  
  nAdventure = sum(adventure==1),
  pAdventure = nAdventure/length(adventure),
  rAdventure = ifelse(is.na(mean(std.ratings[adventure==1]))|is.nan(mean(std.ratings[adventure==1])),
                      0,
                      mean(std.ratings[adventure==1])),
  iAdventure = ifelse(is.na(mean(std.imdbRating[adventure==1]))|
                        is.nan(mean(std.imdbRating[adventure==1])), 0,
                      mean(std.imdbRating[adventure==1])),
  dAdventure = rAdventure - iAdventure,
  
  nScifi = sum(sci_fi==1),
  pScifi = nScifi/length(sci_fi),
  rScifi = ifelse(is.na(mean(std.ratings[sci_fi==1]))|is.nan(mean(std.ratings[sci_fi==1])), 0,
                  mean(std.ratings[sci_fi==1])),
  iScifi = ifelse(is.na(mean(std.imdbRating[sci_fi==1]))|is.nan(mean(std.imdbRating[sci_fi==1])), 0,
                  mean(std.imdbRating[sci_fi==1])),
  dScifi = rScifi - iScifi,
  
  nThrill = sum(thriller==1),
  pThrill = nThrill/length(thriller),
  rThrill= ifelse(is.na(mean(std.ratings[thriller==1]))|is.nan(mean(std.ratings[thriller==1])), 0,
                  mean(std.ratings[thriller==1])),
  iThrill = ifelse(is.na(mean(std.imdbRating[thriller==1]))|
                     is.nan(mean(std.imdbRating[thriller==1])), 0,
                   mean(std.imdbRating[thriller==1])),
  dThrill = rThrill - iThrill,
  
  nFantasy = sum(fantasy==1),
  pFantasy = nFantasy/length(fantasy),
  rFantasy = ifelse(is.na(mean(std.ratings[fantasy==1]))|is.nan(mean(std.ratings[fantasy==1])), 0,
                    mean(std.ratings[fantasy==1])),
  iFantasy = ifelse(is.na(mean(std.imdbRating[fantasy==1]))|
                      is.nan(mean(std.imdbRating[fantasy==1])), 0,
                    mean(std.imdbRating[fantasy==1])),
  dFantasy = rFantasy - iFantasy,
  
  nChild = sum(childrens==1),
  pChild = nChild/length(childrens),
  rChild = ifelse(is.na(mean(std.ratings[childrens==1]))|is.nan(mean(std.ratings[childrens==1])), 0,
                  mean(std.ratings[childrens==1])),
  iChild = ifelse(is.na(mean(std.imdbRating[childrens==1]))|
                    is.nan(mean(std.imdbRating[childrens==1])), 0,
                  mean(std.imdbRating[childrens==1])),
  dChild = rChild - iChild,
  
  nNoir= sum(film_noir==1),
  pNoir = nNoir/length(film_noir),
  rNoir = ifelse(is.na(mean(std.ratings[film_noir==1]))|is.nan(mean(std.ratings[film_noir==1])), 0,
                 mean(std.ratings[film_noir==1])),
  iNoir = ifelse(is.na(mean(std.imdbRating[film_noir==1]))|
                   is.nan(mean(std.imdbRating[film_noir==1])), 0,
                 mean(std.imdbRating[film_noir==1])),
  dNoir = rNoir - iNoir,
  
  nWestern = sum(western==1),
  pWestern = nWestern/length(western),
  rWestern = ifelse(is.na(mean(std.ratings[western==1]))|is.nan(mean(std.ratings[western==1])), 0,
                    mean(std.ratings[western==1])),
  iWestern = ifelse(is.na(mean(std.imdbRating[western==1]))|
                      is.nan(mean(std.imdbRating[western==1])), 0,
                    mean(std.imdbRating[western==1])),
  dWestern = rWestern - iWestern,
  
  nMystery= sum(mystery==1),
  pMystery = nMystery/length(mystery),
  rMystery= ifelse(is.na(mean(std.ratings[mystery==1]))|is.nan(mean(std.ratings[mystery==1])), 0, 
                   mean(std.ratings[mystery==1])),
  iMystery = ifelse(is.na(mean(std.imdbRating[mystery==1]))|is.nan(mean(std.imdbRating[mystery==1])),
                    0,
                    mean(std.imdbRating[mystery==1])),
  dMystery = rMystery - iMystery,
  
  nDoc = sum(documentary==1),
  pDoc = nDoc/length(documentary),
  rDoc = ifelse(is.na(mean(std.ratings[documentary==1]))|is.nan(mean(std.ratings[documentary==1])), 0,
                mean(std.ratings[documentary==1])),
  iDoc = ifelse(is.na(mean(std.imdbRating[documentary==1]))|
                  is.nan(mean(std.imdbRating[documentary==1])), 0,
                mean(std.imdbRating[documentary==1])),
  dDoc = rDoc - iDoc,
  
  nWar = sum(war==1),
  pWar = nWar/length(war),
  rWar = ifelse(is.na(mean(std.ratings[war==1]))|is.nan(mean(std.ratings[war==1])), 0, 
                mean(std.ratings[war==1])),
  iWar = ifelse(is.na(mean(std.imdbRating[war==1]))|is.nan(mean(std.imdbRating[war==1])), 0,
                mean(std.imdbRating[war==1])),
  dWar = rWar - iWar,
  
  nRom = sum(romance==1),
  pRom = nRom/length(romance),
  rRom= ifelse(is.na(mean(std.ratings[romance==1]))|is.nan(mean(std.ratings[romance==1])), 0,
               mean(std.ratings[romance==1])),
  iRom = ifelse(is.na(mean(std.imdbRating[romance==1]))|is.nan(mean(std.imdbRating[romance==1])), 0,
                mean(std.imdbRating[romance==1])),
  dRom = rRom - iRom,
  
  nMusical = sum(musical==1),
  pMusical = nMusical/length(musical),
  rMusical = ifelse(is.na(mean(std.ratings[musical==1]))|is.nan(mean(std.ratings[musical==1])), 0,
                    mean(std.ratings[musical==1])),
  iMusical = ifelse(is.na(mean(std.imdbRating[musical==1]))|
                      is.nan(mean(std.imdbRating[musical==1])), 0,
                    mean(std.imdbRating[musical==1])),
  dMusical = rMusical - iMusical
)

clusters <- kmeans(clustering_data[,c(seq(3,length(clustering_data),5),seq(6,length(clustering_data),5))], 100,
                   nstart = 20)
clusters_clean <- data.frame(user_id = clustering_data$user_id, cluster = clusters$cluster)
clusters_clean$cluster <- as.factor(clusters_clean$cluster)
test <- merge(test, clusters_clean)

# Generate variable for imdb Rating + Votes
# R = average for the movie (mean) = (Rating)
# v = number of votes for the movie = (votes)
# m = minimum votes required to be listed in the Top 250
# C = the mean vote across the whole report
weighted_rating <- function(R, v, m, C) {
  return (v/(v+m))*R + (m/(v+m))*C
}
# For IMDB general
test <- test %>%
  mutate(wr_imdb_rating_of_ten = weighted_rating(item_imdb_rating_of_ten, item_imdb_count_ratings,
                                                 70000, mean(item_imdb_rating_of_ten))) 
 

# For IMDB staff 
test <- test %>%
  mutate(wr_imdb_staff = weighted_rating(item_imdb_staff_average, item_imdb_staff_votes,
                                         20, mean(item_imdb_staff_average))) 

# For IMDB top 1000 voters

test <- test %>%
  mutate(wr_imdb_1000 = weighted_rating(item_imdb_top_1000_voters_average, item_imdb_top_1000_voters_votes,
                                        400, mean(item_imdb_top_1000_voters_average))) 

# Create a demographic variable for item mean rating.
demo_var <- test %>% group_by(age_band, occupation, gender) %>% summarise(demo_var = mean(item_mean_rating))

test <- left_join(test, demo_var, by = c("age_band", "occupation", "gender"))

# Find out how many genres each movie has. Set it as a factor. 
test$genre_sums <- rowSums(test[10:28])
test$genre_sums <- as.numeric(test$genre_sums)

# Create a variable that gives more weight to movies with more votes combined from imdb staff, compared with their imdb relevant staff rating
test <- test %>% group_by(item_id, item_imdb_staff_average, item_imdb_staff_votes)  %>% 
  mutate(weight = ((item_imdb_staff_votes * item_imdb_staff_average)/(105)))

str(test)
str(train)


#------------------------------FAKE Train and Test set------------------------
#------------------------------FAKE Train and Test set------------------------
#------------------------------FAKE Train and Test set------------------------
str(train)

train$genre <- as.factor(train$genre)
train$rating <- as.numeric(train$rating)

# For XGBoos
train$rating <- as.numeric(train$rating)
train$view  <- as.numeric(train$view)
train$occupation <- as.numeric(train$occupation)
train$zip_code <- as.numeric(train$zip_code)
train$occupation <- as.numeric(train$occupation)
train$cluster <- as.numeric(train$cluster)
train$gender <- as.numeric(train$gender)


test$view  <- as.numeric(test$view)
test$occupation <- as.numeric(test$occupation)
test$zip_code <- as.numeric(test$zip_code)
test$occupation <- as.numeric(test$occupation)
test$cluster <- as.numeric(test$cluster)
test$gender <- as.numeric(test$gender)



# Set rating on first position
train <- train[,c(7,1:6,8:61)]
train <- train[,-c(2,7,8,9,10,11:30,36,48,52:53)]
test <- test[,-c(7:9,10:29,35,47,51:52)]

#check for NA's in the data
apply(test, 2, function(x) any(is.na(x)))
na_count <-sapply(test, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

#Set a random seed so we use the same selection of random observations in each analysis.
set.seed(1)
trainingset_size <- floor(0.8 * nrow(train))

# Set your index of the training set sample from the original data set.
trainingset_index <- sample(seq_len(nrow(train)), size = trainingset_size)
trainingset <- train[trainingset_index, ]
testingset <- train[-trainingset_index, ]

trainingset <- trainingset[,c(7,1:6,8:61)]
testingset <- testingset[,c(7,1:6,8:61)]
trainingset <- trainingset[,-c(2,7,8,9,10,11:30,36,48,52:53)]
testingset <- testingset[,-c(2,7,8,9,10,11:30,36,48,52:53)]

trainingset$rating <- as.numeric(trainingset$rating)
trainingset$view  <- as.numeric(trainingset$view)
trainingset$occupation <- as.numeric(trainingset$occupation)
trainingset$zip_code <- as.numeric(trainingset$zip_code)
trainingset$occupation <- as.numeric(trainingset$occupation)
trainingset$cluster <- as.numeric(trainingset$cluster)
trainingset$gender <- as.numeric(trainingset$gender)


testingset$view  <- as.numeric(testingset$view)
testingset$occupation <- as.numeric(testingset$occupation)
testingset$zip_code <- as.numeric(testingset$zip_code)
testingset$occupation <- as.numeric(testingset$occupation)
testingset$cluster <- as.numeric(testingset$cluster)
testingset$gender <- as.numeric(testingset$gender)

#----------------------Prediction--------------------


# Ensure the training set and testing sets are respectively 50% and 50% the size of the original data set, repurch. 
nrow(trainingset); nrow(testingset); nrow(train)

# Build a linear regression model on the original User, Item, Ratings Variables

# calculate correlation matrix

correlationMatrix <- cor(train[,c("age","rating","item_mean_rating","user_age_band_item_mean_rating",
                                  "item_imdb_rating_of_ten", "item_imdb_count_ratings",
                                  "item_imdb_length","item_imdb_staff_votes","item_imdb_staff_average",
                                  "item_imdb_top_1000_voters_votes","item_imdb_top_1000_voters_average",
                                  "user_gender_item_imdb_mean_rating","user_gender_item_imdb_votes",
                                  "user_age_band_item_imdb_votes","user_age_band_item_imdb_mean_rating",
                                  "user_gender_age_band_item_imdb_votes", 
                                  "user_gender_age_band_item_imdb_mean_rating","timeSinceRelease",
                                  "std.imdbRating"
)])

# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)


# 1: RMSE 0.9469404
slm <- lm(rating ~ gender + occupation + age_band + item_imdb_length +
            user_gender_age_band_item_imdb_votes + 
            user_gender_age_band_item_imdb_mean_rating + timeSinceRelease + 
            cluster + genre + monthReviewed + dayReviewed + item_imdb_rating_of_ten
          + item_mean_rating + user_age_band_item_mean_rating + item_imdb_staff_average
          + item_imdb_top_1000_voters_average + user_gender_item_imdb_mean_rating + 
            user_age_band_item_imdb_mean_rating + std.imdbRating,
          data = trainingset, na.action=na.omit) 

# "item_mean_rating", "user_age_band_item_mean_rating",
#"item_imdb_rating_of_ten",
# item_imdb_staff_average",
#, "item_imdb_top_1000_voters_average",
#"user_gender_item_imdb_mean_rating",
#, "user_age_band_item_imdb_mean_rating",
#"user_gender_age_band_item_imdb_votes",
#"user_gender_age_band_item_imdb_mean_rating",
#"std.imdbRating"


# 2: RMSE 1.005489
slm <- lm(rating ~ gender + occupation + age_band + item_imdb_length +
            user_gender_age_band_item_imdb_votes + 
            user_gender_age_band_item_imdb_mean_rating + timeSinceRelease + genre ,
          data = trainingset, na.action=na.omit)

# 3: RMSE 0.896326
slm <- lm(rating ~ timestamp + age_band + gender + zip_code + occupation + item_imdb_rating_of_ten + item_mean_rating + user_age_band_item_mean_rating + user_age_band_item_imdb_mean_rating + user_gender_age_band_item_imdb_votes + user_gender_item_mean_rating + user_id + item_id, data = trainingset)

#wr_imdb_staff
#wr_imdb_1000

testingset$output <- predict(slm, newdata = testingset, type = "response")

testingset$rating <- as.numeric(testingset$rating)

str(testingset$rating)

sqrt( mean( (testingset$rating-testingset$output)^2 , na.rm = TRUE ) )

#------------------------- Gradient Boosting ------------------------- 

# Building GBM with caret
write_csv(testingset,"test_fake")
write_csv(trainingset, "training_fake")

library(h2o)

h2o.init()

trainingset <- h2o.importFile("training_fake")
testingset <- h2o.importFile("test_fake")

# Identify predictors and response
y <- "rating"
x <- setdiff(names(trainingset), y)

ss <- h2o.splitFrame(trainingset, seed = 1)
train_1 <- ss[[1]]
valid <- ss[[2]]

# GBM hyperparamters
gbm_params1 <- list(learn_rate = c(0.01,0.06, 0.1),
                    max_depth = c(3, 5, 9, 10),
                    sample_rate = c(0.6,0.8, 1.0),
                    col_sample_rate = c(0.2, 0.5, 1.0))

# Train and validate a cartesian grid of GBMs
gbm_grid1 <- h2o.grid("gbm", x = x, y = y,
                      grid_id = "gbm_grid1",
                      training_frame = train_1,
                      validation_frame = valid,
                      ntrees = 200,
                      seed = 1,
                      hyper_params = gbm_params1)

# Get the grid results, sorted by validation rmse
gbm_gridperf1 <- h2o.getGrid(grid_id = "gbm_grid1",
                             sort_by = "rmse",
                             decreasing = TRUE)
print(gbm_gridperf1)

# Grab the top GBM model, chosen by validation MSE
best_gbm1 <- h2o.getModel(gbm_gridperf1@model_ids[[1]])

# Now let's evaluate the model performance on a test set
# so we get an honest estimate of top model performance
best_gbm_perf1 <- h2o.performance(model = best_gbm1,
                                  newdata = testingset)
h2o.rmse(best_gbm_perf1)
summary(best_gbm_perf1)

# Look at the hyperparamters for the best model
print(best_gbm1@model[["model_summary"]])






# Calculate the rmse
rmse(testingset$rating, testingset$prediction) 

myGrid <- expand.grid(n.trees = c(300, 400, 600, 1000),
                      interaction.depth = c(5, 6, 7, 8, 9),
                      shrinkage = c(0.075, 0.09, 0.1, 0.11, 0.2),
                      n.minobsinnode = c(7, 10, 12, 15, 18))

trainControl <- trainControl(method="cv", number=10)

set.seed(2019)
gbm_rating <- train(rating ~ zip_code + occupation + item_imdb_rating_of_ten + item_mean_rating
                      user_age_band_item_mean_rating + user_gender_item_mean_rating + demo_var +
                      cluster +timeSinceRelease + view + std.ratings + wr_imdb_staff + 
                      wr_imdb_rating_of_ten,
                    data = trainingset, method = "gbm", distribution = "gaussian",
                    trControl = trainControl,tuneGrid = myGrid, verbose = FALSE )
gbm_rating

gbm_rating$bestTune

varImp(gbm_rating)

# Parameter tuning
gbm_depth = 9 #maximum nodes per tree
gbm_n.min = 14 #minimum number of observations in the trees terminal, important effect on overfitting
gbm_shrinkage=0.009 #learning rate
cores_num = 8 #number of cores
gbm_cv_folds=10 #number of cross-validation folds to perform
num_trees = 200 # Number of iterations

# build the model
gbm_clf = gbm(rating ~ .,
              distribution = "gaussian" ,
              n.trees = num_trees,
              interaction.depth= gbm_depth,
              n.minobsinnode = gbm_n.min,
              shrinkage=gbm_shrinkage,
              cv.folds=gbm_cv_folds,
              verbose = TRUE, #print the preliminary output
              n.cores = cores_num
)

# Estimate the optimal number of iterations (when will the model stop improving)
# The black is the training deviance dropping whilst the green is the test.
best_iter = gbm.perf(gbm_clf, method = "cv")
print(best_iter)

# Gives the variable importance in a graph
summary(gbm_clf,n.trees=best_iter, ylab = "Variable", main = "Variable Relative Importance")

# OR just as a table
summary(gbm_clf)

# Let us get our estimates
testingset$prediction = predict(gbm_clf, testingset, n.trees = best_iter, type = "response")

testingset$rating <- as.numeric(testingset$rating)

# Calculate the rmse
rmse(testingset$rating, testingset$prediction) #0.95835 or it is 0.8946 when zip code is included.

#------------------------- XG Boost ------------------------- 

train_xg<- as.matrix(trainingset, rownames.force=NA)
test_xg<- as.matrix(testingset, rownames.force=NA)
train_xg <- as(train_xg, "sparseMatrix")
test_xg <- as(test_xg, "sparseMatrix")
# Never forget to exclude objective variable in 'data option'
train_Data <- xgb.DMatrix(data = train_xg[,2:32], label = train_xg[,"rating"])

#Then I tune the parameters of xgboost model by building a 20-iteration for-loop. **Not sure whether this method is reliable but really time-consuming**
# Tuning the parameters #
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1,number = 3)

# xgb.grid <- expand.grid(nrounds = 300,
#                         max_depth =7,
#                         eta = 0.05,
#                         gamma = 0.01,
#                         colsample_bytree =0.75,
#                         min_child_weight=0,
#                         subsample =0.5
# ) RMSE 0.9099767

xgb.grid <- expand.grid(nrounds = 100,
                        max_depth =6,
                        eta = 0.3,
                        gamma = 0,
                        colsample_bytree =1,
                        min_child_weight=1,
                        subsample =1
                        
)
xgb_tune <-train(rating ~.,
                 data=trainingset,
                 method="xgbTree",
                 metric = "RMSE",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid,
                 
)

varImp(xgb_tune)

prediction <- predict(xgb_base, testingset)

rmse(as.numeric(testingset$rating),prediction)


input_x <- as.matrix(select(trainingset, -rating))
input_y <- trainingset$rating

grid_default <- expand.grid(
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

train_control <- caret::trainControl(
  method = "none",
  verboseIter = FALSE, # no training log
  allowParallel = TRUE # FALSE for reproducible results 
)

xgb_base <- caret::train(
  x = input_x,
  y = input_y,
  trControl = train_control,
  tuneGrid = grid_default,
  method = "xgbTree",
  verbose = TRUE
)

#Step 1: Number of Iterations and the Learning Rate
nrounds <- 1000

# note to start nrounds from 200, as smaller learning rates result in errors so
# big with lower starting points that they'll mess the scales
tune_grid <- expand.grid(
  nrounds = seq(from = 200, to = nrounds, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(4, 5, 6, 7, 8),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 3, # with n folds 
  #index = createFolds(tr_treated$Id_clean), # fix the folds
  verboseIter = FALSE, # no training log
  allowParallel = TRUE # FALSE for reproducible results 
)
xgb_tune <- caret::train(
  x = input_x,
  y = input_y,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = TRUE
)

# helper function for the plots
tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$RMSE, probs = probs), min(x$results$RMSE))) +
    theme_bw()
}

tuneplot(xgb_tune)

xgb_tune$bestTune

#Step 2: Maximum Depth and Minimum Child Weight

tune_grid2 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = ifelse(xgb_tune$bestTune$max_depth == 2,
                     c(xgb_tune$bestTune$max_depth:4),
                     xgb_tune$bestTune$max_depth - 1:xgb_tune$bestTune$max_depth + 1),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = c(1, 2, 3),
  subsample = 1
)

xgb_tune2 <- caret::train(
  x = input_x,
  y = input_y,
  trControl = tune_control,
  tuneGrid = tune_grid2,
  method = "xgbTree",
  verbose = TRUE
)

tuneplot(xgb_tune2)

xgb_tune2$bestTune

#Step 3: Column and Row Sampling

tune_grid3 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = 0,
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = c(0.5, 0.75, 1.0)
)

xgb_tune3 <- caret::train(
  x = input_x,
  y = input_y,
  trControl = tune_control,
  tuneGrid = tune_grid3,
  method = "xgbTree",
  verbose = TRUE
)

tuneplot(xgb_tune3, probs = .95)

xgb_tune3$bestTune
# nrounds max_depth   eta gamma colsample_bytree min_child_weight subsample
#220    1000         6 0.025     0                1                2      0.75

#Step 4: Gamma

tune_grid4 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

xgb_tune4 <- caret::train(
  x = input_x,
  y = input_y,
  trControl = tune_control,
  tuneGrid = tune_grid4,
  method = "xgbTree",
  verbose = TRUE
)

tuneplot(xgb_tune4)

xgb_tune4$bestTune

# nrounds max_depth   eta gamma colsample_bytree min_child_weight subsample
# 139     950         6 0.025     1                1                2      0.75


#Step 5: Reducing the Learning Rate
tune_grid5 <- expand.grid(
  nrounds = seq(from = 100, to = 10000, by = 100),
  eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = xgb_tune4$bestTune$gamma,
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

xgb_tune5 <- caret::train(
  x = input_x,
  y = input_y,
  trControl = tune_control,
  tuneGrid = tune_grid5,
  method = "xgbTree",
  verbose = TRUE
)

tuneplot(xgb_tune5)

#Fitting the Model

final_grid <- expand.grid(
  nrounds = xgb_tune5$bestTune$nrounds,
  eta = xgb_tune5$bestTune$eta,
  max_depth = xgb_tune5$bestTune$max_depth,
  gamma = xgb_tune5$bestTune$gamma,
  colsample_bytree = xgb_tune5$bestTune$colsample_bytree,
  min_child_weight = xgb_tune5$bestTune$min_child_weight,
  subsample = xgb_tune5$bestTune$subsample
)

xgb_model <- caret::train(
  x = input_x,
  y = input_y,
  trControl = train_control,
  tuneGrid = final_grid,
  method = "xgbTree",
  verbose = TRUE
)


final_grid$bestTune
prediction <- predict(xgb_tune3, testingset)

rmse(as.numeric(testingset$rating),prediction)

#Evaluating the Model Performance

holdout_x <- select(trainingset, -rating)
holdout_y <- trainingset$rating

(linear_base_rmse <- ModelMetrics::rmse(holdout_y, predict(linear_base, newdata = holdout_x)))

(xgb_base_rmse <- ModelMetrics::rmse(holdout_y, predict(xgb_base, newdata = holdout_x)))

(xgb_model_rmse <- ModelMetrics::rmse(holdout_y, predict(xgb_model, newdata = holdout_x)))

#---------------------------------Submission--------------------



train_xg<- as.matrix(train, rownames.force=NA)
test_xg<- as.matrix(test, rownames.force=NA)
train_xg <- as(train_xg, "sparseMatrix")
test_xg <- as(test_xg, "sparseMatrix")
# Never forget to exclude objective variable in 'data option'
train_Data <- xgb.DMatrix(data = train_xg[,2:32], label = train_xg[,"rating"])

#Then I tune the parameters of xgboost model by building a 20-iteration for-loop. **Not sure whether this method is reliable but really time-consuming**
# Tuning the parameters #
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1,number = 3)

# xgb.grid <- expand.grid(nrounds = 300,
#                         max_depth =7,
#                         eta = 0.05,
#                         gamma = 0.01,
#                         colsample_bytree =0.75,
#                         min_child_weight=0,
#                         subsample =0.5
# ) RMSE 0.9099767

xgb.grid <- expand.grid(nrounds = 400,
                        max_depth =7,
                        eta = 0.05,
                        gamma = 2,
                        colsample_bytree =0.8,
                        min_child_weight=0,
                        subsample =0.5
)
xgb_tune <-train(rating ~zip_code + occupation + item_imdb_rating_of_ten +item_mean_rating +
                   user_age_band_item_mean_rating + user_gender_item_mean_rating + demo_var + cluster +
                   timeSinceRelease + view + std.ratings + wr_imdb_staff + wr_imdb_rating_of_ten  +
                   weight + genre_sums + demo_var + wr_imdb_1000 ,
                 data=train,
                 method="xgbTree",
                 metric = "RMSE",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid,
                 
)

varImp(xgb_tune)

test$rating <- predict(xgb_tune, test)

test$user_item <- paste(test$user_id, test$item_id, sep="_")

test <- test[,34:35]

write_csv(test, "prediction_XG.csv")





