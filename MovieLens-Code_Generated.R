###SCRIPT Exploratory Data Analysis

#RMSE function:
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Install other needed libraries if not already present
if(!require(kableExtra)) install.packages("kableExtra")

# Loading other needed libraries
library(kableExtra)

#Split the edx dataset into edx_train and edx_test set.
set.seed(1)
test_index1 <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-test_index1,]
temp1 <- edx[test_index1,]

# Make sure userId and movieId in edx_test set are also in edx_train set
edx_test <- temp1 %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp1, edx_test)
edx_train <- rbind(edx_train, removed)

#Observe the edx dataset
str(edx)
#Check edx for missing values
sum(is.na(edx))
#Determine the number of different users and movies:
edx %>% summarise(Users = n_distinct(userId), Movies = n_distinct(movieId))

#Descriptive statistics of the edx dataset
edx %>% summarize(N_users = n_distinct(userId), N_movies = n_distinct(movieId),
                  Mean_rating = mean(rating), Median_rating = median(rating),
                  Min_rating = min(rating), Max_rating = max(rating)) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

#The first 6 rows of the edx dataset
head(edx) %>%
  kable() %>%
  kable_styling(latex_options = "scale_down",
                bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

#Observe the edx_train dataset
str(edx_train)
#Check edx_train for missing values
sum(is.na(edx_train))
#Determine the number of different users and movies:
edx_train %>% summarise(Users = n_distinct(userId), Movies = n_distinct(movieId))

#Observe the edx_test dataset
str(edx_test)
#Check edx_test for missing values
sum(is.na(edx_test))
#Determine the number of different users and movies:
edx_test %>% summarise(Users = n_distinct(userId), Movies = n_distinct(movieId))

#Observe the validation dataset
str(validation)
#Check edx for missing values
sum(is.na(validation))
#Determine the number of different users and movies:
validation %>% summarise(Users = n_distinct(userId), Movies = n_distinct(movieId))

##The first 6 rows of the validation dataset
head(validation) %>%
  kable() %>%
  kable_styling(latex_options = "scale_down",
                bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

#Descriptive statistics of the validation dataset
validation %>% summarize(N_users = n_distinct(userId), N_movies = n_distinct(movieId),
                         Mean_rating = mean(rating), Median_rating = median(rating),
                         Min_rating = min(rating), Max_rating = max(rating)) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)


#convert timestamp to a human readable date format
edx$date <- as.POSIXct(edx$timestamp, origin="1970-01-01")
edx_train$date <- as.POSIXct(edx_train$timestamp, origin="1970-01-01")
edx_test$date <- as.POSIXct(edx_test$timestamp, origin="1970-01-01")
validation$date <- as.POSIXct(validation$timestamp, origin="1970-01-01")

#Extract the month and year of the movie rating from the converted timestamp variable
edx$Rating_Year <- format(edx$date,"%Y")
edx$Rating_Month <- format(edx$date,"%m")
edx_train$Rating_Year <- format(edx_train$date,"%Y")
edx_train$Rating_Month <- format(edx_train$date,"%m")
edx_test$Rating_Year <- format(edx_test$date,"%Y")
edx_test$Rating_Month <- format(edx_test$date,"%m")
validation$Rating_Year <- format(validation$date,"%Y")
validation$Rating_Month <- format(validation$date,"%m")

#Extract the year of the movie release from the title variable
edx <- edx %>% mutate(year_release = as.numeric(str_sub(title,-5,-2)))
edx_train <- edx_train %>% mutate(year_release = as.numeric(str_sub(title,-5,-2)))
edx_test <- edx_test %>% mutate(year_release = as.numeric(str_sub(title,-5,-2)))
validation <- validation %>% mutate(year_release = as.numeric(str_sub(title,-5,-2)))

#Extract all individual genres of each movie in the edx_test
edx_test_split_genres <- edx_test %>% separate_rows(genres, sep = "\\|")

#remove unnecessary columns in all datasets
edx <- edx %>% select(userId, movieId, rating, title, genres, year_release, Rating_Year, Rating_Month)
edx_train <- edx_train %>%
  select(userId, movieId, rating, title, genres, year_release, Rating_Year, Rating_Month)
edx_test <- edx_test %>%
  select(userId, movieId, rating, title, genres, year_release, Rating_Year, Rating_Month)
validation <- validation %>%
  select(userId, movieId, rating, title, genres, year_release, Rating_Year, Rating_Month)
edx_test_split_genres <- edx_test_split_genres %>%
  select(userId, movieId, rating, title, genre = genres, year_release, Rating_Year, Rating_Month)

#convert columns with time-related variables into the desidered data type (from character to numeric)
edx$Rating_Year <- as.numeric(edx$Rating_Year)
edx$Rating_Month <- as.numeric(edx$Rating_Month)
edx_train$Rating_Year <- as.numeric(edx_train$Rating_Year)
edx_train$Rating_Month <- as.numeric(edx_train$Rating_Month)
edx_test$Rating_Year <- as.numeric(edx_test$Rating_Year)
edx_test$Rating_Month <- as.numeric(edx_test$Rating_Month)
validation$Rating_Year <- as.numeric(validation$Rating_Year)
validation$Rating_Month <- as.numeric(validation$Rating_Month)
edx_test_split_genres$Rating_Year <- as.numeric(edx_test_split_genres$Rating_Year)
edx_test_split_genres$Rating_Month <- as.numeric(edx_test_split_genres$Rating_Month)

#Show the head of edx
head(edx) %>%
  kable() %>%
  kable_styling(latex_options = "scale_down",
                bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

#Show the head of edx_test_split_genres
head(edx_test_split_genres) %>%
  kable() %>%
  kable_styling(latex_options = "scale_down",
                bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

#save datasets in the working directory subfolder rData (optional)
#saveRDS(edx, file = "rData/edx.Rda")
#saveRDS(edx_train, file = "rData/edx_train.Rda")
#saveRDS(edx_test, file = "rData/edx_test.Rda")
#saveRDS(validation, file = "rData/validation.Rda")
#saveRDS(edx_test_split_genres, file = "rData/edx_test_split_genres.Rda")

##DATA VISUALISATION

#Plot the rating distribution
edx %>% ggplot(aes(rating))+
  geom_bar(fill = "turquoise", color = "black")+
  labs(title = "Rating Distribution",
       x = "Rating",
       y = "Frequency")

#Plot the rating year distribution
edx %>% ggplot(aes(Rating_Year))+
  geom_bar(fill = "plum", color = "black")+
  labs(title = "Rating-Year Distribution",
       subtitle = "Years when movies were rated",
       x = "Year",
       y = "Frequency")

#Plot the rating month distribution
edx %>% ggplot(aes(factor(Rating_Month)))+
  geom_bar(fill = "plum1", color = "black")+
  labs(title = "Rating-Month Distribution",
       x = "Month",
       y = "Frequency")

#Plot the movie release year distribution
edx %>% ggplot(aes(year_release))+
  geom_bar(fill = "plum2", color = "black")+
  labs(title = "Release-Year Distribution",
       subtitle = "Years when movies were released",
       x = "Year",
       y = "Frequency")

#Plot the rating frequency distribution per movie
edx %>% ggplot(aes(movieId))+
  geom_bar(fill = "turquoise1", color = "turquoise1")+
  labs(title = "Rating Frequency per Movie",
       x = "MovieID",
       y = "Frequency")

#Confirm that there are no movieIds between 9500 and 25000
edx %>% filter(movieId > 9500 & movieId < 25000)

#Sort genres in edx_test_split_genre by number of ratings
genre_sorted <- edx_test_split_genres %>% group_by(genre) %>%
  summarise(count = n()) %>% arrange(desc(count))

#Plot the rating frequency distribution per genre (*edx_test_split_genres dataset!*)
genre_sorted %>% ggplot(aes(x = reorder(genre, -count), y = count))+
  geom_col(fill = "plum4", color = "black")+
  labs(title = "Rating Frequency per Genre",
       subtitle = "Dataset: edx_test_split_genres",
       x = "Genre",
       y = "Frequency")+
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Plot the genre popularity by rating year
genre_popularity <- edx_test_split_genres %>% 
  select(movieId, Rating_Year, genre) %>%
  mutate(genre = as.factor(genre)) %>%
  group_by(Rating_Year, genre) %>%
  summarise(number = n()) 
genre_popularity %>% filter(genre %in% c("Drama", "Comedy", "Action", "Thriller",
                                         "Western", "Film-Noir", "Documentary", "IMAX")) %>%
  mutate(genre = factor(genre, levels = c("Drama", "Comedy", "Action", "Thriller", #set levels to get the correct order in the legend
                                          "Western", "Film-Noir", "Documentary", "IMAX"))) %>%
  ggplot(aes(x = Rating_Year, y = number, color=genre)) +
  geom_line(aes())+
  labs(title = "Genre Popularity per Rating Year",
       subtitle = "Dataset: edx_test_split_genres",
       x = "Rating Year",
       y = "Number of Ratings",
       color = "Genre")

#Plot the histogram of rating distribution per users
edx %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 50, fill= "turquoise2", color = "black") + 
  scale_x_log10() +
  labs(title = "Rating Distribution per Users",
       x = "Number of Ratings",
       y = "Number of Users")

#15 Top Movies According To The Number of Ratings
top15_frequency <- edx %>% select(title, rating) %>%
  group_by(title) %>%
  summarize(number = n()) %>%
  arrange(desc(number)) %>%
  head(n = 15)
top15_frequency %>% ggplot(aes(reorder(title, -number), number))+
  geom_col(fill = "plum", color = "black")+
  geom_text(aes(title, number, label = number, angle = 90, hjust=1.06))+
  labs(title = "15 Top Movies by Rating Frequency",
       x = "Movie",
       y = "Number of Ratings")+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_x_discrete(labels= c("Pulp Fiction", "Forrest Gump", "Silence of the Lambs",
                             "Jurassic Park", "Shawshank Redemption", "Braveheart",
                             "Fugitive", "Terminator 2: Judgement Day",
                             "Star Wars: Episode IV", "Apollo 13", "Batman",
                             "Toy Story", "Independence Day",
                             "Dances with Wolves", "Schindler's List"))

#15 Top Movies According To The Mean Star Rating
top15_star <- edx %>% select(title, rating) %>%
  group_by(title) %>%
  summarize(number = n(), mean = round(mean(rating), digits = 2)) %>%
  filter(number > 10000) %>% #Only Movies with more than 10,000 ratings included.
  arrange(desc(mean)) %>%
  head(n = 15)

#Install the ggrepel package, if it is not already installed
#install.packages("ggrepel") #Install and plot ggrepel to simplify the plotting of text labels.
library(ggrepel)
top15_star %>% ggplot(aes(reorder(title,-mean), number))+
  geom_point(aes(size = mean, color = mean))+
  geom_text_repel(aes(title, number, label = mean), vjust=-1.3, cex=3)+ #show mean value of each point
  labs(title = "15 Top Movies According to Mean Star Rating",
       x = "Movie",
       y = "Number of Ratings")+
  theme(axis.text.x=element_text(angle=45, hjust=0.95))+
  scale_x_discrete(labels= c("Shawshank Redemption", "Godfather", "Usual Suspects",
                             "Schindler's List", "Casablanca", "Dr. Strangelove",
                             "Godfather: Part II", "One Flew Over the Cuckoo's Nest",
                             "Raiders of the Lost Ark", "Memento", "Star Wars: Episode IV",
                             "Monty Python and the Holy Grail", "Matrix",
                             "Princess Bride", "Silence of the Lambs"))+
  guides(color=guide_legend(), size = guide_legend())+   #to combine color and size into one legend
  scale_color_gradient(low = "turquoise4", high = "turquoise") #change default color scheme


### SCRIPT DATA ANALYSIS

#Calculate the sample mean (training set)
mu_hat <- mean(edx_train$rating)
#Show mu_hat in the text, rounded to 2 decimals
paste("The sample mean is:", round(mu_hat, digits=2))

#Predict Naive model RMSE
naive_rmse <- RMSE(edx_test$rating, mu_hat)
#Show naive_rmse in the text, rounded to 5 decimals
paste("The Naive Model RMSE evaluated on the edx_test dataset is:", round(naive_rmse, digits=5))

#Present the Naive model result in the rmse_results dataframe
rmse_results <- tibble(Model = "Naive Model", RMSE = round(naive_rmse, digits=5))
rmse_results %>%   kable(format = "pandoc", caption = "Naive Model RMSE") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)


# Calculate the average b_i per movie
movie_avgs <- edx_train %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu_hat))

# Compute the predicted ratings of Movie Effect Model on edx_test set
movie_pred <- edx_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(pred = mu_hat + b_i) %>%
  .$pred

#Predict Movie Effect model RMSE
movie_rmse <- RMSE(edx_test$rating, movie_pred)
#Show movie_rmse in the text, rounded to 5 decimals
paste("The Movie Effect Model RMSE evaluated on the edx_test dataset is:", round(movie_rmse, digits=5))

#Present the Movie Effect model result in the rmse_results dataframe
rmse_results <- rmse_results %>% add_row(Model = "Movie Effect Model",
                                         RMSE = round(movie_rmse, digits=5))


#Calculate the average b_u per user
user_avgs <- edx_train %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))

# Compute the predicted ratings of Movie+User Effect Model on edx_test set
movie_user_pred <- edx_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  .$pred

#Predict Movie+User Effect model RMSE
movie_user_rmse <- RMSE(edx_test$rating, movie_user_pred)
#Show movie_user_rmse in the text, rounded to 5 decimals
paste("The Movie and User Effect Model RMSE evaluated on the edx_test dataset is:",
      round(movie_user_rmse, digits=5))

#Present the Movie+User Effect model result in the rmse_results dataframe
rmse_results <- rmse_results %>% add_row(Model = "Movie+User Effect Model",
                                         RMSE = round(movie_user_rmse, digits=5))



#Create a database that connects movieId to movie title:
movie_titles <- edx_train %>%
  select(movieId, title) %>%
  distinct()
#Determine the best 10 movies according to the estimated b_i
table_10best <- movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>%
  select(title, b_i) %>%
  slice(1:10) %>%
  rename("Movie Title" = title)
table_10best %>% kable(format = "pandoc", caption = "Ten Best Movies by b_i") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

#Determine the worst 10 movies according to the estimated b_i
table_10worst <- movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>%
  select(title, b_i) %>%
  slice(1:10) %>%
  rename("Movie Title" = title)
table_10worst %>% kable(format = "pandoc", caption = "Ten Worst Movies by b_i") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

#Number of ratings for the best 10 movies by b_i
table_10best_rat <- edx_train %>% count(movieId) %>%
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>%
  select(title, b_i, n) %>%
  slice(1:10) %>%
  rename("Movie Title" = title)
table_10best_rat %>% kable(format = "pandoc", caption = "Number of Ratings for Ten Best Movies by b_i") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

#Number of ratings for the worst 10 movies by b_i
table_10worst_rat <- edx_train %>% count(movieId) %>%
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>%
  select(title, b_i, n) %>%
  slice(1:10) %>%
  rename("Movie Title" = title)
table_10worst_rat %>% kable(format = "pandoc", caption = "Number of Ratings for Ten Worst Movies by b_i") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)


##Define a sequence of lambdas
lambdas <- seq(0, 10, 0.25)

#Compute the predicted ratings on edx_test dataset using different values of lambda
rmses <- sapply(lambdas, function(lambda) {
  #Calculate the average by user
  b_i <- edx_train %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_hat) / (n() + lambda))
  
  #Compute the predicted ratings on the edx_test dataset
  predicted_ratings <- edx_test %>%
    left_join(b_i, by='movieId') %>%
    mutate(pred = mu_hat + b_i) %>%
    .$pred
  
  #Predict the RMSE on the edx_test dataset
  return(RMSE(edx_test$rating, predicted_ratings))
})

#Plot the rmses vs lambdas
tibble(RMSE = rmses, lambdas = lambdas) %>%
  ggplot(aes(lambdas, rmses)) +
  theme_minimal()+
  geom_point(color = "plum") +
  labs(title = "RMSE vs Lambda - Regularized Movie Effect Model",
       y = "RMSE",
       x = "Lambda")

#Extract the optimal lambda that minimizes RMSE the most
min_lambda <- lambdas[which.min(rmses)]

#Show min_lambda in the text
paste("The optimal lambda that minimizes RMSE the most is:",
      min_lambda)

#Predict Regularized Movie Effect model RMSE
regul_movie_rmse <- mean(rmses)
#Show regul_movie_rmse in the text, rounded to 5 decimals
paste("The Regularized Movie Effect Model RMSE evaluated on the edx_test dataset is:",
      round(regul_movie_rmse, digits=5))

#Add the Regularized Movie Effect model result to the rmse_results dataframe
rmse_results <- rmse_results %>% add_row(Model = "Regularized Movie Effect Model",
                                         RMSE = round(regul_movie_rmse, digits=5))


#Calculate the average b_i per movie in the Regularized Movie Effect model
movie_reg_avgs <- edx_train %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu_hat) / (n() + min_lambda), n_i = n())

#Determine the best 10 movies and their rating frequency
#according to the Regularized Movie Effect model
table_10best_regul <- edx_train %>% count(movieId) %>%
  left_join(movie_reg_avgs, by="movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>%
  select(title, b_i, n) %>%
  slice(1:10) %>%
  rename("Movie Title" = title)
table_10best_regul %>% kable(format = "pandoc", caption = "Ten Best Movies by b_i (Regularized Movie Effect Model)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

#Determine the worst 10 movies and their rating frequency
#according to the Regularized Movie Effect model
table_10worst_regul <- edx_train %>% count(movieId) %>%
  left_join(movie_reg_avgs, by="movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>%
  select(title, b_i, n) %>%
  slice(1:10) %>%
  rename("Movie Title" = title)
table_10worst_regul %>% kable(format = "pandoc", caption = "Ten Worst Movies by b_i (Regularized Movie Effect Model)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)


##Define a sequence of lambdas for the Regul. Movie+User Effect model
lambdas_1 <- seq(0, 10, 0.5)

#Compute the predicted ratings on edx_test dataset using different values of lambda
rmses_1 <- sapply(lambdas_1, function(lambda) {
  #Calculate the regularized estimate of b_i
  b_i <- edx_train %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_hat) / (n() + lambda))
  
  #Calculate the regularized estimate of b_u
  b_u <- edx_train %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu_hat) / (n() + lambda))
  
  #Compute the predicted ratings on the edx_test dataset
  predicted_ratings <- edx_test %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    mutate(pred = mu_hat + b_i + b_u) %>%
    .$pred
  
  #Predict the RMSE on the edx_test dataset
  return(RMSE(edx_test$rating, predicted_ratings))
})

#Plot the rmses_1 vs lambdas
tibble(RMSE = rmses_1, lambdas = lambdas_1) %>%
  ggplot(aes(lambdas_1, rmses_1)) +
  theme_minimal()+
  geom_point(color = "turquoise") +
  labs(title = "RMSE vs Lambda - Regularized Movie+User Effect Model",
       y = "RMSE",
       x = "Lambda")

#Extract the optimal lambda_1 that minimizes RMSE the most
min_lambda_1 <- lambdas_1[which.min(rmses_1)]

#Show min_lambda_1 in the text
paste(min_lambda_1)

#Predict Regularized Movie and User Effect model RMSE
regul_movie_user_rmse <- mean(rmses_1)

#Add the Regularized Movie+User Effect model result to the rmse_results dataframe
rmse_results <- rmse_results %>% add_row(Model = "Regularized Movie+User Effect Model",
                                         RMSE = round(regul_movie_user_rmse, digits=5))

#Show regul_movie_user_rmse in the text, rounded to 5 decimals
paste(round(regul_movie_user_rmse, digits=5))

##Define a sequence of lambdas for the Reg. Mov+User+Gen+Time Effect model
lambdas_2 <- seq(2, 6, 0.5)

#Compute the predicted ratings on edx_test dataset using different values of lambda
rmses_2 <- sapply(lambdas_2, function(lambda) {
  #Calculate the regularized estimate of b_i
  b_i <- edx_train %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_hat) / (n() + lambda))
  
  #Calculate the regularized estimate of b_u
  b_u <- edx_train %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu_hat) / (n() + lambda))
  
  #Calculate the regularized estimate of b_g
  b_g <- edx_train %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - b_u - mu_hat) / (n() + lambda))
  
  #Calculate the regularized estimate of b_t
  b_t <- edx_train %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    group_by(year_release) %>%
    summarize(b_t = sum(rating - b_i - b_u - b_g - mu_hat) / (n() + lambda))
  
  #Compute the predicted ratings on the edx_test dataset
  predicted_ratings <- edx_test %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_g, by="genres") %>%
    left_join(b_t, by="year_release") %>%
    mutate(pred = mu_hat + b_i + b_u + b_g + b_t) %>%
    .$pred
  
  #Predict the RMSE on the edx_test dataset
  return(RMSE(edx_test$rating, predicted_ratings))
})

#Plot the rmses_2 vs lambdas_2
tibble(RMSE = rmses_2, lambdas = lambdas_2) %>%
  ggplot(aes(lambdas_2, rmses_2)) +
  theme_minimal()+
  geom_point(color = "plum") +
  labs(title = "RMSE vs Lambda - Regul. Movie+User+Genre+Time Effect Model",
       y = "RMSE",
       x = "Lambda")

#Extract the optimal lambda_2 that minimizes RMSE the most
min_lambda_2 <- lambdas_2[which.min(rmses_2)]

#Show min_lambda_2 in the text
paste(min_lambda_2)

#Predict Regularized Movie+User+Genre+Time Effect model RMSE
regul_movie_user_genre_time_rmse <- mean(rmses_2)

#Show regul_movie_user_genre_time_rmse in the text, rounded to 5 decimals
paste(round(regul_movie_user_genre_time_rmse, digits=5))

#Add the Regularized Movie+User+Genre+Time Effect model result to the rmse_results dataframe
rmse_results <- rmse_results %>%
  add_row(Model = "Regularized Movie+User+Genre+Time Effect Model",
          RMSE = round(regul_movie_user_genre_time_rmse, digits=5))
rmse_results %>%   kable(format = "pandoc", caption = "RMSEs of All Models") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

#CHECK THE FINAL MODEL RMSE ON THE VALIDATION DATASET

#The lambda to be used for final validation was determined by our selected model
lambda_val <- min_lambda_2

#Compute the predicted ratings on validation dataset using the determined optimal lambda
rmses_val <- sapply(lambda_val, function(lambda) {
  
  #Mean rating in the edx dataset
  mu <- mean(edx$rating)
  
  #Calculate the regularized estimate of b_i
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu) / (n() + lambda))
  
  #Calculate the regularized estimate of b_u
  b_u <- edx %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu) / (n() + lambda))
  
  #Calculate the regularized estimate of b_g
  b_g <- edx %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - b_u - mu) / (n() + lambda))
  
  #Calculate the regularized estimate of b_t
  b_t <- edx %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    group_by(year_release) %>%
    summarize(b_t = sum(rating - b_i - b_u - b_g - mu) / (n() + lambda))
  
  #Compute the predicted ratings on the validation dataset
  predicted_ratings <- validation %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_g, by="genres") %>%
    left_join(b_t, by="year_release") %>%
    mutate(pred = mu + b_i + b_u + b_g + b_t) %>%
    .$pred  
  
  #Predict the RMSE on the validation dataset
  return(RMSE(validation$rating, predicted_ratings))
})

#Show the final model RMSE in a table
tibble("Final Model" = "Regularized Movie+User+Genre+Time Effect Model",
       "RMSE on Validation Set" = round(rmses_val, digits=5)) %>%
  kable(format = "pandoc", caption = "Final Model RMSE on Validation Dataset") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)