#####################################
#####Load data and transform data####
#####################################

movies <- read.csv("join_movie_df.csv")

#Drop X column
drops <- c("X","Unnamed..0")
movies<-movies[ , !(names(movies) %in% drops)]

#Remove those rows that do not include a metacritic score
movies<- subset(movies, !is.na(movies$meta_score))

#create variable for score difference
movies$score_diff<- abs(movies$meta_score - (movies$mov_imdb_rating*10))

#remove duplicates
movies <- movies[!duplicated(movies$mov_name),] 

#remove any remaining NAs
movies<-na.omit(movies) 

#####################################
##### Linear Regressions ############
#####################################

#GLM Regression
#Get x variables remove some categorical variables like directors
x_movies <- movies[,c(1:26,28,30:31,33:38)]

#Get the y variable
y_movies <- movies$score_diff

# Run GLM Regression
#Remove variables that will have high levels of multicollinearity 
reg1 <- glm(y_movies ~ ., data=x_movies[,-c(25:27,29,31)])

#Try the regression with all variables
reg1 <- glm(y_movies ~ ., data=x_movies)
names(x_movies)

#Use a cross validated LASSO regression to get the optimal variable selection
library(gamlr)
sx_movies <- sparse.model.matrix(y_movies ~ ., data=x_movies[,-c(25:27,29,31)])[,-1] # do -1 to drop intercept!
cv_mov_lasso <- cv.gamlr(sx_movies, y_movies)

#Plot results
plot(cv_mov_lasso, bty="n")
coef(cv_mov_lasso)[-1,]
