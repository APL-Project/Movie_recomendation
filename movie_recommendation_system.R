#load necessary packages for recommendation system
library(recommenderlab)       #for recommendation
library(ggplot2)              #for visualization
library(data.table)
library(reshape2)

#retrieving the data from data-set
setwd("C:/Users/Jemish Mangukiya/Desktop/APL_project")
movie_data = read.csv("movies.csv", stringsAsFactors = FALSE)
rating_data = read.csv("ratings.csv")
str(movie_data)
str(rating_data)

#summary of movies and rating data set
summary(movie_data)
summary(rating_data)

#view of data set 
View(movie_data)
View(rating_data)

#convert genres into more usable format
movie_genre = as.data.frame(movie_data$genres, stringsAsFactors=FALSE)
movie_genre1 = as.data.frame(tstrsplit(movie_genre[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE) 

colnames(movie_genre1) = c(1:10)
list_genre = c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")
genre_mat = matrix(0,10330,18)
genre_mat[1,] = list_genre
colnames(genre_mat) = list_genre
for (index in 1:nrow(movie_genre1)) {
  for (col in 1:ncol(movie_genre1)) {
    gen_col = which(genre_mat[1,] == movie_genre1[index,col]) 
    genre_mat[index+1,gen_col] <- 1
  }
}
genre_mat1 = as.data.frame(genre_mat[-1,], stringsAsFactors=FALSE)  #remove first row which was the genre list
for (col in 1:ncol(genre_mat1)) {
  genre_mat1[,col] = as.integer(genre_mat1[,col])                   #convert from char to int
} 
str(genre_mat1)

#create a search matrix
searchMatrix = cbind(movie_data[,1:2], genre_mat1)
head(searchMatrix)

#many movies have several genre
#let's create a sparse matrix for recommendation
ratingMatrix = dcast(rating_data, userId~movieId, value.var = "rating", na.rm = FALSE)
ratingMatrix = as.matrix(ratingMatrix[,-1])       #remove userId

#convert rating matrix into recommenderlab sparse matrix
ratingMatrix = as(ratingMatrix, "realRatingMatrix")
ratingMatrix

#recommendation model
recommendation_model = recommenderRegistry$get_entries(datatype = "realRatingMatrix")
names(recommendation_model)

lapply(recommendation_model, "[[", "description")

#collaborative Filtering
recommendation_model$IBCF_realRatingMatrix$parameters

#Exploring similar data
similarity_matrix = similarity(ratingMatrix[1:4, ], method = "cosine", which = "users")
as.matrix(similarity_matrix)
image(as.matrix(similarity_matrix), main = "User's similarities")

#Exploring similar movies
similar_movies = similarity(ratingMatrix[, 1:4], method = "cosine", which = "items")
as.matrix(similar_movies)
image(as.matrix(similar_movies), main = "Movies similarities")

#rating values
rating_values = as.vector(ratingMatrix@data)
unique(rating_values)       #extracting unique ratings

#creating a count of movie ratings
table_of_ratings = table(rating_values)
table_of_ratings

#most viewed movies visualization
movie_views = colCounts(ratingMatrix)       #count views for each movie
table_views = data.frame(movie = names(movie_views), views = movie_views)       #create data frame of views
table_views = table_views[order(table_views$views, decreasing = TRUE), ]        #sort by number of views
table_views$title = NA
for (index in 1:10325) {
  table_views[index,3] = as.character(subset(movie_data, movie_data$movieId == table_views[index,1])$title)
}
table_views[1:6,]

#visualize a bar plot of total number of views of top films
ggplot(table_views[1:6, ], aes(x = title, y = views)) + 
  geom_bar(stat = "identity", fill = 'steelblue') +
  geom_text(aes(label=views), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  ggtitle("Total Views of the Top Films")

#heatmap of movie ratings
image(ratingMatrix[1:20, 1:25], axes = FALSE, main = "Heatmap of first 25 rows and 25 columns")

#performing data preparation
#now
#1.Selecting useful data
#2.normalizing data
#3.Binarizing the data
movie_ratings = ratingMatrix[rowCounts(ratingMatrix) > 50, colCounts(ratingMatrix) > 50]
movie_ratings

minimum_movies = quantile(rowCounts(movie_ratings), 0.98)
minimum_users = quantile(colCounts(movie_ratings), 0.98)
image(movie_ratings[rowCounts(movie_ratings) > minimum_movies,
                    colCounts(movie_ratings) > minimum_users],
      main = "Heatmap of the top users and movies")

#visualize the distribution of the average ratings per user.
average_ratings = rowMeans(movie_ratings)
qplot(average_ratings, fill=I("steelblue"), col=I("red")) +
  ggtitle("Distribution of the average rating per user")


