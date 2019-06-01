library(data.table)
library(dplyr)
library(Matrix)
library(recommenderlab)
library(tidyr)

# set your own working directory

# read in data. oringial ratings dataset is about 6 million obs.
# it's too big for the shiny app, therefore, we saved a rating-sample.csv file with about 30k obs

books <- fread('books.csv')
ratings <- fread('ratings-sample.csv')


#subset ratings dataset once more due to shiny App web deloyment has limit memory space.
set.seed(1)
user_fraction <- 0.4
users <- unique(ratings$user_id)
sample_users <- sample(users, round(user_fraction * length(users)))

ratings <- filter(ratings, user_id %in% sample_users)

new_user_id <- max(ratings$user_id) + 1


#creat a dimension_names list
dimension_names <- list(user_id = sort(unique(ratings$user_id)), book_id = sort(unique(ratings$book_id)))

#Spread a key-value pair across multiple columns.spread() takes two columns (key & value),
#and spreads into multiple columns: it makes “long” data wider.(will take several minutes 
#due to the large dataset)
ratingmat <- spread(select(ratings, book_id, user_id, rating), book_id, rating) %>%
  select(-user_id)

#Create dataset matrix with rows as user_id and columns as book_id
ratingmat = as.matrix(ratingmat)

#set the dimnames of matrix
dimnames(ratingmat) <- dimension_names

# replacing null values dataset with 0(will take several minutes)
ratingmat[is.na(ratingmat)] <- 0

#sparseMatrix
sparse_ratings <- as(ratingmat, "sparseMatrix") 

#Convert ratings matrix into real matrix which makes it dense.
real_ratings <- new("realRatingMatrix", data=sparse_ratings)

#Create Recommender Model. The parameters are UBCF and "pearson" similarity. We take 10 nearest neighbours
rec_mod <- Recommender(real_ratings, method = "UBCF", param=list(method="pearson",nn=10)) 

# due to only used part of ratings dataset, 
# so need to make sure books that we give to user to rating are in the ratings dataset
sample_books = filter(books, book_id %in% dimension_names$book_id)

# define functions to be called in server.R
getSampleBooks <- function(num) {
  sample_n(sample_books, num) # only get books from those in the sampled ratings
}

getRecommendations <- function(user_ratings) {
  # build a 1x10000 matrix (1 user X 10000 book ) with given ratings
  user_sparse_mat = sparseMatrix(
    i = rep(1, length(user_ratings$book_id)),
    j = user_ratings$book_id,
    x = user_ratings$rating,
    dims = c(1, length(dimension_names$book_id))
  )
  dimnames(user_sparse_mat) = list(user_id = c(new_user_id), book_id = dimension_names$book_id)
  user_real_ratings = new("realRatingMatrix", data = user_sparse_mat)

  Top_5_pred <- predict(rec_mod, user_real_ratings, type = "ratings")
  Top_5_df <- as(Top_5_pred, "data.frame")
  result <- arrange(Top_5_df, -Top_5_df$rating)
  result <- head(result, 5)
  filter(books, book_id %in% result$item)
}
