
# System I ----------------------------------------------------------------
data <- read.csv("Rmat.csv")
movie_names <- names(data)

movie_genre_reference <- data.frame(
  movie_name = movie_names,
  genre = sample(c("Action", "Comedy", "Drama", "Sci-Fi", "Thriller"), length(movie_names), replace = TRUE)
)

# Recommendation function
recommend_movies <- function(data, movie_genre_reference, favorite_genre) {
  movies_in_genre <- movie_genre_reference[movie_genre_reference$genre == favorite_genre, "movie_name"]
  movies_ratings <- data[movies_in_genre]
  movie_popularity <- colSums(!is.na(movies_ratings))
  top_5_popular_movies <- names(sort(movie_popularity, decreasing = TRUE))[1:5]
  
  return(top_5_popular_movies)
}

# Example usage:
favorite_genre <- "Action"
recommended_movies <- recommend_movies(data, movie_genre_reference, favorite_genre)

print(recommended_movies)



# System II ---------------------------------------------------------------

ratings = data

# 1. Normalize the rating matrix by centering each row
ratings_norm <- t(apply(ratings, 1, function(x) {
  mean_val <- mean(x, na.rm = TRUE)
  x[!is.na(x)] <- x[!is.na(x)] - mean_val
  return(x)
}))

# 2. Compute Cosine similarity among the 3,706 movies
library(lsa)
item_sim <- cosine(as.matrix(ratings_norm))

# 3. For each row, sort non-NA similarity measures and keep the top 30, setting the rest to NA
top_k <- function(x, k) {
  x[order(x, decreasing = TRUE)][(k + 1):length(x)] <- NA
  return(x)
}

item_sim_top30 <- t(apply(item_sim, 1, top_k, k = 30))

# Save the similarity matrix online
library(gh)
write.csv(top_similarities, file = "top30.csv")
local_file_path <- "top30.csv"
github_file_path <- "https://github.com/lidiayl/psl-project4/top30.csv"  # Replace with your desired folder and file name
repo <- gh::gh("lidiayl/psl-project4") 
gh::gh_auth()
gh::gh_push(repo, file = local_file_path, path = github_file_path, message = "Updated file")

# 4. Create myIBCF function
myIBCF <- function(newuser, item_sim_top30) {
  # Compute predictions for movies not rated by the new user
  pred <- rep(NA, length(newuser))
  
  for (l in which(newuser == 0)) {
    S_l <- which(!is.na(item_sim_top30[l, ]))
    pred[l] <- 1 / sum(item_sim_top30[l, S_l] * !is.na(newuser[S_l])) * 
      sum(item_sim_top30[l, S_l] * newuser[S_l])
  }
  
  # Sort predictions in decreasing order
  pred_sorted <- sort(pred, decreasing = TRUE)
  
  # Get the top 10 movie recommendations
  rec <- names(pred_sorted)[1:min(10, sum(!is.na(pred)))]
  
  # If fewer than 10 predictions are non-NA, suggest movies not rated by the user
  if (length(rec) < 10) {
    unrated_movies <- colnames(ratings)[newuser == 0]
    additional_movies <- sample(unrated_movies, 10 - length(rec))
    rec <- c(rec, additional_movies)
  }
  
  # Return the recommendations
  return(rec)
}

# 5. Testing

# Function to display similarity values for specific movies
display_similarity_for_movies <- function(similarity_matrix, movie_names, movies_to_display) {
  movie_indices <- match(movies_to_display, movie_names)
  similarity_values <- round(similarity_matrix[movie_indices, ], 7)
  rownames(similarity_values) <- movies_to_display
  colnames(similarity_values) <- colnames(similarity_matrix)
  print(similarity_values)
}

# Display similarity matrix for specified movies
movies_to_display <- c("m1", "m10", "m100", "m1510", "m260", "m3212")
#movie_indices <- match(movies_to_display, colnames(normalized_data)[-1])
movie_indices <- match(movies_to_display, colnames(ratings_norm))
similarity_values <- item_sim[movie_indices, movie_indices]
round(similarity_values, 7)

# Read the csv file from GitHub
top30 <- read.csv("https://github.com/lidiayl/psl-project4/top30.csv", row.names = 1)


# User "u1181" from rating matrix R
u1181_recommended = myIBCF(as.numeric(ratings["u1181", ], top30))

# User "u1351" from rating matrix R
u1351_recommended = myIBCF(as.numeric(ratings["u1351", ], top30))

# A hypothetical user who rates movie "m1613" with 5 and movie "m1755" with 4
newuser <- rep(0, ncol(ratings))
newuser[c("m1613", "m1755")] <- c(5, 4)
hypo_recommended = myIBCF(newuser, top30)
