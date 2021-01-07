devtools::load_all()
library(fasttime)
library(Matrix)

USER = "tobias"

# read .fst
votes01 <- read_fst_data(user = USER, data_name = "Votes_01")
votes16 <- read_fst_data(user = USER, data_name = "Votes_16")
posts01 <- read_fst_data(user = USER, data_name = "Postings_01")
posts16 <- read_fst_data(user = USER, data_name = "Postings_16")
votes <- rbind(votes01, votes16)
posts <- rbind(posts01, posts16)

# combine data
# 1. Merge and Rename
# 2. New IDs!
# 3. Proper Date columns
df <- create_user_vote_user_data(df_votes = votes, df_posts = posts)

## Prepare Data

### insights all data
cat_("Dimension = ", collapse_(c("Rows:", "Cols:"), dim(df)))
for (i in grep_get("ID_", colnames(df)))
  cat_("Unique ", i, ": ", length(unique(df[[i]])))
head(df)

### Create Sparse Matrices at each timepoint
# x ... row
# y ... column
# takes around 3 seconds
df_sub <- df[ArticleChannel == "International"]
adj_mat <- create_adj_mat_time(df = df_sub,
                               time_var = "VoteCreated",
                               x = "ID_VoteUser",
                               y = "ID_PostUser")

### Insights time Points
y_n <- sapply(adj_mat, sum)
time_points <- sort(unique(df_sub[["VoteCreated"]]))
# simple plot function
plot_votes_per_day <- function(x, y) {
  # x ... time-point vector, y ... value vector
  plot(x, y, main = "Votes Per Day", type = "l", xlab = "date", ylab = "count votes")
  points(x, y, pch = 19, cex = 1, col = "white")
  points(x, y, pch = 19, cex = .4)
}
plot_votes_per_day(time_points, y_n)


###
# store results by supplying a path
# letztere Matrizen werden 5-10gb groß
# for international takes ~20s / time point -> 5 minutes for 20 days
path_temp <- paste0(get_path(USER), "adj_mat_inv.rds")
#B <- get_alphas(A = adj_mat, a = .3, path = path_temp)
B <- readRDS(path_temp)
DCM <- dcm_simple(B = B)
# check memory
gc()

### Try 2
path_temp <- paste0(get_path(USER), "adj_mat_inv.rds")
#B <- get_alphas(A = adj_mat, a = .3, path = path_temp)
B <- readRDS(path_temp)

get_dcm_k <- function(B, k, path = NULL) {
  Q <- B[[1]]
  if (k > 1) {
    for (i in 2:k) {
      begin <- Sys.time()
      Q <- Q %*% B[[i]]
      cat("\rDCM ", i-1, "/", k, " done in ",
          round(Sys.time() - begin, 1), ".")
    }
  }
  return(Q)
}

system.time(Q_k <- get_dcm_k(B, 7))

# top international
par(mfrow = c(4, 1), mar = c(1, 2, 3, 2))
for (i in 1:4) plot_ranks(user = "tobias", channel = "Lifestyle", size = "small", time_point = i)
