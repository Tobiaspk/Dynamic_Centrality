devtools::load_all()
library(fasttime)
library(Matrix)

## GLOBAL
data_names <- c("Votes", "Postings", "Following")

# store data as .fst
read_csv_data(user = "tobias",
              data_names = data_names,
              save_as_fst = TRUE,
              return_df = FALSE)

# read .fst
votes <- read_fst_data(user = "tobias", data_name = "Votes")
posts <- read_fst_data(user = "tobias", data_name = "Postings")

# combine data
df <- create_user_vote_user_data(df_votes = votes, df_posts = posts)

## Prepare Data
# map user ids from random ids (199, 30, 5000000,...) to (1, 2, 3, 4...)
# date columns to actual dates
users_uq <- unique(c(df$ID_PostUser, df$ID_VoteUser))
df1 <- df[, .(ID_Posting = map_id(ID_Posting),
              ID_PostUser = map_id(ID_PostUser, levels = users_uq),
              ID_VoteUser = map_id(ID_VoteUser, levels = users_uq),
              ID_Article = map_id(ID_Article),
              ArticleChannel = ArticleChannel,
              VoteCreated = fastdate(VoteCreatedAt),
              PostingCreated = fastdate(PostingCreatedAt),
              ArticleCreated = fastdate(ArticlePublishingDate))]

### insights all data
cat_("Dimension = ", collapse_(c("Rows:", "Cols:"), dim(df1)))
for (i in grep_get("ID_", colnames(df1)))
  cat_("Unique ", i, ": ", length(unique(df1[[i]])))
head(df1)

### Create Sparse Matrices at each timepoint
adj_mat <- create_adj_mat_time(df = df1,
                               time_var = "VoteCreated",
                               x = "ID_VoteUser",
                               y = "ID_PostUser")

### Insights time Points
y_n <- sapply(adj_mat, sum)
plot(time_points, y_n, main = "Votes Per Day", type = "l")
points(time_points, y_n, pch = 19, cex = 1, col = "white")
points(time_points, y_n, pch = 19, cex = .4)

###
path_temp <- paste0(get_path("tobias"), "adj_mat_inv.rds")
B <- store_alpha(A = adj_mat, a = .3, path = path_temp)
DCM <- dcm_simple(B = B)


