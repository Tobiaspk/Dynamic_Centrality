# for comments check testcodes/Tobias_Try.R
devtools::load_all()
library(fasttime)
library(Matrix)

USER = "tobias"

# get relevant data
votes <- read_fst_data(user = USER, data_name = "Votes")
posts <- read_fst_data(user = USER, data_name = "Postings")
df <- create_user_vote_user_data(df_votes = votes, df_posts = posts)

# get adjecency matrix of subset
df_sub <- df[ArticleChannel == "Lifestyle"][VoteCreatedWeek %in% c("2019-17", "2019-18", "2019-19")]
adj_mat_time <- create_adj_mat_time(df = df_sub,
                               time_var = "VoteCreatedWeek",
                               x = "ID_VoteUser",
                               y = "ID_PostUser")
adj_mat <- adj_mat_time$matrix
mapping <- adj_mat_time$mapping

# quick insight
plot_adj_matrix_agg(A = adj_mat, time_axis = "VoteCreateWeek")

# get alphas
path_temp <- paste0(get_path(USER), "adj_mat_inv.rds") # set path
B <- get_alphas(A = adj_mat, a = .3, path = path_temp) # calculate and store alphas to path (only once!)
B <- readRDS(path_temp) # read from path

# get dcm
DCM <- dcm_simple(B = B)


