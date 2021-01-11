devtools::load_all()
library(fasttime)
library(Matrix)
library(igraph)

USER = "tine"

# get relevant data
votes <- read_fst_data(user = USER, data_name = "Votes")
posts <- read_fst_data(user = USER, data_name = "Postings")
df <- create_user_vote_user_data(df_votes = votes, df_posts = posts)

# get adjacency matrix of subset
channels <- unique(df$ArticleChannel)

nvotes_eachweek <- df[, .N, keyby = .(ArticleChannel, VoteCreatedWeek)]

problematic_channels <- character()

for (i in 1:length(channels)){
  cat(channels[i], "\n")
  df_sub <- df[ArticleChannel == channels[i]][VoteCreatedWeek %in% c("2019-17", "2019-18", "2019-19", "2019-20", "2019-21")]
  adj_mat_time <- create_adj_mat_time(df = df_sub,
                                      time_var = "VoteCreatedWeek",
                                      x = "ID_VoteUser",
                                      y = "ID_PostUser")

  adj_mat <- adj_mat_time$matrix
  mapping <- adj_mat_time$mapping

  upperbounda <- get_upperbound_a(adj_mat)
  cat("upper bound:", upperbounda, "\n")

  if(upperbounda < 0.015){
    problematic_channels <- c(problematic_channels, channels[i])
    next
  }

  adj_mat_as_graphs<- lapply(adj_mat, graph.adjacency)
  degree_cent <- lapply(adj_mat_as_graphs, centr_degree)
  userrank_degree <- get_userrank_centrality(degree_cent, mapping)
  saveRDS(userrank_degree, file = paste0(get_path(USER), "userrank_", channels[i], "_degree.rds"))
  close_cent <- lapply(adj_mat_as_graphs, centr_clo)
  userrank_close <- get_userrank_centrality(close_cent, mapping)
  saveRDS(userrank_close, file = paste0(get_path(USER), "userrank_", channels[i], "_close.rds"))
  eigen_cent <- lapply(adj_mat_as_graphs, centr_eigen)
  userrank_eigen <- get_userrank_centrality(eigen_cent, mapping, var = "vector")
  saveRDS(userrank_eigen, file = paste0(get_path(USER), "userrank_", channels[i], "_eigen.rds"))

  # get alphas
  path_temp <- paste0(get_path(USER), "adj_mat_inv_", channels[i], ".rds") # set path

  if(file.exists(path_temp)){
    B <- readRDS(path_temp) # read from path
  } else {
    B <- get_alphas(A = adj_mat, a = round(upperbounda, 3)-0.001, path = path_temp) # calculate and store alphas to path (only once!)
  }
  dcm <- dcm_simple(B)

  rdcm_bigb <- rdcm_simple(B = B, b = 2, dt = rep(1,length(B)))
  if(any(sapply(rdcm_bigb, min) < 0)){
    stop("negative values in RDCM!")
  }
  rdcm_smallb <- rdcm_simple(B = B, b = .5, dt = rep(1,length(B)))
  if(any(sapply(rdcm_smallb, min) < 0)){
    stop("negative values in RDCM!")
  }

  ## user "importance" ranking (average was used when ties occur)
  ## based on broadcast communicabilities
  userrank_bigb <- get_user_rank(rdcm_bigb, mappings = mapping)
  saveRDS(userrank_bigb, file = paste0(get_path(USER), "userrank_", channels[i], "_bigb.rds"))
  userrank_smallb <- get_user_rank(rdcm_smallb, mappings = mapping)
  saveRDS(userrank_smallb, file = paste0(get_path(USER), "userrank_", channels[i], "_smallb.rds"))
  userrank_dcm <- get_user_rank(dcm, mappings = mapping)
  saveRDS(userrank_dcm, file = paste0(get_path(USER), "userrank_", channels[i], "_dcm.rds"))
}


#### problematic_channels
problematic_channels <- c("Inland", "Meinung", "Wirtschaft", "Etat", "International", "Panorama",
                          "Web", "Kultur", "Sport")
df[ArticleChannel %in% problematic_channels & VoteCreatedWeek %in% c("2019-17", "2019-18", "2019-19", "2019-20", "2019-21"),
   VoteCreatedDay := yday(VoteCreated)]

problematic_channels_new <- character()

for (i in 1:length(problematic_channels)){
  cat(problematic_channels[i], "\n")
  #df[ArticleChannel %in% problematic_channels[i] & VoteCreatedWeek %in% c("2019-17", "2019-18"),
  #   VoteCreatedDay_category := as.numeric(cut(VoteCreatedDay, breaks = 5))]
  df_sub <- df[ArticleChannel == problematic_channels[i]][VoteCreatedDay %in% 128:132]
  adj_mat_time <- create_adj_mat_time(df = df_sub,
                                      time_var = "VoteCreatedDay",
                                      x = "ID_VoteUser",
                                      y = "ID_PostUser")

  adj_mat <- adj_mat_time$matrix
  mapping <- adj_mat_time$mapping



  upperbounda <- get_upperbound_a(adj_mat)
  cat("upper bound:", upperbounda, "\n")

  if(upperbounda < 0.015){
    problematic_channels_new <- c(problematic_channels_new, problematic_channels[i])
    next
  }

  adj_mat_as_graphs<- lapply(adj_mat, graph.adjacency)
  degree_cent <- lapply(adj_mat_as_graphs, centr_degree)
  userrank_degree <- get_userrank_centrality(degree_cent, mapping)
  saveRDS(userrank_degree, file = paste0(get_path(USER), "userrank_", problematic_channels[i], "_degree.rds"))
  close_cent <- lapply(adj_mat_as_graphs, centr_clo)
  userrank_close <- get_userrank_centrality(close_cent, mapping)
  saveRDS(userrank_close, file = paste0(get_path(USER), "userrank_", problematic_channels[i], "_close.rds"))
  eigen_cent <- lapply(adj_mat_as_graphs, centr_eigen)
  userrank_eigen <- get_userrank_centrality(eigen_cent, mapping, var = "vector")
  saveRDS(userrank_eigen, file = paste0(get_path(USER), "userrank_", problematic_channels[i], "_eigen.rds"))

  # get alphas
  path_temp <- paste0(get_path(USER), "adj_mat_inv_", problematic_channels[i], ".rds") # set path

  if(file.exists(path_temp)){
    B <- readRDS(path_temp) # read from path
  } else {
    B <- get_alphas(A = adj_mat, a = round(upperbounda, 3)-0.001, path = path_temp) # calculate and store alphas to path (only once!)
  }
  dcm <- dcm_simple(B)
  rdcm_bigb <- rdcm_simple(B = B, b = 2, dt = rep(1,length(B)))
  if(any(sapply(rdcm_bigb, min) < 0)){
    stop("negative values in RDCM!")
  }
  rdcm_smallb <- rdcm_simple(B = B, b = .5, dt = rep(1,length(B)))
  if(any(sapply(rdcm_smallb, min) < 0)){
    stop("negative values in RDCM!")
  }

  ## user "importance" ranking (average was used when ties occur)
  ## based on broadcast communicabilities
  userrank_bigb <- get_user_rank(rdcm_bigb, mappings = mapping)
  saveRDS(userrank_bigb, file = paste0(get_path(USER), "userrank_", problematic_channels[i], "_bigb.rds"))
  userrank_smallb <- get_user_rank(rdcm_smallb, mappings = mapping)
  saveRDS(userrank_smallb, file = paste0(get_path(USER), "userrank_", problematic_channels[i], "_smallb.rds"))
  userrank_dcm <- get_user_rank(dcm, mappings = mapping)
  saveRDS(userrank_dcm, file = paste0(get_path(USER), "userrank_", problematic_channels[i], "_dcm.rds"))
}
