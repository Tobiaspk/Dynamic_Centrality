devtools::load_all()
library(Matrix)

## load data
df_votes <- read_fst_data(user = "tine",
                          data_name = "Votes",
                          column_names = c("ID_Posting", "ID_CommunityIdentity", "VoteCreatedAt", "VotePositive"))
df_posts <- read_fst_data(user = "tine",
                          data_name = "Postings",
                          column_names = c("ID_Posting", "ID_CommunityIdentity", "ArticleChannel"))

## merge data
df <- create_user_vote_user_data(df_votes, df_posts, channels = "International") %>%
  .[, time := fasttime::fastPOSIXct(VoteCreatedAt)] %>% # add time variable
  .[, .N, keyby = .(ID_VoteUser, ID_PostUser, yday(time))] # aggregate

## not efficient, to enable creating an adjacent matrix with rows/columns containing only zeros
pretab <- rbindlist(list(df,
                         df[,.(ID_VoteUser = ID_PostUser,
                               ID_PostUser = ID_VoteUser,
                               yday = yday[1],
                               N = 0)]))

## days of year
ydays <- sort(unique(df$yday))

## initialize result
res <- vector("list", length(ydays))
names(res) <- paste0("time", ydays)

## creating adjacent matrix for i time point
for(i in 1:length(ydays)){
  print(i, ".Iteration")
  res[[i]] <- copy(pretab) %>%
    .[yday != ydays[i], N := 0] %>%
    .[, .(N = sum(N)), by = .(ID_VoteUser, ID_PostUser)] %>%
    dcast("ID_VoteUser~ID_PostUser", value.var="N", fill = 0, drop = FALSE) %>%
    as.matrix %>%
    Matrix(sparse = TRUE)
}

## save list of sparse matrices
saveRDS(res,
        paste0(get_path(user = "tine"),
               "sparsematrix_international_yday.rds"))
