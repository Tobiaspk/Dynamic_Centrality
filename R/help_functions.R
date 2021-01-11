cat_ <- function(...) cat(paste0(..., "\n"))
collapse_ <- function(..., collapse = ", ") paste0(..., collapse = collapse)
fastdate <- function(x) as.Date(fasttime::fastPOSIXct(x))
map_id <- function(x, levels) as.integer(factor(x, levels = levels))
grep_get <- function(pattern, x) x[grep(pattern = tolower(pattern), x = tolower(x))]

rescale <- function(x, scaling)
# results christine modify
get_top_id <- function(ranks) ranks$User_ID[sapply(ranks, which.min)[grepl("rank", colnames(ranks))]]
get_ranks <- function(ranks, id) ranks[User_ID == id, grepl("rank", colnames(ranks)), with = FALSE]
get_values <- function(ranks, id) unlist(ranks[User_ID == id, grepl("value", colnames(ranks)), with = FALSE])
get_q_val <- function(ranks, q = 95) sapply(ranks[, grepl("value", colnames(ranks)), with = FALSE], function(x) quantile(x, q))
get_result <- function(user, channel, size) readRDS(paste0(get_path(user), "userrank/userrank_", channel, "_", size, "b.rds"))
