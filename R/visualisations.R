library(lubridate)

### Insights time Points
plot_adj_matrix_agg <- function(A, time_axis, agg = sum) {
  y_n <- sapply(adj_mat, sum)
  time_points <- factor(unique(df_sub[["VoteCreatedWeek"]]))  # required since week is character
  # simple plot function
  plot_votes_per_day <- function(x, y) {
    # x ... time-point vector, y ... value vector
    xn <- as.numeric(x)
    plot(xn, y, main = "Votes Per Day", type = "l", ylim = c(0, max(y)),
         xlab = "date", ylab = "count votes", xaxt = "n")
    axis(1, xn, x)
    points(xn, y, pch = 19, cex = 1, col = "white")
    points(xn, y, pch = 19, cex = .4)
  }
  plot_votes_per_day(time_points, y_n)
}


# visualise daily votes of an id
plot_daily_votes <- function(id, channel, ranks, df, main = NULL) {
  if (is.null(main)) {
    main = paste("Votes Per Day | id =", id, "| channel =", channel)
  }
  temp <- df[ID_PostUser == id & ArticleChannel == channel]
  agg <- table(temp$VoteCreated)
  x <- as.Date(names(agg))
  y <- c(agg)
  scaling <- max(y)
  plot(x, y, main = main, type = "l",
       ylim = c(0, max(y)*1.4), xlim = range(df$VoteCreated),
       xlab = "date", ylab = "count votes")
  abline(v = unique(df$VoteCreated), lty = 2, col = "grey90")
  # add weeks
  weeks <- sort(unique(floor_date(unique(df$VoteCreated), unit="week")))
  weeks <- c(weeks, weeks[length(weeks)] + 7)
  cols <- rainbow(length(weeks), alpha = .2)
  q95 <- get_q_val(ranks = ranks, q = .95)
  q99 <- get_q_val(ranks = ranks, q = .99)
  q100 <- get_q_val(ranks = ranks, q = 1)
  qid <- get_values(ranks = ranks, id = id)
  scaling_loc <- max(q100)
  q95_scaled <- q95/scaling_loc * scaling
  q99_scaled <- q99/scaling_loc * scaling
  q100_scaled <- q100/scaling_loc * scaling
  qid_scaled <- qid/scaling_loc * scaling
  axis_mult <- c(0, .2, .4, .6, .8, 1)
  axis(4, scaling * axis_mult, labels = round(scaling_loc * axis_mult, 1), cex.axis = .65)
  for (i in 1:(length(weeks)-1)) {
    polygon(c(weeks[i], weeks[i+1], weeks[i+1], weeks[i]),
            c(-10, -10, max(y)*2, max(y)*2),
            col = cols[i])
    lines(c(weeks[i], weeks[i+1]),
          c(q95_scaled[i], q95_scaled[i]),
          type = "lv", col = "grey70")
    lines(c(weeks[i], weeks[i+1]),
          c(q99_scaled[i], q99_scaled[i]),
          type = "l", lty = 2, col = "grey80")
    lines(c(weeks[i], weeks[i+1]),
          c(q100_scaled[i], q100_scaled[i]),
          type = "l", lty = 3, col = "red")
    lines(c(weeks[i], weeks[i+1]),
          c(qid_scaled[i], qid_scaled[i]),
          type = "l", lty = 3, col = "green")
  }
  lines(x, y)
  points(x, y, pch = 19, cex = 1, col = "white")
  points(x, y, pch = 19, cex = .4)
  ranks_id <- get_ranks(ranks, id)
  text(weeks + 3.5, max(y)*1.35, format(weeks, "Week %U"))
  text(weeks + 3.5, max(y)*1.18, paste0("Rank: ", ranks_id), cex = .8)
}

####
# wrapper for upper function that also reads the respective data
plot_ranks <- function(user, channel, size, time_point, df) {
  if (channel %in% c("Inland", "Meinung", "Wirtschaft", "Etat",
                     "International", "Panorama", "Web", "Kultur", "Sport")) {
    warning("Calculation for chosen channel failed. Result of reduced data shown.")
  }
  if (!size %in% c("small", "big")) {
    stop("Please choose size from 'small' and 'big'")
  }
  if (!time_point %in% 1:5) {
    stop("Time_point must be an integer between 1 and 5")
  }
  ranks <- get_result(user, channel, size)
  id_temp <- get_top_id(ranks)[time_point]
  plot_daily_votes(id = id_temp, channel = channel, ranks = ranks, df = df)
}


####
plot_value_time <- function(user, channel, size, df, ids = NULL, type = "value", log = FALSE) {
  ranks <- get_result(user = user, channel = channel, size = size)
  if (is.null(ids)) {
    ids <- unique(get_top_id(ranks = ranks))
  }
  temp <- ranks[User_ID %in% ids]
  temp_val <- temp[, grepl(type, colnames(temp)), with = FALSE]
  if (log) {
    temp_val <- log(temp_val)
  }
  timepoints <- seq(temp_val)
  cols <- rainbow(length(ids))
  cols2 <- rainbow(length(ids), alpha = .3)
  if (size == "big") {
    sizet <- 2
  } else {
    sizet <- 0.5
  }
  plot(0, 0, type = "n",
       ylab = paste("RDCM", type), main = paste("Channel =", channel, "| b =", sizet, "|", type),
       xlim = range(timepoints), ylim = range(temp_val))
  for (i in seq(ids)) {
    lines(x = timepoints, y = temp_val[i, ], type = "l", col = cols2[i], lwd = .5)
    #points(x = timepoints, y = temp_val[i, ], pch = 19, col = "black")
    points(x = timepoints, y = temp_val[i, ], pch = 8, col = cols[i], cex = 1)
  }
}
