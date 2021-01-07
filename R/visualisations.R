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
plot_daily_votes <- function(id, channel, df, main = NULL) {
  if (is.null(main)) {
    main = paste("Votes Per Day | id =", id, "| channel =", channel)
  }
  temp <- df[ID_PostUser == id & ArticleChannel == channel]
  agg <- table(temp$VoteCreated)
  x <- as.Date(names(agg))
  y <- c(agg)
  plot(x, y, main = main, type = "l",
       ylim = c(0, max(y)*1.3), xlim = range(df$VoteCreated),
       xlab = "date", ylab = "count votes")
  abline(v = unique(df$VoteCreated), lty = 2, col = "grey90")
  # add weeks
  weeks <- sort(unique(floor_date(unique(df$VoteCreated), unit="week")))
  weeks <- c(weeks, weeks[length(weeks)] + 7)
  cols <- rainbow(length(weeks), alpha = .2)
  for (i in 1:(length(weeks)-1)) {
    print(cols[i])
    polygon(c(weeks[i], weeks[i+1], weeks[i+1], weeks[i]),
            c(-10, -10, max(y)*2, max(y)*2),
            col = cols[i])
  }
  lines(x, y)
  points(x, y, pch = 19, cex = 1, col = "white")
  points(x, y, pch = 19, cex = .4)
  text(weeks + 3.5, max(y)*1., format(weeks, "Week %U"))
}
