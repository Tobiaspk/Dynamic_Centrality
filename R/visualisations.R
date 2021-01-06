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
