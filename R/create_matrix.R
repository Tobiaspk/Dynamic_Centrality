#' Create adjacency matrix from given  data. Rows are typically voters
#' and columns are posters. Nrow and and ncol should be identical.
#' Matrix is one if an edge voter-poster exists.
#'
#' Vector of voters and posters should be adjusted to represent numbers from
#' 1 to #unique users
#'
#' @param voter vector of voters
#' @param poster poster vector of posters
#' @param nrow number of rows of resulting table
#' @param ncol number of columns of resulting table
#' @return sparse adjency matrix
#' @export

create_adj_mat <- function(x, y, nrow, ncol = nrow) {
  mat <- Matrix::Matrix(0, nrow = nrow, ncol = ncol)
  mat[cbind(x, y)] <- 1
  return(mat)
}

#' Create a list of adjacency matrix of a time variable. Creates a list of
#' matrices (A1, A2, ..., Ak)
#'
#'@param df inputs data frame
#'@param time_var column name of time variable to use
#'@param x outgoing column name
#'@param y ingoing column name
#'@return list of:
#'  "matrix" - list of adjacency matrix at each time point
#'  "mapping" - mapping for the user ids using mapping[user_id]

create_adj_mat_time <- function(df_input,
                                time_var,
                                x = "ID_VoteUser",
                                y = "ID_PostUser") {
  # get unique ids
  users_uq <- unique(c(df_input[[x]], df_input[[y]]))

  # define setting: which time points and global users
  time_points <- sort(unique(df_input[[time_var]]))
  # dimension should be nxn where n is the number of unique users
  n <- length(users_uq)
  mapping <- 1:n
  names(mapping) <- users_uq

  # apply create_adj_mat to one time_points
  # attention: dependent on local variables
  create_adj_mat_temp <- function(date) {
    sub <- df_input[df_input[[time_var]] == date]
    return(create_adj_mat(x = mapping[as.character(sub[[x]])],
                          y = mapping[as.character(sub[[y]])],
                          nrow = n, ncol = n))
  }

  # create list of adjency matrices at each time point
  adj_mat <- lapply(time_points, create_adj_mat_temp)
  return(list(matrix = adj_mat, mapping = mapping))
}
