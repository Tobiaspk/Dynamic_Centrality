#' Stores list of alpha centralities of list of adjacency matrices
#'
#'@param A is a list of adjacency matrices. Each adjacency matrix must be
#'square
#'@param a penalty term
#'@return List of Alpha Centralities

store_alpha <- function(A, a, path = NULL) {
  # input A must be type list
  n <- nrow(A[[1]])  # dimension of matrices

  # check if each matrix is symmetric and of same dimension
  check_dim <- function(x, n_ = n) (nrow(x) == n_) & (ncol(x) == n_)
  if (!all(sapply(A, check_dim)))
    stop("Each adj. Matrix must be square!")

  # init output
  B <- list()
  Unit <- Matrix::Diagonal(n)

  # alpha centrality for each adjacency matrix
  for (i in seq(A)) {
    cat("\rAlpha Centrality ", i, "/", length(A), " done.")
    B[[i]] <- solve(Unit - a*A[[i]])
  }

  if (!is.null(path)) saveRDS(object = B, file = path)
  return(B)
}

#'Input list of alpha centralities created using the function store_alpha(...)
#'and calculates dynamic communicability matrices as described in "A matrix
#'iteration for dynamic network summaries" by Grindrod, P. and Higham.
#'
#'@param B List of alpha centrality matrices
#'@return List of dynamic communicability matrices

dcm_simple <- function(B) {
  k <- length(B)
  # init output
  Q <- list(B[[1]])
  if (k > 1)
    for (i in 2:k)
      Q[[i]] <- B[[i]] %*% Q[[i-1]]
}
