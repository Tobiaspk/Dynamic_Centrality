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
  B <- vector("list", length(A))
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
  Q <- vector("list", k)
  Q[[1]] <- B[[1]]
  if (k > 1)
    for (i in 2:k)
      Q[[i]] <- B[[i]] %*% Q[[i-1]]
}


#'Input list of alpha centralities created using the function store_alpha(...)
#'and parameter b to calculate running dynamic communicability matrices as described
#'in "A matrix iteration for dynamic network summaries" by Grindrod, P. and Higham.
#'
#'@param B List of alpha centrality matrices
#'@param b parameter for downweighting older walks
#'@return List of running dynamic communicability matrices

rdcm_simple <- function(B, b, dt) {
  # check if b is bigger than zero
  if(b <= 0){
    stop("b must be larger than 0")
  }

  k <- length(B)
  n <- nrow(B[[1]])
  Unit <- Matrix::Diagonal(n)
  # init output
  S <- vector("list", k)
  S[[1]] <- B[[1]]-Unit
  for(i in 2:k){
    S[[i]] <- ((Unit+exp(-b*dt[i-1])*S[[i-1]]) %*% B[[i]]) - Unit
  }
  return(S)
}

#'Input list of adjacency matrices to calculate the upper bound of parameter a
#'through the spectral radius of the adjacency matrices
#'
#'WARNING: can be REALLY slow!!!
#'may not be recommended at all, if matrix is too large or eigenvalue can not be found
#'
#'an upper bound of largest eigenvalue should be used instead
#'CN: hard to find one for our case: non-negative and asymmetric matrix (+non-regular graph)
#'maybe Theorem 2.1 from https://www.sciencedirect.com/science/article/pii/S0024379513005405
#'--> still have to look into that
#'
#'@param A list of adjacency matrices
#'@param exact boolean, if TRUE the exact eigenvalues are calculated, else only the upper
#'bound of the largest eigenvalue
#'@return upper bound of parameter a
get_upperbound_a <- function(A, exact = FALSE){
  if(exact == TRUE){
    spectral_radius <- numeric(length(A))
    for(i in 1:length(A)){
      cat("\rcalc eigenvalues of A[", i, "]")
      tmp_eigen <- eigen(A[[i]], symmetric = FALSE, only.values = TRUE)$values
      spectral_radius[i] <- max(abs(tmp_eigen))
    }
    max_spectralradius <- max(spectral_radius)
  } else {


  }
  return(1/max_spectralradius)
}
