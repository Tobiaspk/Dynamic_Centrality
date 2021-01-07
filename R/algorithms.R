#'Get Alpha Centrality
#'
#'@param A Square adjacency matrix
#'@param a penalty term
#'
alpha_centrality <- function(A, a) {
  stopifnot(nrow(A) == ncol(A))
  Unit <- Matrix::Diagonal(nrow(A))
  return(solve(Unit - a*A))
}

#'Stores list of alpha centralities of list of adjacency matrices
#'
#'@param A is a list of adjacency matrices. Each adjacency matrix must be
#'square
#'@param a penalty term
#'@return List of Alpha Centralities

get_alphas <- function(A, a, path = NULL) {
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
  cat("\rAlpha Centrality 0 /", length(A), " done.")
  for (i in seq(A)) {
    begin <- Sys.time()
    B[[i]] <- alpha_centrality(A = A[[i]], a = a)
    cat("\rAlpha Centrality ", i, "/", length(A), " done in ",
        round(Sys.time() - begin, 1), ".")
  }

  if (!is.null(path)) saveRDS(object = B, file = path)
  return(B)
}

#'Dynamic Communicability Matrix (dcm)
#'
#'Input list of alpha centralities created using the function \code{\link{store_alpha}}
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
  if (k > 1) {
    cat("\rDCM 0 /", length(B), " done.")
    for (i in 2:k) {
      begin <- Sys.time()
      Q[[i]] <- B[[i]] %*% Q[[i-1]]
      cat("\rDCM ", i, "/", length(B), " done in ",
          round(Sys.time() - begin, 1), ".")
    }
  }
  return(Q)
}


#'Running Dynamic Communicability Matrix (RDCM)
#'
#'Input list of alpha centralities created using the function \code{\link{store_alpha}}
#'and parameter b to calculate running dynamic communicability matrices as described
#'in "A matrix iteration for dynamic network summaries" by Grindrod, P. and Higham.
#'
#'@param B List of alpha centrality matrices
#'@param b parameter for downweighting older walks
#'@param dt vector of time differences between two timepoints
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
    begin <- Sys.time()
    S[[i]] <- ((Unit+exp(-b*dt[i-1])*S[[i-1]]) %*% B[[i]]) - Unit
    cat("\rRDCM ", i, "/", length(B), " done in ",
        round(Sys.time() - begin, 1), ".")
  }
  return(S)
}

#'Find upper bound for alpha
#'
#'Input list of adjacency matrices to calculate the upper bound of parameter a
#'through the spectral radius of the adjacency matrices.
#'WARNING: can be REALLY slow!!!
#'May not be recommended at all, if matrix is too large or eigenvalue can not be found.
#'An upper bound of largest eigenvalue or spectral radius should be used instead
#'hard to find one for our case: non-negative and asymmetric matrix (+non-regular graph)
#'used Theorem 2.1 from https://www.sciencedirect.com/science/article/pii/S0024379513005405
#'calculating an upper bound of spectral radius.
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

    spectral_radius <- numeric(length(A))

    for(i in 1:length(A)){
      cat("\rcalc upper bound for spectral radius of A[", i, "]\n")
      tmp_rs <- sort(Matrix::rowSums(A[[i]]), decreasing = TRUE)
      tmp_phi <- numeric(length(tmp_rs))
      ### since we have an adjacency matrix without self-loops, N = 1, M = 0
      for(j in 1:length(tmp_rs)){
        tmp_phi[j] <- tmp_rs[j]-1+sqrt((tmp_rs[j]+1)^2+4*sum(tmp_rs[1:(j-1)]-tmp_rs[j]))
      }
      spectral_radius[i] <- min(tmp_phi)
    }
    max_spectralradius <- min(spectral_radius)
  }
  return(1/max_spectralradius)
}

#'Rank users
#'
#'Return ranked users regarding the dynamic broadcast or receive communicabilities
#'for each time point
#'Input list of running dynamic communicability matrices (an outcome of the function
#'\code{\link{rdcm_simple}})
#'
#'@param rdcm List of running dynamic communicability matrices
#'@param type either "broadcast" or "receive"
#'@param mappings vector of mapping between "ID" and "Node" (an outcome of the function
#'\code{\link{create_adj_mat_time}})
#'@param tiesmethod method to rank when ties occur
#'@return matrix of user rank for each time point
get_user_rank <- function(rdcm,
                          type = "broadcast",
                          mappings,
                          tiesmethod = "average"){

  users_rank <- data.table(names(mapping))

  for(i in 1:length(rdcm)){
    if(type == "broadcast"){
      tmp_S <- Matrix::colSums(rdcm[[i]])
    } else if(type == "receive"){
      tmp_S <- Matrix::rowSums(rdcm[[i]])
    }
    users_rank <- cbind(users_rank, tmp_S, frank(-tmp_S, ties.method = tiesmethod))
  }
  colnames(users_rank) <- c("User_ID", paste0(c("value_t", "rank_t"), rep(1:length(rdcm), each = 2)))
  return(users_rank)
}
