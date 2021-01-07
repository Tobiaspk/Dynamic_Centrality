#' get correct path
#'
#' @param user name of user
#' @return path
#' @export
get_path <- function(user = 'tobias'){
  if(user == "tobias"){
    path = "/Users/tobiaspk1/Documents/TU_Master/SocialNetworkAnalysis/Project/data/"
  } else if(user == "tine"){
    path = "~/Schreibtisch/Social\ Network\ Analysis/SNA_Project/data/"
  }
}


#' To read all relevant files csv files
#'
#' @param user name of user
#' @param data_names data to be loaded
#' @param save_as_fst default FALSE, whether to save data in fst format or not
#' fst-format very flexible: read specific rows and columns
#' see more: http://www.fstpackage.org/
#' @param return_df returns data only if true.
#' @return data of class data.table in a list
#' @export
#' @import data.table fst
read_csv_data <- function(user = 'tobias',
                          data_names = c("Following", "Votes", "Postings"),
                          save_as_fst = FALSE,
                          return_df = TRUE){
  ## get path
  path <- get_path(user)

  ## columns to keep for the returning data
  colnames_to_keep <- c("ID_CommunityIdentity",
                        "ID_CommunityIdentityConnectedTo",
                        "ID_CommunityConnectionType",
                        "ID_Posting",
                        "VotePositive",
                        "VoteCreatedAt",
                        "ArticleChannel",
                        "ID_Article",
                        "ArticlePublishingDate",
                        "PostingCreatedAt")

  ## Temporary functions
  # create paths to files
  get_path_temp <- function(path, pattern) {
    paste0(path, grep_get(pattern, list.files(path, pattern = ".csv")))
  }

  ## get paths to all files
  pattern <- paste(data_names, collapse = "|")
  paths <- get_path_temp(path = path, pattern = pattern)
  # get "following", "votes" etc in correct order
  path_names <- regmatches(paths, regexpr(pattern, paths))

  ## read data using data.table
  df <- lapply(paths, data.table::fread)
  cat(paste0("Read .csv Files:\n\t - ", paste(path_names, collapse = "\n\t - "), "\n"))

  ## save data to fst
  if (save_as_fst) {
    paths_save <- paste0(path, path_names, ".fst")
    for (d in seq(df)) fst::write.fst(df[[d]], paths_save[d])
  }

  ## update names
  names(df) <- path_names

  ## subset data
  # should this really be here and not above "write.fst"?
  for (d in path_names)
    df[[d]] <- df[[d]][, colnames(df[[d]]) %in% colnames_to_keep, with = FALSE]

  ## return nothing
  if (!return_df) df <- NULL

  return(df)
}



#' To read fst file
#'
#' @param user name of user
#' @param data_name data to be loaded; Options: "Following", "Votes", "Postings"
#' @param column_names columns to be loaded; as list, if data_name is bigger than 1
#' @return data
#' @export
#' @import data.table fst
read_fst_data  <- function(user = 'tobias',
                             data_name = "Postings",
                             column_names = NULL){

  path <- get_path(user)
  filename <- paste0(path, data_name, ".fst")

  # create data if not yet exists
  if (!file.exists(filename)) {
    cat("Create fst Data.")
    read_csv_data(user = user,
                  data_names = data_name,
                  save_as_fst = TRUE,
                  return_df = FALSE)
  }

  # read data
  cat("Reading Data.")
  df <- fst::read.fst(path = filename,
                      columns = column_names,
                      as.data.table = TRUE)

  return(df)

  }


##### deprecate
#' To read fst file
#'
#' @param user name of user
#' @param data_name data to be loaded; Options: "Following", "Votes", "Postings"
#' @param column_names columns to be loaded; as list, if data_name is bigger than 1
#' @return data
#' @export
#' @import data.table fst
read_fst_data_old  <- function(user = 'tobias',
                           data_name = c("Following", "Votes", "Postings"),
                           column_names = NULL){
  # note: better only allow to read one "data_name"
  if (length(data_name) != 1 & class(column_names) != "list"){
    stop("if more than one data should be loaded, column_names must be a list")
  }

  if (length(data_name) != 1 & length(data_name) != length(column_names)){
    stop("if more than one data should be loaded, length of column_names
         should be the same as length of data_name")
  }

  path <- get_path(user)
  filenames <- paste0(path, data_name, ".fst")

  if(length(data_name == 1)){
    df <- fst::read.fst(path = filenames,
                        columns = column_names,
                        as.data.table = TRUE)
  } else {
    df <- list()
    for (i in 1:length(data_name)) {
      df[[data_name[i]]] <- fst::read.fst(path = filenames[i],
                                          columns = column_names[[i]],
                                          as.data.table = TRUE)
    }
  }
  return(df)

  }
