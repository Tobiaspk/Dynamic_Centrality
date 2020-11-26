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
#' @return data of class data.table in a list
#' @export
#' @import data.table fst
read_csv_data <- function(user = 'tobias',
                      data_names = c("Following", "Votes", "Postings"),
                      save_as_fst = FALSE){
  ## get path
  path <- get_path(user)

  ## get file names
  file_names <- list.files(path, pattern = ".csv", full.names = TRUE)

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

  ## load data
  ind <- sapply(data_names, function(x) grep(x, file_names))
  df <- vector("list", length(data_names))
  names(df) <- data_names
  for(i in 1:length(ind)){
    df[[i]] <- rbindlist(lapply(ind[[i]], function(x) fread(file_names[x])))

    if(save_as_fst){
      write.fst(df[[i]], paste0(path, names(df)[i], ".fst"))
    }

    df[[i]] <- df[[i]][, colnames(df[[i]]) %in% colnames_to_keep, with = FALSE]
  }

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
                           data_name = c("Following", "Votes", "Postings"),
                           column_names = NULL){
  if(length(data_name) != 1 & class(column_names) != "list"){
    stop("if more than one data should be loaded, column_names must be a list")
  }

  if(length(data_name) != 1 & length(data_name) != length(column_names)){
    stop("if more than one data should be loaded, length of column_names
         should be the same as length of data_name")
  }

  path <- get_path(user)

  if(length(data_name == 1)){
    df <- read.fst(paste0(path, data_name, ".fst"),
                          columns = column_names) %>%
      as.data.table
  } else {
    df <- sapply(1:length(data_name), function(x)
      as.data.table(read.fst(paste0(path, data_name[x], ".fst"),
                             columns = column_names[[x]])))
    names(df) <- data_name
  }
  return(df)

}
