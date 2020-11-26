#' create user-vote-user-data
#'
#' @param df_votes data of votes with at least "ID_Posting" and "ID_CommunityIdentity" columns
#' @param df_posts data of posts with at least "ID_Posting" and "ID_CommunityIdentity" columns
#' @param channels default NULL (all channels), specify specific article channels
#' @param save_path default NULL, if not NULL: save data under this path
#' @return data of class data.table
#' @export
#' @import data.table fst
create_user_vote_user_data <- function(df_votes, df_posts, channels = NULL, save_path = NULL){
  ## choose only positive votes
  if("VotePositive" %in% colnames(df_votes)) {
    df_votes <- df_votes[VotePositive == 1] %>%
      .[, VotePositive := NULL]
  }

  ## change user-id name
  setnames(df_votes, "ID_CommunityIdentity", "ID_VoteUser")
  setnames(df_posts, "ID_CommunityIdentity", "ID_PostUser")

  ## join data
  df <- df_posts[df_votes, on = .(ID_Posting)] %>%
    .[ID_VoteUser != ID_PostUser] # delete self-loops (however no self-loops)

  ## choose channel
  if(!is.null(channels)){
    df <- df[ArticleChannel %in% channels]
  }

  ## save data
  if(!is.null(save_path)){
    write.fst(df, save_path)
  }

  return(df)
}
