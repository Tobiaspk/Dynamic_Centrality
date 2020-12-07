#' create user-vote-user-data
#'
#' @param df_votes data of votes with at least "ID_Posting" and "ID_CommunityIdentity" columns
#' @param df_posts data of posts with at least "ID_Posting" and "ID_CommunityIdentity" columns
#' @param channels default NULL (all channels), specify specific article channels
#' @param save_path default NULL, if not NULL: save data under this path
#' @return data of class data.table
#' @export
#' @import data.table fst magrittr
create_user_vote_user_data <- function(df_votes, df_posts, channels = NULL, save_path = NULL){
  # copy required, else input data is manipulated
  df_votes <- copy(df_votes)
  df_posts <- copy(df_posts)

  ## choose only positive votes
  if("VotePositive" %in% colnames(df_votes)) {
    df_votes <- df_votes[VotePositive == 1]
    df_votes[, VotePositive := NULL]  # i think that must be in own row? (tobi)
  }

  ## change user-id name
  setnames(df_votes, "ID_CommunityIdentity", "ID_VoteUser")
  setnames(df_posts, "ID_CommunityIdentity", "ID_PostUser")

  ## join data
  df <- merge(df_posts, df_votes, by = "ID_Posting")
  # delete self-loops (however no self-loops)
  df <- df[ID_VoteUser != ID_PostUser]

  # create custom IDs, choose only date (not datetime) and subset data
  users_uq <- unique(c(df$ID_PostUser, df$ID_VoteUser))
  df <- df[, .(ID_Posting = map_id(ID_Posting),
                ID_PostUser = map_id(ID_PostUser, levels = users_uq),
                ID_VoteUser = map_id(ID_VoteUser, levels = users_uq),
                ID_Article = map_id(ID_Article),
                ArticleChannel = ArticleChannel,
                VoteCreated = fastdate(VoteCreatedAt),
                PostingCreated = fastdate(PostingCreatedAt),
                ArticleCreated = fastdate(ArticlePublishingDate))]
  
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
