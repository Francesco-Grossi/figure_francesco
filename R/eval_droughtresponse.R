#' Evaluate the model's drought response
#'
#' xxx
#'
#' @param df A data frame containing all data for all sites, modelled and observed
#' @param obsvarnam The variable name of the observational data, given as a column in \code{df}
#' @param modvarnam The variable name of the modelled data, given as a column in \code{df}
#'
#' @return A data frame of data aligned by drought events and aggregated across events (for different quantiles)
#' @export
#'
#' @examples xxx
#'
eval_droughtresponse <- function( df, df_flue, before, after, leng_threshold, nbins, do_norm = FALSE ){

  ## Get fLUE Stocker et al., 2018 publicly available data
  df_flue <- df_flue %>%
    dplyr::select(-year, -doy, -cluster) %>%
    dplyr::rename( isevent = is_flue_drought )
  
  ## Rearrange data. Function returns list( df_dday, df_dday_aggbydday )
  dovars <- c("bias")
  df <- df %>%
    mutate(bias = mod - obs)
  
  out_align <- align_events( df, df_flue, dovars, leng_threshold, before, after, nbins, do_norm = do_norm )
  
  return( out_align$df_dday_aggbydday )
}

na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }
