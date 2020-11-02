#' Title
#'
#' This function calculates a passed in statistic (mean, median, or standard deviation)
#' for average medicare payments of DRG data.
#'
#' @param df a dataframe
#' @param x a string indicating the statistic to calculate. Choose either "mean",
#' "median", or "sd"
#'
#' @return A tibble of the dataframe with \code{x}, the statistic of choice
#' @export
#'
#' @import dplyr
#'
#'
#' @examples
#'
#' drg_data = read.csv("drg.csv")
#'
#' calc_stats(drg_data, "mean")


calc_stats = function(df, x) {
  ## choose relevant columns
  df = dplyr::select(df, DRG.Definition, Average.Medicare.Payments)
  ## group by DRG code
  df = dplyr::group_by(df, DRG.Definition)
  ## calculate the passed in statistic per DRG code
  df = dplyr::summarise(df,
                        Statistic = get(x)(Average.Medicare.Payments)
  )
  ## Show the tibble
  df
}
