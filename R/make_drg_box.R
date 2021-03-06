#' Title
#'
#' This function produces a ggplot boxplot of \code{variable} versus DRG Code
#'
#' @param df a dataframe
#' @param variable a string for the y variable. Please choose between:
#' "Average.Medicare.Payments", "Average.Total.PAyments", and "Average.Covered.Charges"
#'
#' @return A plot with \code{variable} versus DRG code
#' @export
#'
#' @import dplyr
#' @import ggplot2
#'
#' @examples
#'
#' drg_data = read.csv("drg.csv")
#'
#' make_drg_box(drg_data, "Average.Medicare.Payments")
#'

make_drg_box = function(df, variable) {
  ## Shorten to numeric DRG code
  df = dplyr::mutate(df, DRG.Definition = substring(DRG.Definition, 1, 3))
  ## Initialize ggplot object of passed in variable vs DRG code,
  ## ordered by decreasing payment value
  ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = reorder(DRG.Definition, -get(variable)),
                                                    y = get(variable))) +
    ## Create boxplot
    ggplot2::geom_boxplot(data = df) +
    ## Modify x axis tick labels, remove legend
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 7.5, angle = 60, hjust = 1.25),
                   legend.position = "none") +
    ## Remove "." from passed in variable and set as y axis label
    ggplot2::ylab(paste(gsub("\\.", " ", variable), "(dollars)")) +
    ## Change x axis label
    ggplot2::xlab(paste("DRG Code")) +
    ## Add title according to passed in variable name
    ggplot2::ggtitle(paste((gsub("\\.", " ", variable)), "by DRG Code"))
}
