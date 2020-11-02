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
#' @importFrom dplyr mutate
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
  df = mutate(df, DRG.Definition = substring(DRG.Definition, 1, 3))
  ## Initialize ggplot object of passed in variable vs DRG code,
  ## ordered by decreasing payment value
  ggplot(data = df, mapping = aes(x = reorder(DRG.Definition, -get(variable)),
                                  y = get(variable))) +
    ## Create boxplot
    geom_boxplot(data = df) +
    ## Modify x axis tick labels, remove legend
    theme(axis.text.x = element_text(size = 7.5, angle = 60, hjust = 1.25),
          legend.position = "none") +
    ## Remove "." from passed in variable and set as y axis label
    ylab(paste(gsub("\\.", " ", variable), "(dollars)")) +
    ## Change x axis label
    xlab(paste("DRG Code")) +
    ## Add title according to passed in variable name
    ggtitle(paste((gsub("\\.", " ", variable)), "by DRG Code"))
}
