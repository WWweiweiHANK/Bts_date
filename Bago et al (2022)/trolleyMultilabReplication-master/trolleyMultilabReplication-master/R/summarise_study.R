#' Function to summarize studies into variables
#'
#' Creates a datatable of aggregated values based
#' on a datatable with the raw variables.
#'
#' @param data datatable containing the raw values
#'
#' @return The function returns the summarized dataframe.
#' @export
summarise_study <- function(data = NULL){

  stopifnot(!is.null(data))

  data %>%
    dplyr::summarise(
      N = dplyr::n(),
      `Age Mean` = mean(Age, na.rm = TRUE),
      `Age SD` = sd(Age, na.rm = TRUE),
      `Male %` = mean(sex == 1, na.rm = TRUE),
      `Higher education %` = mean(`Higher education`, na.rm = TRUE),
       .groups = "drop")

}
