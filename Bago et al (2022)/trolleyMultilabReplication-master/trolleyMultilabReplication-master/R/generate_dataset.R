#' Generate a dataset of integer values with specified distribution paramaters
#'
#' Please add a description Tamas.
#'
#' @param n TODO
#' @param mean TODO
#' @param sd TODO
#' @param floor TODO
#' @param ceiling TODO
#' @param tasks TODO
#' @param groups TODO
#'
#' @return A tibble in long format.
#' @export
#' @examples
#' \dontrun{
#' generate_dataset(n = 1000, mean = 5, sd = 2, tasks = 4, groups = c("a", "b"))
#' }
generate_dataset <- function(n,
                             mean,
                             sd,
                             floor = 1,
                             ceiling = 9,
                             tasks = 4,
                             groups = c("a", "b", "c")){
  # Gerate  the variable names
  variables <-
    paste(
      paste("v", 1:tasks, sep = ""),
      rep(groups, each = tasks),
      sep = "_"
    )

  # Create a list of values and cast it into a tibble
  purrr::map(variables,
             ~draw_values(n, mean, sd, floor, ceiling)) %>%
    purrr::set_names(variables) %>%
    dplyr::as_tibble()

}
# Generate a dataset of integer values with specified distribution paramaters
# OUTPUT: A tibble in long format
# EXAMPLE: generate_dataset(n = 1000, mean = 5, sd = 2, tasks = 4, groups = c("a", "b"))
