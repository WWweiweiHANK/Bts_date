#' Create summary table
#'
#' Summarize the result of several simulations.
#'
#' @param combinations Tamas please add this
#' @param out_list Tamas please add this
#'
#' @return The function returns a tibble that shows correct
#' and incorrect inference rates for different scenarios
#' @export
#' @examples
#' \dontrun{
#' aggregate_inferences(out)
#' }
aggregate_inferences <- function(combinations, out_list){

  out_list %>%
    furrr::future_map_dfr(.,
                          ~tibble::enframe(.x, name = NULL) %>%
                            t() %>%
                            tibble::as_tibble()) %>%
    dplyr::rename(correct_inference_rate = 1,
                  # incorrect_inference_rate = 2
    ) %>%
    dplyr::bind_cols(combinations, .) %>%
    dplyr::group_by(!!!syms(names(select(combinations, -sample)))) %>%
    dplyr::summarise(correct_inference_rate = mean(as.logical(correct_inference_rate)),
                     # incorrect_inference_rate = mean(as.logical(incorrect_inference_rate))
    ) %>%
    dplyr::ungroup()
}
