#' @importFrom dplyr slice_sample group_by mutate select
#' @importFrom tidyr pivot_wider
#' @export
sample_batch_from_marginals <- function(size, marginals) {
    marginals %>%
    dplyr::group_by(covariate) %>%
    dplyr::slice_sample(n = size, weight_by = prob, replace = TRUE) %>%
    dplyr::mutate(.id = 1:n(), level = as.character(level)) %>%
    dplyr::select(.id, covariate, level) %>%
    tidyr::pivot_wider(id_cols = .id, names_from = covariate, values_from = level) %>%
    dplyr::select(-.id)
}