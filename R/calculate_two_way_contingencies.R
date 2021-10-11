#' @importFrom purrr map_chr
#' @importFrom dplyr group_by tally ungroup mutate
#' @importFrom rlang enexprs as_string
#' @export
calculate_two_way_contingencies <- function(df, ...) {
    rlang::enexprs(...) %>%
    purrr::map_chr(rlang::as_string) %>%
    utils::combn(2) %>%
    apply(2, function(vv) {
        vv <- rlang::syms(vv)
        dplyr::group_by(df, !!!vv) %>%
        dplyr::tally() %>%
        dplyr::ungroup() %>%
        dplyr::mutate(prob = n / sum(n))
    })
}