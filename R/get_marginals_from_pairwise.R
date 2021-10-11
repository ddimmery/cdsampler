#' @importFrom dplyr group_by summarize mutate select ungroup bind_rows
#' @importFrom rlang list2
#' @export
get_marginals_from_pairwise <- function(...) {
    dots <- rlang::list2(...)
    dfs <- list()
    for (pr_df in dots) {
        nm1 <- names(pr_df)[1]
        nm2 <- names(pr_df)[2]
        df1 <- pr_df %>%
            dplyr::group_by(.data[[nm1]]) %>%
            dplyr::summarize(prob = sum(prob)) %>%
            dplyr::mutate(covariate = nm1, level = as.character(.data[[nm1]])) %>%
            dplyr::select(covariate, level, prob)
        df2 <- pr_df %>%
            dplyr::group_by(.data[[nm2]]) %>%
            dplyr::summarize(prob = sum(prob)) %>%
            dplyr::mutate(covariate = nm2, level = as.character(.data[[nm2]])) %>%
            dplyr::select(covariate, level, prob)
        dfs <- c(dfs, list(df1), list(df2))
    }
    dplyr::bind_rows(!!!dfs) %>%
        dplyr::group_by(covariate, level) %>%
        dplyr::summarize(prob = mean(prob), .groups = "drop") %>%
        dplyr::group_by(covariate) %>%
        dplyr::mutate(prob = prob / sum(prob)) %>%
        dplyr::ungroup()
}