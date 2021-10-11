#' @importFrom dplyr inner_join relocate select filter
#' @importFrom rlang list2
prepare_pairwise_probs <- function(marginals, ...) {
    dots <- rlang::list2(...)
    result <- list()
    for (pr_df in dots) {
        cov1 <- names(pr_df)[1]
        cov2 <- names(pr_df)[2]
        pr_df[[cov1]] <- as.character(pr_df[[cov1]])
        pr_df[[cov2]] <- as.character(pr_df[[cov2]])
        marg1 <- dplyr::filter(marginals, covariate == cov1)
        marg1[[cov1]] <- marg1$level
        marg1 <- dplyr::select(marg1, -covariate, -level)
        pr_df <- dplyr::inner_join(marg1, pr_df, by = cov1, suffix = c(".1", ""))
        marg2 <- dplyr::filter(marginals, covariate == cov2)
        marg2[[cov2]] <- marg2$level
        marg2 <- dplyr::select(marg2, -covariate, -level)
        pr_df <- dplyr::inner_join(marg2, pr_df, by = cov2, suffix = c(".2", ""))
        pr_df$odds <- with(pr_df, prob / prob.1 / prob.2)
        pr_df <- dplyr::relocate(pr_df, prob.1, prob.2, prob, odds, .after = n)
        pr_df[[1]] <- as.character(pr_df[[1]])
        pr_df[[2]] <- as.character(pr_df[[2]])
        result <- c(result, list(pr_df))
    }
    result
}