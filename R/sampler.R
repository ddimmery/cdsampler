#' @importFrom progress progress_bar
#' @importFrom dplyr slice_sample
#' @importFrom rlang list2
#' @export
sample_correlated_discrete <- function(size, ..., .batch_size = 1000, .initial_acceptance_pr = 0.1) {
    dots <- rlang::list2(...)

    check_input(...)

    variable_marginals <- get_marginals_from_pairwise(...)

    pr_dfs <- prepare_pairwise_probs(variable_marginals, ...)

    sample_batch <- sample_batch_from_marginals(
        .batch_size,
        variable_marginals
    )
    sample_idx <- 1

    result <- matrix(
        NA_character_,
        nrow = size,
        ncol = length(unique(variable_marginals$covariate))
    )

    n_accepted <- 0
    base_accept_pr <- .initial_acceptance_pr

    accept_prs <- rep(NA_real_, 10000)

    pb <- progress_bar$new(
        format = "=> sampling [:bar] :percent eta: :eta", total = size,
        clear = FALSE, width = 80, show_after = 2
    )
    hashmap <- new.env()

    while (n_accepted < size) {
        if (sample_idx > nrow(sample_batch)) {
            sample_idx <- 1
            sample_batch <- sample_batch_from_marginals(
                .batch_size,
                variable_marginals
            )
        }

        sample_row <- unlist(sample_batch[sample_idx, ])

        log2_acc <- log2(n_accepted)
        if (log2_acc > 3 && abs(log2_acc - floor(log2_acc)) < 1e-15) {
            base_accept_pr <- 1 / max(accept_prs, na.rm = TRUE) * 1.025
            .batch_size <- (
                mean(accept_prs, na.rm = TRUE) / base_accept_pr * (size - n_accepted)
            )
        }
        accept_pr <- base_accept_pr
        for (pr_df in pr_dfs) {
            nms <- names(pr_df)
            lvl1 <- sample_row[nms[1]]
            lvl2 <- sample_row[nms[2]]
            key <- paste(nms[1], lvl1, "_", nms[2], lvl2, sep = "_")
            if (exists(key, envir = hashmap)) {
                pairwise_odds <- get(key, envir = hashmap)
            } else {
                pairwise_odds <- unlist(pr_df[
                    pr_df[[nms[1]]] == lvl1 & pr_df[[nms[2]]] == lvl2,
                    "odds"
                ])
                assign(key, pairwise_odds, envir = hashmap)
            }
            accept_pr <- accept_pr * pairwise_odds
        }

        accept_prs <- c(accept_prs[2:length(accept_prs)], accept_pr / base_accept_pr)

        if (runif(1) <= accept_pr) {
            n_accepted <- n_accepted + 1
            result[n_accepted, ] <- sample_row
            pb$tick()
        }
        sample_idx <- sample_idx + 1
    }

    result <- dplyr::as_tibble(result, .name_repair = "minimal")
    names(result) <- names(sample_batch)

    dplyr::slice_sample(result, prop = 1)
}