#' @importFrom checkmate assert_data_frame
#' @importFrom rlang list2
check_input <- function(...) {
    dots <- rlang::list2(...)
    for (pr_df in dots) {
        checkmate::assert_data_frame(pr_df, any.missing = FALSE, min.rows = 4, min.cols = 3)
        if (!("prob" %in% names(pr_df))) stop("Must include `prob` column.")
        nlvl1 <- length(unique(pr_df[[1]]))
        if (nlvl1 <= 1) stop("Must have distinct values for first variable in dataframe.")
        if (nlvl1 >= 100) warning("Over 100 distinct values for first variable in dataframe.")
        nlvl2 <- length(unique(pr_df[[2]]))
        if (nlvl2 <= 1) stop("Must have distinct values for second variable in dataframe.")
        if (nlvl2 >= 100) warning("Over 100 distinct values for second variable in dataframe.")
    }
}