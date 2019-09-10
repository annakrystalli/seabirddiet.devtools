
#' Fuzzy match check for typos
#'
#' @param check_var character string, variable name to check
#' @param df dataset
#' @param threshold distance threshold. Matches with distances greater than the
#' threshold are considered non-matches
#'
#' @return a tibble of potential typos
#' @export
check_fuzzy_values <- function(check_var, df, threshold = 0.1){
    check_var <- match.arg(check_var, choices = names(df))
    var <- as.name(check_var)
    var.x <- as.name(paste0(check_var, ".x"))
    dplyr::select(tbl, !!var) %>% dplyr::distinct() %>%
        fuzzyjoin::stringdist_join(., .,
                                   mode = "left",
                                   ignore_case = FALSE,
                                   method = "jw",
                                   max_dist = 99,
                                   distance_col = "dist") %>%
        dplyr::filter(.data$dist > 0 & .data$dist < as.vector(threshold)) %>%
        dplyr::group_by(!!var.x) %>%
        dplyr::top_n(1, -dist)

}
