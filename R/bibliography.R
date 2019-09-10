
#' Tidy references
#'
#' Separate and tidy references into a long format.
#' @param ref_manual dataframe of manual corrections, output in the reference
#' processing workbook.
#'
#' @return a long, tidy tibble of references
#' @export
#'
#' @examples
#' \dontrun{
#' tidy_refs(ref_manual)
#' }
#' @import dplyr
tidy_refs <- function(ref_manual){
    n_ref <- stringr::str_count(ref_manual$rename, ";") %>%
        max() + 1
    ref_manual %>%
    mutate(raw_id = get_ref_code(1:nrow(.), type = "raw")) %>%
    tidyr::separate(rename, into = as.character(1:n_ref), sep = ";") %>%
    tidyr::gather(key = ref_n, value = ref_valid, as.character(1:4)) %>%
    filter(!is.na(ref_valid)) %>%
    mutate(ref_valid = stringr::str_trim(ref_valid))
}



#' Extract table of distinct references
#'
#' @param ref_tidy tidy reference tibble
#'
#' @return a tibble containing the ref_id and valid reference fields of distinct
#' references.
#' @export
#'
#' @examples
#' \dontrun{
#' get_ref_tbl(ref_tidy)
#' }
#' @import dplyr
get_ref_tbl <- function(ref_tidy){
    ref_tidy %>% distinct(ref_valid) %>%
        arrange(ref_valid) %>%
        mutate(ref_id = get_ref_code(1:nrow(.), type = "ref")) %>%
        select(ref_id, ref_valid) %>%
        distinct()
}


#' Get join_ref table
#'
#' Get join_ref table to be joined to seabird data at the final processing stage
#'
#' @param ref_tidy tidy reference tibble
#' @param ref ref_table exported by `get_ref_table()`
#'
#' @return a tibble containing the ref_id and valid reference fields for
#' each row in the seabird data. This table is prepared to be joined to the data
#' at the final processing stage.
#' @export
#'
#' @examples
#' \dontrun{
#' get_join_ref(ref_tidy)
#' }
#' @import dplyr
get_join_ref <- function(ref_tidy, ref){
    ref_tidy %>% left_join(ref, by = "ref_valid") %>%
        group_by(raw_id) %>%
    mutate(ref_ids = paste0(unique(ref_id), collapse = "; "),
           ref_n = as.integer(max(ref_n))) %>%
    ungroup() %>%
    select(reference, ref_n, ref_ids) %>%
        distinct()
}

#' Find references missing from the ref_manual db
#'
#' @param data
#' @param ref_manual
#'
#' @return
#' @export
ref_find_missing_manual <- function(data, ref_manual) {
    data[!data$reference %in% ref_manual$reference,] %>%
        dplyr::select(reference) %>%  dplyr::distinct()
}

#' Title
#'
#' @param seabirddiet
#' @param ref_manual
#'
#' @return
#' @export
ref_bind_matched_missing_manual <- function(seabirddiet, ref_manual) {
    ref_find_missing_manual(seabirddiet, ref_manual) %>%
        fuzzyjoin::stringdist_join(ref_manual,
                                   by = "reference",
                                   mode = "left",
                                   ignore_case = FALSE,
                                   method = "jw",
                                   max_dist = 0.1,
                                   distance_col = "dist") %>%
        dplyr::group_by(reference.x) %>%
        dplyr::top_n(1, -dist) %>%
        dplyr::ungroup() %>%
        dplyr::select(-.data$reference.y, -.data$dist) %>%
        dplyr::rename(reference = reference.x) %>%
        dplyr::bind_rows(ref_manual, .) %>%
        dplyr::distinct()
}

#' Title
#'
#' @param references
#' @param ref_manual
#'
#' @return
#' @export
ref_add_manual <- function(references, ref_manual) {
    dplyr::add_row(ref_manual,
                   reference = references,
                   rename = references,
                   multi = FALSE)
}


#' Title
#'
#' @param data
#'
#' @return
#' @export
df_recode_report <- function(data) {
    dplyr::mutate(seabirddiet,
                  notes2 = dplyr::case_when(.data$reference == "Report" ~ NA_character_,
                                            TRUE ~ notes),
                  source = dplyr::case_when(.data$reference == "Report" ~ "Report",
                                            TRUE ~ source),
                  reference = dplyr::case_when(.data$reference == "Report" ~ .data$notes,
                                               TRUE ~  .data$reference),
                  notes = notes2) %>%
        dplyr::select(-.data$notes2)
}

get_ref_code <- function(x, type = c("ref", "raw"), width = 3){
    type <- match.arg(type)
    prefix <- switch(type,
                     "ref" = "REF",
                     "raw" = "RAW")
    glue::glue("{prefix}{stringr::str_pad(x, {width}, pad = 0)}")
}

