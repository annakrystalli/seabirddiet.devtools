#' Extract date ranges from a vector of years
#'
#' @param x vector of years
#' @param type what output to return. `both` for the range,
#' `start` for only the start date, `end` for the end date
#'
#' @return years are converted to ISO dates of 1st Jan for a `begin` date
#' and 31st Dec for `end` dates. If `type = "range"` a vector of length 2
#' is returned. Otherwise a named character string of the specified
#' boundary is returned.
#' @export
yr_to_isorange <- function(x, type = c("both", "start", "end")){
    type <- match.arg(type)
    range <- c(start = as.Date(ISOdate(min(x, na.rm = T),
                                       1, 1)),
               end = as.Date(ISOdate(max(x, na.rm = T),
                                     12, 31))
               )
    return(
        switch (type,
        "both" = range,
        "start" =  range["start"],
        "end" = range["end"])
        )
}


#' Extract creators to list format
#'
#' Function to be applied to the `creators_df` to exctract tabular information
#' into creators format.
#' @param x a row in the `creators_df` data.frame
#' (read from `data-raw/metadata/creators.csv``)
#'
#' @return an eml creator list element
#' @export
extr_creator <- function(x){
    eml_address <- EML::eml$address(
        deliveryPoint =  x["address"],
        administrativeArea = x["amdin_area"],
        country = x["country"])

    EML::eml$creator(
        individualName = EML::eml$individualName(
            givenName = x["givenName"],
            surName = x["familyName"]),
        electronicMailAddress = x["email"],
        userId = EML::eml$userId(
            directory = paste0("http://orcid.org/", x["ORCID-ID"])),
        address = eml_address,
        organizationName = x["affiliation"]
        )
}


extr_citations <- function(x){
    citation <- list(unstructured_citation = list(x[["ref_valid"]]))
    attr(citation, "key") <- glue::glue('ref={x["ref_id"]}')
    citation
}

#' Create crossref citation_list
#'
#' Create a citation_list of unstructured citations following crossref schema from `references.csv`.
#' @param ref_path path to `references.csv` (defaults to `data-raw/metadata/references.csv`)
#'
#' @return a crossref `citation_list` in list format. To be supplied to the `metadata` element of `additionalMetadata`.
#' @export
citation_list_make <- function(ref_path = here::here("data-raw", "metadata", "references.csv" )){
    references_df <- readr::read_csv(ref_path)
    citation_list <- list(citation_list = references_df %>% apply(1, extr_citations) %>% setNames(rep("citation", times = length(.))))
    attr(citation_list, "schemaLocation") <- "http://www.crossref.org/schema/4.3.7 http://www.crossref.org/schema/deposit/crossref4.3.7.xsd"
    attr(citation_list, "xmlns:crossref") <- "http://www.crossref.org/schema/4.3.7"
    citation_list
}
