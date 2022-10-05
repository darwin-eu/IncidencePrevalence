#' Record attrition in tibble
#'
#' @param table table that contains a person_id
#' @param id id variable
#' @param reason the reason for the attrition
#' @param existing_attrition previous attrition to append results
#' @return a tibble
record_attrition <- function(table, id="person_id", reason = NULL,
                             existing_attrition=NULL) {

  error_message <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(any(class(table) %in% c("tbl_dbi", "tbl", "data.frame", "tibble")))
  checkmate::assert_character(id, add = error_message)
  checkmate::assert_character(reason, null.ok = TRUE, add = error_message)
  if(!is.null(existing_attrition)){
  checkmate::assertTRUE(any(class(existing_attrition) %in% c("tbl")))}
  checkmate::reportAssertions(collection = error_message)

  attrition <- tibble::tibble(
    current_n = table %>%
      dplyr::select(.env$id) %>%
      dplyr::distinct() %>%
      dplyr::tally()  %>%
      dplyr::pull(),
    reason = reason
  )

  if(!is.null(existing_attrition)){
  attrition <- dplyr::bind_rows(existing_attrition,attrition) %>%
     dplyr::mutate(excluded = dplyr::lag(.data$current_n) - .data$current_n)
  } else {
    attrition <- attrition %>%
      dplyr::mutate(excluded = NA)
  }

  return(attrition)
}
