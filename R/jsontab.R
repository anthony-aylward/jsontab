#===============================================================================
# jsontab.R
#===============================================================================

#' @title Load a file that may be in either JSON or tabular format
#'
#' @description Regardless of the input format, a list will be returned
#'
#' @param txt a JSON string, a URL, or a JSON or tabular file
#' @param json indicates JSON input
#' @param tab indicates tabular input
#' @param ... additional arguments to be passed to `fromJSON` or `read.table`
#' @return a list representing the input data
#' @export
from_json_or_tab <- function(
  txt, 
  json = FALSE, 
  tab = FALSE,
  header = FALSE,
  stringsAsFactors = FALSE,
  ...
) {
  if (json && !table) {
    fromJSON(txt, ...)
  } else if (!json && table) {
    list(
      as.list(
        read.table(
          txt,
          header = header,
          stringsAsFactors = stringAsFactors,
          ...
        )
      )
    )
  } else {
    tryCatch(
      fromJSON(txt, ...),
      error = function() {
        list(
          as.list(
            read.table(
              txt,
              header = header,
              stringsAsFactors = stringAsFactors,
              ...
            )
          )
        )
      }
    )
  }
}
