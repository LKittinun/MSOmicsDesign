#' @title Generate plate position
#' @description
#' This function retrieve plate matrices from `fill.plate()` and flatten into vector in the form that can be filled into MS sequence file.
#'
#' @importFrom purrr imap list_c
#'
#' @param plate A list of plate matrices generated from fill.plate().
#' @param flatten If `TRUE` will flatten a list into a single vector.
#'
#' @return A vector or list of vectors containing `plate_name:plate_position`
#' @family plate
#' @export
#'
#' @examples
#' x_plate <- fill.plate(1:100)
#' plate_list <- generate.position(x_plate)
#' plate_list
#'
#' ## Use unlist to flatten the list into vectors
#' unlist(plate_list, recursive = FALSE, use.names = FALSE)
#'
#' ## Or use flatten = TRUE to flatten all results
#' all.equal(generate.position(x_plate, flatten = TRUE),
#' unlist(plate_list, recursive = TRUE, use.names = FALSE)) ## TRUE

generate.position <- function(plate, flatten = FALSE) {
  if (!("plate_list" %in% class(plate))) {
    stop("Please use a matrix list from fill.plate()")
  }

  plate <- plate[["plate"]]

  position_list <- imap(plate, \(x, y) {
    position <- c(outer(rownames(x), colnames(x), FUN = \(i, j) {
      paste(i, j, sep = "")
    }))
    position <- paste(y, position, sep = ":")
    position <- position[c(x) != ""]
  })

  if (flatten) {
    position_list <- unlist(position_list, recursive = T, use.names = F)
  }

  return(position_list)
}
