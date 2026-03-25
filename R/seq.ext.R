#' @title Extend sequence to max value
#' @description
#' This function extends a sequence to the maximum value and allows the option to remove the last element from the sequence.
#'
#' @param from `from` for seq
#' @param to `to` for seq
#' @param by `by` for seq
#' @param ... other arguments passed to native `seq`
#' @param rm.last whether to remove last value before adding max value
#'
#' @return
#' A sequence with extension to max value
#'
#' @importFrom utils tail
#' @family utils
#' @export
#'
#' @examples
#' seq.ext(1,9,3)
#' seq.ext(1,9,3, rm.last = TRUE)

seq.ext <- function(
  from = 1,
  to = 1,
  by = ((to - from) / (length.out - 1)),
  ...,
  rm.last = FALSE
) {
  extra_args <- list(...)

  seq_v <- do.call(seq, c(list(from = from, to = to, by = by), extra_args))

  if ((to != tail(seq_v, 1)) && (!rm.last)) {
    seq_v <- c(seq_v, to)
  } else if ((to != tail(seq_v, 1)) && (rm.last)) {
    seq_v <- c(seq_v[-length(seq_v)], to)
  }

  return(seq_v)
}
