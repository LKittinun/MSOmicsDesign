#' @title Include QC samples in sample sequence
#' @description
#' Include QC samples by repeated interval or at specified positions in a sample sequence.
#'
#' @param samples A vector containing sample names
#' @param interval An interval at which QC samples should be inserted. Specify only either `interval` or `position`.
#' @param position A vector of positions at which QC samples should be inserted. The position can contain duplicated value, it will add QC next to each other. Specify only either `interval` or `position`.
#' @param permute If TRUE will randomize an order within each QC block. Can only be used with `interval`.
#' @param replace If `TRUE` and `position` is specified, samples at the given
#'   positions are replaced by QC labels in-place (sequence length unchanged).
#'   If `FALSE` (default), QC labels are inserted as additional entries
#'   adjacent to the specified positions, increasing the sequence length.
#'   `replace` is ignored when `interval` is used.
#' @param label Prefix of added QC
#'
#' @return A vector containing added QC
#' @family plate
#'
#' @export
#'
#' @examples
#' samples <- LETTERS[1:20]
#' addQC(samples, interval = 5)
#' addQC(samples, interval = 5, permute = TRUE)
#' addQC(samples, position = c(3, 9), replace = TRUE)
#' addQC(samples, position = c(rep(3,5), rep(9,10)))

addQC <- function(
  samples = NULL,
  interval = NULL,
  position = NULL,
  replace = FALSE,
  permute = FALSE,
  label = "QC"
) {
  if (!is.vector(samples)) {
    samples <- try(as.character(samples))
  }

  stopifnot(
    "Please specify a vector of sample sequence." = is.vector(samples),
    "Please specify interval or position" = any(
      !is.null(interval),
      !is.null(position)
    ),
    "Please specify only interval or position" = !all(
      !is.null(interval),
      !is.null(position)
    ),
    "Interval cannot be longer than length of samples" = is.null(interval) ||
      (interval <= length(samples)),
    "Position cannot be longer than length of samples" = is.null(position) ||
      all(position <= length(samples))
  )

  if (permute) {
    stopifnot("Permute can only be used with interval" = is.null(position))
  }

  if (!is.null(interval)) {
    stopifnot(
      "Interval must be a positive integer" = interval > 0 &&
        interval == floor(interval) &&
        length(interval) == 1
    )

    samplist <- split(
      samples,
      findInterval(
        seq_along(samples),
        1:floor(length(samples) / interval) * interval + 1
      )
    )

    sampQC <- unlist(
      mapply(
        \(x, y) {
          if (permute) {
            sample(c(x, y))
          } else {
            c(x, y)
          }
        },
        samplist,
        paste0(label, "_", 1:length(samplist)),
        SIMPLIFY = F
      ),
      use.names = F
    )
  } else if (!is.null(position)) {
    if (replace) {
      stopifnot(
        "Position must be positive integers" = all(position > 0) &&
          all(position == floor(position))
      )

      sampQC <- samples
      for (pos in seq_along(position)) {
        sampQC[position[pos]] <- paste0(label, "_", pos)
      }
    } else {
      stopifnot(
        "Position must be positive integers or zero" = all(position >= 0) &&
          all(position == floor(position))
      )

      sampQC <- samples
      position <- sort(position, decreasing = TRUE, index.return = TRUE)
      x <- position$x
      ix <- position$ix
      dup_position <- x[duplicated(x) | duplicated(x, fromLast = TRUE)]

      if (length(dup_position) > 0) {
        for (dup in unique(dup_position)) {
          dup_pos <- which(x == dup)
          ix[dup_pos] <- rev(ix[dup_pos])
        }
      }
      for (pos in seq_along(x)) {
        sampQC <- append(sampQC, paste0(label, "_", ix[pos]), after = x[pos])
      }
    }
  }
  return(sampQC)
}
