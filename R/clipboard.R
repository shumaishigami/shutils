#' Read data.frame from a clipboard
#' @export
read_from_clip <- function(sep = "\t", ...) {
  read.table("clipboard", sep = sep, stringsAsFactors = FALSE, header = TRUE, ...)
}

#' Write data.frame to a clipboard
#'
#' @param data data.frame
#' @param ... options for write.table
#'
#' @export
write_to_clip <- function(data, ...) {
  write.table(data, paste0("clipboard-", Inf), sep = "\t", row.names = FALSE, ...)
}

#' Write column names of a data.frame to a clipboard
#'
#' @param data data.frame
#'
#' @examples
#' clip_cols(iris)
#' # Ctrl + V
#' # Sepal.Length
#' # Sepal.Width
#' # Petal.Length
#' # Petal.Width
#' # Species
#'
#' clip_cols(iris, ", ")
#' # Paste the clipboard
#' # Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, Species
#'
#' @export
clip_cols <- function(data, collapse = "\n") writeClipboard(paste(names(data), collapse = collapse))


#' Normalize path in clipboard
#' @export
normalize_clippath <- function(winslash = "/") writeClipboard(normalizePath(readClipboard(), winslash = winslash))
