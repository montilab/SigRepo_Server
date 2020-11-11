#' @title copyFile
#' @param input_file input file object
#' @param destination target directory
#' @export
copyFile <- function(input_file, destination) {
  # check if input is empty or not. if empty, do nothing.
  if (input_file != "") {
    file.copy(input_file, destination)
  }
}
