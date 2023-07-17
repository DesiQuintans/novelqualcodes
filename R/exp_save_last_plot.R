
#' Save the most recent plot to a file
#'
#' @param filename (Character) The path and filename of the file to create.
#' @param size (Character) The output size of the file, in the form `"width x height unit`. For example:
#'      - `"5 x 7 in"`
#'      - `"12 x 8 cm"`
#'      - `"300 x 150 mm"`
#'      - `"1920 x 1080 px"`
#' @param dpi (Integer) The resolution (dots per inch) of the output file.
#' @param ... Other arguments passed to [ggplot2::ggsave()].
#'
#' @return This function returns nothing, but has the side-effect of writing a file to `filename`.
#' @export
#'
#' @examples
#' \donttest{
#' # Coding matrices included with the package
#' path_to_matrices <- system.file("insect_study/matrices/", package = "novelqualcodes")
#'
#' # Import the data
#' my_matrices    <- import_coding_matrices(path_to_matrices)
#'
#' # Score novel and duplicate codes
#' my_scores <- score_codes(my_matrices)
#'
#' # Generate a plot with no refinements
#' plot_richness(score_df = my_scores)
#'
#' # Save it to a temporary directory
#' save_last_plot(file.path(tempdir(), "test_plot.png"), size = "4 x 3 in")
#'
#' # Open the temporary directory (if session is running interactively)
#' if (interactive()) { utils::browseURL(tempdir()) }
#' }
#'
#' @seealso [plot_novelty()], [plot_richness()]
#'
#' @md
save_last_plot <- function(filename = stop("A save path and filename must be specified."),
                           size = "4 x 3 in", dpi = 300, ...) {
    split_size <- unlist(regmatches(size,
                                    regexec("([0-9.]+)\\s*(x|\\s)\\s*([0-9.]+)\\s*(.+)",
                                            size)))

    ggplot2::ggsave(filename = filename, plot = ggplot2::last_plot(),
                    width = as.numeric(split_size[2]), height = as.numeric(split_size[4]),
                    units = split_size[5], dpi = dpi, ...)
}
