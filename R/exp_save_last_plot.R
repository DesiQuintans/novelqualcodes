
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
#' \dontrun{
#' plot_novelty(interview_scores)
#'
#' save_last_plot("my_file.png", "8 x 6 in")
#' }
#'
#' @seealso [plot_novelty()], [plot_richness()]
#'
#' @md
save_last_plot <- function(filename, size, dpi = 300, ...) {
    split_size <- unlist(regmatches(size,
                                    regexec("([0-9.]+)\\s*(x|\\s)\\s*([0-9.]+)\\s*(.+)",
                                            size)))

    ggplot2::ggsave(filename = filename, plot = ggplot2::last_plot(),
                    width = as.numeric(split_size[2]), height = as.numeric(split_size[4]),
                    units = split_size[5], dpi = dpi, ...)
}
