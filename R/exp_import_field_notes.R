
#' Import field notes from an Excel spreadsheet
#'
#' @aliases import_fieldnotes
#'
#' @description
#' 'Field notes' in this context is a spreadsheet that records the refinements
#' that a researcher makes throughout their interview process. This package is
#' opinionated about what these field notes should look like: use [create_field_notes_template()]
#' to get a template for what the package accepts.
#'
#' @param path (Character) The full path (including filename) to the Excel spreadsheet.
#' @param ... Other named arguments that will be passed to [readxl::read_excel()].
#'
#' @return A named list of class `field_notes`.
#' @export
#'
#' @examples
#' \dontrun{
#' my_interviews <- import_coding_matrices("C:/path/to/folder/of/interviews")
#' my_codes <- score_codes(my_interviews)
#' my_notes <- import_field_notes("C:/path/to/notes.xlsx")
#'
#' # Demonstrating usage in plots
#' plot_novelty(my_codes, refinements = my_notes)
#' plot_richness(my_codes, refinements = my_notes)
#'}
#'
#' @md
import_field_notes <- function(path, ...) {
    if (!file.exists(path))
        stop("This file does not exist:\n", path)

    fieldnotes <- readxl::read_excel(path = path,
                                     col_names = TRUE,
                                     ...)

    result <- list(ref_points = as.integer(unique(fieldnotes[[1]])),
                   df = fieldnotes)

    class(result) <- "field_note"

    return(result)
}

#' @rdname import_field_notes
#' @export
import_fieldnotes <- import_field_notes
