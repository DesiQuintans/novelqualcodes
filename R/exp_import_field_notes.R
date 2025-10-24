
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
#' # An example field notes spreadsheet included with the package.
#' path_to_notes <- system.file("insect_study/records/refinements.xlsx", package = "novelqualcodes")
#' print(path_to_notes)
#'
#' # Importing the spreadsheet
#' my_refinements <- import_field_notes(path_to_notes)
#'
#' # Looking at its contents
#' str(my_refinements, max.level = 1)
#' print(my_refinements$df)
#'
#' @md
import_field_notes <- function(path, ...) {
    if (!file.exists(path))
        stop("This file does not exist:\n", path)

    fieldnotes <- readxl::read_excel(path = path,
                                     col_names = TRUE,
                                     ...)

    result <- list(ref_points = suppressWarnings(as.integer(unique(fieldnotes[[1]]))),
                   df = fieldnotes)

    stopifnot(
        "The `interview_num` column in your field notes is not numeric.\n  Expecting a set of numbers describing which interviews had refinements, e.g. 1, 3, 6, 11..." =
            all(is.na(result$ref_points)) == FALSE
    )

    class(result) <- "field_notes"

    return(result)
}

#' @rdname import_field_notes
#' @export
import_fieldnotes <- import_field_notes
