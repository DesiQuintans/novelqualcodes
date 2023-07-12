
#' Create a template for refinement field notes
#'
#' @aliases create_fieldnotes_template
#'
#' @description
#' It is good practice to record when refinements were made, what was done, and
#' the reasons behind them. These "field notes" are used by this package to
#' annotate plots created by [plot_novelty()] and [plot_richness()], and they're
#' also requested by peer reviewers as part of the publication process.
#'
#' This template contains 5 columns (currently only the first one, `interview_num`,
#' is actually used by the package, but this is subject to change):
#'
#' 1. `interview_num` is the upcoming interview number where these refinements
#'    will take effect.
#' 2. `refinement_type` is free text describing the kind of refinement,
#'    e.g. "add" or "rephrase" or "remove".
#' 3. `refinement` is the actual text that has been changed.
#' 4. `reason` describes the rationale behind the refinement.
#' 5. `other` is for additional information you may want to include.
#'
#' @param path (Character) The path where the field notes template should
#'     be created.
#'
#' @return Invisibly returns the path to the template (Character). In an
#'     interactive session, also opens `path` in the system's file viewer.
#' @export
#'
#' @examples
#' \donttest{
#' # Create the template in a temporary directory.
#' create_field_notes_template(path = tempdir())
#' }
#'
#' @md
create_field_notes_template <- function(path) {
    # Note that writing a .xlsx file from R can't be done without the `xlsx`
    # package, which is Java-based and may introduce some headaches because of
    # that requirement. Instead, I've pre-made a template in inst/template/ and
    # we simply copy that to wherever the user wants it.

    if (length(path) > 1)
        stop("'path' should only have one path in it (a length of 1).")

    dir.create(path, showWarnings = FALSE, recursive = TRUE)

    if (!dir.exists(path))
        stop("Unable to create '", path, "', or it does not exist. Does R have
             permission to write to this path?")

    template_loc <- system.file("template", "fieldnotes_template.xlsx",
                                package = "novelqualcodes",
                                mustWork = TRUE)

    file.copy(from = template_loc, to = path, overwrite = TRUE)

    if (!file.exists(file.path(path, "fieldnotes_template.xlsx")))
        stop("Failed to create the template in '", path, "'. Does R have
             permission to write to this path?")

    message("Success! Field notes template was created in\n", file.path(path, "fieldnotes_template.xlsx"),
            "'.")

    if (interactive())
        utils::browseURL(path)

    return(invisible(file.path(path, "fieldnotes_template.xlsx")))
}


#' @rdname create_field_notes_template
#' @export
create_fieldnotes_template <- create_field_notes_template
