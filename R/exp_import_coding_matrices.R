
#' Read exported NVivo Coding Matrices from a folder
#'
#' @description
#' Coding matrices are built inside NVivo's _Matrix Coding Query_ tool, with codes
#' as rows and one participant ("case") as column. These files should be exported as
#' Excel spreadsheets (XLS or XLSX format), which is the default for NVivo.
#'
#' Filenames **must** reflect the chronological order of interviews when they are
#' sorted. You can do this by naming them in sequence like _"Interview 07 PID 2345"_,
#' or by including a YMD HM timestamp like _"2023-06-17 1345"_. Sorting is number-aware
#' and only uses the filename itself (i.e. file path is ignored during sorting).
#'
#' @param path (Character) Path to a folder that contains coding matrices
#'    exported from NVivo (_Explore → Matrix Coding Query → Export Coding Matrix_). All
#'    files with _.XLS_ or _.XLSX_ extensions will be imported.
#' @param recursive (Logical) If `TRUE`, also imports files inside subfolders of `path`.
#'
#' @return A list of dataframes.
#' @export
#'
#' @md
import_coding_matrices <- function(path, recursive = FALSE) {
    # Get all Excel files in the path
    flist <- list.files(path,
                        pattern = "\\.(xls|xlsx)$",
                        ignore.case = TRUE,
                        recursive = recursive,
                        include.dirs = FALSE,
                        full.names = TRUE)

    # Use file basename for sorting instead of the full path, or else files in
    # subfolders will become mis-sorted.
    # naturalorder() is essential; sort() is not number-aware and puts
    # "int1" and "int10" together.
    flist <- flist[naturalsort::naturalorder(basename(flist))]

    # Import sorted file list. When the codes are counted, it will iterate on
    # each interview in their proper chronological order.
    result <-
        Map(function(this_file) {
                df <- readxl::read_excel(this_file, col_types = "text", .name_repair = "minimal")

                if (ncol(df) != 2) {
                    # Guard against multi-person export.
                    stop(sprintf("The input file '%s' has %i columns, but it should only have 2 columns.\nHave you exported more than one participant in this file?\nExpecting only 2 columns: #1 for codes, and #2 for counts.",
                                 path, ncol(df)))
                }

                colnames(df) <- c("code", "freq")

                df$code <- gsub("^\\d+\\s*:\\s*", "", df$code)  # Remove Nvivo-generated ID numbers at start of codes.

                df <- df[(df$freq > 0) & !is.na(df$freq), ]  # Remove codes that are unused
            },
            flist)

    # TODO: Announce the basenames here so the user can double-check them.

    return(result)
}
