
#' Read exported NVivo Coding Matrices from a folder
#'
#' @description
#' Coding matrices are built inside NVivo's _Matrix Coding Query_ tool, with codes
#' as rows and one participant ("case") as column. These files should be exported as
#' Excel spreadsheets (XLS or XLSX format), which is the default for NVivo. There
#' must only be one participant per file.
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
#' @seealso [score_codes()]
#'
#' @md
import_coding_matrices <- function(path, recursive = FALSE) {
    # Get all Excel files in the path
    flist <- list.files(path,
                        pattern      = "(xlsx|xls)$",
                        all.files    = TRUE,
                        full.names   = TRUE,
                        recursive    = recursive,
                        ignore.case  = TRUE,
                        include.dirs = FALSE,
                        no..         = TRUE)

    # Use file basename for sorting instead of the full path, or else files in
    # subfolders will become mis-sorted.
    # naturalorder() is essential; sort() is not number-aware and puts
    # "int1" and "int10" together.
    filenames <- basename(flist)
    flist <- flist[naturalsort::naturalorder(filenames)]

    num_files <- length(flist)

    message()  # Newline

    result <- vector(mode = "list", length = num_files)

    for (i in seq_along(flist)) {
        this_file <- flist[i]

        msg_format <- sprintf("(%%%1$i.0i / %%%1$i.0i) Reading %%s", nchar(num_files))
        message(sprintf(msg_format, i, num_files, basename(this_file)))

        df <- readxl::read_excel(this_file, col_types = "text", .name_repair = "minimal")

        if (ncol(df) != 2) {
            # Guard against multi-person export.
            stop(sprintf("The input file '%s' has %i columns, but it should only have 2 columns.\nHave you exported more than one participant in this file?\nExpecting only 2 columns: #1 for codes, and #2 for counts.",
                         path, ncol(df)))
        }

        colnames(df) <- c("code", "freq")

        df$code <- gsub("^\\d+\\s*:\\s*", "", df$code)  # Remove Nvivo-generated ID numbers at start of codes.

        df <- df[(df$freq > 0) & !is.na(df$freq), ]  # Remove codes that are unused

        # 1. NVivo turns folders into codes (but those codes are never used)
        # 2. Removes codes that were used by others in the future


        result[[i]] <- df
    }

    message("\n", length(result), " files were read.")

    # Also double-check for Excel sheets in subdirectories.
    if (recursive == FALSE) {
        nested_files <- list.files(path,
                                   pattern      = "\\.(xls|xlsx)$",
                                   all.files    = TRUE,
                                   full.names   = TRUE,
                                   recursive    = TRUE,
                                   ignore.case  = TRUE,
                                   include.dirs = FALSE,
                                   no..         = TRUE)

        nested_files <- setdiff(nested_files, flist)  # Files in the recursive search that were not in the original search.

        num_others <- length(nested_files)

        if (num_others > 0) {
            message("\n")

            warning(sprintf("Additionally, %i Excel file(s) were found in sub-directories but not imported:\n\n%s%s\n\nIf you wanted these, then set 'recursive = TRUE'.\nIf you didn't want them, then ignore this warning.",
                            num_others,
                            paste(paste("- ", utils::head(nested_files, 6)), collapse = "\n"),
                            ifelse(num_others > 6, sprintf("\n... and %i others.", num_others - 6), "")
                            ),
                    call. = FALSE)

        }
    }

    return(result)
}
