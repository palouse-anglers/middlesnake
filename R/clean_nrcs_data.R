#' Clean and Deduplicate NRCS Data from an Excel Workbook
#'
#' This function reads an Excel file containing NRCS practice data, standardizes column names,
#' checks whether the user-supplied HUC12 watershed codes are present, flags duplicated practice
#' entries, and returns a cleaned, deduplicated dataset with total applied amounts per group.
#'
#' Duplicate records are defined in two stages:
#' 1. By `land_unit_id`, `practice_code`, and `applied_year` (`duplicates_id_yr_code`)
#' 2. By the same fields plus `applied_amount` (`duplicates_id_yr_code_amt`)
#'
#' If multiple records exist with the same identifiers, they are flagged, summed, and collapsed
#' to a single row per group. All variables from the first record in each group are retained.
#' CLI messages summarize dataset dimensions, duplicate detection, and rows flagged for removal.
#'
#' @param dataset_path A character string indicating the path to the `.xlsx` Excel file to read.
#' @param huc_12_codes A character vector of HUC12 watershed codes to verify against the dataset.
#'
#' @return A `data.frame` containing:
#' \itemize{
#'   \item Cleaned and standardized variable names
#'   \item Duplicate detection variables: `duplicates_id_yr_code`, `duplicates_id_yr_code_amt`
#'   \item Duplicated rows, each representing a unique `land_unit_id`–`practice_code`–`applied_year`
#'   \item `applied_amount` updated to reflect the total for grouped duplicates
#' }
#'
#' @details
#' The original dataset names will be cleaned to lowercase snake_case
#' using `janitor::clean_names()`.
#'
#' @importFrom readxl read_xlsx
#' @importFrom janitor clean_names
#' @importFrom dplyr %>% group_by ungroup mutate summarise slice_head row_number if_else select
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning cli_alert_danger cli_ul
#'
#' @examples
#' \dontrun{
#' clean_nrcs_data(
#'   dataset_path = "data/nrcs_practices.xlsx",
#'   huc_12_codes = c("170601010101", "170601010102")
#' )
#' }
#'
#' @export


clean_nrcs_data <- function(dataset_path, huc_12_codes) {
  # Ensure required packages are installed
  required_packages <- c("cli", "readxl", "janitor", "dplyr")
  missing <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if (length(missing) > 0) {
    stop("The following required packages are not installed: ", paste(missing, collapse = ", "))
  }

  # Validate dataset path
  if (!file.exists(dataset_path)) {
    cli::cli_alert_danger("The file {.file {dataset_path}} does not exist.")
    stop("Invalid file path.")
  }

  if (!grepl("\\.xlsx?$", dataset_path, ignore.case = TRUE)) {
    cli::cli_alert_danger("The file {.file {dataset_path}} is not an Excel (.xlsx) file.")
    stop("Invalid file type.")
  }

  cli::cli_alert_info("Reading and cleaning dataset from {.file {dataset_path}}...")

  # Read and clean data
  nrcs_data <- readxl::read_xlsx(dataset_path) %>%
    janitor::clean_names()

  # Confirm expected column exists
  if (!"huc12" %in% names(nrcs_data)) {
    cli::cli_alert_danger("The dataset does not contain a 'huc12' column after cleaning.")
    stop("Missing 'huc12' column.")
  }


  # Check for missing HUC12 codes
  available_hucs <- unique(as.character(nrcs_data$huc12))
  huc_12_codes <- as.character(huc_12_codes)
  missing_hucs <- setdiff(huc_12_codes, available_hucs)

  if (length(missing_hucs) == 0) {
    cli::cli_alert_success("All {.strong {length(huc_12_codes)}} provided HUC12 codes are present in the dataset.")
  } else {
    cli::cli_alert_warning("Some HUC12 codes were not found in the dataset:")
    cli::cli_ul(missing_hucs)
  }


  # ---- Identify duplicate practices ----
  cli::cli_alert_info("🐍 Middlesnake is hunting for duplicates...")

  duplicated_practices <- nrcs_data %>%
    dplyr::filter(huc12 %in% huc_12_codes) %>%
    dplyr::group_by(land_unit_id, practice_code, applied_year) %>%
    dplyr::mutate(duplicates_id_yr_code = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(land_unit_id, practice_code, applied_year, applied_amount) %>%
    dplyr::mutate(duplicates_id_yr_code_amt = dplyr::row_number()) %>%
    dplyr::ungroup()

  # Row counts
  total_rows <- nrow(duplicated_practices)
  dup_code_count <- sum(duplicated_practices$duplicates_id_yr_code > 1, na.rm = TRUE)
  dup_amt_count <- sum(duplicated_practices$duplicates_id_yr_code_amt > 1, na.rm = TRUE)

  cli::cli_alert_info("Total rows in dataset: {.strong {total_rows}}")
  cli::cli_alert_info("Rows with duplicates by land unit id, practice, and year: {.strong {dup_code_count}}")
  cli::cli_alert_info("Rows with duplicates by land unit id, practice, year, and amount: {.strong {dup_amt_count}}")

  # Flag rows where both duplication types occur
  duplicated_practices <- duplicated_practices %>%
    dplyr::mutate(
      flagged_for_removal = dplyr::if_else(
        duplicates_id_yr_code > 1 & duplicates_id_yr_code_amt > 1,
        1L,
        0L
      )
    )

  # Count flagged rows
  flagged_count <- sum(duplicated_practices$flagged_for_removal == 1, na.rm = TRUE)

  cli::cli_alert_warning("⚠️ Rows removed (dupes by land unit id, practice, year, and amount): {.strong {flagged_count}}")

  duplicated_practices <- duplicated_practices %>%
   filter(flagged_for_removal < 1)

  total_rows <- nrow(duplicated_practices)

  cli::cli_alert_info("Rows remaining in dataset: {.strong {total_rows}}")

  cli::cli_alert_info("🧹 Removing duplicate rows while summing applied_amount...")

  nrcs_final <- duplicated_practices %>%
    dplyr::group_by(land_unit_id, practice_code, applied_year) %>%
    dplyr::mutate(group_total_applied_amount = sum(applied_amount, na.rm = TRUE)) %>%
    dplyr::slice_head(n = 1) %>%  # Keep one row per group
    dplyr::ungroup() %>%
    dplyr::mutate(applied_amount = group_total_applied_amount) %>%
    dplyr::select(-group_total_applied_amount)

  cli::cli_alert_success("✅ final dataset contains {.strong {nrow(nrcs_final)}} rows.")

  return(nrcs_final)

}
