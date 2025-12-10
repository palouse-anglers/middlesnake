#' Clean and Consolidate CPDES BMP Implementation Records
#'
#' This function reads a BMP dataset (Excel file), standardizes column names,
#' identifies duplicates based on participant, practice, and measurement,
#' and consolidates true duplicates with identical values while summing others.
#'
#' @param dataset_path A character string path to the `.xlsx` file
#'
#' @return A cleaned data frame with duplicates removed or consolidated
#'
#' @importFrom readxl read_xlsx
#' @importFrom janitor clean_names
#' @importFrom dplyr group_by ungroup mutate summarise slice_head row_number if_else select filter
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning cli_alert_danger
#' @export
clean_bmp_data <- function(dataset_path) {
  # Ensure required packages are installed
  required_packages <- c("cli", "readxl", "janitor", "dplyr")
  missing <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Missing required packages: ", paste(missing, collapse = ", "))
  }

  # Validate dataset path
  if (!file.exists(dataset_path)) {
    cli::cli_alert_danger("The file {.file {dataset_path}} does not exist.")
    stop("Invalid file path.")
  }

  cli::cli_alert_info("Reading and cleaning BMP dataset from {.file {dataset_path}}...")


  if (grepl("\\.xlsx$", dataset_path, ignore.case = TRUE)) {
    bmp_data <- readxl::read_xlsx(dataset_path) %>%
      janitor::clean_names()
  } else if (grepl("\\.csv$", dataset_path, ignore.case = TRUE)) {
    bmp_data <- readr::read_csv(dataset_path) %>%
      janitor::clean_names()
  } else {
    stop("Unsupported file type: must be .csv or .xlsx")
  }


  # Rename columns for clarity (optional, can keep as-is)
  bmp_data <- bmp_data %>%
    dplyr::rename(
      participant_id = participant_id,
      bmp_name = bmp_name,
      measurement = measurements,
      value = value,
      latitude = property_latitude,
      longitude = property_longitude
    )

  # ---- Identify duplicates ----
  cli::cli_alert_info("Identifying duplicates by participant, BMP, and measurement...")

  bmp_data <- bmp_data %>%
    dplyr::group_by(participant_id, bmp_name, measurement) %>%
    dplyr::mutate(duplicate_flag = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(participant_id, bmp_name, measurement, value) %>%
    dplyr::mutate(duplicate_value_flag = dplyr::row_number()) %>%
    dplyr::ungroup()

  # Flag rows that are true duplicates
  bmp_data <- bmp_data %>%
    dplyr::mutate(
      flagged_for_removal = dplyr::if_else(
        duplicate_flag > 1 & duplicate_value_flag > 1, 1L, 0L
      )
    )

  total_rows <- nrow(bmp_data)
  flagged <- sum(bmp_data$flagged_for_removal == 1)

  cli::cli_alert_warning("⚠️ True duplicates flagged for removal: {.strong {flagged}}")
  cli::cli_alert_info("Total rows in dataset before cleaning: {.strong {total_rows}}")

  # Remove true duplicates
  bmp_data_clean <- bmp_data %>%
    dplyr::filter(flagged_for_removal < 1)

  # Now consolidate remaining groups by summing values where needed
  bmp_data_final <- bmp_data_clean %>%
    dplyr::group_by(participant_id, bmp_name, measurement, units, latitude, longitude) %>%
    dplyr::mutate(group_total_value = sum(value, na.rm = TRUE)) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(value = group_total_value) %>%
    dplyr::select(-group_total_value, -duplicate_flag, -duplicate_value_flag, -flagged_for_removal)

  cli::cli_alert_success("✅ Cleaned dataset contains {.strong {nrow(bmp_data_final)}} rows.")

  return(bmp_data_final)
}
