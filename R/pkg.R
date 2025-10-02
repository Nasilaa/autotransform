# ============================================================================
# PACKAGE: autotransform
# Smart Data Type Detection and Transformation for R
# ============================================================================

# ----------------------------------------------------------------------------
# FILE: DESCRIPTION
# ----------------------------------------------------------------------------
#' @docType package
#' @name autotransform
#' @title Automatic Data Type Detection and Transformation with Rollback
#' @description Automatically detects and transforms ambiguous data types in
#'     data frames. Handles numeric strings, categorical
#'     variables, and more. Includes rollback functionality to revert
#'     transformations entirely or for specific columns. Perfect for cleaning
#'     messy datasets with mixed or unclear data types.
#' @author Abednego Nasila <alisanabednego@gmail.com>
#' @import lubridate
#' @importFrom readxl read_xlsx read_xls
#' @keywords internal
"_PACKAGE"

# Global environment to store transformation history
.at_history <- new.env(parent = emptyenv())

# ----------------------------------------------------------------------------
# FILE: R/type_detection.R
# ----------------------------------------------------------------------------

#' Check if column is mostly numeric
#'
#' @param x A vector
#' @param threshold Proportion threshold (default: 0.8)
#' @return Logical
#' @export
is_mostly_numeric <- function(x, threshold = 0.8) {
  if (is.numeric(x)) return(TRUE)
  if (!is.character(x)) return(FALSE)

  x_clean <- trimws(x)
  x_clean <- x_clean[!is.na(x_clean) & x_clean != ""]

  if (length(x_clean) == 0) return(FALSE)

  numeric_count <- sum(!is.na(suppressWarnings(as.numeric(x_clean))))
  return(numeric_count / length(x_clean) >= threshold)
}

#' Check if column is mostly integer
#'
#' @param x A vector
#' @param threshold Proportion threshold (default: 0.8)
#' @return Logical
#' @export
is_mostly_integer <- function(x, threshold = 0.8) {
  if (!is_mostly_numeric(x, threshold)) return(FALSE)

  if (is.numeric(x)) {
    x_numeric <- x[!is.na(x)]
  } else {
    x_clean <- trimws(as.character(x))
    x_clean <- x_clean[!is.na(x_clean) & x_clean != ""]
    x_numeric <- suppressWarnings(as.numeric(x_clean))
    x_numeric <- x_numeric[!is.na(x_numeric)]
  }

  if (length(x_numeric) == 0) return(FALSE)

  integer_count <- sum(abs(x_numeric - round(x_numeric)) < 0.01)
  return(integer_count / length(x_numeric) >= threshold)
}

#' Check if column is mostly categorical/factor
#'
#' @param x A vector
#' @param max_unique_ratio Maximum ratio of unique to total values (default: 0.5)
#' @param min_frequency Minimum frequency for most common value (default: 2)
#' @return Logical
#' @export
is_mostly_factor <- function(x, max_unique_ratio = 0.5, min_frequency = 2) {
  if (is.numeric(x)) return(FALSE)

  x_clean <- trimws(as.character(x))
  x_clean <- x_clean[!is.na(x_clean) & x_clean != ""]

  if (length(x_clean) == 0) return(FALSE)

  unique_ratio <- length(unique(x_clean)) / length(x_clean)
  freq_table <- table(x_clean)
  most_frequent <- max(freq_table)

  return(unique_ratio <= max_unique_ratio && most_frequent >= min_frequency)
}

#' Check if column is mostly logical/boolean
#'
#' @param x A vector
#' @param threshold Proportion threshold (default: 0.8)
#' @return Logical
#' @export
is_mostly_logical <- function(x, threshold = 0.8) {
  if (is.logical(x)) return(TRUE)

  if (is.numeric(x)) {
    x_clean <- x[!is.na(x)]
    if (length(x_clean) == 0) return(FALSE)
    binary_count <- sum(x_clean %in% c(0, 1))
    return(binary_count / length(x_clean) >= threshold)
  }

  if (!is.character(x)) return(FALSE)

  x_clean <- tolower(trimws(as.character(x)))
  x_clean <- x_clean[!is.na(x_clean) & x_clean != ""]

  if (length(x_clean) == 0) return(FALSE)

  boolean_values <- c("true", "false", "yes", "no", "y", "n", "1", "0",
                      "t", "f", "on", "off", "enabled", "disabled")

  boolean_count <- sum(x_clean %in% boolean_values)
  return(boolean_count / length(x_clean) >= threshold)
}

#' Check if column contains dates
#'
#' @param x A vector
#' @param threshold Proportion threshold (default: 0.6)
#' @return Logical
#' @export
is_mostly_date <- function(x, threshold = 0.6) {

  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return(TRUE)

  if (is.numeric(x)) return(FALSE)

  x_clean <- trimws(as.character(x))
  x_clean <- x_clean[!is.na(x_clean) & x_clean != ""]

  if (length(x_clean) == 0) return(FALSE)

  # Try unambiguous date patterns
  ymd_parsed <- suppressWarnings(lubridate::ymd(x_clean))
  successful_parses <- sum(!is.na(ymd_parsed))

  if (successful_parses == 0) {
    date_patterns <- c("%Y-%m-%d", "%Y/%m/%d", "%Y-%m-%d %H:%M:%S")

    for (pattern in date_patterns) {
      parsed <- suppressWarnings(as.Date(x_clean, format = pattern))
      successful_parses <- successful_parses + sum(!is.na(parsed))
    }
  }

  return(successful_parses / length(x_clean) >= threshold)

}

#' Check if column contains time information
#'
#' @param x A vector
#' @param threshold Proportion threshold (default: 0.6)
#' @return Logical
#' @export
is_mostly_time <- function(x, threshold = 0.6) {
  if (inherits(x, c("POSIXct", "POSIXlt"))) return(TRUE)
  if (is.numeric(x)) return(FALSE)

  x_clean <- trimws(as.character(x))
  x_clean <- x_clean[!is.na(x_clean) & x_clean != ""]

  if (length(x_clean) == 0) return(FALSE)

  time_pattern <- "\\b([01]?[0-9]|2[0-3]):[0-5][0-9](:[0-5][0-9])?\\b"
  time_matches <- sum(grepl(time_pattern, x_clean))

  datetime_matches <- sum(grepl("\\d{4}-\\d{2}-\\d{2}.*\\d{2}:\\d{2}", x_clean))

  total_matches <- time_matches + datetime_matches
  return(total_matches / length(x_clean) >= threshold)
}

#' Determine best data type for a column
#'
#' @param x A vector
#' @param col_name Column name for logging (optional)
#' @return Character string indicating recommended type
#' @keywords internal
determine_data_type <- function(x, col_name = "") {
  # Order matters - more specific checks first
  if (is_mostly_time(x)) {
    return("POSIXct")
  } else if (is_mostly_date(x)) {
    return("Date")
  } else if (is_mostly_logical(x)) {
    return("logical")
  } else if (is_mostly_integer(x)) {
    return("integer")
  } else if (is_mostly_numeric(x)) {
    return("numeric")
  } else if (is_mostly_factor(x)) {
    return("factor")
  } else {
    return("character")
  }
}

# ----------------------------------------------------------------------------
# FILE: R/transformation.R
# ----------------------------------------------------------------------------

#' Convert N/A strings to R's NA
#'
#' @param data A data frame
#' @return Data frame with N/A strings converted to NA
#' @keywords internal
convert_na_strings <- function(data) {
  na_patterns <- c("N/A", "NA", "n/a", "na", "N.A.", "n.a.", "#N/A")

  for (i in seq_along(data)) {
    if (is.character(data[[i]]) || is.factor(data[[i]])) {
      # Convert to character for consistent handling
      col_data <- as.character(data[[i]])
      # Replace N/A patterns with actual NA
      col_data[trimws(col_data) %in% na_patterns] <- NA
      # Convert back to original type if it was factor
      if (is.factor(data[[i]])) {
        data[[i]] <- as.factor(col_data)
      } else {
        data[[i]] <- col_data
      }
    }
  }

  return(data)
}

#' Safely convert data types
#'
#' @param x A vector to convert
#' @param target_type Target data type
#' @param col_name Column name for logging
#' @return Converted vector
#' @keywords internal
safe_convert <- function(x, target_type, col_name = "") {
  tryCatch({
    result <- switch(target_type,
                     "integer" = {
                       if (is.numeric(x)) {
                         as.integer(x)
                       } else {
                         as.integer(as.numeric(x))
                       }
                     },
                     "numeric" = {
                       if (is.numeric(x)) {
                         as.numeric(x)
                       } else {
                         as.numeric(x)
                       }
                     },
                     "factor" = as.factor(x),
                     "logical" = {
                       if (is.numeric(x)) {
                         as.logical(x)
                       } else {
                         x_clean <- tolower(trimws(as.character(x)))
                         ifelse(x_clean %in% c("true", "yes", "y", "1", "t", "on", "enabled"), TRUE,
                                ifelse(x_clean %in% c("false", "no", "n", "0", "f", "off", "disabled"), FALSE, NA))
                       }
                     },
                     "Date" = {
                       # Excel serial dates (numeric) - as.Date handles this automatically
                       if (is.numeric(x)) {
                         # Excel origin: December 30, 1899
                         excel_origin <- as.Date("1899-12-30")
                         return(as.Date(x, origin = excel_origin))
                       }

                       # String dates - try multiple formats
                       result <- suppressWarnings(lubridate::ymd(x))
                       if (sum(!is.na(result)) == 0) {
                         result <- suppressWarnings(lubridate::mdy(x))
                       }
                       if (sum(!is.na(result)) == 0) {
                         result <- suppressWarnings(lubridate::dmy(x))
                       }
                       if (sum(!is.na(result)) == 0) {
                         result <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
                       }
                       if (sum(!is.na(result)) == 0) {
                         result <- suppressWarnings(as.Date(x, format = "%Y/%m/%d"))
                       }
                       result
                     },
                     "POSIXct" = {
                       if (is.numeric(x)) {
                         excel_origin <- as.POSIXct("1899-12-30 00:00:00", tz = "UTC")
                         return(excel_origin + (x * 86400))
                       }

                       result <- suppressWarnings(lubridate::ymd_hms(x))
                       if (sum(!is.na(result)) == 0) {
                         result <- suppressWarnings(as.POSIXct(x))
                       }
                       result
                     },
                     "character" = as.character(x),
                     as.character(x)
    )
    return(result)
  }, error = function(e) {
    warning(paste("Failed to convert column", col_name, "to", target_type, ":", e$message))
    return(x)
  })
}

#' Transform data frame with automatic type detection
#'
#' @param data A data frame to transform
#' @param cols Character vector of column names to transform. If NULL (default),
#'     transforms all columns that need transformation.
#' @param transform_NAs If TRUE (default), converts "N/A", "NA", "n/a" strings to R's NA.
#'     Set to FALSE to preserve these as literal strings.
#' @param serialToStd Character vector of column names containing Excel serial dates
#'     to convert to standard Date format. If NULL (default), no forced conversion.
#'     Use this when you have numeric/integer columns that are actually Excel dates.
#' @param verbose If FALSE (default), returns transformed data frame directly.
#'     If TRUE, returns a list with data, original, log, and analysis components.
#' @return If verbose=FALSE: transformed data frame with rollback capability.
#'     If verbose=TRUE: a list containing transformed data and transformation history.
#' @export
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   id = c("1", "2", "3"),
#'   date = c(45232, 44982, 45000),
#'   active = c("yes", "no", "yes"),
#'   score = c("95", "87", "N/A")
#' )
#'
#' # Transform all columns (default, converts N/A to NA)
#' transformed_df <- at_transform(df)
#'
#' # Keep N/A as literal string
#' transformed_df <- at_transform(df, transform_NAs = FALSE)
#'
#' # Transform only specific columns
#' transformed_df <- at_transform(df, cols = c("date", "active"))
#'
#' # Force convert numeric column to Date (Excel serial dates)
#' transformed_df <- at_transform(df, serialToStd = "date")
#'
#' # Rollback transformations (works with verbose=FALSE too!)
#' original_df <- at_rollback(transformed_df)
#'
#' # Get full details with verbose=TRUE
#' result <- at_transform(df, verbose = TRUE)
#' transformed_df <- result$data
#' history <- result$log
#' }
#'
# Generate additional unique transform id for serial to std cols
# so that they're tracked as special since they're transformed twice
# Generate additional unique transform id for serial to std cols
# so that they're tracked as special since they're transformed twice
at_transform <- function(data, cols = NULL, transform_NAs = TRUE, serialToStd = NULL, verbose = FALSE) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }

  # Validate cols parameter
  if (!is.null(cols)) {
    if (!is.character(cols)) {
      stop("'cols' must be a character vector of column names")
    }
    missing_cols <- setdiff(cols, names(data))
    if (length(missing_cols) > 0) {
      stop(paste("Columns not found in data:", paste(missing_cols, collapse = ", ")))
    }
  }

  # Store TRULY ORIGINAL data BEFORE any conversions
  truly_original_data <- data

  # Track pre-conversion types for serialToStd columns
  serial_preconversion_info <- list()

  # Validate serialToStd parameter
  if (!is.null(serialToStd)) {
    if (!is.character(serialToStd)) {
      stop("'serialToStd' must be a character vector of column names")
    }
    missing_serial_cols <- setdiff(serialToStd, names(data))
    if (length(missing_serial_cols) > 0) {
      stop(paste("serialToStd columns not found in data:", paste(missing_serial_cols, collapse = ", ")))
    }

    # Capture TRUE original state before any conversion
    for (col in serialToStd) {
      original_class <- class(data[[col]])[1]
      original_type <- typeof(data[[col]])

      # Store the actual ambiguous state info
      serial_preconversion_info[[col]] <- list(
        original_class = original_class,
        original_type = original_type,
        original_data = data[[col]],  # Store before ANY conversion
        sample_values = head(data[[col]], 3)
      )

      if (verbose) {
        cat("Capturing pre-conversion state for '", col, "':\n",
            "  Class: ", original_class, "\n",
            "  Type: ", original_type, "\n",
            "  Sample: ", paste(head(data[[col]], 3), collapse = ", "), "\n", sep = "")
      }

      # Now do the conversion
      if (!is.numeric(data[[col]])) {
        data[[col]] <- as.Date(as.numeric(data[[col]]), origin = "1899-12-30")
      }
    }
  }

  # Store original data for rollback (after NA conversion but before analysis)
  original_data <- data

  # Handle N/A string conversion if requested
  if (transform_NAs) {
    data <- convert_na_strings(data)
  }

  # Analyze all columns
  analysis <- at_analyze(data, verbose = FALSE)

  # Filter analysis to only specified columns if cols is provided
  if (!is.null(cols)) {
    analysis <- analysis[analysis$Column %in% cols, ]
  }

  # Override recommended type for serialToStd columns
  if (!is.null(serialToStd)) {
    for (col in serialToStd) {
      idx <- which(analysis$Column == col)
      if (length(idx) > 0) {
        analysis$Recommended_Type[idx] <- "Date"
        analysis$Is_Excel_Date[idx] <- TRUE
        if (verbose) {
          cat("Column '", col, "' marked for Excel serial date conversion (serialToStd)\n", sep = "")
        }
      }
    }
  }

  # Apply transformations
  transformed_data <- data
  transformation_log <- list()
  serial_transform_ids <- list()  # Track special serial date transformations

  if (verbose) {
    cat("\nApplying transformations...\n")
    cat(paste(rep("=", 60), collapse = ""), "\n\n")
  }

  for (i in 1:nrow(analysis)) {
    col_name <- analysis$Column[i]
    current_type <- analysis$Current_Type[i]
    recommended_type <- analysis$Recommended_Type[i]

    # Transform if:
    # 1. Current type differs from recommended type, OR
    # 2. It's an Excel date that needs conversion (even if already numeric/integer), OR
    # 3. Column is in serialToStd list
    is_forced_serial <- !is.null(serialToStd) && col_name %in% serialToStd

    should_transform <- (current_type != recommended_type) ||
      (analysis$Is_Excel_Date[i] && recommended_type == "Date") ||
      is_forced_serial

    if (should_transform) {
      if (verbose) {
        if (is_forced_serial) {
          cat("Converting", col_name, "from", current_type, "to", recommended_type, "(forced via serialToStd)\n")
        } else if (analysis$Is_Excel_Date[i]) {
          cat("Converting", col_name, "from Excel serial date (", current_type, ") to", recommended_type, "\n")
        } else {
          cat("Converting", col_name, "from", current_type, "to", recommended_type, "\n")
        }
      }

      transformed_data[[col_name]] <- safe_convert(
        transformed_data[[col_name]],
        recommended_type,
        col_name
      )

      # Generate special tracking ID for serial-to-standard date conversions
      if (is_forced_serial || (analysis$Is_Excel_Date[i] && recommended_type == "Date")) {
        serial_id <- paste0("serial_", col_name, "_",
                            format(Sys.time(), "%Y%m%d_%H%M%S_"),
                            sample(1000:9999, 1))
        serial_transform_ids[[col_name]] <- serial_id

        if (verbose) {
          cat("  -> Special tracking ID for serial date:", serial_id, "\n")
        }
      }

      # Determine which original data to use
      if (col_name %in% names(serial_preconversion_info)) {
        # Use the TRULY original ambiguous data
        original_for_log <- serial_preconversion_info[[col_name]]$original_data
        original_type_for_log <- serial_preconversion_info[[col_name]]$original_class
      } else {
        # Use standard original data
        original_for_log <- original_data[[col_name]]
        original_type_for_log <- current_type
      }

      transformation_log[[col_name]] <- list(
        original_type = original_type_for_log,
        new_type = recommended_type,
        is_excel_date = analysis$Is_Excel_Date[i],
        is_serial_to_std = is_forced_serial || (analysis$Is_Excel_Date[i] && recommended_type == "Date"),
        serial_transform_id = serial_transform_ids[[col_name]],
        serial_preconversion_info = serial_preconversion_info[[col_name]],
        original_data = original_for_log  # Store the TRULY original ambiguous data
      )
    }
  }

  if (verbose) {
    cat("\n", length(transformation_log), "column(s) were transformed.\n")
    if (length(serial_transform_ids) > 0) {
      cat("  -", length(serial_transform_ids), "serial-to-standard date conversion(s) with special tracking\n")
    }
  }

  # Generate unique ID for this transformation
  transform_id <- paste0("transform_", format(Sys.time(), "%Y%m%d_%H%M%S_"),
                         sample(1000:9999, 1))

  # Store transformation history in global environment
  .at_history[[transform_id]] <- list(
    original = truly_original_data,  # Store the TRULY original data
    log = transformation_log,
    analysis = analysis,
    serial_transforms = serial_transform_ids,
    serial_preconversion_info = serial_preconversion_info,  # Store pre-conversion details
    timestamp = Sys.time()
  )

  # Return format depends on verbose setting
  if (verbose) {
    # Return full list with metadata
    result <- structure(
      list(
        data = transformed_data,
        original = truly_original_data,  # Return the TRULY original data
        log = transformation_log,
        analysis = analysis,
        transform_id = transform_id,
        serial_transform_ids = serial_transform_ids
      ),
      class = "at_transform"
    )
    return(result)
  } else {
    # Attach transform_id as attribute to data frame
    attr(transformed_data, "at_transform_id") <- transform_id
    attr(transformed_data, "at_serial_ids") <- serial_transform_ids
    attr(transformed_data, "at_transformed") <- TRUE
    class(transformed_data) <- c("at_data", class(transformed_data))
    return(transformed_data)
  }
}

# ----------------------------------------------------------------------------
# FILE: R/rollback.R
# ----------------------------------------------------------------------------

#' Rollback transformations
#'
#' @param data Either a data frame returned by at_transform() or a result
#'     object from at_transform(verbose = TRUE)
#' @param cols Column names to rollback (NULL = all columns)
#' @return Data frame with specified columns rolled back
#' @export
#' @examples
#' \dontrun{
#' # Works with verbose=FALSE (default)
#' transformed_df <- at_transform(df)
#' original_df <- at_rollback(transformed_df)
#'
#' # Also works with verbose=TRUE
#' result <- at_transform(df, verbose = TRUE)
#' original_df <- at_rollback(result)
#'
#' # Rollback specific columns
#' partial_rollback <- at_rollback(transformed_df, cols = c("date", "active"))
#' }
# Rollback with special handling for serial-to-standard date columns
at_rollback <- function(data, cols = NULL, to_intermediate = FALSE, verbose = TRUE) {
  transform_id <- NULL
  transformed_result <- NULL

  # Determine the source and extract transform_id
  if (inherits(data, "at_transform")) {
    # verbose=TRUE result
    transform_id <- data$transform_id
    transformed_result <- data
  } else if (inherits(data, "at_data")) {
    # verbose=FALSE result
    transform_id <- attr(data, "at_transform_id")
    if (is.null(transform_id)) {
      stop("Cannot find transformation history. Data may not be from at_transform().")
    }
    # Reconstruct result from stored history
    if (!exists(transform_id, envir = .at_history)) {
      stop("Transformation history not found. It may have been cleared.")
    }
    history <- .at_history[[transform_id]]
    transformed_result <- list(
      data = data,
      original = history$original,
      log = history$log,
      analysis = history$analysis,
      serial_transform_ids = history$serial_transforms
    )
  } else {
    stop("Input must be a result from at_transform()")
  }

  # Start with transformed data
  rolled_back <- transformed_result$data

  # Determine which columns to rollback
  if (is.null(cols)) {
    # Rollback all transformed columns
    cols_to_rollback <- names(transformed_result$log)
  } else {
    # Rollback only specified columns
    cols_to_rollback <- intersect(cols, names(transformed_result$log))

    if (length(cols_to_rollback) < length(cols)) {
      missing_cols <- setdiff(cols, names(transformed_result$log))
      warning(paste(
        "The following columns were not transformed and cannot be rolled back:",
        paste(missing_cols, collapse = ", ")
      ))
    }
  }

  # Track rollback details for serial date columns
  serial_rollback_info <- list()

  # Perform rollback
  for (col_name in cols_to_rollback) {
    log_entry <- transformed_result$log[[col_name]]

    # Check if this was a serial-to-standard date conversion
    is_serial_conversion <- !is.null(log_entry$is_serial_to_std) &&
      log_entry$is_serial_to_std

    if (is_serial_conversion) {
      # This column underwent two-step transformation
      original_type <- log_entry$original_type
      serial_id <- log_entry$serial_transform_id

      if (to_intermediate) {
        # Rollback to intermediate numeric state (after as.numeric, before as.Date)
        # This preserves the Excel serial number
        rolled_back[[col_name]] <- as.numeric(log_entry$original_data)

        serial_rollback_info[[col_name]] <- list(
          rollback_state = "intermediate_numeric",
          original_type = original_type,
          serial_id = serial_id,
          note = paste0("Rolled back to Excel serial number (numeric). ",
                        "Original type was: ", original_type)
        )

        if (verbose) {
          cat("Column '", col_name, "': Rolled back to intermediate numeric (Excel serial)\n",
              "  Original ambiguous type: ", original_type, "\n",
              "  Serial tracking ID: ", serial_id, "\n", sep = "")
        }
      } else {
        # Full rollback to original ambiguous state (text/int/numeric)
        rolled_back[[col_name]] <- log_entry$original_data

        serial_rollback_info[[col_name]] <- list(
          rollback_state = "original_ambiguous",
          original_type = original_type,
          serial_id = serial_id,
          note = paste0("Rolled back to original ambiguous state. ",
                        "Type: ", original_type,
                        " (was numeric/text/int before serial conversion)")
        )

        if (verbose) {
          cat("Column '", col_name, "': Rolled back to original ambiguous state\n",
              "  Original type: ", original_type, "\n",
              "  Serial tracking ID: ", serial_id, "\n",
              "  Note: This was a serial date (two-step conversion)\n", sep = "")
        }
      }
    } else {
      # Standard rollback for non-serial columns
      rolled_back[[col_name]] <- log_entry$original_data

      if (verbose) {
        cat("Column '", col_name, "': Rolled back from ",
            log_entry$new_type, " to ", log_entry$original_type, "\n", sep = "")
      }
    }
  }

  if (verbose) {
    cat("\nRolled back", length(cols_to_rollback), "column(s):",
        paste(cols_to_rollback, collapse = ", "), "\n")

    if (length(serial_rollback_info) > 0) {
      cat("\nSerial date columns rolled back:", length(serial_rollback_info), "\n")
      if (to_intermediate) {
        cat("  State: Intermediate numeric (Excel serial numbers)\n")
      } else {
        cat("  State: Original ambiguous format (text/int/numeric)\n")
      }
    }
  }

  # Attach rollback metadata for serial columns
  if (length(serial_rollback_info) > 0) {
    attr(rolled_back, "at_serial_rollback_info") <- serial_rollback_info
  }

  # Remove transformation attributes if rolling back all columns
  if (is.null(cols)) {
    attr(rolled_back, "at_transform_id") <- NULL
    attr(rolled_back, "at_serial_ids") <- NULL
    attr(rolled_back, "at_transformed") <- NULL
    class(rolled_back) <- setdiff(class(rolled_back), "at_data")
  }

  return(rolled_back)
}

# Helper function to inspect serial date rollback history
at_inspect_serial_rollback <- function(data) {
  info <- attr(data, "at_serial_rollback_info")

  if (is.null(info) || length(info) == 0) {
    cat("No serial date rollback information found.\n")
    return(invisible(NULL))
  }

  cat("Serial Date Rollback Information:\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")

  for (col_name in names(info)) {
    col_info <- info[[col_name]]
    cat("Column:", col_name, "\n")
    cat("  Rollback State:", col_info$rollback_state, "\n")
    cat("  Original Type:", col_info$original_type, "\n")
    cat("  Serial ID:", col_info$serial_id, "\n")
    cat("  Note:", col_info$note, "\n\n")
  }

  return(invisible(info))
}

# Helper function to get original type for serial columns
at_get_serial_original_type <- function(data, col_name) {
  transform_id <- attr(data, "at_transform_id")

  if (is.null(transform_id)) {
    stop("No transformation history found")
  }

  if (!exists(transform_id, envir = .at_history)) {
    stop("Transformation history not found in global environment")
  }

  history <- .at_history[[transform_id]]

  if (!col_name %in% names(history$log)) {
    stop(paste("Column", col_name, "not found in transformation log"))
  }

  log_entry <- history$log[[col_name]]

  if (!is.null(log_entry$is_serial_to_std) && log_entry$is_serial_to_std) {
    return(list(
      column = col_name,
      original_type = log_entry$original_type,
      is_excel_date = log_entry$is_excel_date,
      serial_id = log_entry$serial_transform_id,
      transformation_path = paste0(
        log_entry$original_type,
        " -> numeric (Excel serial) -> ",
        log_entry$new_type
      )
    ))
  } else {
    return(list(
      column = col_name,
      original_type = log_entry$original_type,
      is_excel_date = FALSE,
      transformation_path = paste0(log_entry$original_type, " -> ", log_entry$new_type)
    ))
  }
}

#' Get transformation history
#'
#' @param data Either a data frame returned by at_transform() or a result
#'     object from at_transform(verbose = TRUE)
#' @return Data frame summarizing transformations
#' @export
#' @examples
#' \dontrun{
#' # Works with both verbose=FALSE and verbose=TRUE
#' transformed_df <- at_transform(df)
#' history <- at_get_history(transformed_df)
#' }
at_get_history <- function(data) {
  transform_id <- NULL

  if (inherits(data, "at_transform")) {
    transform_id <- data$transform_id
  } else if (inherits(data, "at_data")) {
    transform_id <- attr(data, "at_transform_id")
  }

  if (is.null(transform_id)) {
    stop("Cannot find transformation history. Data may not be from at_transform().")
  }

  if (!exists(transform_id, envir = .at_history)) {
    stop("Transformation history not found. It may have been cleared.")
  }

  history_data <- .at_history[[transform_id]]

  if (length(history_data$log) == 0) {
    return(data.frame(
      Column = character(),
      Original_Type = character(),
      New_Type = character(),
      Is_Excel_Date = logical()
    ))
  }

  history <- data.frame(
    Column = names(history_data$log),
    Original_Type = sapply(history_data$log, function(x) x$original_type),
    New_Type = sapply(history_data$log, function(x) x$new_type),
    Is_Excel_Date = sapply(history_data$log, function(x) x$is_excel_date),
    stringsAsFactors = FALSE
  )

  return(history)
}

#' Clear transformation history
#'
#' @param transform_id Specific transformation ID to clear, or NULL to clear all
#' @export
#' @examples
#' \dontrun{
#' # Clear all transformation history
#' at_clear_history()
#'
#' # Clear specific transformation
#' at_clear_history(attr(transformed_df, "at_transform_id"))
#' }
at_clear_history <- function(transform_id = NULL) {
  if (is.null(transform_id)) {
    # Clear all history
    rm(list = ls(.at_history), envir = .at_history)
    cat("Cleared all transformation history\n")
  } else {
    if (exists(transform_id, envir = .at_history)) {
      rm(list = transform_id, envir = .at_history)
      cat("Cleared transformation history for:", transform_id, "\n")
    } else {
      warning("Transformation ID not found in history")
    }
  }
}

# ----------------------------------------------------------------------------
# FILE: R/analyze.R
# ----------------------------------------------------------------------------

#' Analyze data frame for type recommendations
#'
#' @param data A data frame to analyze
#' @param cols Character vector of column names to analyze. If NULL (default),
#'     analyzes all columns.
#' @param verbose Print analysis details (default: TRUE)
#' @return Data frame with type analysis for each column
#' @export
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   id = c("1", "2", "3"),
#'   date = c(45232, 44982, 45000),
#'   active = c("yes", "no", "yes")
#' )
#'
#' # Analyze all columns
#' analysis <- at_analyze(df)
#'
#' # Analyze specific columns
#' analysis <- at_analyze(df, cols = c("date", "active"))
#' }
at_analyze <- function(data, cols = NULL, verbose = TRUE) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }

  # Validate cols parameter
  if (!is.null(cols)) {
    if (!is.character(cols)) {
      stop("'cols' must be a character vector of column names")
    }
    missing_cols <- setdiff(cols, names(data))
    if (length(missing_cols) > 0) {
      stop(paste("Columns not found in data:", paste(missing_cols, collapse = ", ")))
    }
    # Filter data to only specified columns
    data <- data[, cols, drop = FALSE]
  }

  if (verbose) {
    cat("\nAnalyzing data types...\n")
    cat(paste(rep("=", 60), collapse = ""), "\n\n")
  }

  analysis <- data.frame(
    Column = names(data),
    Current_Type = sapply(data, function(x) class(x)[1]),
    Recommended_Type = character(ncol(data)),
    Is_Numeric = logical(ncol(data)),
    Is_Integer = logical(ncol(data)),
    Is_Factor = logical(ncol(data)),
    Is_Logical = logical(ncol(data)),
    Is_Date = logical(ncol(data)),
    Is_Excel_Date = logical(ncol(data)),
    Is_Time = logical(ncol(data)),
    Unique_Values = integer(ncol(data)),
    Missing_Count = integer(ncol(data)),
    Sample_Values = character(ncol(data)),
    stringsAsFactors = FALSE
  )

  for (i in 1:ncol(data)) {
    col_name <- names(data)[i]
    col_data <- data[[i]]

    #warning(paste('war',serialToStd))

    analysis$Recommended_Type[i] <- determine_data_type(col_data, col_name)
    analysis$Is_Numeric[i] <- is_mostly_numeric(col_data)
    analysis$Is_Integer[i] <- is_mostly_integer(col_data)
    analysis$Is_Factor[i] <- is_mostly_factor(col_data)
    analysis$Is_Logical[i] <- is_mostly_logical(col_data)
    analysis$Is_Date[i] <- is_mostly_date(col_data)
    analysis$Is_Time[i] <- is_mostly_time(col_data)
    analysis$Unique_Values[i] <- length(unique(col_data[!is.na(col_data)]))
    analysis$Missing_Count[i] <- sum(is.na(col_data))

    sample_vals <- head(unique(col_data[!is.na(col_data)]), 3)
    analysis$Sample_Values[i] <- paste(sample_vals, collapse = ", ")
  }

  if (verbose) {
    print(analysis[, c("Column", "Current_Type", "Recommended_Type",
                       "Is_Excel_Date", "Sample_Values")])
  }

  return(analysis)
}

# ----------------------------------------------------------------------------
# FILE: R/print_methods.R
# ----------------------------------------------------------------------------

#' Print method for at_transform objects
#' @param x A at_transform object
#' @param ... Additional arguments (unused)
#' @export
print.at_transform <- function(x, ...) {
  cat("autotransform transformation result\n")
  cat(paste(rep("=", 40), collapse = ""), "\n")
  cat("Transformed", length(x$log), "column(s)\n\n")

  if (length(x$log) > 0) {
    cat("Transformations:\n")
    for (col_name in names(x$log)) {
      if (x$log[[col_name]]$is_excel_date) {
        cat("  ", col_name, ":", x$log[[col_name]]$original_type, "->",
            x$log[[col_name]]$new_type, "(Excel serial date)\n")
      } else {
        cat("  ", col_name, ":", x$log[[col_name]]$original_type, "->",
            x$log[[col_name]]$new_type, "\n")
      }
    }
  }

  cat("\nAccess components:\n")
  cat("  $data      - Transformed data frame\n")
  cat("  $original  - Original data frame\n")
  cat("  $log       - Transformation log\n")
  cat("  $analysis  - Type analysis\n")

  cat("\nUse at_rollback() to undo transformations\n")
}

#' Print method for at_data objects
#' @param x A at_data object
#' @param ... Additional arguments passed to default print
#' @export
print.at_data <- function(x, ...) {
  cat("Transformed data frame (use at_rollback() to undo transformations)\n")
  transform_id <- attr(x, "at_transform_id")
  if (!is.null(transform_id) && exists(transform_id, envir = .at_history)) {
    history <- .at_history[[transform_id]]
    cat("Transformed columns:", length(history$log), "\n")
    cat("Transformation time:", format(history$timestamp, "%Y-%m-%d %H:%M:%S"), "\n\n")
  }
  # Remove class temporarily to print as regular data frame
  class(x) <- setdiff(class(x), "at_data")
  print(x, ...)
}


