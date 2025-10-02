# autotransform

<!-- badges: start -->
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen.svg)](https://github.com/nasilaa/autotransform)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## Smart Data Type Detection and Transformation for R

`autotransform` automatically detects and transforms ambiguous data types in data frames. It intelligently handles numeric strings, categorical variables, logical values, dates, and more—perfect for cleaning messy datasets with mixed or unclear data types.

### Key Features

- 🔍 **Automatic Type Detection** - Intelligently identifies the true data type of each column
- 🔄 **Safe Transformations** - Converts data types with comprehensive error handling
- ↩️ **Full Rollback Capability** - Undo transformations entirely or for specific columns
- 📅 **Excel Date Handling** - Optional conversion of numeric columns to dates via `serialToStd` parameter
- 🧹 **N/A String Cleanup** - Automatically converts "N/A", "n/a", "#N/A" strings to R's NA
- 📊 **Detailed Analysis** - Inspect data type recommendations before transforming
- 🎯 **Selective Transformation** - Transform all columns or choose specific ones

## Installation

You can install the development version of autotransform from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("nasilaa/autotransform")
```

## Quick Start

```r
library(autotransform)

# Sample messy data
df <- data.frame(
  id = c("1", "2", "3", "4"),
  purchase_date = c(45232, 44982, 45000, 45100),  # Excel serial dates
  amount = c("99.50", "150.00", "N/A", "200.25"),
  active = c("yes", "no", "yes", "yes"),
  category = c("A", "B", "A", "C"),
  stringsAsFactors = FALSE
)

# Transform with automatic type detection
clean_df <- at_transform(df)

# View the results
print(clean_df)
#   id purchase_date amount active category
# 1  1         45232  99.50   TRUE        A
# 2  2         44982 150.00  FALSE        B
# 3  3         45000     NA   TRUE        A
# 4  4         45100 200.25   TRUE        C
```

## Core Functions

### `at_transform()` - Main Transformation Function

Transform your data with intelligent type detection:

```r
# Basic usage - transform all columns
transformed_df <- at_transform(df)

# Transform specific columns only
transformed_df <- at_transform(df, cols = c("amount", "active"))

# Keep N/A as literal strings instead of converting to NA
transformed_df <- at_transform(df, transform_NAs = FALSE)

# Force numeric columns to be converted to dates (Excel serial dates)
transformed_df <- at_transform(df, serialToStd = c("purchase_date", "ship_date"))

# Get detailed transformation information
result <- at_transform(df, verbose = TRUE)
print(result$log)
```

### `at_analyze()` - Analyze Before Transforming

Preview type recommendations without making changes:

```r
# Analyze all columns
analysis <- at_analyze(df)

# Analyze specific columns
analysis <- at_analyze(df, cols = c("amount", "active"))

# Silent analysis (no console output)
analysis <- at_analyze(df, verbose = FALSE)
```

### `at_rollback()` - Undo Transformations

Easily revert transformations:

```r
# Rollback all transformations
original_df <- at_rollback(transformed_df)

# Rollback specific columns only
partial_rollback <- at_rollback(transformed_df, cols = c("amount", "active"))
```

### `at_get_history()` - View Transformation History

```r
# Get a summary of what was transformed
history <- at_get_history(transformed_df)
print(history)
```

## Supported Data Types

`autotransform` intelligently detects and converts:

| Type | Detection Criteria | Example Input | Output |
|------|-------------------|---------------|--------|
| **Integer** | Numeric strings/values with no decimals | `"1", "2", "3"` | `1L, 2L, 3L` |
| **Numeric** | Numeric strings with decimals | `"99.5", "100.0"` | `99.5, 100.0` |
| **Logical** | Boolean-like strings | `"yes", "TRUE", "1"` | `TRUE, TRUE, TRUE` |
| **Factor** | Low-cardinality categorical data | `"A", "B", "A", "C"` | `factor()` |
| **Date** | Date strings or Excel serial dates* | `"2023-10-25"` or `45232` | `Date` |
| **POSIXct** | DateTime strings | `"2023-10-25 14:30:00"` | `POSIXct` |
| **Character** | Default fallback | Any non-matching data | `character` |

\* *Excel serial dates are only converted when explicitly specified using the `serialToStd` parameter*

## Excel Serial Date Conversion

**Important Note:** Numeric or integer columns are **NOT** automatically converted to dates. This prevents unintended conversions of regular numeric data.

To convert Excel serial dates (numeric values representing dates), use the `serialToStd` parameter:

```r
df <- data.frame(
  id = 1:3,
  purchase_date = c(45232, 44982, 45000),  # These are Excel dates
  amount = c(100, 200, 150)                # These are NOT dates
)

# Explicitly convert purchase_date from Excel serial dates
transformed_df <- at_transform(df, serialToStd = "purchase_date")

# Result:
#   id purchase_date amount
# 1  1    2023-10-25    100
# 2  2    2023-02-16    200
# 3  3    2023-03-06    150
```

## Advanced Usage

### Chaining with Rollback

```r
# Transform data
df_transformed <- at_transform(df)

# Do some analysis...
summary(df_transformed)

# Rollback if needed
df_original <- at_rollback(df_transformed)
```

### Selective Column Transformation

```r
# Only transform date and amount columns
df_partial <- at_transform(df, cols = c("purchase_date", "amount"))

# Add Excel date conversion
df_partial <- at_transform(df, 
                           cols = c("purchase_date", "amount"),
                           serialToStd = "purchase_date")
```

### Verbose Mode for Debugging

```r
result <- at_transform(df, verbose = TRUE)

# Access components
clean_data <- result$data
original_data <- result$original
transformation_log <- result$log
type_analysis <- result$analysis
```

## Real-World Example

```r
library(autotransform)

# Messy survey data from Excel
survey <- data.frame(
  respondent_id = c("001", "002", "003", "004"),
  submission_date = c(45232, 45233, 45234, 45235),  # Excel dates
  age = c("25", "34", "N/A", "42"),
  income = c("50000.00", "75000.50", "62000", "N/A"),
  satisfied = c("Yes", "No", "Yes", "Yes"),
  region = c("North", "South", "North", "West"),
  stringsAsFactors = FALSE
)

# Clean it up in one step
clean_survey <- at_transform(survey, serialToStd = "submission_date")

# Check the results
str(clean_survey)
# 'data.frame': 4 obs. of 6 variables:
#  $ respondent_id  : int 1 2 3 4
#  $ submission_date: Date, format: "2023-10-25" "2023-10-26" ...
#  $ age            : int 25 34 NA 42
#  $ income         : num 50000 75051 62000 NA
#  $ satisfied      : logi TRUE FALSE TRUE TRUE
#  $ region         : Factor w/ 3 levels "North","South",..: 1 2 1 3
```

## Detection Thresholds

The package uses sensible defaults for type detection:

- **Numeric/Integer**: 80% of values must be numeric
- **Logical**: 80% of values must be boolean-like
- **Date**: 60% of values must be parseable as dates
- **Factor**: Max 50% unique values with minimum frequency of 2

These thresholds ensure robust detection while handling messy real-world data.

## Function Reference

| Function | Description |
|----------|-------------|
| `at_transform()` | Main transformation function with rollback capability |
| `at_analyze()` | Analyze and preview type recommendations |
| `at_rollback()` | Undo transformations (all or specific columns) |
| `at_get_history()` | View transformation history |
| `at_clear_history()` | Clear stored transformation history |
| `is_mostly_numeric()` | Check if column is mostly numeric |
| `is_mostly_integer()` | Check if column is mostly integer |
| `is_mostly_logical()` | Check if column is mostly logical/boolean |
| `is_mostly_factor()` | Check if column is mostly categorical |
| `is_mostly_date()` | Check if column contains dates |
| `is_mostly_time()` | Check if column contains time information |

## Why autotransform?

- **Save Time**: No more manual type conversions
- **Reduce Errors**: Intelligent detection prevents common mistakes
- **Maintain Control**: Rollback capability and selective transformation
- **Handle Mess**: Built for real-world, messy data
- **Excel-Friendly**: Handles Excel serial dates with explicit control

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

MIT License - see LICENSE file for details

## Author

Abednego Nasila <alisanabednego@gmail.com>

## Citation

If you use autotransform in your research, please cite:

```
@Manual{autotransform,
  title = {autotransform: Smart Data Type Detection and Transformation},
  author = {Abednego Nasila},
  year = {2025},
  note = {R package version 0.1.0},
  url = {https://github.com/nasilaa/autotransform}
}
```
