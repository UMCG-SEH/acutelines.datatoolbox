# Functions to clean Acutelines data

#' Normalize decimal format in a data frame
#' Interprets both "." and "," as decimal indicators
#' @param df Data frame to be normalized
#' @return Data frame with normalized decimal format
#' @export
normalize_decimal_format <- function(df) {
    # Interpret both "." and "," as decimal indicators
    df[] <- lapply(
        df, function(x) {
            x_clean <- gsub(",", ".", x)
            x_num <- suppressWarnings(as.numeric(x_clean))
            if (all(is.na(x_num) == is.na(x_clean))) x_num else x
        }
    )
}


#' Clean laboratory variables by removing '<' and '>' characters and converting to numeric
#' Cleans all columns starting with "lab_" (per Acutelines standard)
#' 
#' @param df Data frame containing laboratory variables
#' 
#' @return Data frame with cleaned laboratory variables
#' @export
clean_lab <- function(df) {
    df <- df %>% mutate(across(starts_with("lab_"), ~ as.numeric(gsub("[<>]", "", .))))
}


