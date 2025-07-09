#' Create categorical variables based on variable and variable-details sheets
#'
#' This function generates a mock categorical variable based on specifications from
#' a variable details dataframe. It handles both regular categories and NA categories,
#' including those defined as ranges.
#'
#' @param var A character string specifying the variable name to create.
#' @param cycle A character string indicating the cycle (e.g., "CCHS2001", "CCHS2003") to filter
#'   `variable_details` by.
#' @param variable_details A data frame containing variable specifications, typically
#'   with columns like `variable`, `databaseStart`, `variableStart`, `recStart`, and `recEnd`.
#' @param length An integer specifying the desired length of the generated categorical variable.
#' @param has_NA A logical value. If TRUE, NA levels defined in `variable_details` will be
#'   included in the sampling. Defaults to FALSE.
#' @param seed An integer for setting the random seed to ensure reproducibility. Defaults to 100.
#'
#' @return A data frame with a single column containing the generated
#'   categorical variable.
#'
#' @examples
#' # Assuming 'variable_details' is a pre-defined dataframe
#' # var_details_df <- data.frame(
#' #   variable = c("VAR1", "VAR1", "VAR2"),
#' #   databaseStart = c("CYC1", "CYC1", "CYC1"),
#' #   variableStart = c("CYC1::VAR1_raw", "CYC1::VAR1_raw", "CYC1::VAR2_raw"),
#' #   recStart = c("1", "2", "A"),
#' #   recEnd = c("1", "2", "A")
#' # )
#' # create_cat_var("VAR1", "CYC1", var_details_df, 10)
#' # create_cat_var("VAR1", "CYC1", var_details_df, 10, has_NA = TRUE)
#'
#' @importFrom stringr str_detect str_split str_extract_all
#' @importFrom stats runif

create_cat_var <- function(var, cycle, variable_details, length, has_NA = FALSE, seed = 100) {
    # related rows in variable_details
    var_details <- variable_details[variable_details$variable == var & str_detect(variable_details$variableStart, cycle),]

    if (nrow(var_details) > 0) {
        # extract the row variable name in the corresponding cycle
        temp <- unlist(str_split(var_details$variableStart[1],","))
        var_raw <- unlist(str_split(temp[str_detect(temp, cycle)],"::"))[2]

        # extract categories from `recStart` column
        # real levels
        labels <- var_details$recStart[!(str_detect(var_details$recEnd, "NA"))]
        if (any(str_detect(labels, ","))) { # unpack the ranges
            temp <- labels[!str_detect(labels, ",")]
            ranges <- labels[str_detect(labels, ",")]
            for (range in ranges) {
                temp <- c(temp, unpack_range(range))
            }
            labels <- sort(temp)
        }
        # NA levels
        NA_labels <- var_details$recStart[str_detect(var_details$recEnd, "NA")]
        if (any(str_detect(NA_labels, ","))) { # unpack the ranges
            temp <- NA_labels[!str_detect(NA_labels, ",")]
            ranges <- NA_labels[str_detect(NA_labels, ",")]
            for (range in ranges) {
                temp <- c(temp, unpack_range(range))
            }
            NA_labels <- sort(temp)
        }

        # create mock variable
        if (has_NA) labels <- c(labels, NA_labels)
        set.seed(seed)
        col <- data.frame(new = sample(labels, length, replace = T)) 
        names(col)[1] <- var_raw

        return(col)
    }
}


