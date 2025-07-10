#' @description This function creates categorical variables based on specifications from `variable` and `variable-details` sheets.
#'
#' @param var string. The variable name to be created.
#' @param cycle string. The cycle to which the variable belongs (e.g., "CCHS2001").
#' @param variable_details data.frame. A data frame containing variable details, typically loaded from a "variable-details" sheet.
#' @param length integer. The desired length of the categorical variable vector.
#' @param prop_NA numeric. Optional. The proportion of NA values to be introduced. If NULL, no NA values are introduced.
#' @param seed integer. Optional. Seed for reproducibility. Default is 100.
#' @return A data frame with one column representing the newly created categorical variable.
#' @examples
#' # Assuming 'variable_details' is loaded
#' # create_cat_var("gender", "HC1", variable_details, 100)
#' # create_cat_var("race", "HC1", variable_details, 100, prop_NA = 0.1)

create_cat_var <- function(var, cycle, variable_details, length, prop_NA = NULL, seed = 100) {
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
        if (is.null(prop_NA)) {
            set.seed(seed)
            col <- data.frame(new = sample(labels, length, replace = T)) 
        } else { # optional: add NA levels
            set.seed(seed)
            vec <- sample(labels, length * (1-prop_NA), replace = T)
            set.seed(seed)
            vec.na <- sample(NA_labels, length * prop_NA, replace = T)

            vec <- sample(c(vec, vec.na)) # combine and randomly sorted
            col <- data.frame(new = c(vec, vec.na))[1:length] 
        }
        names(col)[1] <- var_raw
        
        return(col)
    }
}



