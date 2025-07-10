#' @description This function creates continuous variables based on specifications from `variable` and `variable-details` sheets.
#'
#' @param var A string representing the variable name.
#' @param cycle A string indicating the cycle (e.g., "CCHS2001").
#' @param variable_details A data frame containing details about variables, including ranges and NA specifications.
#' @param length An integer specifying the desired length of the generated variable.
#' @param type A string indicating the distribution type for the continuous variable. Can be "normal" or "uniform". Default is "normal".
#' @param prop_NA A numeric value between 0 and 1, representing the proportion of NAs to introduce. If NULL, no NAs are introduced. Default is NULL.
#' @param seed An integer for setting the random seed to ensure reproducibility. Default is 100.
#'
#' @return A data frame with one column, representing the newly created continuous variable. The column name will be the raw variable name extracted from `variable_details`.
#' @examples
#' # Assuming 'variable_details' is a pre-loaded data frame with variable specifications
#' # create_con_var("age", "cycle1", variable_details, 1000, type = "normal")
#' # create_con_var("income", "cycle2", variable_details, 500, type = "uniform", prop_NA = 0.1)
#'

create_con_var <- function(var, cycle, variable_details, length, 
  type = "normal", prop_NA = NULL, seed = 100) {
    # related rows in variable_details
    var_details <- variable_details[variable_details$variable == var & str_detect(variable_details$variableStart, cycle),]

    if (nrow(var_details) > 0) {
        # extract the row variable name in the corresponding cycle
        temp <- unlist(str_split(var_details$variableStart[1],","))
        var_raw <- unlist(str_split(temp[str_detect(temp, cycle)],"::"))[2]

        # extract values from `recStart` column
        # real levels
        rng <- var_details$recStart[!(str_detect(var_details$recEnd, "NA"))]
        rng <- as.numeric(unlist(str_extract_all(rng, "\\d+\\.?\\d*"))) # extract integers or float
        
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
            col <- data.frame(new = create_vec(length, rng, type, seed)) 
        } else { # optional: add NA levels
            vec <- create_vec(length * (1-prop_NA), rng, type, seed)
            set.seed(seed)
            vec.na <- sample(NA_labels, length * prop_NA, replace = T)

            vec <- sample(c(vec, vec.na)) # combine and randomly sorted
            col <- data.frame(new = c(vec, vec.na))[1:length] 
        }
        names(col)[1] <- var_raw
      
        return(col)
    }
}

# ---------------------
create_vec <- function(length, range, type, seed) {
  set.seed(seed)
  if (type == "normal") {
    vec <- rnorm(length, mean = mean(range), sd = diff(range)/10) # DISCUSSION: sd
    vec[vec < range[1]] <- range[1]
    vec[vec > range[2]] <- range[2]
  } else if (type == "uniform") {
    vec <- runif(length, min = range[1], max = range[2])
  }
  return(vec)
}
