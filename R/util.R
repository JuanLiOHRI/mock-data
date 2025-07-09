library(stringr)
# utility functions

# unpack the ranges in `recStart`
unpack_range <- function(range) {
  range_num <- as.numeric(unlist(str_extract_all(range, "\\d+")))
  vec <- seq(range_num[1], range_num[2])
  return(as.character(vec))
}
