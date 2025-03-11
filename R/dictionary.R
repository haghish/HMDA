# # Create a sample data frame
# df <- data.frame(x = 1:5, y = 6:10)
#
# # Attach a description as metadata to the entire data frame
# attr(df, "description") <- "This dataset contains sample values for x and y."
#
# # Attach metadata to individual columns
# attr(df$x, "label") <- "Sample x values"
# attr(df$y, "label") <- "Sample y values"
#
# # View the metadata for the data frame
# attributes(df$x)
# attributes(df$y)
#
# dictionary(df, "label")

#' @export

# extract the attributes for the given label and create the Description columns
dictionary <- function(df, attribute, na.rm = TRUE) {
  vars <- names(df)
  descriptions <- sapply(df, function(x) {
    desc <- attr(x, attribute)
    if (is.null(desc)) NA else desc
  })
  dict_df <- data.frame(name = vars, description = descriptions,
                        stringsAsFactors = FALSE)
  rownames(dict_df) <- NULL

  if (na.rm) dict_df <- na.omit(dict_df)
  return(dict_df)
}

# dictionary(raw, "label")
