
#' @export

hmda.wmshap.table <- function(wmshap,
                          method = c("shapratio"),
                          cutoff = 0.01,
                          round = 3,
                          exclude_features = NULL,
                          dict = NULL,
                          markdown.table = TRUE,
                          split.tables = 120,
                          split.cells = 50) {

  return(shapley::shapley.table(wmshap = wmshap,
                         method = method,
                         cutoff = cutoff,
                         round = round,
                         exclude_features = exclude_features,
                         dict = dict,
                         markdown.table = markdown.table,
                         split.tables = split.tables,
                         split.cells = split.cells))
}

#shapley.table(wmshap, method = "shapratio", cutoff = 0.01, dict = dictionary(raw, attribute = "label"))
#shapley.table(wmshap, method = "shapratio", cutoff = 0.01, dict = dict)
#shapley.table(wmshap, method = "shapratio", cutoff = 0.01)
