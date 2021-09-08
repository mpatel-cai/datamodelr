#' Print data model graph
#'
#' @param x data model object.
#' @param ... further arguments passed to or from other methods.
#' @export
print.data_model <- function(x, ...) {
  cat("Data model object:\n")
  tables <- paste(utils::head(x$tables$table, 4), collapse = ", ")
  if(length(x$tables$table) > 4) {
    tables <- paste(tables, "...")
  }
  cat(" ", nrow(x$tables), "tables: ", tables,"\n")
  cat(" ", nrow(x$columns), "columns\n")
  cat(" ", length(unique(x$columns[x$columns[["key"]] != 0,"table"])), "primary keys\n")
  cat(" ", ifelse(is.null(x$references), "no", nrow(unique(x$references))),
      "references\n")
}
