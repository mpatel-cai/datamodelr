#' Set table segment
#'
#' Change tables' segment name in a data model
#'
#' @param dm A data model object
#' @param table_segments A named list of vectors with segments as element names
#'   and tables as values in vectors
#' @export
#' @rdname set_segment
set_segment <- function(dm, table_segments) {

  if(!is.data_model(dm))
    stop("Not a data model object.")
  for(s in names(table_segments)) {
    table_names <- table_segments[[s]]
    dm$tables$segment[dm$tables$table %in% table_names ] <- s
  }
  dm
}

#' Set table display
#'
#' Change tables' display in a data model
#'
#' @param dm A data model object
#' @param display A named list of vectors with display as element names
#'   and tables as values in vectors
#' @export
#' @rdname set_display
set_display <- function(dm, display) {

  if(!is.data_model(dm))
    stop("Not a data model object.")
  for(s in names(display)) {
    table_names <- display[[s]]
    dm$tables$display[dm$tables$table %in% table_names ] <- s
  }
  dm
}

#' Set key
#'
#' Set column as a primary key
#'
#' @param dm A data model object
#' @param table Table name
#' @param column Column(s) name
#' @export
#' @rdname set_key
set_key <- function(dm, table, column) {
  update_cols <- dm$columns$table == table & dm$columns$column %in% column
  if(!any(update_cols)) {
    stop("Column not found.")
  }
  dm$columns$key[update_cols] <- seq_along(column)
  dm
}

#' Set column attribute
#'
#' Set column attribute value
#'
#' @param dm A data model object
#' @param table Table name
#' @param column Column(s) name
#' @param attr Column attribute name
#' @param value New value
#' @export
#' @keywords internal
set_col_attr <- function(dm, table, column, attr, value) {
  update_cols <- dm$columns$table == table & dm$columns$column == column
  if(!any(update_cols)) {
    stop("Column not found.")
  }
  dm$columns[update_cols, attr] <- value
  dm
}
