#' Read YAML
#'
#' Reads a file in YAML format and returns a data model object.
#'
#' @details YAML description should include table names (first level),
#' columns (second level) and column attributes (third level).
#' Expected (but not required) column attributes are
#'   \code{key} (Yes|No),
#'   \code{ref} (Name of referenced table),
#'   \code{comment} (column description).
#'
#' @param file A file in YAML format
#' @param text A YAML formated character string
#' @examples
#' dm <-
#'   dm_read_yaml(text = "
#'
#'     Person:
#'       Person ID: {key: yes}
#'       Name:
#'       E-mail:
#'       Street:
#'       Street number:
#'       City:
#'       ZIP:
#'
#'     Order:
#'       Order ID: {key: yes}
#'       Customer: {ref: Person}
#'       Sales person: {ref: Person}
#'       Order date:
#'       Requested ship date:
#'       Status:
#'
#'     Order Line:
#'       Order ID: {key: yes, ref: Order}
#'       Line number: {key: yes}
#'       Order item: {ref: Item}
#'       Quantity:
#'       Price:
#'
#'     Item:
#'       Item ID: {key: yes}
#'       Item Name:
#'       Description:
#'   ")
#' @export
dm_read_yaml <- function(file = NULL, text = NULL) {

  if( !requireNamespace("yaml", quietly = TRUE)) {
    stop("yaml package needed for this function to work. Please install it.",
         call. = FALSE)
  }


  if(missing(text)) {
    if(!missing(file)) {
      if(!file.exists(file)) stop("File does not exist.")
      dm <- yaml::yaml.load_file(file)
    } else {
      stop("A file or text needed.")
    }
  } else {
    dm <- yaml::yaml.load(text)
  }
  if(is.null(dm)) {
    return(NULL)
  }

  col_table <- dm_list2coltable(dm)
  return(as.data_model(col_table))
}


#' List to column table
#'
#' Convert a 3 level named list to a data frame with column info
#'
#' @details The funcion is used when creating data model object
#'   from list provided by yaml parser.
#' @param x a named list
#' @export
#' @keywords internal
dm_list2coltable <- function(x) {

  if(!is.list(x)) {
    stop("Input must be a list.")
  }

  if(is.null(names(x))) {
    # parsed yaml with sequences
    x_tables <- x[sapply(x, function(x) !is.null(x[["table"]]))]

    table_names <- sapply(x_tables, function(tab) tab[["table"]])
    columns <- lapply(x_tables, function(tab) {
      tab_name <- tab[["table"]]
      if(!is.null(tab_name)) {
        cols <- tab[["columns"]]
      }
    })
    names(columns) <- table_names

    column_names <- lapply(columns, names)
    column_attributes <- unique( unlist( lapply(columns, sapply, names)))

  } else {
    # Named list (parsed yaml with maps)
    columns <- x
    table_names <- names(columns)
    column_names <- lapply(columns, names)
    column_attributes <- unique( unlist( lapply(columns, sapply, names)))
  }


  table_list <-
    lapply(table_names, function(tab_name) {
      if(is.null(column_names[[tab_name]])) {
        column_names[[tab_name]] <- NA
      }
      tab <- data.frame(
        table = tab_name,
        column = column_names[tab_name],
        stringsAsFactors = FALSE
      )
      names(tab) <- c("table", "column")

      for(a in column_attributes) {
        attr_value <-
          unlist(
            sapply(column_names[[tab_name]], function(cname) {
              if(is.list(columns[[tab_name]][[cname]]))
                value <- columns[[tab_name]][[cname]][[a]]
              else
                value <- NA
              ifelse(is.null(value), NA, value)
            })
          )
        tab[[a]] <- attr_value
      }
      tab
    })

  ret <- do.call(rbind, table_list)

  table_attrs <- dm_get_table_attrs(x)
  if(!is.null(table_attrs) && is.null(table_attrs$segment))
    table_attrs$segment <- NA
  attr(ret, "tables") <- table_attrs

  ret
}
