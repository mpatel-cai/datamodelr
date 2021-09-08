#' Postgres ER Diagram Query
#'
#' Returns a string with SQL query to reverse engineer a database
#'
#' @return
#' Postgres SQL query as a string.
#' @export
#' @rdname render_pg_query
#' @examples
#' \dontrun{
#' library(RPostgreSQL)
#' # dvdrental sample database: http://www.postgresqltutorial.com/postgresql-sample-database
#' con <- dbConnect(dbDriver("PostgreSQL"), dbname="dvdrental", user ="postgres")
#' sQuery <- dm_re_query("postgres")
#' dm_dvdrental <- dbGetQuery(con, sQuery)
#' dbDisconnect(con)
#' }

render_pg_query <- function(schema) {

  stopifnot(!missing(schema))

  file_name <-
    system.file(package ="neRd",
                "sql",
                "postgres.sql")
  glue::glue(
    paste(readLines(file_name),
          collapse = "\n"))
}

#' Redshift ER Diagram Query
#'
#' Returns a string with SQL query to reverse engineer a database
#'
#' @return
#' Postgres SQL query as a string. (Same SQL query in Redshift.)
#' @export
#' @rdname render_rs_query
#' @examples
#' \dontrun{
#' library(RPostgreSQL)
#' # dvdrental sample database: http://www.postgresqltutorial.com/postgresql-sample-database
#' con <- dbConnect(dbDriver("PostgreSQL"), dbname="dvdrental", user ="postgres")
#' sQuery <- dm_re_query("postgres")
#' dm_dvdrental <- dbGetQuery(con, sQuery)
#' dbDisconnect(con)
#' }


render_rs_query <- function(schema) {

  stopifnot(!missing(schema))

  file_name <-
    system.file(package ="neRd",
                "sql",
                "redshift.sql")
  glue::glue(
    paste(readLines(file_name),
          collapse = "\n"))
}



#' Reverse engineer query
#'
#' Returns a string with SQL query to reverse engineer a database
#'
#' @param rdbms Which database ("postgres" or "sqlserver")
#' @return A character string with sql query
#' @export
#' @examples
#' \dontrun{
#' library(RPostgreSQL)
#' # dvdrental sample database: http://www.postgresqltutorial.com/postgresql-sample-database
#' con <- dbConnect(dbDriver("PostgreSQL"), dbname="dvdrental", user ="postgres")
#' sQuery <- dm_re_query("postgres")
#' dm_dvdrental <- dbGetQuery(con, sQuery)
#' dbDisconnect(con)
#' }
dm_re_query <- function(rdbms) {
  sql_script <- sprintf("sql/%s.sql", rdbms)
  file_name <- system.file(sql_script, package ="datamodelr")
  if( !file.exists(file_name) ) {
    stop("This rdbs not supported")
  }
  sQuery <- paste(readLines(file_name), collapse = "\n")
  sQuery
}
