#' Fetch all sites from the database
#' @export
fetch_all_sites <- function(pool) {
  DBI::dbGetQuery(pool, "SELECT * FROM sites ORDER BY id DESC")
}

#' Get the next available Site ID
#' @export
get_next_site_id <- function(pool) {
  res <- DBI::dbGetQuery(pool, "SELECT MAX(id) as max_id FROM sites")
  if (is.na(res$max_id)) return(1) else return(res$max_id + 1)
}

#' Add a new site to the database
#' @export
add_site <- function(pool, name, description) {
  # Use parameterized queries to prevent SQL injection
  query <- "INSERT INTO sites (name, description) VALUES (?, ?)"
  DBI::dbExecute(pool, query, params = list(name, description))
}