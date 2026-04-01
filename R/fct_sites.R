#' @export
db_site_add <- function(pool, s_id, s_desc) {
  query <- "INSERT INTO sites (site_id, site_desc) VALUES (?, ?)"
  pool::dbExecute(pool, query, params = list(s_id, s_desc))
}

#' @export
db_site_fetch_all <- function(pool) {
  pool::dbGetQuery(pool, "SELECT * FROM sites ORDER BY site_id DESC")
}
