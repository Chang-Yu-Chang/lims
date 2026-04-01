#' Initialize Database
#' @export
initialize_database <- function() {
  db_path <- "lims_data.db"
  
  if (!file.exists(db_path)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    
    # Create the 'sites' table with site_id as the primary handle
    DBI::dbExecute(con, "
      CREATE TABLE sites (
        site_id TEXT PRIMARY KEY,
        site_desc TEXT,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )
    ")
    
    DBI::dbDisconnect(con)
  }
  db_path
}

