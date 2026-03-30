#' Initialize Database
#'
#' Creates SQLite database with required schema for LIMS
#'
#' @return Path to the database file
#' @export
initialize_database <- function() {
  db_path <- "lims_data.db"
  
  if (!file.exists(db_path)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    
    # Sites table
    DBI::dbExecute(con, "
      CREATE TABLE sites (
        site_id TEXT PRIMARY KEY,
        site_name TEXT NOT NULL,
        description TEXT,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )
    ")
    
    # Labels table
    DBI::dbExecute(con, "
      CREATE TABLE labels (
        label_id TEXT PRIMARY KEY,
        site_id TEXT NOT NULL,
        sample_id TEXT NOT NULL,
        qr_code_path TEXT,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY(site_id) REFERENCES sites(site_id)
      )
    ")
    
    # Batch scans table
    DBI::dbExecute(con, "
      CREATE TABLE batch_scans (
        scan_id INTEGER PRIMARY KEY AUTOINCREMENT,
        site_id TEXT NOT NULL,
        scanner_input TEXT,
        scanned_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY(site_id) REFERENCES sites(site_id)
      )
    ")
    
    DBI::dbDisconnect(con)
  }
  
  db_path
}

#' Insert Site
#'
#' @param con Database connection
#' @param site_id Site ID
#' @param site_name Site name
#' @param description Site description (optional)
#' @export
insert_site <- function(con, site_id, site_name, description = NULL) {
  DBI::dbExecute(con, 
    "INSERT INTO sites (site_id, site_name, description) VALUES (?, ?, ?)",
    list(site_id, site_name, description)
  )
}

#' Check if Site Exists
#'
#' @param con Database connection
#' @param site_id Site ID to check
#' @return Logical TRUE if exists
#' @export
check_site_exists <- function(con, site_id) {
  result <- DBI::dbGetQuery(con, 
    "SELECT COUNT(*) as n FROM sites WHERE site_id = ?",
    list(site_id)
  )
  result$n > 0
}

#' Fetch All Sites
#'
#' @param con Database connection
#' @return Data frame of all sites
#' @export
fetch_all_sites <- function(con) {
  DBI::dbGetQuery(con, "SELECT * FROM sites ORDER BY created_at DESC")
}

#' Fetch Site by ID
#'
#' @param con Database connection
#' @param site_id Site ID to fetch
#' @return Data frame of matching site
#' @export
fetch_site_by_id <- function(con, site_id) {
  DBI::dbGetQuery(con, 
    "SELECT * FROM sites WHERE site_id = ?",
    list(site_id)
  )
}

#' Insert Label
#'
#' @param con Database connection
#' @param label_id Label ID
#' @param site_id Site ID
#' @param sample_id Sample ID
#' @param qr_code_path Path to QR code image
#' @export
insert_label <- function(con, label_id, site_id, sample_id, qr_code_path) {
  DBI::dbExecute(con,
    "INSERT INTO labels (label_id, site_id, sample_id, qr_code_path) VALUES (?, ?, ?, ?)",
    list(label_id, site_id, sample_id, qr_code_path)
  )
}

#' Fetch All Labels
#'
#' @param con Database connection
#' @return Data frame of all labels
#' @export
fetch_all_labels <- function(con) {
  DBI::dbGetQuery(con, "SELECT * FROM labels ORDER BY created_at DESC")
}

#' Insert Batch Scan
#'
#' @param con Database connection
#' @param site_id Site ID
#' @param scanner_input Scanned data
#' @export
insert_batch_scan <- function(con, site_id, scanner_input) {
  DBI::dbExecute(con,
    "INSERT INTO batch_scans (site_id, scanner_input) VALUES (?, ?)",
    list(site_id, scanner_input)
  )
}

#' Fetch All Batch Scans
#'
#' @param con Database connection
#' @return Data frame of all batch scans with site names
#' @export
fetch_all_batch_scans <- function(con) {
  DBI::dbGetQuery(con, "
    SELECT bs.*, s.site_name 
    FROM batch_scans bs
    LEFT JOIN sites s ON bs.site_id = s.site_id
    ORDER BY bs.scanned_at DESC
  ")
}

#' Generate Site ID
#'
#' @return Generated site ID (ST####)
#' @export
generate_site_id <- function() {
  paste0("ST", sprintf("%04d", sample(1:9999, 1)))
}

#' Generate Plant ID
#'
#' @return Generated plant ID (P####)
#' @export
generate_plant_id <- function() {
  paste0("P", sprintf("%04d", sample(1:9999, 1)))
}

#' Generate Processing ID
#'
#' @return Generated processing ID (PR####)
#' @export
generate_proc_id <- function() {
  paste0("PR", sprintf("%04d", sample(1:9999, 1)))
}

#' Validate Site ID
#'
#' @param site_id Site ID to validate
#' @return Logical TRUE if valid format
#' @export
validate_site_id <- function(site_id) {
  grepl("^ST\\d{4}$", site_id)
}

#' Validate Sample ID
#'
#' @param sample_id Sample ID to validate
#' @return Logical TRUE if not empty
#' @export
validate_sample_id <- function(sample_id) {
  !is.null(sample_id) && nzchar(trimws(sample_id))
}

#' Generate Label PNG
#'
#' @param qr_text Text for QR code
#' @param filename Output filename
#' @return Path to generated image
#' @export
generate_label_png <- function(qr_text, filename) {
  qr <- qrcode::qr_code(qr_text)
  qrcode::plot(qr, type = "html")
  filename
}

#' Get Next Sequential Site ID
#'
#' Generates the next sequential site ID based on existing sites in database.
#' Starts with ST0001 if no sites exist.
#'
#' @param con Database connection
#' @return Next site ID (ST0001, ST0002, etc.)
#' @export
get_next_site_id <- function(con) {
  result <- DBI::dbGetQuery(con, "
    SELECT site_id FROM sites 
    WHERE site_id LIKE 'ST%' 
    ORDER BY site_id DESC 
    LIMIT 1
  ")
  
  if (nrow(result) == 0) {
    return("ST0001")
  }
  
  last_id <- result$site_id[1]
  last_num <- as.numeric(substr(last_id, 3, 6))
  next_num <- last_num + 1
  
  paste0("ST", sprintf("%04d", next_num))
}

#' Add Site
#'
#' Adds a new site with auto-generated sequential ID (ST0001, ST0002, etc.)
#'
#' @param con Database connection
#' @param site_name Site name
#' @param description Site description
#' @return Invisibly returns the new site ID
#' @export
add_site <- function(con, site_name, description = NULL) {
  if (!validate_sample_id(site_name)) {
    stop("Site name cannot be empty")
  }
  
  new_id <- get_next_site_id(con)
  insert_site(con, new_id, site_name, description)
  
  invisible(new_id)
}
