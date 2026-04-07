#' Parse Printer-Style Range Strings
#'
#' @description Converts a string like "1, 2, 5-8" into a numeric vector c(1, 2, 5, 6, 7, 8).
#' @param text A character string containing numbers, commas, and dashes.
#' @return A numeric vector or NULL if parsing fails.
#' @export
parse_range <- function(text) {
  if (is.null(text) || text == "") return(NULL)
  
  tryCatch({
    # Remove all whitespace
    text <- gsub("\\s+", "", text)
    
    # Split by comma to get individual units (e.g., "1" or "5-8")
    parts <- strsplit(text, ",")[[1]]
    
    # Map over parts and expand ranges
    result <- unlist(lapply(parts, function(x) {
      if (grepl("-", x)) {
        # Handle ranges like 5-10
        rng_parts <- as.numeric(strsplit(x, "-")[[1]])
        if (length(rng_parts) != 2) stop("Invalid range format")
        seq(rng_parts[1], rng_parts[2])
      } else {
        # Handle single numbers
        as.numeric(x)
      }
    }))
    
    return(result)
  }, error = function(e) {
    return(NULL)
  })
}