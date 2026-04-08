#' Parse Printer-Style Range Strings
#'
#' @description Converts a string like "1, 2, 5-8" into a numeric vector c(1, 2, 5, 6, 7, 8).
#' @param text A character string containing numbers, commas, and dashes.
#' @return A numeric vector or NULL if parsing fails.
#' @export
parse_range <- function(text) {
  if (is.null(text) || text == "") {
    return(NULL)
  }

  tryCatch(
    {
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
    },
    error = function(e) {
      return(NULL)
    }
  )
}


#' Generate QR Code for Plant Label
#'
#' @description Creates a QR code PNG file for a plant ID and saves it to inst/app/www/labels/
#'
#' @param plant_id Character string in format "ST0001-P001"
#' @param app_dir Character string path to the app directory (default: inst/app)
#'
#' @return Character string with the relative web path (e.g., "labels/qr_ST0001_P001.png")
#'
#' @details
#' - Creates www/labels/ directory if it doesn't exist
#' - Sanitizes plant_id to create filename (e.g., ST0001-P001 becomes ST0001_P001)
#' - Returns path suitable for web access and database storage
#'
#' @importFrom qrcode qr_code
#' @import grDevices
#'
#' @export
generate_plant_qr <- function(plant_id, app_dir = "inst/app") {
  # Validate input
  if (is.null(plant_id) || plant_id == "") {
    stop("plant_id cannot be empty")
  }

  tryCatch(
    {
      # Create labels directory if it doesn't exist
      labels_dir <- file.path(app_dir, "www", "labels")
      if (!dir.exists(labels_dir)) {
        dir.create(labels_dir, recursive = TRUE, showWarnings = FALSE)
      }

      # Sanitize filename (replace hyphen with underscore)
      filename <- paste0(plant_id, ".png")
      filepath <- file.path(labels_dir, filename)

      # 1. Generate QR code as a matrix
      qr <- qrcode::qr_code(plant_id)
      current_time <- Sys.Date() # date
      tb_qr <- tibble::as_tibble(as.matrix(qr))
      tb_qr$y <- factor(1:nrow(tb_qr), nrow(qr):1)
      plot_qr <- tidyr::pivot_longer(tb_qr, cols = -y, names_to = "x")
      plot_qr$x <- factor(stringr::str_remove(plot_qr$x, "V"), 1:nrow(qr))
      plot_qr$value <- as.logical(plot_qr$value)
      
      library(ggplot2)
      #plot(qr)
      p_qr <- ggplot(plot_qr) +
        geom_tile(aes(x = x , y = y, fill = value)) +
        scale_fill_gradient(low = "white", high = "black") +
        theme_void() + 
        theme(legend.position='none')

      
      # 3. Create a blank plot area
      # mar = c(0,0,0,0) removes margins so we use the whole space
      par(mar = c(0, 0, 0, 0))
      plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 3), ylim = c(0, 1), axes = FALSE)

      # 4. Add the Text (Left Side)
      # adj = 0 means left-aligned
      text(x = 0.1, y = 0.85, labels = plant_id, cex = 2.5, font = 2, adj = 0)
      text(x = 0.2, y = 0.15, labels = current_time, cex = 2.2, font = 2, adj = 0)

      # 5. Add the QR Code (Right Side)
      # We convert the QR matrix to a raster image
      qr_raster <- as.raster(qr)
      rasterImage(qr_raster, xleft = 1.8, ybottom = 0.1, xright = 2.8, ytop = 0.9)

      dev.off()

      # # Return relative path for web access (what goes in database)
      relative_path <- file.path("labels", filename)
      return(relative_path)
    },
    error = function(e) {
      stop(paste("Failed to generate QR code for", plant_id, ":", e$message))
    }
  )
}
