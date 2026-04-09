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
generate_plant_qr <- function(plant_id, date_sam, app_dir = "inst/app") {
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

      # Generate QR code
      qr <- qrcode::qr_code(plant_id)
      dat <- date_sam
      qr_grob <- grid::rasterGrob(as.raster(1 - qr), interpolate = FALSE)

      # Create the ggplot
      p <- ggplot() +
        # Set the coordinate system (x from 0 to 3, y from 0 to 1)
        coord_fixed(ratio = 1, xlim = c(0, 3), ylim = c(0, 1)) +

        # Add Plant ID (Top Left)
        annotate("text",
          x = 0.1, y = 0.8, label = plant_id,
          hjust = 0, vjust = 1, size = 5, fontface = "bold"
        ) +
        # Add Date (Bottom Left)
        annotate("text",
          x = 0.1, y = 0.2, label = dat,
          hjust = 0, vjust = 0, size = 5, fontface = "bold"
        ) +
        # Add QR Code (Right Side)
        annotation_custom(qr_grob, xmin = 2, xmax = 3, ymin = 0, ymax = 1) +

        # Clean up the theme for a "sticker" look
        theme_void() +
        theme(panel.background = element_rect(fill = "white", color = "white"))

      ggsave(paste0(labels_dir, "/", filename), p, width = 12, height = 4, units = "cm", dpi = 500)
      cat("\n", plant_id, " generated")

      relative_path <- file.path("labels", filename)
      return(relative_path)
    },
    error = function(e) {
      stop(paste("Failed to generate QR code for", plant_id, ":", e$message))
    }
  )
}

#' Generate QR Code for Soil Label
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
generate_soil_qr <- function(soil_id, date_sam, app_dir = "inst/app") {
  # Validate input
  if (is.null(soil_id) || soil_id == "") {
    stop("soil_id cannot be empty")
  }

  tryCatch(
    {
      # Create labels directory if it doesn't exist
      labels_dir <- file.path(app_dir, "www", "labels")
      if (!dir.exists(labels_dir)) {
        dir.create(labels_dir, recursive = TRUE, showWarnings = FALSE)
      }

      # Sanitize filename (replace hyphen with underscore)
      filename <- paste0(soil_id, ".png")
      filepath <- file.path(labels_dir, filename)

      # Generate QR code
      qr <- qrcode::qr_code(soil_id)
      dat <- date_sam
      qr_grob <- grid::rasterGrob(as.raster(1 - qr), interpolate = FALSE)

      # Create the ggplot
      p <- ggplot() +
        # Set the coordinate system (x from 0 to 3, y from 0 to 1)
        coord_fixed(ratio = 1, xlim = c(0, 3), ylim = c(0, 1)) +

        # Add Plant ID (Top Left)
        annotate("text",
          x = 0.1, y = 0.8, label = soil_id,
          hjust = 0, vjust = 1, size = 5, fontface = "bold"
        ) +
        # Add Date (Bottom Left)
        annotate("text",
          x = 0.1, y = 0.2, label = dat,
          hjust = 0, vjust = 0, size = 5, fontface = "bold"
        ) +
        # Add QR Code (Right Side)
        annotation_custom(qr_grob, xmin = 2, xmax = 3, ymin = 0, ymax = 1) +

        # Clean up the theme for a "sticker" look
        theme_void() +
        theme(panel.background = element_rect(fill = "white", color = "white"))

      ggsave(paste0(labels_dir, "/", filename), p, width = 12, height = 4, units = "cm", dpi = 500)
      cat("\n", soil_id, " generated")

      relative_path <- file.path("labels", filename)
      return(relative_path)
    },
    error = function(e) {
      stop(paste("Failed to generate QR code for", soil_id, ":", e$message))
    }
  )
}
