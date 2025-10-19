#!/usr/bin/env Rscript

# Simplified STL creation using only rgl
create_stl_from_gam <- function(gam_model,
                               x_var = "wind_max_gust",
                               y_var = "sum_butterflies_direct_sun",
                               data = NULL,
                               output_file = "surface.stl",
                               n_grid = 100,
                               print_size = 100,  # mm for x and y
                               max_height = 40,   # mm for max z
                               base_height = 5) { # mm for base

  library(rgl)

  # Get data ranges
  if (!is.null(data)) {
    x_range <- range(data[[x_var]], na.rm = TRUE)
    y_range <- range(data[[y_var]], na.rm = TRUE)
  } else {
    x_range <- range(gam_model$model[[x_var]], na.rm = TRUE)
    y_range <- range(gam_model$model[[y_var]], na.rm = TRUE)
  }

  # Create prediction grid
  x_seq <- seq(x_range[1], x_range[2], length.out = n_grid)
  y_seq <- seq(y_range[1], y_range[2], length.out = n_grid)

  pred_grid <- expand.grid(x_seq, y_seq)
  names(pred_grid) <- c(x_var, y_var)

  # Add other variables
  formula_vars <- all.vars(formula(gam_model))
  for (var in formula_vars) {
    if (!(var %in% c(x_var, y_var, formula_vars[1])) && var %in% names(gam_model$model)) {
      if (is.numeric(gam_model$model[[var]])) {
        pred_grid[[var]] <- mean(gam_model$model[[var]], na.rm = TRUE)
      } else if (is.factor(gam_model$model[[var]])) {
        pred_grid[[var]] <- levels(gam_model$model[[var]])[1]
      }
    }
  }

  # Get predictions (UNCLIPPED)
  pred_vals <- predict(gam_model, newdata = pred_grid, type = "terms", se.fit = FALSE)

  # Find tensor product column
  ti_pattern <- paste0("ti\\(", x_var, ",", y_var, "\\)|",
                      "ti\\(", y_var, ",", x_var, "\\)")
  ti_col <- grep(ti_pattern, colnames(pred_vals), value = TRUE)

  if (length(ti_col) == 0) {
    stop("No tensor product interaction found")
  }

  # Get z values (UNCLIPPED)
  z_vals <- matrix(pred_vals[, ti_col], nrow = n_grid, ncol = n_grid)

  # Scale to print dimensions
  x_print <- seq(0, print_size, length.out = n_grid)
  y_print <- seq(0, print_size, length.out = n_grid)

  # Scale z - map full range to base_height to max_height
  z_min <- min(z_vals, na.rm = TRUE)
  z_max <- max(z_vals, na.rm = TRUE)
  z_print <- ((z_vals - z_min) / (z_max - z_min)) * (max_height - base_height) + base_height

  cat(sprintf("Original z range: %.2f to %.2f\n", z_min, z_max))
  cat(sprintf("Print dimensions: %.0f x %.0f x %.1f mm\n",
              print_size, print_size, max(z_print)))

  # Open rgl device
  open3d(visible = FALSE)
  clear3d()

  # Create the surface
  surface3d(x_print, y_print, z_print, col = "gray")

  # Write STL
  writeSTL(output_file)
  close3d()

  cat(sprintf("STL saved: %s\n", output_file))

  invisible(list(
    original_z_range = c(z_min, z_max),
    print_dimensions = c(print_size, print_size, max(z_print))
  ))
}