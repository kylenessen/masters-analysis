#!/usr/bin/env Rscript

# Create 3D surface plot for GAM tensor product interaction
# This creates a perspective plot similar to typical GAM visualization

create_3d_interaction_surface <- function(gam_model,
                                        x_var = "wind_max_gust",
                                        y_var = "sum_butterflies_direct_sun",
                                        data = NULL,
                                        n_grid = 50,
                                        theta = 45,  # viewing angle (azimuth)
                                        phi = 25,    # viewing angle (colatitude)
                                        expand = 0.5,
                                        ticktype = "detailed",
                                        xlab = "Wind speed (m/s)",
                                        ylab = "Butterflies in sun",
                                        zlab = "Effect",
                                        main = "",
                                        color_scheme = "viridis",
                                        shade = 0.4,
                                        ltheta = -45,
                                        border = NA,
                                        box = TRUE) {

  # Load required packages
  if (!requireNamespace("viridis", quietly = TRUE)) {
    stop("Package 'viridis' needed for color scheme. Please install it.")
  }

  # Get data ranges
  if (!is.null(data)) {
    x_range <- range(data[[x_var]], na.rm = TRUE)
    y_range <- range(data[[y_var]], na.rm = TRUE)
  } else {
    # Use model's original data if available
    x_range <- range(gam_model$model[[x_var]], na.rm = TRUE)
    y_range <- range(gam_model$model[[y_var]], na.rm = TRUE)
  }

  # Create prediction grid
  x_seq <- seq(x_range[1], x_range[2], length.out = n_grid)
  y_seq <- seq(y_range[1], y_range[2], length.out = n_grid)

  # Create grid with proper column names
  pred_grid <- expand.grid(x_seq, y_seq)
  names(pred_grid) <- c(x_var, y_var)

  # Add other required variables at their means
  # Only add variables that are in the model formula
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

  # Get predictions for the tensor product term only
  pred_vals <- predict(gam_model, newdata = pred_grid, type = "terms", se.fit = FALSE)

  # Find the tensor product column
  ti_pattern <- paste0("ti\\(", x_var, ",", y_var, "\\)|",
                      "ti\\(", y_var, ",", x_var, "\\)")
  ti_col <- grep(ti_pattern, colnames(pred_vals), value = TRUE)

  if (length(ti_col) == 0) {
    stop("No tensor product interaction found for specified variables")
  }

  # Extract z values and reshape to matrix
  z_vals <- matrix(pred_vals[, ti_col], nrow = n_grid, ncol = n_grid)

  # Create color palette
  if (color_scheme == "viridis") {
    colors <- viridis::viridis(100)
  } else if (color_scheme == "plasma") {
    colors <- viridis::plasma(100)
  } else if (color_scheme == "coolwarm") {
    # Create a blue-white-red palette
    colors <- colorRampPalette(c("#0571b0", "#92c5de", "#f7f7f7", "#f4a582", "#ca0020"))(100)
  } else {
    colors <- terrain.colors(100)
  }

  # Create the 3D surface plot
  persp_out <- persp(x_seq, y_seq, z_vals,
                     theta = theta,
                     phi = phi,
                     expand = expand,
                     col = colors[cut(z_vals, 100, labels = FALSE)],
                     xlab = xlab,
                     ylab = ylab,
                     zlab = zlab,
                     main = main,
                     ticktype = ticktype,
                     shade = shade,
                     ltheta = ltheta,
                     border = border,
                     box = box)

  # Return the perspective matrix for potential annotation
  invisible(list(
    persp_matrix = persp_out,
    x = x_seq,
    y = y_seq,
    z = z_vals,
    x_range = x_range,
    y_range = y_range,
    z_range = range(z_vals, na.rm = TRUE)
  ))
}

# Alternative using plotly for interactive 3D plot
create_3d_interaction_plotly <- function(gam_model,
                                       x_var = "wind_max_gust",
                                       y_var = "sum_butterflies_direct_sun",
                                       data = NULL,
                                       n_grid = 50,
                                       xlab = "Wind speed (m/s)",
                                       ylab = "Butterflies in sun",
                                       zlab = "Effect",
                                       title = "GAM Interaction Surface",
                                       colorscale = "Viridis",
                                       use_diverging = TRUE,
                                       clip_symmetric = TRUE) {

  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' needed for interactive plot. Please install it.")
  }

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

  # Create grid with proper column names
  pred_grid <- expand.grid(x_seq, y_seq)
  names(pred_grid) <- c(x_var, y_var)

  # Add other required variables at their means
  # Only add variables that are in the model formula
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

  # Get predictions
  pred_vals <- predict(gam_model, newdata = pred_grid, type = "terms", se.fit = FALSE)

  # Find the tensor product column
  ti_pattern <- paste0("ti\\(", x_var, ",", y_var, "\\)|",
                      "ti\\(", y_var, ",", x_var, "\\)")
  ti_col <- grep(ti_pattern, colnames(pred_vals), value = TRUE)

  if (length(ti_col) == 0) {
    stop("No tensor product interaction found for specified variables")
  }

  # Extract z values and reshape to matrix
  z_vals <- matrix(pred_vals[, ti_col], nrow = n_grid, ncol = n_grid)

  # Set up color scale
  if (use_diverging) {
    # Create a diverging red-white-blue color scale
    # Red for negative, white for zero, blue for positive

    if (clip_symmetric) {
      # Find the maximum absolute value for symmetric clipping
      min_val <- min(z_vals, na.rm = TRUE)
      max_val <- max(z_vals, na.rm = TRUE)

      # Use the absolute value of the most negative value as the symmetric limit
      # Clip positive values to same magnitude as negative
      symmetric_limit <- abs(min_val)

      # Clip values to symmetric range
      z_vals[z_vals < -symmetric_limit] <- -symmetric_limit
      z_vals[z_vals > symmetric_limit] <- symmetric_limit

      # Set color scale limits - fully symmetric
      zmin <- -symmetric_limit
      zmax <- symmetric_limit

      cat(sprintf("Using symmetric color scale: %.2f to %.2f\n", zmin, zmax))
      cat(sprintf("Original data range: %.2f to %.2f\n",
                  min(pred_vals[, ti_col], na.rm = TRUE),
                  max(pred_vals[, ti_col], na.rm = TRUE)))
    } else {
      zmin <- min(z_vals, na.rm = TRUE)
      zmax <- max(z_vals, na.rm = TRUE)
    }

    # For symmetric scale, zero is exactly at the midpoint (0.5)
    if (clip_symmetric) {
      zero_pos <- 0.5
    } else {
      # Calculate the position of zero in the color scale
      zero_pos <- abs(zmin) / (abs(zmin) + zmax)
    }

    # Define diverging colorscale: blue (negative) - white (zero) - red (positive)
    # REVERSED: blue for negative, red for positive
    colorscale_diverging <- list(
      list(0, "rgb(8, 48, 107)"),       # Dark blue for most negative
      list(0.25, "rgb(33, 113, 181)"),  # Blue
      list(0.45, "rgb(166, 217, 244)"), # Light blue
      list(0.5, "rgb(255, 255, 255)"),  # White at midpoint (zero)
      list(0.55, "rgb(253, 174, 97)"),  # Light orange
      list(0.75, "rgb(220, 50, 47)"),   # Red
      list(1, "rgb(178, 10, 28)")       # Dark red for most positive
    )

    colorscale_to_use <- colorscale_diverging
  } else {
    colorscale_to_use <- colorscale
    zmin <- min(z_vals, na.rm = TRUE)
    zmax <- max(z_vals, na.rm = TRUE)
  }

  # Create plotly surface plot
  p <- plotly::plot_ly(
    x = x_seq,
    y = y_seq,
    z = z_vals,
    type = "surface",
    colorscale = colorscale_to_use,
    cmin = zmin,  # Use cmin instead of zmin for color scale
    cmax = zmax,  # Use cmax instead of zmax for color scale
    colorbar = list(
      title = zlab,
      tickmode = "linear",
      tick0 = zmin,
      dtick = (zmax - zmin) / 5
    ),
    contours = list(
      z = list(
        show = TRUE,
        usecolormap = TRUE,
        highlightcolor = "#ff0000",
        project = list(z = TRUE)
      )
    )
  ) %>%
    plotly::layout(
      title = title,
      scene = list(
        xaxis = list(title = xlab),
        yaxis = list(title = ylab),
        zaxis = list(
          title = zlab,
          range = c(zmin, zmax)
        ),
        camera = list(
          eye = list(x = 1.5, y = 1.5, z = 1.5)
        )
      )
    )

  return(p)
}

# Create animated HTML version with auto-rotation
create_3d_interaction_plotly_animated <- function(gam_model,
                                                 x_var = "wind_max_gust",
                                                 y_var = "sum_butterflies_direct_sun",
                                                 data = NULL,
                                                 n_grid = 50,
                                                 xlab = "Wind speed (m/s)",
                                                 ylab = "Butterflies in sun",
                                                 zlab = "Effect",
                                                 title = "GAM Interaction Surface",
                                                 use_diverging = TRUE,
                                                 clip_symmetric = TRUE,
                                                 rotation_duration = 10000) {  # milliseconds for full rotation

  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' needed for interactive plot. Please install it.")
  }

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

  # Add other required variables at their means
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

  # Get predictions
  pred_vals <- predict(gam_model, newdata = pred_grid, type = "terms", se.fit = FALSE)

  # Find the tensor product column
  ti_pattern <- paste0("ti\\(", x_var, ",", y_var, "\\)|",
                      "ti\\(", y_var, ",", x_var, "\\)")
  ti_col <- grep(ti_pattern, colnames(pred_vals), value = TRUE)

  if (length(ti_col) == 0) {
    stop("No tensor product interaction found for specified variables")
  }

  # Extract z values and reshape to matrix
  z_vals <- matrix(pred_vals[, ti_col], nrow = n_grid, ncol = n_grid)

  # Set up color scale
  if (use_diverging) {
    if (clip_symmetric) {
      min_val <- min(z_vals, na.rm = TRUE)
      symmetric_limit <- abs(min_val)

      # Clip values to symmetric range
      z_vals[z_vals < -symmetric_limit] <- -symmetric_limit
      z_vals[z_vals > symmetric_limit] <- symmetric_limit

      zmin <- -symmetric_limit
      zmax <- symmetric_limit

      cat(sprintf("Using symmetric color scale: %.2f to %.2f\n", zmin, zmax))
      cat(sprintf("Original data range: %.2f to %.2f\n",
                  min(pred_vals[, ti_col], na.rm = TRUE),
                  max(pred_vals[, ti_col], na.rm = TRUE)))
    } else {
      zmin <- min(z_vals, na.rm = TRUE)
      zmax <- max(z_vals, na.rm = TRUE)
    }

    # Define diverging colorscale
    colorscale_diverging <- list(
      list(0, "rgb(8, 48, 107)"),       # Dark blue for most negative
      list(0.25, "rgb(33, 113, 181)"),  # Blue
      list(0.45, "rgb(166, 217, 244)"), # Light blue
      list(0.5, "rgb(255, 255, 255)"),  # White at midpoint (zero)
      list(0.55, "rgb(253, 174, 97)"),  # Light orange
      list(0.75, "rgb(220, 50, 47)"),   # Red
      list(1, "rgb(178, 10, 28)")       # Dark red for most positive
    )

    colorscale_to_use <- colorscale_diverging
  } else {
    colorscale_to_use <- "Viridis"
    zmin <- min(z_vals, na.rm = TRUE)
    zmax <- max(z_vals, na.rm = TRUE)
  }

  # Create the base plotly surface plot
  p <- plotly::plot_ly(
    x = x_seq,
    y = y_seq,
    z = z_vals,
    type = "surface",
    colorscale = colorscale_to_use,
    cmin = zmin,
    cmax = zmax,
    colorbar = list(
      title = zlab,
      tickmode = "linear",
      tick0 = zmin,
      dtick = (zmax - zmin) / 5
    ),
    contours = list(
      z = list(
        show = TRUE,
        usecolormap = TRUE,
        highlightcolor = "#ff0000",
        project = list(z = TRUE)
      )
    )
  )

  # Add layout with animation configuration
  p <- p %>%
    plotly::layout(
      title = title,
      scene = list(
        xaxis = list(title = xlab),
        yaxis = list(title = ylab),
        zaxis = list(
          title = zlab,
          range = c(zmin, zmax)
        ),
        camera = list(
          eye = list(x = 1.5, y = 1.5, z = 1.5),
          center = list(x = 0, y = 0, z = 0)
        )
      ),
      updatemenus = list(
        list(
          type = "buttons",
          showactive = FALSE,
          buttons = list(
            list(
              label = "Rotate",
              method = "animate",
              args = list(
                NULL,
                list(
                  frame = list(duration = rotation_duration / 36, redraw = TRUE),
                  transition = list(duration = 0),
                  fromcurrent = TRUE,
                  mode = "immediate"
                )
              )
            ),
            list(
              label = "Pause",
              method = "animate",
              args = list(
                list(),
                list(
                  frame = list(duration = 0, redraw = FALSE),
                  mode = "immediate"
                )
              )
            )
          ),
          x = 0.1,
          y = 0,
          xanchor = "left",
          yanchor = "bottom"
        )
      )
    )

  # Generate rotation frames
  n_frames <- 36
  frames <- list()

  for(i in 1:n_frames) {
    angle <- 2 * pi * i / n_frames

    # Calculate camera position for rotation
    camera_x <- 1.5 * cos(angle)
    camera_y <- 1.5 * sin(angle)

    frames[[i]] <- list(
      name = as.character(i),
      data = list(list(
        x = x_seq,
        y = y_seq,
        z = z_vals
      )),
      layout = list(
        scene = list(
          camera = list(
            eye = list(x = camera_x, y = camera_y, z = 1.5)
          )
        )
      )
    )
  }

  # Add frames to the plot
  p <- p %>%
    plotly::animation_opts(
      frame = rotation_duration / n_frames,
      transition = 0,
      redraw = TRUE
    ) %>%
    plotly::animation_slider(
      hide = TRUE
    )

  # Add the frames
  p$x$frames <- frames

  return(p)
}

# Function to create a nice static 3D plot with filled.contour base
create_3d_surface_with_contour <- function(gam_model,
                                         x_var = "wind_max_gust",
                                         y_var = "sum_butterflies_direct_sun",
                                         data = NULL,
                                         n_grid = 50,
                                         xlab = "Wind speed (m/s)",
                                         ylab = "Butterflies in sun",
                                         main = "Tensor Product Interaction Effect") {

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

  # Create grid with proper column names
  pred_grid <- expand.grid(x_seq, y_seq)
  names(pred_grid) <- c(x_var, y_var)

  # Add other required variables at their means
  # Only add variables that are in the model formula
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

  # Get predictions
  pred_vals <- predict(gam_model, newdata = pred_grid, type = "terms", se.fit = FALSE)

  # Find the tensor product column
  ti_pattern <- paste0("ti\\(", x_var, ",", y_var, "\\)|",
                      "ti\\(", y_var, ",", x_var, "\\)")
  ti_col <- grep(ti_pattern, colnames(pred_vals), value = TRUE)

  if (length(ti_col) == 0) {
    stop("No tensor product interaction found for specified variables")
  }

  # Extract z values and reshape to matrix
  z_vals <- matrix(pred_vals[, ti_col], nrow = n_grid, ncol = n_grid)

  # Set up the layout for two plots side by side
  par(mfrow = c(1, 2), mar = c(4, 4, 3, 2))

  # First plot: Filled contour
  filled.contour(x_seq, y_seq, z_vals,
                xlab = xlab,
                ylab = ylab,
                main = "Contour View",
                color.palette = function(n) viridis::viridis(n))

  # Second plot: 3D perspective
  par(mar = c(2, 2, 3, 2))

  # Calculate colors for surface
  colors <- viridis::viridis(100)
  z_facet <- z_vals[-1, -1] + z_vals[-1, -n_grid] +
             z_vals[-n_grid, -1] + z_vals[-n_grid, -n_grid]
  facet_col <- colors[cut(z_facet/4, 100, labels = FALSE)]

  persp(x_seq, y_seq, z_vals,
        theta = 30,
        phi = 20,
        expand = 0.5,
        col = facet_col,
        xlab = xlab,
        ylab = ylab,
        zlab = "Effect",
        main = "3D Surface",
        ticktype = "detailed",
        shade = 0.3,
        ltheta = -120,
        border = NA,
        box = TRUE)

  # Reset par
  par(mfrow = c(1, 1))
}

# Function to create an animated rotating GIF of the 3D surface
create_3d_rotation_gif <- function(gam_model,
                                  x_var = "wind_max_gust",
                                  y_var = "sum_butterflies_direct_sun",
                                  data = NULL,
                                  n_grid = 50,
                                  xlab = "Wind speed (m/s)",
                                  ylab = "Butterflies in sun",
                                  zlab = "Effect",
                                  main = "GAM Interaction Surface",
                                  output_file = "3d_rotation.gif",
                                  n_frames = 36,
                                  fps = 10,
                                  width = 800,
                                  height = 600) {

  if (!requireNamespace("animation", quietly = TRUE)) {
    cat("Package 'animation' not found. Trying magick package instead...\n")

    if (!requireNamespace("magick", quietly = TRUE)) {
      stop("Neither 'animation' nor 'magick' package found. Please install one of them.")
    }

    # Use magick package approach
    return(create_3d_rotation_gif_magick(gam_model, x_var, y_var, data, n_grid,
                                         xlab, ylab, zlab, main, output_file,
                                         n_frames, fps, width, height))
  }

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

  # Add other required variables at their means
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

  # Get predictions
  pred_vals <- predict(gam_model, newdata = pred_grid, type = "terms", se.fit = FALSE)

  # Find the tensor product column
  ti_pattern <- paste0("ti\\(", x_var, ",", y_var, "\\)|",
                      "ti\\(", y_var, ",", x_var, "\\)")
  ti_col <- grep(ti_pattern, colnames(pred_vals), value = TRUE)

  if (length(ti_col) == 0) {
    stop("No tensor product interaction found for specified variables")
  }

  # Extract z values and reshape to matrix
  z_vals <- matrix(pred_vals[, ti_col], nrow = n_grid, ncol = n_grid)

  # Set up color scheme with symmetric clipping
  min_val <- min(z_vals, na.rm = TRUE)
  max_val <- max(z_vals, na.rm = TRUE)
  symmetric_limit <- abs(min_val)

  # Clip values to symmetric range (same as HTML version)
  z_vals_clipped <- z_vals
  z_vals_clipped[z_vals_clipped < -symmetric_limit] <- -symmetric_limit
  z_vals_clipped[z_vals_clipped > symmetric_limit] <- symmetric_limit

  # Use symmetric color scale
  zmin <- -symmetric_limit
  zmax <- symmetric_limit

  # Create diverging color palette - blue (negative) to white (0) to red (positive)
  # White should be exactly at the midpoint since we have symmetric scale
  n_colors <- 100
  n_neg <- 50  # Half for negative values
  n_pos <- 50  # Half for positive values

  # Create symmetric diverging palette
  colors_neg <- colorRampPalette(c("#084b6b", "#2171b5", "#6baed6", "#c6dbef"))(n_neg)
  colors_pos <- colorRampPalette(c("#fcbba1", "#fc9272", "#fb6a4a", "#de2d26", "#a50f15"))(n_pos)
  colors <- c(colors_neg, "white", colors_pos)

  # Map z values to colors - accounting for symmetric scale
  z_facet <- z_vals_clipped[-1, -1] + z_vals_clipped[-1, -n_grid] +
             z_vals_clipped[-n_grid, -1] + z_vals_clipped[-n_grid, -n_grid]

  # Normalize to color range (now symmetric around 0)
  z_norm <- (z_facet/4 - zmin) / (zmax - zmin)
  facet_col_idx <- pmax(1, pmin(length(colors), ceiling(z_norm * length(colors))))
  facet_col <- colors[facet_col_idx]

  # Set fixed axis limits for consistent bounding box
  # Use clipped z_vals for display
  z_range <- c(zmin, zmax)

  # Create animation
  animation::saveGIF({
    # Set up consistent plot parameters
    par(mar = c(3, 3, 3, 3))  # Consistent margins

    for(i in 1:n_frames) {
      theta <- 30 + (i - 1) * 360 / n_frames  # Rotate 360 degrees

      persp(x_seq, y_seq, z_vals_clipped,  # Use clipped values
            theta = theta,
            phi = 25,
            expand = 0.5,
            col = facet_col,
            xlab = xlab,
            ylab = ylab,
            zlab = zlab,
            main = main,
            zlim = z_range,  # Fixed z-axis limits
            ticktype = "detailed",
            shade = 0.3,
            ltheta = -120,
            border = NA,
            box = TRUE,
            r = sqrt(3),  # Fixed viewing distance
            d = 1)        # Fixed perspective amount
    }
  }, movie.name = output_file, interval = 1/fps, ani.width = width, ani.height = height)

  cat(sprintf("Animated GIF saved: %s\n", output_file))
}

# Alternative using magick package (more reliable)
create_3d_rotation_gif_magick <- function(gam_model,
                                         x_var = "wind_max_gust",
                                         y_var = "sum_butterflies_direct_sun",
                                         data = NULL,
                                         n_grid = 50,
                                         xlab = "Wind speed (m/s)",
                                         ylab = "Butterflies in sun",
                                         zlab = "Effect",
                                         main = "GAM Interaction Surface",
                                         output_file = "3d_rotation.gif",
                                         n_frames = 36,
                                         fps = 10,
                                         width = 800,
                                         height = 600) {

  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package 'magick' needed for GIF creation. Please install it.")
  }

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

  # Add other required variables at their means
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

  # Get predictions
  pred_vals <- predict(gam_model, newdata = pred_grid, type = "terms", se.fit = FALSE)

  # Find the tensor product column
  ti_pattern <- paste0("ti\\(", x_var, ",", y_var, "\\)|",
                      "ti\\(", y_var, ",", x_var, "\\)")
  ti_col <- grep(ti_pattern, colnames(pred_vals), value = TRUE)

  if (length(ti_col) == 0) {
    stop("No tensor product interaction found for specified variables")
  }

  # Extract z values and reshape to matrix
  z_vals <- matrix(pred_vals[, ti_col], nrow = n_grid, ncol = n_grid)

  # Set up color scheme with symmetric clipping
  min_val <- min(z_vals, na.rm = TRUE)
  max_val <- max(z_vals, na.rm = TRUE)
  symmetric_limit <- abs(min_val)

  # Clip values to symmetric range
  z_vals_clipped <- z_vals
  z_vals_clipped[z_vals_clipped < -symmetric_limit] <- -symmetric_limit
  z_vals_clipped[z_vals_clipped > symmetric_limit] <- symmetric_limit

  # Use symmetric color scale
  zmin <- -symmetric_limit
  zmax <- symmetric_limit
  z_range <- c(zmin, zmax)

  # Create diverging color palette - blue (negative) to white (0) to red (positive)
  n_colors <- 100
  n_neg <- 50
  n_pos <- 50

  # Create symmetric diverging palette with white at center
  colors_neg <- colorRampPalette(c("#084b6b", "#2171b5", "#6baed6", "#c6dbef"))(n_neg)
  colors_pos <- colorRampPalette(c("#fcbba1", "#fc9272", "#fb6a4a", "#de2d26", "#a50f15"))(n_pos)
  colors <- c(colors_neg, "white", colors_pos)

  # Map z values to colors - accounting for symmetric scale
  z_facet <- z_vals_clipped[-1, -1] + z_vals_clipped[-1, -n_grid] +
             z_vals_clipped[-n_grid, -1] + z_vals_clipped[-n_grid, -n_grid]

  # Normalize to color range (now symmetric around 0)
  z_norm <- (z_facet/4 - zmin) / (zmax - zmin)
  facet_col_idx <- pmax(1, pmin(length(colors), ceiling(z_norm * length(colors))))
  facet_col <- colors[facet_col_idx]

  # Create temporary directory for frames
  temp_dir <- tempdir()
  frame_files <- character(n_frames)

  # Generate frames
  cat("Generating frames...\n")
  for(i in 1:n_frames) {
    theta <- 30 + (i - 1) * 360 / n_frames  # Rotate 360 degrees

    # Create temporary file for this frame
    frame_file <- file.path(temp_dir, sprintf("frame_%03d.png", i))
    png(frame_file, width = width, height = height)

    # Set consistent plot parameters
    par(mar = c(3, 3, 3, 3))  # Consistent margins

    persp(x_seq, y_seq, z_vals_clipped,  # Use clipped values
          theta = theta,
          phi = 25,
          expand = 0.5,
          col = facet_col,
          xlab = xlab,
          ylab = ylab,
          zlab = zlab,
          main = main,
          zlim = z_range,  # Fixed z-axis limits
          ticktype = "detailed",
          shade = 0.3,
          ltheta = -120,
          border = NA,
          box = TRUE,
          r = sqrt(3),  # Fixed viewing distance
          d = 1)        # Fixed perspective amount

    dev.off()
    frame_files[i] <- frame_file

    if (i %% 10 == 0) cat(sprintf("  Frame %d/%d\n", i, n_frames))
  }

  # Read all frames
  cat("Creating animated GIF...\n")
  frames <- magick::image_read(frame_files)

  # Set animation speed (delay in 1/100 seconds)
  frames <- magick::image_animate(frames, fps = fps)

  # Write GIF
  magick::image_write(frames, path = output_file)

  # Clean up temporary files
  unlink(frame_files)

  cat(sprintf("Animated GIF saved: %s\n", output_file))
}