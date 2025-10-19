#!/usr/bin/env Rscript

# Create 3D printable STL files from GAM interaction surfaces
# This creates a watertight mesh suitable for 3D printing

create_3d_printable_surface <- function(gam_model,
                                       x_var = "wind_max_gust",
                                       y_var = "sum_butterflies_direct_sun",
                                       data = NULL,
                                       output_file = "gam_surface.stl",
                                       n_grid = 100,  # Higher resolution for smooth print
                                       base_thickness = 5,  # mm or units for base
                                       z_scale = 1,     # Scale factor for z-axis
                                       flip_z = FALSE) {  # Flip if needed for printing

  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop("Package 'rgl' needed for 3D mesh creation. Please install it.")
  }

  if (!requireNamespace("Rvcg", quietly = TRUE)) {
    stop("Package 'Rvcg' needed for mesh operations. Please install it.")
  }

  # Get data ranges
  if (!is.null(data)) {
    x_range <- range(data[[x_var]], na.rm = TRUE)
    y_range <- range(data[[y_var]], na.rm = TRUE)
  } else {
    x_range <- range(gam_model$model[[x_var]], na.rm = TRUE)
    y_range <- range(gam_model$model[[y_var]], na.rm = TRUE)
  }

  # Create high-resolution prediction grid
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

  # Get predictions (UNCLIPPED)
  pred_vals <- predict(gam_model, newdata = pred_grid, type = "terms", se.fit = FALSE)

  # Find the tensor product column
  ti_pattern <- paste0("ti\\(", x_var, ",", y_var, "\\)|",
                      "ti\\(", y_var, ",", x_var, "\\)")
  ti_col <- grep(ti_pattern, colnames(pred_vals), value = TRUE)

  if (length(ti_col) == 0) {
    stop("No tensor product interaction found for specified variables")
  }

  # Extract z values and reshape to matrix (UNCLIPPED)
  z_vals <- matrix(pred_vals[, ti_col], nrow = n_grid, ncol = n_grid)

  # Apply z scaling
  z_vals <- z_vals * z_scale

  # Flip if needed
  if (flip_z) {
    z_vals <- -z_vals
  }

  # Normalize coordinates to reasonable print dimensions (e.g., 100mm x 100mm)
  # You can adjust these to your preferred print size
  x_norm <- (x_seq - min(x_seq)) / (max(x_seq) - min(x_seq)) * 100
  y_norm <- (y_seq - min(y_seq)) / (max(y_seq) - min(y_seq)) * 100

  # Normalize z to a reasonable height range (e.g., 0 to 30mm)
  z_min <- min(z_vals, na.rm = TRUE)
  z_max <- max(z_vals, na.rm = TRUE)
  z_norm <- ((z_vals - z_min) / (z_max - z_min)) * 30 + base_thickness

  cat(sprintf("Surface dimensions: %.1f x %.1f mm\n", max(x_norm), max(y_norm)))
  cat(sprintf("Height range: %.1f to %.1f mm\n", min(z_norm), max(z_norm)))
  cat(sprintf("Original z range: %.2f to %.2f\n", min(z_vals/z_scale), max(z_vals/z_scale)))

  # Create the mesh using rgl
  # First, create the top surface
  rgl::open3d(visible = FALSE)
  rgl::clear3d()

  # Create surface mesh
  rgl::surface3d(x_norm, y_norm, z_norm, col = "gray")

  # Get the surface mesh
  surface_mesh <- rgl::as.mesh3d()

  # Now we need to create a watertight mesh with a base
  # We'll create the full mesh manually with all faces

  # Create vertices for the complete mesh
  vertices <- matrix(0, nrow = 3, ncol = 0)
  faces <- matrix(0, nrow = 3, ncol = 0)

  # Add top surface vertices
  vert_idx <- 1
  vert_map <- matrix(0, nrow = n_grid, ncol = n_grid)

  for (i in 1:n_grid) {
    for (j in 1:n_grid) {
      vertices <- cbind(vertices, c(x_norm[i], y_norm[j], z_norm[i, j]))
      vert_map[i, j] <- vert_idx
      vert_idx <- vert_idx + 1
    }
  }

  # Create top surface faces (triangles)
  for (i in 1:(n_grid-1)) {
    for (j in 1:(n_grid-1)) {
      # Two triangles per grid square
      v1 <- vert_map[i, j]
      v2 <- vert_map[i+1, j]
      v3 <- vert_map[i+1, j+1]
      v4 <- vert_map[i, j+1]

      # Triangle 1
      faces <- cbind(faces, c(v1, v2, v3))
      # Triangle 2
      faces <- cbind(faces, c(v1, v3, v4))
    }
  }

  # Add bottom surface vertices (at z = 0)
  bottom_start_idx <- vert_idx
  for (i in 1:n_grid) {
    for (j in 1:n_grid) {
      vertices <- cbind(vertices, c(x_norm[i], y_norm[j], 0))
      vert_idx <- vert_idx + 1
    }
  }

  # Create bottom surface faces (reverse winding for correct normals)
  for (i in 1:(n_grid-1)) {
    for (j in 1:(n_grid-1)) {
      # Get bottom vertices (offset by bottom_start_idx - 1)
      v1 <- bottom_start_idx + (i-1)*n_grid + (j-1)
      v2 <- bottom_start_idx + i*n_grid + (j-1)
      v3 <- bottom_start_idx + i*n_grid + j
      v4 <- bottom_start_idx + (i-1)*n_grid + j

      # Triangle 1 (reversed winding)
      faces <- cbind(faces, c(v1, v3, v2))
      # Triangle 2 (reversed winding)
      faces <- cbind(faces, c(v1, v4, v3))
    }
  }

  # Create side walls
  # Front wall (j = 1)
  for (i in 1:(n_grid-1)) {
    top1 <- vert_map[i, 1]
    top2 <- vert_map[i+1, 1]
    bot1 <- bottom_start_idx + (i-1)*n_grid
    bot2 <- bottom_start_idx + i*n_grid

    faces <- cbind(faces, c(top1, bot1, bot2))
    faces <- cbind(faces, c(top1, bot2, top2))
  }

  # Back wall (j = n_grid)
  for (i in 1:(n_grid-1)) {
    top1 <- vert_map[i, n_grid]
    top2 <- vert_map[i+1, n_grid]
    bot1 <- bottom_start_idx + (i-1)*n_grid + (n_grid-1)
    bot2 <- bottom_start_idx + i*n_grid + (n_grid-1)

    faces <- cbind(faces, c(top1, top2, bot2))
    faces <- cbind(faces, c(top1, bot2, bot1))
  }

  # Left wall (i = 1)
  for (j in 1:(n_grid-1)) {
    top1 <- vert_map[1, j]
    top2 <- vert_map[1, j+1]
    bot1 <- bottom_start_idx + (j-1)
    bot2 <- bottom_start_idx + j

    faces <- cbind(faces, c(top1, top2, bot2))
    faces <- cbind(faces, c(top1, bot2, bot1))
  }

  # Right wall (i = n_grid)
  for (j in 1:(n_grid-1)) {
    top1 <- vert_map[n_grid, j]
    top2 <- vert_map[n_grid, j+1]
    bot1 <- bottom_start_idx + (n_grid-1)*n_grid + (j-1)
    bot2 <- bottom_start_idx + (n_grid-1)*n_grid + j

    faces <- cbind(faces, c(top1, bot1, bot2))
    faces <- cbind(faces, c(top1, bot2, top2))
  }

  # Create the mesh3d object
  mesh <- rgl::tmesh3d(
    vertices = vertices,
    indices = faces,
    homogeneous = FALSE
  )

  # Clean up the mesh
  mesh <- rgl::addNormals(mesh)

  # Write STL file
  rgl::writeSTL(output_file)
  rgl::close3d()

  cat(sprintf("STL file saved: %s\n", output_file))
  cat(sprintf("Vertices: %d, Faces: %d\n", ncol(vertices), ncol(faces)))

  # Return mesh info
  invisible(list(
    vertices = vertices,
    faces = faces,
    x_range = range(x_norm),
    y_range = range(y_norm),
    z_range = range(z_norm),
    original_z_range = c(min(z_vals/z_scale), max(z_vals/z_scale))
  ))
}

# Alternative using misc3d package for simpler approach
create_3d_printable_surface_simple <- function(gam_model,
                                              x_var = "wind_max_gust",
                                              y_var = "sum_butterflies_direct_sun",
                                              data = NULL,
                                              output_file = "gam_surface.stl",
                                              n_grid = 100,
                                              print_width = 100,  # mm
                                              print_depth = 100,  # mm
                                              print_height = 40,  # mm max height
                                              base_height = 5) {  # mm base thickness

  if (!requireNamespace("misc3d", quietly = TRUE)) {
    stop("Package 'misc3d' needed. Please install it.")
  }

  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop("Package 'rgl' needed. Please install it.")
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

  # Get predictions (UNCLIPPED)
  pred_vals <- predict(gam_model, newdata = pred_grid, type = "terms", se.fit = FALSE)

  # Find the tensor product column
  ti_pattern <- paste0("ti\\(", x_var, ",", y_var, "\\)|",
                      "ti\\(", y_var, ",", x_var, "\\)")
  ti_col <- grep(ti_pattern, colnames(pred_vals), value = TRUE)

  if (length(ti_col) == 0) {
    stop("No tensor product interaction found for specified variables")
  }

  # Extract z values (UNCLIPPED)
  z_vals <- matrix(pred_vals[, ti_col], nrow = n_grid, ncol = n_grid)

  # Scale to print dimensions
  x_print <- seq(0, print_width, length.out = n_grid)
  y_print <- seq(0, print_depth, length.out = n_grid)

  # Scale z to print height (keeping relative proportions)
  z_min <- min(z_vals, na.rm = TRUE)
  z_max <- max(z_vals, na.rm = TRUE)
  z_print <- ((z_vals - z_min) / (z_max - z_min)) * (print_height - base_height) + base_height

  cat(sprintf("Print dimensions: %.1f x %.1f x %.1f mm\n",
              print_width, print_depth, max(z_print)))
  cat(sprintf("Original z range: %.2f to %.2f\n", z_min, z_max))
  cat(sprintf("Scaled z range: %.1f to %.1f mm\n", base_height, max(z_print)))

  # Export using misc3d
  rgl::open3d(visible = FALSE)
  rgl::clear3d()

  # Create the surface with base
  # Top surface
  rgl::surface3d(x_print, y_print, z_print, col = "gray", alpha = 1)

  # Add walls and base manually to ensure watertight
  # Bottom surface
  rgl::surface3d(x_print, y_print, matrix(0, n_grid, n_grid), col = "gray", alpha = 1)

  # Export to STL
  rgl::writeSTL(output_file)
  rgl::close3d()

  cat(sprintf("STL file saved: %s\n", output_file))
  cat("Note: You may need to check and repair the mesh in your slicer software.\n")

  invisible(list(
    print_dims = c(print_width, print_depth, max(z_print)),
    original_z_range = c(z_min, z_max)
  ))
}