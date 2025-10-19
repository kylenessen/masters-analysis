#!/usr/bin/env python3

"""
Create 3D printable STL files from GAM surface data exported from R
"""

import numpy as np
import pandas as pd
from stl import mesh
import argparse
import os

def create_stl_from_surface(csv_file, output_file,
                           print_width=100, print_depth=100,
                           max_height=40, base_height=5):
    """
    Create an STL file from surface data in CSV format.

    Parameters:
    -----------
    csv_file : str
        Path to CSV file with columns: x, y, z
    output_file : str
        Path for output STL file
    print_width : float
        Width of print in mm
    print_depth : float
        Depth of print in mm
    max_height : float
        Maximum height of surface in mm
    base_height : float
        Base thickness in mm
    """

    # Read the surface data
    df = pd.read_csv(csv_file)

    # Get unique x and y values
    x_unique = np.sort(df['x'].unique())
    y_unique = np.sort(df['y'].unique())
    n_x = len(x_unique)
    n_y = len(y_unique)

    # Reshape z values into a grid
    z_grid = df.pivot(index='y', columns='x', values='z').values

    # Scale to print dimensions
    x_scaled = np.linspace(0, print_width, n_x)
    y_scaled = np.linspace(0, print_depth, n_y)

    # Scale z values (UNCLIPPED from R)
    z_min = np.min(z_grid)
    z_max = np.max(z_grid)
    z_scaled = ((z_grid - z_min) / (z_max - z_min)) * (max_height - base_height) + base_height

    print(f"Original z range: {z_min:.2f} to {z_max:.2f}")
    print(f"Scaled z range: {base_height:.1f} to {np.max(z_scaled):.1f} mm")
    print(f"Grid size: {n_x} x {n_y}")

    # Create vertices and faces for the mesh
    vertices = []
    faces = []

    # Create vertex grid for top surface
    vertex_grid = np.zeros((n_y, n_x), dtype=int)
    vertex_index = 0

    for i in range(n_y):
        for j in range(n_x):
            vertices.append([x_scaled[j], y_scaled[i], z_scaled[i, j]])
            vertex_grid[i, j] = vertex_index
            vertex_index += 1

    # Create faces for top surface
    for i in range(n_y - 1):
        for j in range(n_x - 1):
            v1 = vertex_grid[i, j]
            v2 = vertex_grid[i, j + 1]
            v3 = vertex_grid[i + 1, j + 1]
            v4 = vertex_grid[i + 1, j]

            # Two triangles per grid square
            faces.append([v1, v2, v3])
            faces.append([v1, v3, v4])

    # Add bottom surface vertices
    bottom_start = vertex_index
    for i in range(n_y):
        for j in range(n_x):
            vertices.append([x_scaled[j], y_scaled[i], 0])
            vertex_index += 1

    # Create faces for bottom surface (reversed winding)
    for i in range(n_y - 1):
        for j in range(n_x - 1):
            v1 = bottom_start + i * n_x + j
            v2 = bottom_start + i * n_x + j + 1
            v3 = bottom_start + (i + 1) * n_x + j + 1
            v4 = bottom_start + (i + 1) * n_x + j

            faces.append([v1, v3, v2])
            faces.append([v1, v4, v3])

    # Add side walls
    # Front wall (i = 0)
    for j in range(n_x - 1):
        top1 = vertex_grid[0, j]
        top2 = vertex_grid[0, j + 1]
        bot1 = bottom_start + j
        bot2 = bottom_start + j + 1

        faces.append([top1, bot1, bot2])
        faces.append([top1, bot2, top2])

    # Back wall (i = n_y - 1)
    for j in range(n_x - 1):
        top1 = vertex_grid[n_y - 1, j]
        top2 = vertex_grid[n_y - 1, j + 1]
        bot1 = bottom_start + (n_y - 1) * n_x + j
        bot2 = bottom_start + (n_y - 1) * n_x + j + 1

        faces.append([top1, top2, bot2])
        faces.append([top1, bot2, bot1])

    # Left wall (j = 0)
    for i in range(n_y - 1):
        top1 = vertex_grid[i, 0]
        top2 = vertex_grid[i + 1, 0]
        bot1 = bottom_start + i * n_x
        bot2 = bottom_start + (i + 1) * n_x

        faces.append([top1, top2, bot2])
        faces.append([top1, bot2, bot1])

    # Right wall (j = n_x - 1)
    for i in range(n_y - 1):
        top1 = vertex_grid[i, n_x - 1]
        top2 = vertex_grid[i + 1, n_x - 1]
        bot1 = bottom_start + i * n_x + n_x - 1
        bot2 = bottom_start + (i + 1) * n_x + n_x - 1

        faces.append([top1, bot1, bot2])
        faces.append([top1, bot2, top2])

    # Convert to numpy arrays
    vertices = np.array(vertices)
    faces = np.array(faces)

    # Create the mesh
    surface_mesh = mesh.Mesh(np.zeros(faces.shape[0], dtype=mesh.Mesh.dtype))
    for i, face in enumerate(faces):
        for j in range(3):
            surface_mesh.vectors[i][j] = vertices[face[j], :]

    # Save to STL file
    surface_mesh.save(output_file)

    print(f"STL file saved: {output_file}")
    print(f"Vertices: {len(vertices)}, Faces: {len(faces)}")
    print(f"Print dimensions: {print_width:.1f} x {print_depth:.1f} x {np.max(z_scaled):.1f} mm")

    return {
        'vertices': len(vertices),
        'faces': len(faces),
        'dimensions': (print_width, print_depth, float(np.max(z_scaled))),
        'original_z_range': (float(z_min), float(z_max))
    }

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Create STL from GAM surface CSV')
    parser.add_argument('csv_file', help='Input CSV file with x, y, z columns')
    parser.add_argument('output_file', help='Output STL file')
    parser.add_argument('--width', type=float, default=100, help='Print width in mm')
    parser.add_argument('--depth', type=float, default=100, help='Print depth in mm')
    parser.add_argument('--height', type=float, default=40, help='Max height in mm')
    parser.add_argument('--base', type=float, default=5, help='Base thickness in mm')

    args = parser.parse_args()

    create_stl_from_surface(
        args.csv_file,
        args.output_file,
        args.width,
        args.depth,
        args.height,
        args.base
    )