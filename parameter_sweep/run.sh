#!/usr/bin/env bash
#
# Parameter sweep orchestrator.
# Run from repo root: bash parameter_sweep/run.sh
#
# Step 1: Python builds all pair datasets
# Step 2: R fits models and generates plots
#
set -euo pipefail

cd "$(dirname "$0")/.."

echo "========================================"
echo "  PARAMETER SWEEP"
echo "  Started: $(date)"
echo "========================================"

echo ""
echo "Step 1/2: Building pair datasets (Python)..."
echo "----------------------------------------"
uv run python3 parameter_sweep/build_pairs.py

echo ""
echo "Step 2/2: Fitting models and plotting (R)..."
echo "----------------------------------------"
Rscript parameter_sweep/fit_and_plot.R

echo ""
echo "========================================"
echo "  DONE: $(date)"
echo "  Results in: parameter_sweep/"
echo "========================================"
