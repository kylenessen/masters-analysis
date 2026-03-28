# Figure Specification for Insects Manuscript

## MDPI/Insects Journal Requirements

- **Resolution:** 600 dpi minimum
- **Format:** PNG, JPEG, or TIFF (no EPS, no PDF for final submission)
- **Color:** RGB, 8-bit per channel. Full color at no extra cost
- **Content:** English text only, correct mathematical symbols (use `-` not `--`, decimal points not commas)
- **Numbers:** Use comma separator for 5+ digit numbers
- **No editable parts** in images (flatten all layers)
- **No unnecessary marks** (red wavy lines, soft returns, etc.)
- **Publication sizing:** Figures may be reduced to half-page width (~8.5 cm). All text must remain legible at that size

## Global Changes (Apply to ALL figures)

From advisor (Francis) feedback:

1. **Increase axis label font sizes** -- axis titles and tick labels need to be readable at half-page width (~8.5 cm). Recommend minimum 10pt at final print size
2. **Increase axis value font sizes** -- same concern
3. **Y-axis label terminology** -- use "Change in Butterfly Index ($\Delta$BI)" where applicable (was "CiBAI" or "Change in Monarch Butterfly Abundance")
4. **X-axis / legend terminology** -- "BAI" becomes "BI", "CiBAI" becomes "$\Delta$BI" wherever it appears
5. **"Sunset window"** becomes **"Next Day Window"** in any figure text or labels
6. **Export as PNG at 600 dpi** for submission

---

## Figure Inventory

### Figure 1: Temporal Analysis Windows
- **File:** `figures/image1_placeholder.pdf` (replace with actual)
- **LaTeX label:** `fig:temporal_windows`
- **Content:** Schematic showing 30-minute lag analysis and Next Day Window analysis approaches. Shows camera observation intervals, day-night cycles, sunrise/sunset transitions
- **Changes needed:**
  - Rename any "sunset window" labels to "Next Day Window"
  - Ensure font sizes are publication-ready

### Figure 2: Wind Speed vs $\Delta$BI (30-minute)
- **File:** `figures/image2_placeholder.pdf`
- **LaTeX label:** `fig:wind_linear_30min`
- **Content:** Scatter plot with linear regression. X = max wind speed (m/s), Y = $\Delta$BI. Red dashed line at 2 m/s threshold. Blue regression line. n = 1,894
- **Changes needed:**
  - Y-axis label: "Change in Butterfly Index ($\Delta$BI)"
  - Update any "BAI" or "CiBAI" in labels to "BI" / "$\Delta$BI"
  - Increase font sizes

### Figure 3: Wind Speed vs $\Delta$BI (Next Day Window)
- **File:** `figures/image3_placeholder.pdf`
- **LaTeX label:** `fig:wind_linear_sunset`
- **Content:** Scatter plot with linear regression. X = max wind speed, Y = $\Delta$BI over Next Day Window interval. Red dashed at 2 m/s. n = 101
- **Changes needed:**
  - Y-axis label: "Change in Butterfly Index ($\Delta$BI)"
  - Title/annotations: "sunset window" to "Next Day Window"
  - Increase font sizes

### Figure 4: Partial Effects (30-minute, 3-panel)
- **File:** `figures/image4_placeholder.pdf`
- **LaTeX label:** `fig:partial_effects_30min`
- **Content:** Three-panel partial effects plot from M50. Panels: (1) previous BI count, (2) temperature, (3) time since sunrise. Shaded 95% CI. Blue vertical shading for flight threshold temperature range
- **Changes needed:**
  - Y-axis labels: "Partial effect on $\Delta$BI"
  - Update any "BAI" terminology
  - Increase font sizes (critical -- this is a multi-panel figure that will be small in print)

### Figure 5: Wind x Sun Interaction Heatmap (30-minute)
- **File:** `figures/image5_placeholder.pdf`
- **LaTeX label:** `fig:interaction_wind_sun_30min`
- **Content:** Bivariate heatmap. X = max wind speed (m/s), Y = butterflies in direct sun. Color = partial effect on $\Delta$BI (red = positive, blue = negative). Black data points. Red dashed at 2 m/s. Gray masking for unreliable regions
- **Changes needed:**
  - Color bar label: "Partial effect on $\Delta$BI"
  - Axis labels: update terminology
  - Increase font sizes

### Figure 6: QQ and Residual Diagnostics (30-minute)
- **File:** `figures/image6_placeholder.pdf`
- **LaTeX label:** `fig:diagnostics_30min`
- **Content:** Two-panel diagnostic. Left: QQ plot. Right: residuals vs fitted values
- **Changes needed:**
  - Increase font sizes
  - Minor: ensure axis labels are clear at reduced size

### Figure 7: ACF Plot (30-minute)
- **File:** `figures/image7_placeholder.pdf`
- **LaTeX label:** `fig:acf_30min`
- **Content:** Autocorrelation function of model residuals. Blue dashed 95% confidence bounds
- **Changes needed:**
  - Increase font sizes

### Figure 8: Threshold Interaction Heatmap (MOVED TO APPENDIX)
- **File:** `figures/image8_placeholder.pdf`
- **LaTeX label:** Previously `fig:threshold_interaction` -- removed from main text in #110
- **Status:** This figure was removed from the main manuscript when threshold results were condensed. If the appendix needs it, regenerate with updated terminology. Otherwise, skip

### Figure 9: Partial Effects Controls (Next Day Window, 2-panel)
- **File:** `figures/image9_placeholder.pdf`
- **LaTeX label:** `fig:partial_effects_sunset`
- **Content:** Two-panel. Left: previous-day max BI (strong negative). Right: window duration (no effect). Shaded 95% CI
- **Changes needed:**
  - Y-axis labels: "Partial effect on $\Delta$BI"
  - X-axis: update any "BAI" to "BI"
  - Title references: "sunset window" to "Next Day Window"
  - Increase font sizes

### Figure 10: Wind x Sun Interaction Heatmap (Next Day Window)
- **File:** `figures/image10_placeholder.pdf`
- **LaTeX label:** `fig:interaction_wind_sun_sunset`
- **Content:** Same format as Figure 5 but for Next Day Window analysis. Color = partial effect on $\Delta$BI (square-root transformed scale)
- **Changes needed:**
  - Color bar label: "Partial effect on $\Delta$BI"
  - Update terminology
  - Increase font sizes

### Figure 11: QQ and Residual Diagnostics (Next Day Window)
- **File:** `figures/image11_placeholder.pdf`
- **LaTeX label:** `fig:diagnostics_sunset`
- **Content:** Two-panel diagnostic for M32
- **Changes needed:**
  - Increase font sizes

### Figure 12: ACF Plot (Next Day Window)
- **File:** `figures/image12_placeholder.pdf`
- **LaTeX label:** `fig:acf_sunset`
- **Content:** ACF of M32 residuals
- **Changes needed:**
  - Increase font sizes

### Figure 13: Wind x Sun Interaction Heatmap (24-hour robustness)
- **File:** `figures/image13_placeholder.pdf`
- **LaTeX label:** `fig:interaction_wind_sun_24hr`
- **Content:** Same format as Figures 5/10 but for 24-hour robustness analysis
- **Changes needed:**
  - Color bar label: "Partial effect on $\Delta$BI"
  - Update terminology
  - Increase font sizes

---

## Output Checklist

For each figure:
- [ ] Terminology updated (BAI->BI, CiBAI->$\Delta$BI, sunset window->Next Day Window)
- [ ] Font sizes increased for half-page reproduction
- [ ] Exported as PNG at 600 dpi
- [ ] RGB color mode, 8-bit
- [ ] No editable text layers (flattened)
- [ ] Decimal points (not commas) for numbers
- [ ] Placed in `figures/` directory with descriptive filename (e.g., `fig1_temporal_windows.png`)

## File Naming Convention

Replace placeholder files with final versions. Suggested naming:
```
figures/fig01_temporal_windows.png
figures/fig02_wind_vs_dbi_30min.png
figures/fig03_wind_vs_dbi_nextday.png
figures/fig04_partial_effects_30min.png
figures/fig05_interaction_wind_sun_30min.png
figures/fig06_diagnostics_30min.png
figures/fig07_acf_30min.png
figures/fig08_threshold_interaction.png  (appendix only, if needed)
figures/fig09_partial_effects_nextday.png
figures/fig10_interaction_wind_sun_nextday.png
figures/fig11_diagnostics_nextday.png
figures/fig12_acf_nextday.png
figures/fig13_interaction_wind_sun_24hr.png
```

Once final figures are placed, update `\includegraphics` paths in `manuscript.tex`.
