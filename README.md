# Quarto COVID-19 Dashboard

> **Note:** Interactive charts will not render in GitHub preview — please download the HTML file.

A sample Quarto-based epidemiologic dashboard developed as a portfolio piece.

This project originated from a coding assessment request to analyze an outbreak line list and deliver a situational report for epidemiologists. The underlying data is a synthetic COVID-19 [dataset](https://github.com/appliedepi/epiRhandbook_eng/tree/master/data/covid_example_data) provided by the global non-profit organization *Applied Epi*.

------------------------------------------------------------------------

## ⚠️ Viewing Instructions (Important)

**This dashboard is a self-contained HTML file and must be downloaded and opened locally in a web browser to render correctly.**\
GitHub’s file preview does **not** support the JavaScript required for interactive charts and tooltips. To viewL

1. Download `quarto-dashboard.html` 
2. Open the file locally in your browser 
3. View in full screen for best experience

Tips for use and interactivity are included on the dashboard landing page.

------------------------------------------------------------------------

## Project Structure

-   `quarto-dashboard.qmd` – Quarto source file
-   `scripts/` – Modularized data preparation and helper functions
-   `covid_dashboard.html` – Rendered dashboard output (download required)

Key visualization and analytic steps are shown inline in the Quarto Markdown (QMD) file, with data wrangling abstracted for readability and reuse.
