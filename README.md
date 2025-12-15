# shinyappM2Proj

This repository contains a Shiny application (Suicide statistics explorer) developed as part of a Master 2 course at Agrocampus Ouest by Zoe Wante, Antoine Lucas and Chloe Tellier.

## What is included
- A Shiny app that visualises suicide counts and rates by country, year, sex, age, and generation.
- Data used: `data/suicide_coord.csv` and world borders shapefile in `data/world/`.

## Quick start
1. Open an R session in the project root (or use RStudio).
2. Install packages (run once):

```r
source("setup.R")
```

Note: `sf` may require system dependencies (gdal/proj). See the message printed by `setup.R` if installation fails.

If you only have `data/suicide_coord.csv` and no shapefile in `data/world/`, the app will still run: the map panel will show point markers (using latitude/longitude from the CSV) instead of country polygons.

3. Run the app:

```r
# from an R session inside the project root
shiny::runApp()
# or
source("app.R")
```

## Files and structure
- `app.R`: launcher that sources `global.R`, `ui.R` and `server.R` and starts the app.
- `global.R`: loads libraries and data.
- `data_management.R`: data parsing and validation helpers.
- `ui.R` and `server.R`: Shiny UI and server code.
- `setup.R`: installs package dependencies.

## Improvements made
- Removed automatic package installation from data-loading code and made installation explicit in `setup.R`.
- Added `global.R` and `app.R` to provide a clear app entry point and reliable startup ordering.
- Improved data parsing (use `readr::read_csv2`) and type conversions (Latitude/Longitude -> numeric).
- Added basic data validation with clear error messages when required files/columns are missing.
- Updated `ui.R` to generate choices dynamically from data instead of hard-coded ranges.
- Added guidance in README on how to install dependencies and run the app.

## Notes and known issues
- If you encounter errors installing `sf`, please install system dependencies for GDAL/PROJ for your OS first.
- Consider using `renv` to lock package versions for reproducibility.

If you'd like, I can also add a small vignette or convert this repo to an R package with `renv` support.
