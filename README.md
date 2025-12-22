# shinyappM2Proj

[![Deploy to Posit Connect Cloud](https://github.com/antoinelucasfra/shinyappM2Proj/actions/workflows/deploy-to-connect.yaml/badge.svg)](https://github.com/antoinelucasfra/shinyappM2Proj/actions/workflows/deploy-to-connect.yaml)

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

## Notes and known issues
- If you encounter errors installing `sf`, please install system dependencies for GDAL/PROJ for your OS first.
- Consider using `renv` to lock package versions for reproducibility.

## Deployment

This app is automatically deployed to [Posit Connect Cloud](https://connect.posit.cloud/antoinelucasfra) when pushing to the `main` branch.

### Setting up deployment

1. Go to [Posit Connect Cloud](https://connect.posit.cloud/antoinelucasfra) and sign in
2. Generate an API key from your account settings
3. Add the API key as a GitHub repository secret named `POSIT_CONNECT_API_KEY`:
   - Go to repository Settings → Secrets and variables → Actions
   - Click "New repository secret"
   - Name: `POSIT_CONNECT_API_KEY`
   - Value: your API key from Posit Connect Cloud

The deployment workflow will run automatically on each push to `main`, or can be triggered manually from the Actions tab.
