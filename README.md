# shinyappM2Proj

This repository contains a Shiny application (Suicide statistics explorer) developed as part of a Master 2 course at Agrocampus Ouest by Zoe Wante, Antoine Lucas and Chloe Tellier.

**🚀 Want to deploy this app to your personal website (GitHub Pages)?** See [DEPLOYMENT_GITHUB_PAGES.md](DEPLOYMENT_GITHUB_PAGES.md) for complete instructions on deploying to shinyapps.io and integrating with your GitHub Pages site!

## What is included
- A Shiny app that visualises suicide counts and rates by country, year, sex, age, and generation.
- Data used: `data/suicide_coord.csv` and world borders shapefile in `data/world/`.
- Easy deployment script for shinyapps.io (`deploy_to_shinyapps.R`)
- Project template for GitHub Pages integration (`github_pages_project_template.qmd`)

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
- `deploy_to_shinyapps.R`: deployment script for shinyapps.io.
- `github_pages_project_template.qmd`: template for adding the app to a Quarto-based GitHub Pages site.

## Deployment

### Deploy to shinyapps.io

The easiest way to make your Shiny app accessible online (and integrate it with GitHub Pages):

1. Install and configure rsconnect:
```r
install.packages('rsconnect')
rsconnect::setAccountInfo(name='<ACCOUNT>', token='<TOKEN>', secret='<SECRET>')
```
Get your credentials from https://www.shinyapps.io/admin/#/tokens

2. Run the deployment script:
```r
source("deploy_to_shinyapps.R")
```

3. Your app will be deployed to: `https://<your-account>.shinyapps.io/suicide-statistics-explorer/`

For detailed instructions on integrating with your GitHub Pages website, see [DEPLOYMENT_GITHUB_PAGES.md](DEPLOYMENT_GITHUB_PAGES.md).

## Notes and known issues
- If you encounter errors installing `sf`, please install system dependencies for GDAL/PROJ for your OS first.
- Consider using `renv` to lock package versions for reproducibility.
- **GitHub Pages limitation**: GitHub Pages only serves static files and cannot run Shiny apps directly. Use shinyapps.io (free tier available) to host the app, then embed or link it from your GitHub Pages site.
