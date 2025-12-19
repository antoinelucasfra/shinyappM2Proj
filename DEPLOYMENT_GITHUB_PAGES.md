# Deploying to shinyapps.io and Integrating with Your GitHub Pages Site

This guide explains how to deploy the Shiny Suicide Statistics Explorer app to shinyapps.io and integrate it into your personal website at https://antoinelucasfra.github.io.

## Overview

Since GitHub Pages only serves static HTML/CSS/JS files and cannot run Shiny apps (which require an R server), we'll use **shinyapps.io** (Posit's free hosting service) to host the Shiny app, then integrate it into your GitHub Pages website.

## Part 1: Deploy to shinyapps.io

### Step 1: Create a shinyapps.io Account

1. Go to https://www.shinyapps.io/
2. Sign up for a free account (supports up to 5 applications and 25 active hours per month)
3. After signing in, go to **Account → Tokens** to get your deployment credentials

### Step 2: Install rsconnect Package

In R or RStudio, install the rsconnect package if you don't have it:

```r
install.packages('rsconnect')
```

### Step 3: Configure Your Account

From the shinyapps.io dashboard, copy your token information and run:

```r
rsconnect::setAccountInfo(
  name='<your-account-name>',
  token='<your-token>',
  secret='<your-secret>'
)
```

Replace `<your-account-name>`, `<your-token>`, and `<your-secret>` with the values from your shinyapps.io account.

### Step 4: Deploy the App

Navigate to the project directory and deploy:

```r
# Set working directory to the app folder
setwd("path/to/shinyappM2Proj")

# Deploy the app
rsconnect::deployApp(
  appName = "suicide-statistics-explorer",
  appTitle = "Suicide Statistics Explorer"
)
```

The deployment process will:
- Bundle your app files
- Upload them to shinyapps.io
- Install required packages on the server
- Start your app

After deployment, you'll receive a URL like:
`https://<your-account-name>.shinyapps.io/suicide-statistics-explorer/`

### Step 5: Test Your App

Visit the URL to ensure your app is working correctly on shinyapps.io.

## Part 2: Integrate into Your GitHub Pages Website

Now we'll add this app to your portfolio at https://antoinelucasfra.github.io.

### Option A: Add as a Project Card (Recommended)

1. Clone your GitHub Pages repository (if not already):
```bash
git clone https://github.com/antoinelucasfra/antoinelucasfra.github.io.git
cd antoinelucasfra.github.io
```

2. Edit `projects.qmd` and add a new project entry in the listing section:

```yaml
      - path: https://<your-account-name>.shinyapps.io/suicide-statistics-explorer/
        image: assets/images/suicide-stats-preview.png  # Add a screenshot
        title: "📊 Suicide Statistics Explorer"
        description: "Interactive Shiny dashboard for visualizing global suicide data by country, year, sex, age, and generation. Built with R Shiny and Leaflet."
        date: "2025-12-19"
        categories: [R, Shiny, Data Visualization]
```

3. (Optional) Add a screenshot:
   - Take a screenshot of your app
   - Save it as `assets/images/suicide-stats-preview.png`
   - Or use the placeholder image that's already there

4. Commit and push:
```bash
git add projects.qmd
# git add assets/images/suicide-stats-preview.png  # if you added an image
git commit -m "Add Suicide Statistics Explorer to projects"
git push
```

### Option B: Create a Dedicated Project Page with Embedded App

If you want a full page for this project:

1. Create `projects/suicide-statistics-explorer.qmd`:

```yaml
---
title: "Suicide Statistics Explorer"
description: "Interactive dashboard for visualizing global suicide statistics"
date: "2025-12-19"
categories: [R, Shiny, Data Visualization]
image: ../assets/images/suicide-stats-preview.png
---

## Overview

An interactive Shiny application that visualizes suicide counts and rates by country, year, sex, age, and generation using data from 1985 to 2016.

## Features

- **Interactive Map**: Explore suicide rates across countries with an interactive Leaflet map
- **Time Series Analysis**: Analyze trends over time by various demographic factors
- **Country Rankings**: Compare countries by suicide rates and total counts
- **Data Exploration**: Browse and filter the raw dataset

## Technologies

- R Shiny for the interactive web application
- Leaflet for interactive mapping
- Plotly for interactive charts
- Tidyverse for data manipulation

## Live Application

<iframe src="https://<your-account-name>.shinyapps.io/suicide-statistics-explorer/" width="100%" height="800px" style="border: 1px solid #ccc; border-radius: 4px;"></iframe>

[Open in new window](https://<your-account-name>.shinyapps.io/suicide-statistics-explorer/){.btn .btn-primary target="_blank"}

## Source Code

The complete source code is available on GitHub:

[View on GitHub](https://github.com/antoinelucasfra/shinyappM2Proj){.btn .btn-secondary}

## About the Data

The dataset includes suicide statistics from 1985 to 2016 across multiple countries, with breakdowns by:
- Age groups
- Gender
- Generation
- Geographic coordinates for mapping
```

2. Update `projects.qmd` to link to this page:

```yaml
      - path: projects/suicide-statistics-explorer.qmd
        image: assets/images/suicide-stats-preview.png
        title: "📊 Suicide Statistics Explorer"
        description: "Interactive Shiny dashboard for visualizing global suicide data by country, year, sex, age, and generation."
        date: "2025-12-19"
        categories: [R, Shiny, Data Visualization]
```

3. Commit and push as above.

## Part 3: Maintenance

### Updating the App

When you make changes to your Shiny app:

1. Test locally:
```r
shiny::runApp()
```

2. Deploy updates:
```r
rsconnect::deployApp()
```

### Managing Your shinyapps.io Account

- Free tier limits: 5 apps, 25 active hours/month
- Monitor usage in your shinyapps.io dashboard
- Apps automatically sleep after 15 minutes of inactivity (on free tier)
- First visitor after sleep will experience ~10 second startup time

### Troubleshooting

**App won't deploy:**
- Check that all required packages are listed in the app
- Ensure data files are in the correct location
- Check shinyapps.io logs for error messages

**App shows errors:**
- View logs at: `https://www.shinyapps.io/admin/#/application/<app-id>`
- Common issues: missing packages, incorrect file paths

**Exceeded hours limit:**
- Upgrade to a paid plan, or
- Archive less-used apps
- Apps on free tier automatically sleep after inactivity

## Alternative: Using Shinylive (Experimental)

For a fully client-side solution that runs entirely in the browser without a server, you could convert your app to use [shinylive](https://posit-dev.github.io/r-shinylive/). However, this requires:
- Converting the app to work with shinylive (some packages may not be supported)
- Larger initial page load
- All processing happens in the user's browser

For most use cases, shinyapps.io is the recommended approach.

## Summary

1. ✅ Deploy Shiny app to shinyapps.io (free hosting)
2. ✅ Add project card to your GitHub Pages site
3. ✅ Optional: Create dedicated project page with embedded app
4. ✅ Your portfolio now includes an interactive Shiny application!

Your app will be accessible at:
- **Direct link**: `https://<your-account-name>.shinyapps.io/suicide-statistics-explorer/`
- **From your portfolio**: `https://antoinelucasfra.github.io/projects.html`
