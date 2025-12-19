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

## Deployment

### Docker Deployment (Recommended for Personal Website)

This app can be easily deployed on any server using Docker. This is the recommended approach for deploying to a personal website.

#### Prerequisites
- Docker installed on your server
- Docker Compose (optional, but recommended)
- Internet connection for building the image (to download R packages)

#### Option 1: Using Docker Compose (Easiest)

1. Clone this repository to your server:
```bash
git clone https://github.com/antoinelucasfra/shinyappM2Proj.git
cd shinyappM2Proj
```

2. Build and run the container:
```bash
docker-compose up -d
```

The first build will take 5-10 minutes as it installs all R packages. Subsequent runs will be much faster.

3. The app will be available at `http://your-server-ip:3838`

4. To stop the app:
```bash
docker-compose down
```

5. To view logs:
```bash
docker-compose logs -f
```

#### Option 2: Using Docker directly

1. Build the Docker image:
```bash
docker build -t shiny-suicide-stats .
```

2. Run the container:
```bash
docker run -d -p 3838:3838 --name shiny-app shiny-suicide-stats
```

3. The app will be available at `http://your-server-ip:3838`

4. To view logs:
```bash
docker logs -f shiny-app
```

#### Production Deployment Tips

For production deployment on your personal website:

1. **Use a reverse proxy** (nginx or Apache) to:
   - Serve the app on port 80/443 instead of 3838
   - Add SSL/TLS certificates for HTTPS
   - Set up a custom domain

2. **Example nginx configuration**:
```nginx
server {
    listen 80;
    server_name your-domain.com;

    location / {
        proxy_pass http://localhost:3838;
        proxy_redirect off;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
```

3. **For automatic restarts**, the docker-compose.yml includes `restart: unless-stopped`

4. **Monitor resource usage**: The app may require 512MB-1GB RAM depending on usage

### shinyapps.io Deployment (Alternative)

You can also deploy to Posit's shinyapps.io:

1. Install rsconnect:
```r
install.packages('rsconnect')
```

2. Configure your shinyapps.io account (get credentials from https://www.shinyapps.io/admin/#/tokens):
```r
rsconnect::setAccountInfo(name='<ACCOUNT>', token='<TOKEN>', secret='<SECRET>')
```

3. Deploy the app:
```r
rsconnect::deployApp()
```

## Troubleshooting

### Docker Build Issues

**Problem**: Packages fail to install during `docker build`
- **Solution**: Ensure your server has internet access. The build process downloads R packages from CRAN.

**Problem**: "sf" package installation fails
- **Solution**: The Dockerfile includes all necessary system dependencies (gdal, proj, geos). If issues persist, ensure you're using a recent Docker version.

**Problem**: Build takes too long
- **Solution**: The first build takes 5-10 minutes to install all R packages. This is normal. Use Docker layer caching to speed up subsequent builds.

### Runtime Issues

**Problem**: App won't start or shows errors in logs
- **Solution**: Check logs with `docker logs <container-name>`. Common issues:
  - Missing data files: Ensure `data/suicide_coord.csv` exists
  - Port already in use: Change the port mapping in docker-compose.yml or `docker run` command

**Problem**: Can't access app from outside the server
- **Solution**: 
  - Check firewall rules allow port 3838
  - For cloud servers (AWS, GCP, Azure), configure security groups
  - Consider using a reverse proxy (nginx) for production

## Notes and known issues
- If you encounter errors installing `sf`, please install system dependencies for GDAL/PROJ for your OS first.
- Consider using `renv` to lock package versions for reproducibility.
- The Docker image includes all necessary system dependencies for the `sf` package.
