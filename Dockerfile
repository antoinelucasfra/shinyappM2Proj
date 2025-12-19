# Use the rocker/shiny base image which has R and Shiny Server pre-installed
FROM rocker/shiny:4.3.2

# Install system dependencies for R packages (especially sf which needs gdal/proj)
RUN apt-get update && apt-get install -y \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /srv/shiny-server/shinyappM2Proj

# Copy the app files
COPY . .

# Install R package dependencies
RUN R -e "source('setup.R')"

# Copy custom Shiny Server configuration
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Make sure the app is executable
RUN chmod -R 755 /srv/shiny-server

# Expose port 3838 (default Shiny Server port)
EXPOSE 3838

# Run Shiny Server
CMD ["/usr/bin/shiny-server"]
