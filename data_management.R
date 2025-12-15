## data_management.R
## Lightweight data preparation helpers for the Shiny app.
##
## This file no longer installs packages automatically. Use `setup.R` to
## install the packages required to run the app. This file assumes those
## packages are available and focuses on reading/parsing the project data
## and performing basic validation and type conversion.

# Read suicide data (semicolon-separated CSV exported for the project).
# The CSV uses '.' as decimal separator but ';' as delimiter, so use
# `read_delim()` with an explicit locale to avoid warnings about decimal/grouping marks.
suicide <- readr::read_delim(
  "data/suicide_coord.csv",
  delim = ";",
  locale = readr::locale(decimal_mark = "."),
  show_col_types = FALSE
)

# Normalize column names to syntactic names (e.g. "Capital/Major City" -> "Capital.Major.City")
names(suicide) <- make.names(names(suicide))

# Basic type corrections and factor levels
suicide <- suicide |>
  dplyr::mutate(
    country = as.factor(country),
    sex = as.factor(sex),
    age = factor(
      age,
      levels = c(
        "5-14 years",
        "15-24 years",
        "25-34 years",
        "35-54 years",
        "55-74 years",
        "75+ years"
      )
    ),
    generation = as.factor(generation),
    Capital.Major.City = as.factor(Capital.Major.City),
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  )

# Basic validation checks and informative messages
if (nrow(suicide) == 0) {
  stop("`suicide` dataset appears empty. Check data/suicide_coord.csv")
}
if (
  !all(c("country", "year", "suicides_no", "population") %in% names(suicide))
) {
  stop(
    "`suicide` dataset is missing required columns (country, year, suicides_no, population)"
  )
}

# Read world borders shapefile. The folder `data/world` should contain a shapefile
# (TM_WORLD_BORDERS-0.3.*). We try to read the shapefile and provide an informative
# message if it fails.
world_shp <- list.files("data/world", pattern = "\\.shp$", full.names = TRUE)
if (length(world_shp) == 0) {
  message(
    "No shapefile found in data/world. The map will fall back to point markers using coordinates from the CSV."
  )
  world <- NULL
} else {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "Shapefile found in data/world but package 'sf' is not installed. Install 'sf' to use the polygon map features."
    )
  }
  world <- sf::read_sf(world_shp[1])
  # Coerce NAME to character for safer joins
  if (!"NAME" %in% names(world)) {
    stop(
      "`world` shapefile does not contain a 'NAME' column required for joins"
    )
  }
  world$NAME <- as.character(world$NAME)
}

# Provide a small helper that computes suicide rate per 100k
# Provide a small helper that computes suicide rate per 100k
suicide_rate <- function(df) {
  df |>
    dplyr::group_by(dplyr::across(dplyr::everything())) |>
    dplyr::summarise(.groups = "drop")
}
# small helper to check if world polygons are available
has_world <- !is.null(world)

# End of data_management.R
