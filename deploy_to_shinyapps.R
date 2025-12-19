## deploy_to_shinyapps.R
## Script to deploy the Shiny app to shinyapps.io
##
## Prerequisites:
## 1. Install rsconnect: install.packages('rsconnect')
## 2. Configure your account (run once):
##    rsconnect::setAccountInfo(name='<ACCOUNT>', token='<TOKEN>', secret='<SECRET>')
##    Get these values from: https://www.shinyapps.io/admin/#/tokens
##
## Usage:
##    source("deploy_to_shinyapps.R")

# Check if rsconnect is installed
if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop(
    "Package 'rsconnect' is required but not installed.\n",
    "Install it with: install.packages('rsconnect')\n",
    "Then configure your account with:\n",
    "rsconnect::setAccountInfo(name='<ACCOUNT>', token='<TOKEN>', secret='<SECRET>')"
  )
}

# Check if account is configured
accounts <- rsconnect::accounts()
if (nrow(accounts) == 0) {
  stop(
    "No shinyapps.io account configured.\n",
    "Configure your account with:\n",
    "rsconnect::setAccountInfo(name='<ACCOUNT>', token='<TOKEN>', secret='<SECRET>')\n",
    "Get credentials from: https://www.shinyapps.io/admin/#/tokens"
  )
}

# Display configured accounts
cat("Configured shinyapps.io accounts:\n")
print(accounts)
cat("\n")

# App configuration
app_name <- "suicide-statistics-explorer"
app_title <- "Suicide Statistics Explorer"

# Confirm deployment
cat("Ready to deploy:\n")
cat("  App name: ", app_name, "\n")
cat("  App title: ", app_title, "\n")
cat("  Account: ", accounts$name[1], "\n")
cat("\n")

response <- readline(prompt = "Deploy to shinyapps.io? (yes/no): ")

if (tolower(response) %in% c("yes", "y")) {
  cat("\nDeploying app to shinyapps.io...\n")
  cat("This may take a few minutes...\n\n")
  
  # Deploy the app
  rsconnect::deployApp(
    appName = app_name,
    appTitle = app_title,
    forceUpdate = TRUE,
    launch.browser = TRUE
  )
  
  cat("\n✓ Deployment complete!\n")
  cat("Your app should open in your browser.\n")
  cat("URL: https://", accounts$name[1], ".shinyapps.io/", app_name, "/\n", sep = "")
} else {
  cat("Deployment cancelled.\n")
}
