# Deployment Quick Start Guide

This guide will help you deploy the Shiny Suicide Statistics Explorer on your personal website.

## Recommended: Docker Deployment

### Step 1: Prepare Your Server

You'll need:
- A server (VPS, cloud instance, or dedicated server) with:
  - Ubuntu 20.04+ or Debian 11+ (or any Linux with Docker support)
  - At least 1GB RAM
  - Docker and Docker Compose installed
  - Port 3838 open (or 80/443 if using nginx)

### Step 2: Install Docker (if not already installed)

```bash
# Update package index
sudo apt update

# Install Docker
curl -fsSL https://get.docker.com -o get-docker.sh
sudo sh get-docker.sh

# Install Docker Compose
sudo apt install docker-compose -y

# Add your user to docker group (to run docker without sudo)
sudo usermod -aG docker $USER
# Log out and back in for this to take effect
```

### Step 3: Clone and Deploy

```bash
# Clone the repository
git clone https://github.com/antoinelucasfra/shinyappM2Proj.git
cd shinyappM2Proj

# Build and start the app
docker-compose up -d

# Check if it's running
docker-compose ps
docker-compose logs
```

Your app should now be accessible at `http://your-server-ip:3838`

### Step 4: Set Up Domain and HTTPS (Optional but Recommended)

If you want to use a custom domain with HTTPS:

1. **Point your domain to your server's IP** using your DNS provider

2. **Install nginx** (if not already installed):
```bash
sudo apt install nginx -y
```

3. **Create nginx configuration** (`/etc/nginx/sites-available/shiny-app`):
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
    }
}
```

4. **Enable the site**:
```bash
sudo ln -s /etc/nginx/sites-available/shiny-app /etc/nginx/sites-enabled/
sudo nginx -t
sudo systemctl reload nginx
```

5. **Add HTTPS with Let's Encrypt** (free SSL certificate):
```bash
sudo apt install certbot python3-certbot-nginx -y
sudo certbot --nginx -d your-domain.com
```

Now your app will be accessible at `https://your-domain.com`

## Alternative: shinyapps.io Deployment

If you prefer a hosted solution without managing your own server:

1. **Create an account** at https://www.shinyapps.io (free tier available)

2. **In R/RStudio**:
```r
# Install rsconnect
install.packages('rsconnect')

# Configure account (get credentials from shinyapps.io dashboard)
rsconnect::setAccountInfo(
  name='your-account-name',
  token='your-token',
  secret='your-secret'
)

# Deploy
setwd("path/to/shinyappM2Proj")
rsconnect::deployApp()
```

3. Your app will be available at `https://your-account-name.shinyapps.io/shinyappM2Proj/`

## Maintenance

### Updating the App

```bash
cd shinyappM2Proj
git pull
docker-compose down
docker-compose build
docker-compose up -d
```

### Viewing Logs

```bash
docker-compose logs -f
```

### Stopping the App

```bash
docker-compose down
```

### Restarting the App

```bash
docker-compose restart
```

## Common Issues

**Port 3838 already in use**: Change the port in `docker-compose.yml`:
```yaml
ports:
  - "8080:3838"  # Use port 8080 instead
```

**Can't access from outside**: Check firewall:
```bash
sudo ufw allow 3838/tcp
# or for nginx
sudo ufw allow 80/tcp
sudo ufw allow 443/tcp
```

**App crashes or shows errors**: Check logs:
```bash
docker-compose logs
```

## Support

For issues specific to the app, please open an issue on GitHub:
https://github.com/antoinelucasfra/shinyappM2Proj/issues
