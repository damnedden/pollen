FROM erlang:25-slim

# Setup of initial working directories
WORKDIR /var/www/minichat

VOLUME /var/www/minichat

COPY . /var/www/minichat
