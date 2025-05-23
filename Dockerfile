FROM erlang:25-slim

# Setup of initial working directories
WORKDIR /var/www/pollen

VOLUME /var/www/pollen

COPY . /var/www/pollen
