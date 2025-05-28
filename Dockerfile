FROM erlang:25-slim

# Install git (needed for rebar3 git deps)
RUN apt-get update && apt-get install -y git

# Install essentianls (needed for rebar3 compiling like cc)
RUN apt-get install -y build-essential

# Setup of initial working directories
WORKDIR /var/www/pollen
VOLUME /var/www/pollen
COPY . /var/www/pollen

# RUN rebar3 compile

# Entrypoint
# CMD ["rebar3", "shell"]
