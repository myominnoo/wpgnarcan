# Pulling Rocker image with RStudio and R version 4.2
FROM rocker/rstudio:4.5

ARG QUARTO_VERSION=1.2.335
ENV QUARTO_VERSION=$QUARTO_VERSION

# Disabling the authentication step
ENV USER="rstudio"
CMD ["/usr/lib/rstudio-server/bin/rserver", "--server-daemonize", "0", "--auth-none", "1"]

# Install jq to parse json files
# Layer 6
RUN apt-get update && apt-get install -y --no-install-recommends \
    jq \
    libxml2-dev \
    zlib1g \
    g++-11 \
    libz-dev \
    freetype2-demos \
    libpng-dev \
    libtiff-dev \
    libjpeg-dev \
    make \
    fontconfig \
    libfribidi-dev \
    libharfbuzz-dev \
    libfontconfig1-dev \
    git \
    vim \
    libgl1 \
    libegl1 \
    libxrandr2 \
    libxss1 \
    libxcursor1 \
    libxcomposite1 \
    libasound2t64 \
    libxi6 \
    libxtst6 \
    gpg-agent \
    libcurl4-openssl-dev \
    && rm -rf /var/lib/apt/lists/*

# installing R packages
RUN mkdir packages 
COPY install_packages.R packages/
COPY packages.json packages/
RUN Rscript packages/install_packages.R

# Installing Quarto
COPY install_quarto.sh packages/
RUN  bash packages/install_quarto.sh $QUARTO_VERSION

# Install additional R packages
RUN Rscript -e "remotes::install_github('Chicago/RSocrata')"

EXPOSE 8787