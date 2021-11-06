# Adapted from https://github.com/rpodcast/r_dev_projects/blob/main/.devcontainer/Dockerfile
# License: MIT
FROM rocker/r-ver:4.1.0

# [Choice] Ubuntu version (use hirsuite or bionic on local arm64/Apple Silicon): hirsute, focal, bionic
ARG INSTALL_ZSH="true"
ARG UPGRADE_PACKAGES="true"

# Install needed packages and setup non-root user. Use a separate RUN statement to add your own dependencies.
ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID
COPY library-scripts/*.sh /tmp/library-scripts/
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && /bin/bash /tmp/library-scripts/common-debian.sh "${INSTALL_ZSH}" "${USERNAME}" "${USER_UID}" "${USER_GID}" "${UPGRADE_PACKAGES}" "true" "true" \
    && usermod -a -G staff ${USERNAME} \
    && apt-get -y install \
        python3-pip \
        libgit2-dev \
        libcurl4-openssl-dev \
        libssl-dev \
        libxml2-dev \
        libxt-dev \
        # specific to eplusr
        ## units
        libudunits2-dev \
        ## rgl
        libfreetype6-dev \
        libglu1-mesa-dev \
        libpng-dev \
        libgl1-mesa-dev \
        pandoc \
        zlib1g-dev \
        libicu-dev \
    && apt-get autoremove -y && apt-get clean -y && rm -rf /var/lib/apt/lists/* /tmp/library-scripts \
    && python3 -m pip --no-cache-dir install radian \
    && install2.r --error --skipinstalled --ncpus -1 \
        devtools \
        languageserver \
        httpgd \
    && rm -rf /tmp/downloaded_packages

# Install package dependencies
RUN export DEBIAN_FRONTEND=noninteractive \
    && install2.r --error --skipinstalled --ncpus -1 \
        callr \
        checkmate \
        cli \
        crayon \
        data.table \
        hms \
        lubridate \
        methods \
        processx \
        progress \
        R6 \
        RSQLite \
        stringi \
        units \
        decido \
        rgl \
    && rm -rf /tmp/downloaded_packages

# R Session watcher settings.
# See more details: https://github.com/REditorSupport/vscode-R/wiki/R-Session-watcher
RUN echo 'source(file.path(Sys.getenv("HOME"), ".vscode-R", "init.R"))' >> ${R_HOME}/etc/Rprofile.site