FROM debian AS builder

#####################################################################
# INSTALL PREREQUISITES
#####################################################################
RUN apt update -y \
    && apt-get -y install \
        sudo \
        libtool \
        build-essential \
        git \
        man \
        curl \
        wget \
        bash \
        libffi-dev \
        libffi6 \
        libgmp-dev \
        libgmp10 \
        libncurses-dev \
        libncurses5 \
        libtinfo5 \
        libtinfo-dev \
        pkg-config \
        libssl-dev \
        libsystemd-dev \
        zlib1g-dev \
        npm \
        r-base \
        libcurl4-openssl-dev

RUN npm install -g yarn
RUN npm install -g purescript purescript-spago --unsafe-perm=true

#####################################################################
# INSTALL GHCUP
#####################################################################
RUN curl --proto '=https' --tlsv1.2 -sSf https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup -o /usr/local/bin/ghcup \
    && chmod +x /usr/local/bin/ghcup
ENV PATH=root/.cabal/bin:/root/.ghcup/bin:$PATH


#####################################################################
# INSTALL CORRECT GHC VERSION
#####################################################################
RUN ghcup install cabal \
    && ghcup install ghc 8.8.3 \
    && ghcup install ghc 8.10.1 \
    && ghcup set ghc 8.8.3 \
    && cabal update


#####################################################################
# INSTALL R DEPS FOR SHINY
#####################################################################
RUN Rscript -e 'install.packages("shiny", repos="https://cloud.r-project.org")' \
    && Rscript -e 'install.packages("shinyjs", repos="https://cloud.r-project.org")' \
    && Rscript -e 'install.packages("curl", repos="https://cloud.r-project.org")' \
    && Rscript -e 'install.packages("httr", repos="https://cloud.r-project.org")' \
    && Rscript -e 'install.packages("plotly", repos="https://cloud.r-project.org")' \
    && Rscript -e 'install.packages("purrr", repos="https://cloud.r-project.org")'


COPY nix.conf /etc/nix/nix.conf
RUN sudo groupadd -g 30000 nixbld \
    && sudo useradd -u 30000 -g nixbld -G nixbld nixbld
RUN curl -L https://nixos.org/nix/install | sh

RUN echo "USER=fake source /root/.nix-profile/etc/profile.d/nix.sh" >> $HOME/.bashrc

# Environment variables that could be used, but default to empty in the servers
ENV GITHUB_CLIENT_ID="fake"
ENV GITHUB_CLIENT_SECRET="fake"
ENV JWT_SIGNATURE="fake"
ENV GITHUB_REDIRECT_URL="fake"
ENV WEBGHC_URL="fake"

EXPOSE 8080 8009 8081