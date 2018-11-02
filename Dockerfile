FROM r-base:latest

MAINTAINER The Tech "techno@tech.mit.edu"

RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev/unstable \
    libxt-dev \
    libssl-dev

# Download and install shiny server
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb

RUN R -e "install.packages(c('shiny'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('rmarkdown'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('flexdashboard'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('dplyr'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('ggplot2'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('tidyr'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('reshape2'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('plotly'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('DT'), repos='http://cran.rstudio.com/')"

COPY /myapp/ /srv/shiny-server/myapp/


EXPOSE 8080

COPY shiny-server.sh /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]
