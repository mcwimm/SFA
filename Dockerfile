FROM rocker/shiny-verse

RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    cmake \
    libcurl4-gnutls-dev \
	libcairo2-dev \
	libxt-dev \
	libssl-dev \
	libssh2-1-dev

RUN rm -rf /var/lib/apt/lists/*
RUN rm -rf /tmp/downloaded_packages

RUN install2.r --skipinstalled --error --deps TRUE shinydashboard
RUN install2.r --skipinstalled --error --deps TRUE shinycssloaders
RUN install2.r --skipinstalled --error --deps TRUE shinyFiles
RUN install2.r --skipinstalled --error --deps TRUE markdown
RUN install2.r --skipinstalled --error --deps TRUE DT
RUN install2.r --skipinstalled --error --deps TRUE ggpubr
RUN install2.r --skipinstalled --error --deps TRUE scales

RUN addgroup --system app && adduser --system --ingroup app app

WORKDIR /home/app

COPY app .

RUN chown app:app -R /home/app

USER app

EXPOSE 3838

ENTRYPOINT R -e "shiny::runApp('/home/app',3838,FALSE,'0.0.0.0')"
