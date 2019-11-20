FROM rocker/tidyverse:3.6.1
MAINTAINER "Ed Jee" edjee96@gmail.com
USER root



RUN apt-get update \
	&& apt-get install -y --no-install-recommends apt-utils ed libnlopt-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/

COPY install.R /home/rstudio/
RUN chown -R  rstudio /home/rstudio/

RUN if [ -f /home/rstudio/install.R ]; then R --quiet -f /home/rstudio/install.R; fi








