# Base image https://hub.docker.com/u/rocker/
FROM tosku/rob-men:1.2

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

#RUN apt install openjdk-14-jdk -y
#RUN R CMD javareconf

# copy necessary files
## app folder
COPY rob-men /srv/shiny-server
## renv.lock file
#COPY rob-men/renv.lock renv.lock

# install renv & restore packages
#RUN Rscript -e 'install.packages("renv")'
#RUN Rscript -e 'renv::consent(provided = TRUE)'
#RUN Rscript -e 'renv::restore()'



EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
