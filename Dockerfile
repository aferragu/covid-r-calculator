FROM rocker/shiny:4.0.3

WORKDIR /home/shiny/

COPY init.R .
RUN /usr/local/bin/R --no-init-file --no-save --quiet --slave -f init.R

ENV PORT=8080

COPY run.R plumber.R ./
CMD ["/usr/local/bin/R", "--no-save", "--gui-none", "-f run.R"]
