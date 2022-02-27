FROM rocker/tidyverse:4.0.3
RUN R -e "install.packages('ROAuth')"
RUN R -e "install.packages('hms')"
RUN R -e "install.packages('tm')"
RUN R -e "install.packages('wordcloud')"
RUN R -e "install.packages('stringr')"
RUN R -e "install.packages('ggeasy')"
RUN R -e "install.packages('janeaustenr')"
RUN R -e "install.packages('widyr')"
RUN R -e "install.packages('httr')"
COPY /projeto.R /projeto.R

COPY /ttxt.rda /ttxt.rda 
COPY /ttxta.rda /ttxta.rda 
COPY /ttxtd.rda /ttxtd.rda
COPY /positive.rda /positive.rda
COPY /negative.rda /negative.rda


CMD Rscript /projeto.R