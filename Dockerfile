#load existing file
FROM rocker/tidyverse:4.1.2
RUN R -e "install.packages('rtweet')"
RUN R -e "install.packages('twitteR')"
RUN R -e "install.packages('ROAuth')"
RUN R -e "install.packages('hms')"
RUN R -e "install.packages('lubridate')"
RUN R -e "install.packages('tidytext')"
RUN R -e "install.packages('tm')"
RUN R -e "install.packages('wordcloud')"
RUN R -e "install.packages('igraph')"
RUN R -e "install.packages('glue')"
RUN R -e "install.packages('plyr')"
RUN R -e "install.packages('stringr')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('ggeasy')"
RUN R -e "install.packages('plotly')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('magrittr')"
RUN R -e "install.packages('tidyverse')"
RUN R -e "install.packages('janeaustenr')"
RUN R -e "install.packages('widyr')"
RUN R -e "install.packages('httr')"
COPY /projeto.R /projeto.R
COPY /tweets.Rda /tweets.Rda 
COPY /tweetsantes.Rda /tweetsantes.Rda
COPY /tweetsdepois.Rda /tweetsdepois.Rda


CMD Rscript /projeto.R