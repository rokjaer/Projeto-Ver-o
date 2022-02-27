#load existing file
FROM rocker/tidyverse:4.0.0
RUN R -e "install.packages('ROAuth')"
RUN R -e "install.packages('tm')"
RUN R -e "install.packages('wordcloud')"
RUN R -e "install.packages('igraph')"
RUN R -e "install.packages('stringr')"
RUN R -e "install.packages('ggeasy')"
RUN R -e "install.packages('janeaustenr')"
RUN R -e "install.packages('widyr')"
RUN R -e "install.packages('httr')"
COPY /sent_abort.R /sent_abort.R
COPY /pos.words_pt.rda /pos.words_pt.rda 
COPY /neg.words_pt.rda /neg.words_pt.rda
COPY /pos.words_es.rda /pos.words_es.rda
COPY /neg.words_es.rda /neg.words_es.rda
COPY /tweets_es.rda /tweets_es.rda
COPY /tweets_es_pos.rda /tweets_es_pos.rda
COPY /tweets_pt.rda /tweets_pt.rda
COPY /tweets_pt_pos.rda /tweets_pt_pos.rda

CMD Rscript /sent_abort.R