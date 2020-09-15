# install.packages('gutenbergr')
# install.packages('tidytext')
# install.packages('dplyr')

library(gutenbergr)
library(tidytext)
library(dplyr)

libro = gutenberg_download(c(45))

letras = libro %>% unnest_tokens(chars, text, "characters")
