library(tidyverse)
library(lubridate)

lista <- list.files("csv")

lista <- paste("csv", lista, sep = "/")

KWP <- read_csv(lista)

KWP[is.na(KWP)] = 0

Pol <- KWP %>%
  select()