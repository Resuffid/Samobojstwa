library(tidyverse)
library(lubridate)

lista_plikow <- list.files("csv")

lista_plikow <- paste("csv", lista_plikow, sep = "/")

dane_bez <- read_csv(lista_plikow)

df <- dane_bez%>%
  replace(is.na(.), 0)

df1 <- df%>%
  mutate(wojewodztwo = case_when(`Przedział wiekowy/KWP`=="BG KWP Bydgoszcz" ~ "kuj-pom",
                                 `Przedział wiekowy/KWP`=="BK KWP Białystok" ~ "podlaskie",
                                 `Przedział wiekowy/KWP`=="GD KWP Gdańsk" ~ "pomorskie",
                                 `Przedział wiekowy/KWP`=="GO KWP Gorzów Wlkp." ~ "lubuskie",
                                 `Przedział wiekowy/KWP`=="KA KWP Katowice" ~ "slaskie",
                                 `Przedział wiekowy/KWP`=="KI KWP Kielce" ~ "swietokrzyskie",
                                 `Przedział wiekowy/KWP`=="KK KWP Kraków" ~ "malopolskie",
                                 `Przedział wiekowy/KWP`=="LD KWP Łódź" ~ "lodzkie",
                                 `Przedział wiekowy/KWP`=="LU KWP Lublin" ~ "lubelskie",
                                 `Przedział wiekowy/KWP`=="OL KWP Olsztyn" ~ "warm-maz",
                                 `Przedział wiekowy/KWP`=="OP KWP Opole" ~ "opolskie",
                                 `Przedział wiekowy/KWP`=="PO KWP Poznań" ~ "wielkopolskie",
                                 `Przedział wiekowy/KWP`=="RZ KWP Rzeszów" ~ "podkarpackie",
                                 `Przedział wiekowy/KWP`=="SC KWP Szczecin" ~ "zachpom",
                                 `Przedział wiekowy/KWP`=="WA KSP Warszawa" ~ "mazowieckie",
                                 `Przedział wiekowy/KWP`=="WA KWP Radom" ~ "mazowieckie",
                                 `Przedział wiekowy/KWP`=="WR KWP Wrocław" ~ "dolnoslaskie",
                                 `Przedział wiekowy/KWP`=="Razem" ~ "razem")
  )%>%
  select(-"Przedział wiekowy/KWP")
