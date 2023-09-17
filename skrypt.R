library(tidyverse)
library(lubridate)


lista_plikow <- list.files("csv")

lista_plikow <- paste("csv", lista_plikow, sep = "/")

dane_bez <- read_csv(lista_plikow)

df <- dane%>%
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

lista_plikow1 <- list.files("bezrob")

lista_plikow1 <- paste("bezrob", lista_plikow1, sep = "/")

dane_bez <- read_csv(lista_plikow1)

dol <- dane_bez%>%
  subset(Województwa == "Woj. DOLNOŚLĄSKIE")

pol <- dane_bez%>%
  subset(Województwa == "POLSKA")

kujpom <- dane_bez%>%
  subset(Województwa == "Woj. KUJAWSKO-POMORSKIE")

zachpom <- dane_bez%>%
  subset(Województwa == "Woj. ZACHODNIOPOMORSKIE")

wlkp <- dane_bez%>%
  subset(Województwa == "Woj. WIELKOPOLSKIE")

warmmaz <- dane_bez%>%
  subset(Województwa == "Woj. WARMIŃSKO-MAZURSKIE")

swt <- dane_bez%>%
  subset(Województwa == "Woj. ŚWIĘTOKRZYSKIE")

slk <- dane_bez%>%
  subset(Województwa == "Woj. ŚLĄSKIE")

pom <- dane_bez%>%
  subset(Województwa == "Woj. POMORSKIE")

pdl <- dane_bez%>%
  subset( Województwa == "Woj. PODLASKIE")

pod <- dane_bez%>%
  subset(Województwa == "Woj. PODKARPACKIE")

opo <- dane_bez%>%
  subset(Województwa == "Woj. OPOLSKIE")

mazo <- dane_bez%>%
  subset(Województwa == "Woj. MAZOWIECKIE")

mlp <- dane_bez%>%
  subset(Województwa == "Woj. MAŁOPOLSKIE")

ldz <- dane_bez%>%
  subset(Województwa == "Woj. ŁÓDZKIE")

lubu <- dane_bez%>%
  subset(Województwa == "Woj. LUBUSKIE")

lube <- dane_bez%>%
  subset(Województwa =="Woj. LUBELSKIE")

dfall <- pol%>%
  full_join(dol)%>%
  full_join(kujpom)%>%
  full_join(zachpom)%>%
  full_join(pom)%>%
  full_join(ldz)%>%
  full_join(lubu)%>%
  full_join(lube)%>%
  full_join(mlp)%>%
  full_join(mazo)%>%
  full_join(opo)%>%
  full_join(pdl)%>%
  full_join(pod)%>%
  full_join(slk)%>%
  full_join(swt)%>%
  full_join(warmmaz)%>%
  full_join(wlkp)

df1 <- df1%>%
  group_by(wojewodztwo)

write.csv(dfall, "bezrob.csv")

bezrob <- read.csv("bezrob.csv")

write.csv(df1, "sam.csv")

samobojstwa <- read.csv("sam.csv")


inflacja <- read.csv("inflacja.csv")

lista_plikowenv <- list.files("pollution")

lista_plikowenv <- paste("pollution", lista_plikowenv, sep = "/")

pollution <- read_csv(lista_plikowenv)

bezrob <- bezrob %>%
  group_by(Województwa, Data)

samobojstwa <- samobojstwa %>%
  group_by(wojewodztwo, Data)

inflacja <- inflacja%>%
  mutate(inflacja = Inflacja-100)%>%
  select(-c(Inflacja))

samo_infl <- samobojstwa%>%
  select(Razem, wojewodztwo, Data)%>%
  filter(wojewodztwo=="polska")

samo_infl <- merge(inflacja, samo_infl, by="Data")

samo_infl <- samo_infl%>%
  select(-c(wojewodztwo, Jednostka.terytorialna))

write_csv(samo_infl, "analiza1.csv")

analiza1 <- read.csv("analiza1.csv")

ggplot(data = analiza1, aes(x = Data, y = suicide_prom))+
  geom_point()


cor.test(analiza1$Razem, analiza1$inflacja)

cor(analiza1$Razem, analiza1$inflacja)

analiza1 <- analiza1%>%
  mutate(date = as.Date(Data, format = "%b/%Y"))

pol2017 <- 38433558
analiza1 <- analiza1 %>%
  mutate(suicide_prom = (Razem/pol2017)*1000)

class(analiza1$Data)

analiza1 <- analiza1 %>%
  mutate(date=dmy(analiza1$Data))

write_csv(analiza1, "analiza2.csv")

analiza1 <- analiza1%>%
  arrange(date)%>%
  select(-c("Lp.", "Data"))

ggplot(analiza1, aes(date, suicide_prom))+
  geom_line(na.rm = TRUE)+
  geom_line(aes(y=inflacja))
