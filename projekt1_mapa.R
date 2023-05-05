library(dplyr)
library(mapdata)
library(ggplot2)
library(SmarterPoland)
mapaswiata <- map_data("world") %>% 
  rename(country = region)
mapaswiata[mapaswiata == "UK"] <- "United Kingdom"

europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Republic","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden", "United Kingdom", "Ukraine", "Turkey", "Switzerland",
                   "Serbia", "Norway", "Montenegro", "Moldova", "Luxembourg",
                   "Iceland", "Georgia", "Bosnia and Herzegovina", "Belarus", "Azerbaijan", "Armenia",
                   "Andorra", "Albania")

dane <- read.csv("df_complete-2.csv", header=TRUE, stringsAsFactors = FALSE, na.string="")
dane <- dane %>% rename("name"="track")
dane2 <- read.csv("charts.csv")
nowa <- merge(dane2, dane, by.x = "name", all.x = FALSE ,all.y= FALSE)
ostateczna <- select(nowa, name, artists, streams, danceability, country, position, year, energy, tempo)
danceable <- arrange(ostateczna, -danceability) 
countries_codes <- read.csv("wikipedia-iso-country-codes.csv")
countries_codes <- countries_codes %>%
  mutate(country_code = tolower(Alpha.2.code))
wszystko <- merge(countries_codes, danceable, by.x = "country_code", by.y = "country", 
                  all.x = FALSE, all.y = TRUE) 
wszystko <- wszystko %>% 
  rename(country = English.short.name.lower.case)
wszystko_razem <- merge(wszystko, countries, by.x = "country", by.y = "country", 
                        all.x = TRUE, all.y = FALSE)

domapy <- wszystko_razem%>%
  filter(year == 2018) %>% 
  filter(continent=="Europe")
dance_suma <- domapy %>% group_by(country) %>% summarise(dance = mean(danceability))
europamapa <- mapaswiata[which(mapaswiata$country%in%europeanUnion), ]
mapa <- full_join(europamapa, dance_suma, by = "country", all.y = TRUE)
mapa$przedziały = cut(mapa$dance, breaks=seq(from=0.667, to=0.778, length.out=8))
dancemap <- ggplot(data = mapa) +
  geom_polygon(aes(x = long, y = lat, fill = przedziały, group = group),colour = "white")+
  coord_fixed(1.5) +
  scale_fill_manual(values = c("#d37bfb", "#b937f7", "#7b04ba", "#3d0459"))+
  labs(title = "Poziom taneczności najpopularniejszych utworów",
       x = "",
       y = "") +
  theme( panel.background = element_rect(fill = "black", colour = "black", size = 0.5, linetype = "solid"),plot.background=element_rect(fill = "black"),
         legend.background = element_rect(fill = "black"), plot.title = element_text(color = "white", size = 15),
         axis.title = element_text(color = "white"),
         axis.text = element_blank(),
         legend.title = element_text(color="white"),
         legend.text = element_text(color="white"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())