
require(hrbrthemes, quietly = T)
require(tidytext, quietly = T)

require(ggrepel, quietly = T)
require(stringi, quietly = T)
require(ggthemes, quietly = T)
require(lubridate, quietly = T)

require(RColorBrewer, quietly = T)
#require(hrbthemes, quietly = T)
require(tidyverse, quietly = T)
# Sys.setlocale("LC_TIME", "es_ES.UTF-8") MAC
# Sys.setlocale("LC_TIME", "Spanish") WINDOWS

setwd("~/GitHub/Nexos_COVID_Twitter/")
caption1 <- "Elaboración propia con datos de Twitter | @lolo7no | @guzmart_"

# Menciones

ats <- readRDS("01_datos/ats_fecha.rds") %>%
  complete(fecha, ats)%>%
  mutate(n = ifelse(is.na(n), 0, n))%>%
  group_by(ats)%>%
  mutate(
    n_acumulada = cumsum(n)
  )%>%
  filter(n_acumulada>0)%>%
  mutate(fecha = as.POSIXct(fecha, 
                            format = "%Y-%m-%d %T"),
         dia = as.numeric(fecha-min(fecha)),
         max = max(n_acumulada))%>%
  ungroup()%>%
  mutate(rank = as.numeric(as.factor(rank(-max))),
         principales = ifelse(rank <= 10, as.character(ats), "Otros"))%>%
  filter(rank<=30)%>%
  group_by(ats)%>%
  mutate(ultimodia = ifelse(dia == max(dia), T, F)) %>%
  ungroup()



ur <- ggplot(ats, 
             aes(x = fecha,
                 group = ats,
                 color = reorder(principales, rank)), 
             size = 1) +
  geom_point(aes(y = ifelse(ultimodia, n_acumulada, NA)))+
  geom_line(aes(x = fecha,
                group = ats,
                color = reorder(principales, rank),
                y = ifelse(principales == "Otros",n_acumulada, NA)),
            lineend = "round",
            size = 1)+
  geom_line(aes(x = fecha,
                group = ats,
                color = reorder(principales, rank),
                y = ifelse(principales != "Otros",n_acumulada, NA)),
            lineend = "round",
            size = 1)+
  geom_text_repel(aes(y = n_acumulada,
                      label = ifelse(ultimodia & principales != "Otros",
                                     as.character(paste0("bold(",
                                                         "\"",
                                                         ats, 
                                                         "\"",
                                                         ")~ (",
                                                         "\"\n",
                                                         scales::comma(n_acumulada),
                                                         "\"",
                                                         ")")), 
                                     NA)),
                  hjust = "left",
                  color = "black",
                  segment.color = "grey70",
                  box.padding = 0.01,
                  direction = "y",
                  force = 2,
                  parse = T,
                  nudge_x = -0.2,
                  nudge_y = 3,
                  size = 5)+
  scale_x_datetime(date_breaks = "1 day", date_labels =  "%b/%d %H:00")+
  scale_colour_manual(values = c(brewer.pal(10, "Spectral"),
                                 "grey80"))+
  labs(title = str_wrap("Total de menciones acumuladas más importantes de cuentas de Twitter en debate sobre COVID-19", 70),
       subtitle = paste0("Menciones sumadas cada minuto; las 10 más importantes del",
                         fectoprint, " de 2020"),
       caption = "Elaboración con datos de Twitter",
       colour = "Cuenta",
       x = "Tiempo",
       y = element_blank()) +
  theme_minimal() +
  theme(plot.title = element_text(size = 25),
      plot.subtitle = element_text(size = 15),
      plot.caption = element_text(size = 12),
      legend.key.size = unit(1, "cm"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10),
      axis.title.x = element_text(size = 20),
      axis.text.y = element_text(size = 15),
      axis.text.x = element_text(angle = 90),
      legend.position="none")

ggsave("03_graficas/linea_tiempo_tweets_ats.png", plot = ur,
       width = 15, height = 10, dpi = 85)


##### hashtags

hashes <- readRDS("01_datos/hashes_fecha.rds")%>%
  complete(fecha, hashtags)%>%
  mutate(n = ifelse(is.na(n), 0, n))%>%
  group_by(hashtags)%>%
  mutate(
    n_acumulada = cumsum(n)
  )%>%
  filter(n_acumulada>0)%>%
  mutate(fecha = as.POSIXct(fecha, 
                            format = "%Y-%m-%d %T"),
         dia = as.numeric(fecha-min(fecha)),
         max = max(n_acumulada))%>%
  ungroup()%>%
  mutate(rank = as.numeric(as.factor(rank(-max))),
         principales = ifelse(rank <= 10, as.character(hashtags), "Otros"))%>%
  filter(rank<=30)%>%
  group_by(hashtags)%>%
  mutate(ultimodia = ifelse(dia == max(dia), T, F)) %>%
  ungroup()

ur <- ggplot(hashes, 
             aes(x = fecha,
                 group = hashtags,
                 color = reorder(principales, rank)), 
             size = 1) +
  geom_point(aes(y = ifelse(ultimodia, n_acumulada, NA)))+
  geom_line(aes(x = fecha,
                group = hashtags,
                color = reorder(principales, rank),
                y = ifelse(principales == "Otros",n_acumulada, NA)),
            lineend = "round",
            size = 1)+
  geom_line(aes(x = fecha,
                group = hashtags,
                color = reorder(principales, rank),
                y = ifelse(principales != "Otros",n_acumulada, NA)),
            lineend = "round",
            size = 1)+
  geom_text_repel(aes(y = n_acumulada,
                      label = ifelse(ultimodia & principales != "Otros",
                                     as.character(paste0("bold(",
                                                         "\"",
                                                         hashtags, 
                                                         "\"",
                                                         ")~ (",
                                                         "\"\n",
                                                         scales::comma(n_acumulada),
                                                         "\"",
                                                         ")")), 
                                     NA)),
                  hjust = "left",
                  color = "black",
                  segment.color = "grey70",
                  box.padding = 0.01,
                  direction = "y",
                  force = 2,
                  parse = T,
                  nudge_x = -0.2,
                  nudge_y = 3,
                  size = 5)+
  scale_x_datetime(date_breaks = "1 day", date_labels =  "%b/%d %H:00")+
  scale_colour_manual(values = c(brewer.pal(10, "Spectral"),
                                 "grey80"))+
  labs(title = str_wrap("Total de hashtags acumulados más importantes en debate sobre COVID-19",70),
       subtitle = paste0("Hashtags sumados cada minuto; los 10 más importantes del ",
                         fectoprint, " de 2020"),
       caption = "Elaboración con datos de Twitter",
       colour = "Hashtags",
       x = "Tiempo",
       y = element_blank()) +
  theme_minimal() +
  theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(size = 12),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(angle = 90),
        legend.position="none")

ggsave("03_graficas/linea_tiempo_tweets_hashtags.png", plot = ur,
       width = 15, height = 10, dpi = 85)

# Linea de tiempo 

tweets_sum <-readRDS("01_datos/tweets_fecha.rds")

minday <- format(min(tweets_sum$Fecha), "%d")
minmonth <- format(min(tweets_sum$Fecha), "%m")
maxday <- format(max(tweets_sum$Fecha), "%d")
maxmonth <- format(max(tweets_sum$Fecha), "%m")
months <- list(
  "01"="enero",
  "02"="febrero",
  "03"="marzo",
  "04"="abril",
  "05"="mayo",
  "06"="junio",
  "07"="julio",
  "08"="agosto",
  "09"="septiembre",
  "10"="octubre",
  "11"="noviembre",
  "12"="diciembre"
)
if(maxmonth == minmonth){
  fectoprint <- paste(minday, "al", maxday,"de", months[minmonth])
} else {
  fectoprint <-   paste(minday,"de",months[minmonth] ,"al", maxday,"de", months[maxmonth])
}

titulo <- "Frecuencia de tweets que mencionan al #COVID19mx"
subtitulo <- paste0("Tweets agrupados por hora del ",fectoprint , " de 2020")

ur <- ggplot(data = tweets_sum, 
             aes(size = `Número de Tweets`,
                 y = `Número de Tweets`,
                 color = `Número de Tweets`,
                 x = Fecha)) +
  geom_smooth(method = "loess",
              show.legend = F,
              colour="black") +
  geom_point() +
  scale_color_continuous(NULL, NULL, NULL)+
  scale_size(NULL, NULL, NULL)+
  scale_x_datetime(date_breaks = "1 day", date_labels =  "%b/%d %H:00")+
  theme_ipsum(grid="Y") +
  labs(title=str_wrap(titulo, width = 80),
       subtitle = str_wrap(subtitulo, width = 80),
       y="Número de tweets",
       x="",
       caption=caption1)+
  theme(plot.title = element_text(size = 35),
        plot.subtitle = element_text(size = 25),
        plot.caption = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(angle = 90))

ggsave("03_graficas/linea_tiempo_tweets.png", plot = ur, 
       width = 15, height = 10, dpi = 100)


# Sentimiento
affin <- readRDS("01_datos/diccionario_pequeño.rds")

general_afinn <- readRDS( "01_datos/palabras_fecha.rds")%>%
  inner_join(afinn,., by = "Palabra") %>%
  group_by(month = month(fecha), day = day(fecha), hour = hour(fecha), id) %>% 
  summarise(puntuacion = sum(Puntuacion)) %>%
  group_by(month, day, hour) %>% 
  summarise(puntuacion = mean(puntuacion),
            n = n()) %>%
  mutate(puntuacion = ifelse(is.na(puntuacion), 0, puntuacion),
         colour = ifelse(puntuacion>0,"positivo",
                         ifelse(puntuacion<0,"negativo","neutral")),
         alpha = abs(puntuacion))%>%
  ungroup()%>%
  mutate(Fecha = as.POSIXct(paste0(month, " " ,day, " ", hour), format = "%m %d %H"),
         `Número de tweets` = n)

titulo <- "Análisis de sentimiento en tweets de COVID19"
subtitulo1 <- "Cada círculo representa un día; el tamaño del círculo indica la cantidad de tweets encontrados por día. Una puntuación mayor a cero representa un sentimiento promedio positivo; una menor, un negativo."

ur <- ggplot(general_afinn ,
             aes(x = Fecha,
                 y = puntuacion,
                 col = colour,
                 size=`Número de tweets`)) +
  geom_smooth(method="loess", show.legend = F, colour="black") +
  geom_point()+ 
  scale_x_datetime(date_breaks = "1 day", date_labels =  "%b/%d %H:00")+
  scale_color_manual(values = c("#FC4E07", "grey",  "#00AFBB")) +
  scale_size_continuous("Sentimiento",
                        guide = guide_legend(override.aes = list(colour = "#E7B800"))) +
  guides(color = F) +
  xlab("") + ylab("Puntuación")  +
  labs(title = str_wrap(titulo, width = 90),
       subtitle= str_wrap(subtitulo1, width = 120),
       caption=caption1,
       x="Tiempo",
       y="Sentimiento") +
  theme_ipsum() +
  theme(plot.title = element_text(size = 30),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(size = 12),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(angle = 90))
ggsave(filename = "03_graficas/linea_tiempo_sentimiento_tweets.png",
       plot = ur ,
       width = 15, height = 10, dpi = 100)
beepr::beep()