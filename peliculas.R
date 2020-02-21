library(tidyverse)
library(ggplot2)
library(gganimate)
library(scales)

load("Data/imdb.Rdata")


#Función para devolver la posición de un valor
position <-function(df,a,val){
  df<-subset(df,anio==a)
  print(nrow(df))
  print(anio)
  print(val)
  pos = 0
  for(i in 1:nrow(df)){
    if(df[i,4]==val){
      pos=i
    }
  }
  
  return(pos)
}

data_genero_anio<-imdb%>%group_by(anio,genero)%>%summarise(votos=mean(votos))%>%
  mutate(ordering = min_rank(votos) * 1.0) %>%
  filter(anio > 1920) %>%
  ungroup() 

data_genero_anio <-data_genero_anio[order(data_genero_anio$anio,-data_genero_anio$ordering),]

boxplot(data_genero_anio$ordering)
data_genero_anio$scale<-0
#nrow(data_genero_anio)
for(i in 1:nrow(data_genero_anio)){
  anio<-as.integer(data_genero_anio[i,1])
  orden<-as.integer(data_genero_anio[i,4])
  pos<-position(data_genero_anio,anio,orden)
  data_genero_anio[i,5]<-pos
}

#Selecciono las 6 primeras posiciones por año
data_genero_anio<-data_genero_anio%>%filter(scale <7)



# hacer objeto de grafica
p <- data_genero_anio %>%
  ggplot(aes(ordering, group = genero)) +
  # utilizar geom_tile en lugar de geom_bar
  geom_tile(aes(y = votos/2,
                height = votos,
                width = 0.9, fill=genero), alpha = 0.9) +
  # texto en las barras
  geom_text(aes(y = votos, label = paste(as.character(round(votos)),'')),
            vjust = 0.4, hjust = 0.55, size = 5) +
  geom_text(aes(y = 0, label = genero),
            vjust = 0.4, hjust = 0.0, size = 5) +
  # nombres
  labs(title = paste("Promedio de votos por Año ",
                     '{closest_state}', sep=""),
       subtitle = 'Para los diferentes generos',
       x = '', y = '') +
  # para que las barras sean horizontales
  coord_flip(clip = 'off') + # clip = 'off' para sacar texto de márgenes
  # modificando algunos elementos del tema principal
  theme(plot.title = element_text(size = 22),
        plot.subtitle = element_text(size = 16),
        axis.ticks.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank(),
        legend.position = "none")


# crear animación
anim <- p +
  # animacion con cambios en los estados del año
  transition_states(anio,
                    transition_length = 1,
                    state_length = 0,
                    wrap = FALSE) +
  # permitir que los ejes cambien
  view_follow()

# animando
anim %>% animate(fps = 5,
                 nframes = 200,
                 detail = 10,
                 start_pause = 10,
                 end_pause = 30,
                 rewind = FALSE,
                 width = 600,
                 height = 400)

#Guardo la animación
anim_save('peliculas_1.gif')
