
#' Primero cargo las librerias que voy a necesitar a lo largo de esta práctica.

library(readr)
library(janitor)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)


#' #### **APARTADO 1** 


marketingdata <- read_csv("mktmix.csv")

#' Cambiaremos los nombres de las columnas con la funcion clean_names

marketingdata <- clean_names(marketingdata)


#' #### **APARTADO 2** 

dim(marketingdata)
 
#' El data frame marketingdata esta formado por 104 filas y 9 columnas.

class(marketingdata$base_price)
class(marketingdata$discount)

#' Ambas columnas son numericas, es decir, numeros decimales y contienen la 
#' informacion referente a los descuentos y precios base.

#' 

#' #### **APARTADO 3**

marketingdata <- marketingdata %>%
  mutate(NewspaperInserts = if_else(is.na(marketingdata$newspaper_inserts),0,1))

#' 


#' #### **APARTADO 4** 

marketing_web <- marketingdata %>%
  filter(!is.na(website_campaign))%>%
  distinct(website_campaign)

#' En esta columna encontramos 3 valores distintos o unicos sin contar los NA,
#' siendo Facebook, Twitter y Website_Campaign2.

#' El resultado del código anterior lo convierto a vector para poder acceder
#' a esos elementos posteriormente con la funcion mutate y crear las columnas. 

marketing_web <- marketing_web[[1]]

marketingdata <- marketingdata %>% 
  mutate(website_campaign = ifelse(is.na(website_campaign), 0, website_campaign))

#' Se crean las columnas Facebook, Twitter y Website_Campaign2, donde tomará el valor 1 cuando tenga valor esa 
#' categoría y 0 en caso contrario. 

marketingdata <- marketingdata %>%
  mutate("Facebook" = if_else(website_campaign == marketing_web[1], 1,0))
marketingdata <- marketingdata %>%
  mutate("Twitter" = if_else(website_campaign == marketing_web[2], 1,0))
marketingdata <- marketingdata %>%
  mutate("Website_Campaign2" = if_else(website_campaign == marketing_web[3], 1,0))


#' #### **APARTADO 5**

sum(marketingdata$Facebook)
sum(marketingdata$Twitter)

#' Tanto en Facebook como en Twitter han habido 4 semanas de campaña.

#' 


#' #### **APARTADO 6**

sum(marketingdata$tv < 50)

#' Las semanas en las que se ha realizado una inversion menor a 50 grp han sido
#' de 3.

#' 


#' #### **APARTADO 7**

marketingdata %>% 
  group_by(is.na(radio)) %>% 
  summarise(mean(tv))

#' Cuando hubo inversion en radio, la media de la inversion en TV fue de 160 
#' mientras que cuando no hubo inversion en radio, la media de inversion 
#' en TV fue de 140. 

#' 


#' #### **APARTADO 8**

ejex <- c(1:nrow(marketingdata))

ggplot(marketingdata, aes(x = ejex, y = new_vol_sales)) +
  geom_line(col = "#00868B")

#' 

       
#' #### **APARTADO 9**

#' Histograma:

ggplot(marketingdata, aes(x=new_vol_sales))+
  geom_histogram(bins = 30, fill = "#A52A2A")


#' Boxplot:

ggplot(marketingdata, aes(y=new_vol_sales))+
  geom_boxplot()

#' Observando los gráficos la mediana es de 20000 y la media sin realizar ningun
#' calculo estimaria que tendria un valor cercano a la mediana, puesto que ambas
#' medidas son de tendencia central y ademas en el grafico se aprecia que la 
#' mediana esta en el centro de la caja por lo que la distribucion es simetrica.

#' 


#' #### **APARTADO 10**

df_media <- tibble(
  radio = marketingdata$radio,
  tv = marketingdata$tv, 
  stout = marketingdata$stout)


df_media <- df_media %>%
  pivot_longer(everything())

ejex <- c(1:nrow(df_media))

ggplot(df_media, aes(x = ejex, y = value)) +
  geom_line()+
  facet_wrap(name~., scales = "free_y", ncol=1)

#' Los gráficos muestran que donde menos se invierte es en stout. En radio 
#' hay periodos donde la inversion es nula, aunque es donde mayor inversion
#' hay entre las tres. 

#' 



#' #### **APARTADO 11**

ggplot(marketingdata, aes(x = in_store, y = new_vol_sales)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm")
 

#' Observando el gráfico podría concluir que existe una correlación positiva y 
#' lineal aunque no es perfecta. A medida que las cifras de stock aumentan, las 
#' ventas tambien lo hacen.

#'


#' #### **APARTADO 12**

#' Cada punto coloreado en función de newspaper_inserts.

ggplot(marketingdata, aes(x = in_store, y = new_vol_sales, colour = as.factor(newspaper_inserts))) +
  geom_point()

#' Cada punto coloreado en función de TV.

ggplot(marketingdata, aes(x = in_store, y = new_vol_sales, colour = tv)) +
  geom_point()

#' 


#' #### **APARTADO 13**

marketingdata%>%
group_by("discount" = if_else(marketingdata$discount > 0, TRUE, FALSE)) %>%
  summarise(media_baseprice = mean(base_price))%>%
  ggplot()+
  geom_col(aes(x = discount, y = media_baseprice), fill = "#2E8B57")


#' 
  

#' #### **APARTADO 14**

regresion <- function(un_vector){
  variables <- marketingdata %>%
    select(un_vector, new_vol_sales)
  my_model <- lm(new_vol_sales ~ ., data = variables)
  calculo <- summary(my_model)$adj.r.squared
  return(calculo)
}

llamadafuncion <- c("tv", "radio")
regresion(llamadafuncion)

#' 

#' #### **APARTADO 15**

#' Creo una lista con los vectores que el apartado me facilita.

lista <- list(c("base_price", "radio", "tv", "stout"),
              c("base_price", "in_store", "discount", "radio", "tv", "stout"),
              c("in_store", "discount"))

#' Utilizo la lista anterior y la funcion del ejercicio 14 para calcular el R
#' cuadrado mediante la funcion sapply.

sapply(lista, regresion)

#' El mejor modelo de acuerdo con el R cuadrado ajustado obtenido es el segundo
#' puesto que tiene el que presenta un R cuadrado mas proximo a 1.
