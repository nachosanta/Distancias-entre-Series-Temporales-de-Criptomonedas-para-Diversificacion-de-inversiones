# TP EAIII
# Distancias entre series temporales para cotización de cryptos

# librerias
library(binancer) # para bajada de datos
library(ggplot2) # graficos
library(scales)
library(plotly) # graficos interactivos
library(data.table)
library(TSdist) # calculo de distancias entre series temporales
library(TSclust)
library(zoo)
library(dplyr)
library(magrittr)
library(ggpubr)
library(hrbrthemes)
library(ggrepel)
library(stringr)
library(DT) # para armar tablas donde se puede ordenar por valor y filtrar

# cryptos a analizar
opciones_cryptos <- 
  c('BTC', 'YFI', #comparación con BTC
    'BNB', # para pagos y compra de bienes y servicios (llevar el nombre de Binance le puede dar mayor credibiliadad y marketing)
    'ETH', 'ADA', #contratos inteligentes
    'DOT', #moneda digital que conecta la tecnología de blockchain de muchas criptomonedas diferentes. Se dice que podria destronar a ETH
    'AVAX', #alta velocidad, bajo costo y ecológico. Otro rival de ETH
    'NEO', #'GASUSDT',(no funciona el de GAS) #contratos inteligentes
    #La plataforma de Neo permite a los desarrolladores usar su software para ejecutar contratos inteligentes (conocidos como NeoContracts) y diseñar nuevos programas con lenguages conocidos (dapps) destinados a replicar productos y servicios del mundo real.
    #NEO, para votar cambios de protocolo, y GAS, para pagar el cómputo en la red
    'IOTA', #diseñada para IoT. NO utiliza mineros para sus transacciones
    #'USDCUSDT', 'BUSDUSDT', #ambas stablecoins atadas al USD
    'XRP', #ofrece poder pagar en distintas monedas del mundo real. Util para transacciones transfronterizas y utiliza un mecanismo sin confianza para facilitar los pagos
    'SOL', #promociona su velocidad para completar transacciones y la solidez general de su plataforma de "escala web"
    'DOGE' #surgio como un chiste. NO tiene limite de emision, como si tiene BTC por ejemplo. Util para pagos y envios de dinero
  )

opciones_referencias <-
  c(
    'USDT','BUSD' #stablecoins como referencia para analizar las cotizaciones de las otras cryptos
  )

intervalo <- '1d' #intervalo de tiempo para toma de datos # cada 1 día
limite <- 183 #alcance temporal del analisis # durante 6 meses

klines <- rbindlist(lapply(
  paste0(opciones_cryptos, opciones_referencias[1]),
  binance_klines,
  interval = intervalo,
  limit = limite)) 

#graficos de velas japonesas
ggplot(klines, aes(open_time)) +
  geom_linerange(aes(ymin = open, ymax = close, color = close < open), size = 2) +
  geom_errorbar(aes(ymin = low, ymax = high), size = 0.25) +
  theme_bw() + theme('legend.position' = 'none') + xlab('') +
  ggtitle(paste('Last Updated:', Sys.time())) +
  scale_color_manual(values = c('#1a9850', '#d73027')) +
  facet_wrap(~symbol, scales = 'free')#, nrow = 2)


# Calculo de distancias entre las series temporales

# Pre procesamiento de datos
# armo una lista para cada cripto con los datos en sus respectivos df
list_symbols <- lapply(
  paste0(opciones_cryptos, opciones_referencias[1]),
  binance_klines,
  interval = intervalo,
  limit = limite) 

# armo los df para la cotización cierre:
df_close <- data.frame()

for (i in length(list_symbols):1){
  symbol_i <- list_symbols[[i]][[1, "symbol"]]
  
  # cotización cierre
  df_symbol_close_i <- list_symbols[[i]] %>%
    select(close)
  colnames(df_symbol_close_i) <- c(paste(symbol_i, "close", sep = " "))
  df_close <- cbind(df_symbol_close_i, df_close)
  
}
#df_close <- cbind(data.frame("close_time"=list_symbols[[1]][["close_time"]]), df_close)

# convierto los df a objeto zoo (no hace falta al final)
#DF <- data.frame(Date = c("01.01.2002","01.01.2003", "01.01.2004", "01.01.2005", "01.01.2006", "01.01.2007", "01.01.2008"), TotalReturns = c(183, 183, 190, 200, 200, 200, 200))
#read.zoo(DF, format = "%d.%m.%Y")
#zoo_close <- read.zoo(df_close, format = "%Y-%m-%d %H:%M:%s", tz =)

# estandarizo las series temporales (X - media) / desvío
df_close <- scale(df_close)

mat_close <- t(as.matrix(df_close))
colnames(mat_close) <- as.character(list_symbols[[1]][["close_time"]])





# Calculo las distancias entre las series, para cada tipo de distancia a analizar
# Calculo un MDS para cada una de ellas y la precisión del mismo al llevarlo a dos dimensiones

tipos_distancias <- c("euclidean", "dissim", "ccor", "cor", "acf", "dtw")

#Cryptos seleccionadas a identificar, si se desea
crypto_id_1 <- paste0('BTC', opciones_referencias[1], " close")
crypto_id_2 <- paste0('ETH', opciones_referencias[1], " close")

lista_dist_pairwise_df <- list() # inicializo las lista para guardar las tablas con distancias
lista_precision <- list() # inicializo las lista para guardar las precisiones de los MDS
lista_mapa <- list() # inicializo las lista para guardar los gráficos de los MDS (los mapas)
for (i in tipos_distancias){
  dist <- TSDatabaseDistances(mat_close, distance = paste0(i))
  print(i)
  # matriz
  dist_matrix <- as.matrix(dist)
  
  # lista de distancias entre pares
  nombres_pairwise <- t(combn(colnames(dist_matrix), 2))
  dist_pairwise_df <- data.frame(nombres_pairwise,
                                 distancia=dist_matrix[nombres_pairwise])
  colnames(dist_pairwise_df) <- c("X1", "X2", paste0(i))
  
  
  # armo una nueva única columna con las siglas de las cyptos a comparar
  pares_cryptos <- paste0(str_split_fixed(dist_pairwise_df$X1, " ", 2)[,1],
                          "-",
                          str_split_fixed(dist_pairwise_df$X2, " ", 2)[,1])
  
  dist_pairwise_df <- cbind(dist_pairwise_df, pares_cryptos)
  dist_pairwise_df$X1 <- NULL
  dist_pairwise_df$X2 <- NULL
  
  lista_dist_pairwise_df[[paste0(i)]] <- dist_pairwise_df
  
  
  # armo el MDS (Multidimensional Scaling)
  mds_completo <- dist  %>% cmdscale(eig = T) # le pido que traiga los autovalores del mds
  
  # Autovalores
  eig_mds <- mds_completo$eig # (le pido los autovalores al mds para ver el % de info en el mismo)
  
  precision <- round(sum(abs(eig_mds[1:2]))/sum(abs(eig_mds))*100, 2)
  lista_precision[[paste0(i)]] <- precision
  
  # MDS coloreando dos cryptos en especifico
  mds <- mds_completo$points %>% # mediante MDS clasico logra la mejor reduccion a 2 dimensiones (deflault k=2)
    as_tibble() %>% # paso la matriz a un tibble
    mutate(Selección = factor(ifelse(rownames(dist_matrix) %in% c(crypto_id_1, crypto_id_2), "Seleccionada", "Otra")),
           Symbol = rownames(dist_matrix), 
           Crypto = substring(rownames(dist_matrix), first=1, last=nchar(rownames(dist_matrix))-nchar('USDT close')))
  colnames(mds) <- c("Coordenada.1", "Coordenada.2", "Selección", "Symbol", "Crypto")
  
  #if(i=='euclidean'){
  #  x <- 400
  #  y <- 400
  #}
  
  #if(i=='dtw'){
  #  x <- 40
  #  y <- 40
  #}
  
  #if(i=='cor'||i=='ccor'||i='acf'){
  #  x <- 5
  #  y <- 5
  #}
  
  # Plot MDS
  mapa <- ggplot(mds, aes(x=Coordenada.1, y=Coordenada.2, 
                          color=ifelse(Selección=="Seleccionada", 'red', 'black'))) + 
    geom_point(size=6) +
    #ggtitle('Mapa de Cryptos') +
    geom_text(
      label=mds$Crypto, 
      nudge_x = 0.5, nudge_y = 0.5, 
      check_overlap = T
    ) +
    ggtitle(paste0('Mapa de Criptomonedas, calculado con la distancia: ', i)) +
    theme('legend.position' = 'none') +
    labs(caption = paste("Precisión del gráfico en 2D: ", precision,"%"))
  
  lista_mapa[[paste0(i)]] <- mapa
  
}
#ggplotly(mapa) #interactivo
#mapa #estatico

lista_dist_pairwise_df # lista con dataframes con pares de cryptos con sus respectivas distancias
lista_mapa # lista con mapas MDS de las crypto para cada distancia
lista_precision # lista con las precisiones de los MDS de las crypto para cada distancia

# armo un único dataframe con los pares de cryptos y sus distancias calculadas
df_pares_dist_completo <- lista_dist_pairwise_df[[1]]
for (j in 1:length(lista_dist_pairwise_df)){
  print(j)
  if (j+1 <= length(lista_dist_pairwise_df)){
    df_pares_dist_completo <- inner_join(df_pares_dist_completo, lista_dist_pairwise_df[[j+1]])
  }
}
df_pares_dist_completo <- relocate(df_pares_dist_completo, pares_cryptos)

df_pares_dist_completo

# armo una data table para visualizar mejor el dataframe de pares
# se pueden ordenar por valor, buscar, filtrar y bajar los datos
datatable(
  df_pares_dist_completo, extensions = 'Buttons', options = list(
    dom = 'Blfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf'),
    lengthMenu = list(c( -1, 10,30, 50), 
                      c( 'All', '10', '30', '50')),
    paging = T)
)

# MDS de cada distancia
# Esto es lo más útil para la busqueda de outliers en las cotizaciones
# y que un inversor pueda observar cómo puede diversificar su cartera

# Euclidea (no tendría sentido calcularla porque es una distancia métrica)
#lista_mapa[["euclidean"]]
#ggplotly(lista_mapa[["euclidean"]]) #mapa dinámico
#paste0('La precisión del MDS es, aproximadamente, del: ', lista_precision[["euclidean"]], '%')

# dissim (integral de la Euclídea) (no tendría sentido calcularla porque es una distancia métrica)
#lista_mapa[["dissim"]]
#ggplotly(lista_mapa[["dissim"]]) #mapa dinámico
#paste0('La precisión del MDS es, aproximadamente, del: ', lista_precision[["dissim"]], '%')

# ccor (correlación cruzada)
lista_mapa[["ccor"]]
ggplotly(lista_mapa[["ccor"]]) #mapa dinámico
paste0('La precisión del MDS es, aproximadamente, del: ', lista_precision[["ccor"]], '%')

# cor (correlación)
lista_mapa[["cor"]]
ggplotly(lista_mapa[["cor"]]) #mapa dinámico
paste0('La precisión del MDS es, aproximadamente, del: ', lista_precision[["cor"]], '%')

# acf (autocorrelación)
lista_mapa[["acf"]]
ggplotly(lista_mapa[["acf"]]) #mapa dinámico
paste0('La precisión del MDS es, aproximadamente, del: ', lista_precision[["acf"]], '%')

# dtw (dynamic time warpping)
lista_mapa[["dtw"]]
ggplotly(lista_mapa[["dtw"]]) #mapa dinámico
paste0('La precisión del MDS es, aproximadamente, del: ', lista_precision[["dtw"]], '%')





