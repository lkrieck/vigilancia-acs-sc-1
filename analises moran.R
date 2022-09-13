### ANALISES MORAN


## ---- biblioteca_moran --------
library(ggplot2)
library(tidyverse)
library(viridis)
library(epiR)
library(scales)
library(readr)
library(leaflet)
library(geobr)
library(sf)
library(leaflegend)
library(spdep)


## ---- arquivos_mapasp1120_1 --------
###### ARQUIVOS MAPA

#abre lista com todos os dados do pacote - parece que geob br nao esta mais disponivel desde 23/01/2022
#datasets <- list_geobr()

#gerando as bases dos dados dos mapas
geoMunicipiosSC <- read_municipality(code_muni="SC", year=2020) %>% 
  st_transform(4326)

#	removendo ultimo digito do codigo do municipio SC
geoMunicipiosSC$code_muni= 
  substr(geoMunicipiosSC$code_muni,1,nchar(geoMunicipiosSC$code_muni)-1)


## ---- analisesmoran --------

bancop1120 <- read_csv2("banco_limpeza.csv", locale = locale(encoding = "ISO8859-1"))

### Municipios

#NASCIDOS VIVOS POR ANO
nv.ano.muni <- bancop1120 %>%
  group_by(AnoNascimento, CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento, CodigoMunicipioResidencia) %>% 
  count() %>% 
  rename(nasc_vivos = n)


#NV COM ANOMALIAS SELECIONADAS
nv.anom.ano.muni <- bancop1120 %>% 
  filter(str_detect(CID10Anomalia,"Q000|Q001|Q002|Q01|Q05|Q02|Q20|Q21|Q22|Q23|Q24|Q25|Q26|Q27|Q28|Q35|Q36|Q37|Q54|Q56|Q66|Q69|Q71|Q72|Q73|Q743|Q792|Q793|Q90")) %>% 
  group_by(AnoNascimento, CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento, CodigoMunicipioResidencia) %>%
  count()


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.ano.muni <- nv.anom.ano.muni %>% 
  right_join(nv.ano.muni, nv.anom.ano.muni, by = c('AnoNascimento', 'CodigoMunicipioResidencia'))

prev.ano.muni <- prev.ano.muni %>% 
  mutate(n = replace_na(n, 0))

#calculando prevalencia
epiprev.ano.muni <-epi.prev(prev.ano.muni$n, prev.ano.muni$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
epiprev.ano.muni_2 <- epiprev.ano.muni[["ap"]]

#unindo os dados de prevalencia
prev.ano.muni <- prev.ano.muni %>% 
  bind_cols(epiprev.ano.muni_2) %>% 
  mutate(est = round(est, 2))


#MORAN

banco_i_moran <- prev.ano.muni %>% 
  dplyr::select(AnoNascimento, CodigoMunicipioResidencia, est) %>%
  rename (Prevalencia10000 = est) %>% 
  rename (code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))


banco_i_moran <- banco_i_moran %>% 
  ungroup()

banco_i_moran <- banco_i_moran %>% 
  add_row(AnoNascimento = 2011, code_muni = "422000", Prevalencia10000 = 0) %>% 
  add_row(AnoNascimento = 2011, code_muni = "421265", Prevalencia10000 = 0) %>%
  add_row(AnoNascimento = 2012, code_muni = "421265", Prevalencia10000 = 0)



# unindo os dados com a base de mapa
banco_i_moran <- dplyr::left_join(geoMunicipiosSC, banco_i_moran, by = "code_muni")



banco_i_moran_matriz = banco_i_moran %>%
  filter(AnoNascimento == 2020)

w <- poly2nb(banco_i_moran_matriz$geom, row.names = banco_i_moran_matriz$name_muni)
ww <-  nb2listw(w, style = 'B') #faz a matriz de pesos 0 ou 1


banco_moran = banco_i_moran %>% filter(AnoNascimento == 2011)
moran_2011=moran.mc(banco_moran$Prevalencia10000, ww, nsim = 99999)
valores_teste=c(moran_2011$statistic[[1]], moran_2011$p.value, 2011) 

for (i in 2012:2020){
  aux_banco_i_moran = banco_i_moran %>%
    filter(AnoNascimento == i)        
  moran_teste = moran.mc(aux_banco_i_moran$Prevalencia10000, ww, nsim = 9999)
  valores_teste = rbind(valores_teste,
                        c(moran_teste$statistic[[1]], moran_teste$p.value, i))
}

valores_teste_tabelado =  data.frame(estatistica = valores_teste[,1], 
                                     p_valor= valores_teste[,2], ano = valores_teste[,3])

write.table(valores_teste_tabelado, file = "teste_moran_sc.txt", row.names = FALSE, col.names = FALSE)





#LISA LOCAL MORAN



## ---- analiseslisa2011 --------

#LISA LOCAL MORAN

lisa2011 <- banco_i_moran %>%
  filter(AnoNascimento == 2011)


locm <- localmoran(lisa2011$Prevalencia10000, ww)
lisa2011$Sgini <- scale(lisa2011$Prevalencia10000)
lisa2011$lag <- lag.listw(ww, lisa2011$Sgini)
lisa2011$pval <- locm[,5]

lisa2011$quad_sig <-
  ifelse(lisa2011$Sgini >= 0 & lisa2011$lag >= 0 & lisa2011$pval <= 0.05, 1,
         ifelse(lisa2011$Sgini <= 0 & lisa2011$lag <= 0 & lisa2011$pval <= 0.05, 2,
                ifelse(lisa2011$Sgini >= 0 & lisa2011$lag <= 0 & lisa2011$pval <= 0.05, 3,
                       ifelse(lisa2011$Sgini <= 0 & lisa2011$lag >= 0 & lisa2011$pval <= 0.05, 4, 5)
                )
         )
  )

breaks <- seq(1, 5, 1)
labels <- c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(lisa2011$quad_sig, breaks)
colors <- c("red", "blue", "lightpink", "skyblue2", "gray")

lisa2011$quad_sig <- factor(
  lisa2011$quad_sig,
  levels = c(1, 2, 3, 4, 5),
  labels = c(
    "Alto-Alto",
    "Baixo-Baixo",
    "Alto-Baixo",
    "Baixo-Alto",
    "Não Significativo"
  )
)


## ---- mapalisa2011 --------

pal <-
  colorFactor(
    palette = "magma",
    domain = lisa2011$quad_sig
  )

y = lisa2011$quad_sig

label_lisa2011 <- lisa2011 %>%
  mutate (pval= scales::number(pval, accuracy = 0.001, big.mark = ".", decimal.mark = ","))



mapa_lisa2011  <- leaflet(data = lisa2011) 

mapa_lisa2011 %>% 
  addPolygons (weight = 0.5, fillColor = ~pal(lisa2011$quad_sig),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               p-valor: </strong>%s",
                               label_lisa2011$name_muni, label_lisa2011$pval) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = lisa2011$quad_sig, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Significância LISA 2011")




## ---- analiseslisa2012 --------

#LISA LOCAL MORAN

lisa2012 <- banco_i_moran %>%
  filter(AnoNascimento == 2012)


locm <- localmoran(lisa2012$Prevalencia10000, ww)
lisa2012$Sgini <- scale(lisa2012$Prevalencia10000)
lisa2012$lag <- lag.listw(ww, lisa2012$Sgini)
lisa2012$pval <- locm[,5]

lisa2012$quad_sig <-
  ifelse(lisa2012$Sgini >= 0 & lisa2012$lag >= 0 & lisa2012$pval <= 0.05, 1,
         ifelse(lisa2012$Sgini <= 0 & lisa2012$lag <= 0 & lisa2012$pval <= 0.05, 2,
                ifelse(lisa2012$Sgini >= 0 & lisa2012$lag <= 0 & lisa2012$pval <= 0.05, 3,
                       ifelse(lisa2012$Sgini <= 0 & lisa2012$lag >= 0 & lisa2012$pval <= 0.05, 4, 5)
                )
         )
  )

breaks <- seq(1, 5, 1)
labels <- c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(lisa2012$quad_sig, breaks)
colors <- c("red", "blue", "lightpink", "skyblue2", "gray")

lisa2012$quad_sig <- factor(
  lisa2012$quad_sig,
  levels = c(1, 2, 3, 4, 5),
  labels = c(
    "Alto-Alto",
    "Baixo-Baixo",
    "Alto-Baixo",
    "Baixo-Alto",
    "Não Significativo"
  )
)


## ---- mapalisa2012 --------

pal <-
  colorFactor(
    palette = "magma",
    domain = lisa2012$quad_sig
  )

y = lisa2012$quad_sig

label_lisa2012 <- lisa2012 %>%
  mutate (pval= scales::number(pval, accuracy = 0.001, big.mark = ".", decimal.mark = ","))



mapa_lisa2012  <- leaflet(data = lisa2012) 

mapa_lisa2012 %>% 
  addPolygons (weight = 0.5, fillColor = ~pal(lisa2012$quad_sig),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               p-valor: </strong>%s",
                               label_lisa2012$name_muni, label_lisa2012$pval) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = lisa2012$quad_sig, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Significância LISA 2012")


## ---- analiseslisa2013 --------

#LISA LOCAL MORAN

lisa2013 <- banco_i_moran %>%
  filter(AnoNascimento == 2013)


locm <- localmoran(lisa2013$Prevalencia10000, ww)
lisa2013$Sgini <- scale(lisa2013$Prevalencia10000)
lisa2013$lag <- lag.listw(ww, lisa2013$Sgini)
lisa2013$pval <- locm[,5]

lisa2013$quad_sig <-
  ifelse(lisa2013$Sgini >= 0 & lisa2013$lag >= 0 & lisa2013$pval <= 0.05, 1,
         ifelse(lisa2013$Sgini <= 0 & lisa2013$lag <= 0 & lisa2013$pval <= 0.05, 2,
                ifelse(lisa2013$Sgini >= 0 & lisa2013$lag <= 0 & lisa2013$pval <= 0.05, 3,
                       ifelse(lisa2013$Sgini <= 0 & lisa2013$lag >= 0 & lisa2013$pval <= 0.05, 4, 5)
                )
         )
  )

breaks <- seq(1, 5, 1)
labels <- c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(lisa2013$quad_sig, breaks)
colors <- c("red", "blue", "lightpink", "skyblue2", "gray")

lisa2013$quad_sig <- factor(
  lisa2013$quad_sig,
  levels = c(1, 2, 3, 4, 5),
  labels = c(
    "Alto-Alto",
    "Baixo-Baixo",
    "Alto-Baixo",
    "Baixo-Alto",
    "Não Significativo"
  )
)


## ---- mapalisa2013 --------

pal <-
  colorFactor(
    palette = "magma",
    domain = lisa2013$quad_sig
  )

y = lisa2013$quad_sig

label_lisa2013 <- lisa2013 %>%
  mutate (pval= scales::number(pval, accuracy = 0.001, big.mark = ".", decimal.mark = ","))



mapa_lisa2013  <- leaflet(data = lisa2013) 

mapa_lisa2013 %>% 
  addPolygons (weight = 0.5, fillColor = ~pal(lisa2013$quad_sig),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               p-valor: </strong>%s",
                               label_lisa2013$name_muni, label_lisa2013$pval) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = lisa2013$quad_sig, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Significância LISA 2013")



## ---- analiseslisa2014 --------

#LISA LOCAL MORAN

lisa2014 <- banco_i_moran %>%
  filter(AnoNascimento == 2014)


locm <- localmoran(lisa2014$Prevalencia10000, ww)
lisa2014$Sgini <- scale(lisa2014$Prevalencia10000)
lisa2014$lag <- lag.listw(ww, lisa2014$Sgini)
lisa2014$pval <- locm[,5]

lisa2014$quad_sig <-
  ifelse(lisa2014$Sgini >= 0 & lisa2014$lag >= 0 & lisa2014$pval <= 0.05, 1,
         ifelse(lisa2014$Sgini <= 0 & lisa2014$lag <= 0 & lisa2014$pval <= 0.05, 2,
                ifelse(lisa2014$Sgini >= 0 & lisa2014$lag <= 0 & lisa2014$pval <= 0.05, 3,
                       ifelse(lisa2014$Sgini <= 0 & lisa2014$lag >= 0 & lisa2014$pval <= 0.05, 4, 5)
                )
         )
  )

breaks <- seq(1, 5, 1)
labels <- c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(lisa2014$quad_sig, breaks)
colors <- c("red", "blue", "lightpink", "skyblue2", "gray")

lisa2014$quad_sig <- factor(
  lisa2014$quad_sig,
  levels = c(1, 2, 3, 4, 5),
  labels = c(
    "Alto-Alto",
    "Baixo-Baixo",
    "Alto-Baixo",
    "Baixo-Alto",
    "Não Significativo"
  )
)


## ---- mapalisa2014 --------

pal <-
  colorFactor(
    palette = "magma",
    domain = lisa2014$quad_sig
  )

y = lisa2014$quad_sig

label_lisa2014 <- lisa2014 %>%
  mutate (pval= scales::number(pval, accuracy = 0.001, big.mark = ".", decimal.mark = ","))



mapa_lisa2014  <- leaflet(data = lisa2014) 

mapa_lisa2014 %>% 
  addPolygons (weight = 0.5, fillColor = ~pal(lisa2014$quad_sig),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               p-valor: </strong>%s",
                               label_lisa2014$name_muni, label_lisa2014$pval) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = lisa2014$quad_sig, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Significância LISA 2014")


## ---- analiseslisa2015 --------

#LISA LOCAL MORAN

lisa2015 <- banco_i_moran %>%
  filter(AnoNascimento == 2015)


locm <- localmoran(lisa2015$Prevalencia10000, ww)
lisa2015$Sgini <- scale(lisa2015$Prevalencia10000)
lisa2015$lag <- lag.listw(ww, lisa2015$Sgini)
lisa2015$pval <- locm[,5]

lisa2015$quad_sig <-
  ifelse(lisa2015$Sgini >= 0 & lisa2015$lag >= 0 & lisa2015$pval <= 0.05, 1,
         ifelse(lisa2015$Sgini <= 0 & lisa2015$lag <= 0 & lisa2015$pval <= 0.05, 2,
                ifelse(lisa2015$Sgini >= 0 & lisa2015$lag <= 0 & lisa2015$pval <= 0.05, 3,
                       ifelse(lisa2015$Sgini <= 0 & lisa2015$lag >= 0 & lisa2015$pval <= 0.05, 4, 5)
                )
         )
  )

breaks <- seq(1, 5, 1)
labels <- c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(lisa2015$quad_sig, breaks)
colors <- c("red", "blue", "lightpink", "skyblue2", "gray")

lisa2015$quad_sig <- factor(
  lisa2015$quad_sig,
  levels = c(1, 2, 3, 4, 5),
  labels = c(
    "Alto-Alto",
    "Baixo-Baixo",
    "Alto-Baixo",
    "Baixo-Alto",
    "Não Significativo"
  )
)


## ---- mapalisa2015 --------

pal <-
  colorFactor(
    palette = "magma",
    domain = lisa2015$quad_sig
  )

y = lisa2015$quad_sig

label_lisa2015 <- lisa2015 %>%
  mutate (pval= scales::number(pval, accuracy = 0.001, big.mark = ".", decimal.mark = ","))



mapa_lisa2015  <- leaflet(data = lisa2015) 

mapa_lisa2015 %>% 
  addPolygons (weight = 0.5, fillColor = ~pal(lisa2015$quad_sig),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               p-valor: </strong>%s",
                               label_lisa2015$name_muni, label_lisa2015$pval) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = lisa2015$quad_sig, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Significância LISA 2015")



## ---- analiseslisa2016 --------

#LISA LOCAL MORAN

lisa2016 <- banco_i_moran %>%
  filter(AnoNascimento == 2016)


locm <- localmoran(lisa2016$Prevalencia10000, ww)
lisa2016$Sgini <- scale(lisa2016$Prevalencia10000)
lisa2016$lag <- lag.listw(ww, lisa2016$Sgini)
lisa2016$pval <- locm[,5]

lisa2016$quad_sig <-
  ifelse(lisa2016$Sgini >= 0 & lisa2016$lag >= 0 & lisa2016$pval <= 0.05, 1,
         ifelse(lisa2016$Sgini <= 0 & lisa2016$lag <= 0 & lisa2016$pval <= 0.05, 2,
                ifelse(lisa2016$Sgini >= 0 & lisa2016$lag <= 0 & lisa2016$pval <= 0.05, 3,
                       ifelse(lisa2016$Sgini <= 0 & lisa2016$lag >= 0 & lisa2016$pval <= 0.05, 4, 5)
                )
         )
  )

breaks <- seq(1, 5, 1)
labels <- c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(lisa2016$quad_sig, breaks)
colors <- c("red", "blue", "lightpink", "skyblue2", "gray")

lisa2016$quad_sig <- factor(
  lisa2016$quad_sig,
  levels = c(1, 2, 3, 4, 5),
  labels = c(
    "Alto-Alto",
    "Baixo-Baixo",
    "Alto-Baixo",
    "Baixo-Alto",
    "Não Significativo"
  )
)


## ---- mapalisa2016 --------

pal <-
  colorFactor(
    palette = "magma",
    domain = lisa2016$quad_sig
  )

y = lisa2016$quad_sig

label_lisa2016 <- lisa2016 %>%
  mutate (pval= scales::number(pval, accuracy = 0.001, big.mark = ".", decimal.mark = ","))



mapa_lisa2016  <- leaflet(data = lisa2016) 

mapa_lisa2016 %>% 
  addPolygons (weight = 0.5, fillColor = ~pal(lisa2016$quad_sig),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               p-valor: </strong>%s",
                               label_lisa2016$name_muni, label_lisa2016$pval) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = lisa2016$quad_sig, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Significância LISA 2016")


## ---- analiseslisa2017 --------

#LISA LOCAL MORAN

lisa2017 <- banco_i_moran %>%
  filter(AnoNascimento == 2017)


locm <- localmoran(lisa2017$Prevalencia10000, ww)
lisa2017$Sgini <- scale(lisa2017$Prevalencia10000)
lisa2017$lag <- lag.listw(ww, lisa2017$Sgini)
lisa2017$pval <- locm[,5]

lisa2017$quad_sig <-
  ifelse(lisa2017$Sgini >= 0 & lisa2017$lag >= 0 & lisa2017$pval <= 0.05, 1,
         ifelse(lisa2017$Sgini <= 0 & lisa2017$lag <= 0 & lisa2017$pval <= 0.05, 2,
                ifelse(lisa2017$Sgini >= 0 & lisa2017$lag <= 0 & lisa2017$pval <= 0.05, 3,
                       ifelse(lisa2017$Sgini <= 0 & lisa2017$lag >= 0 & lisa2017$pval <= 0.05, 4, 5)
                )
         )
  )

breaks <- seq(1, 5, 1)
labels <- c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(lisa2017$quad_sig, breaks)
colors <- c("red", "blue", "lightpink", "skyblue2", "gray")

lisa2017$quad_sig <- factor(
  lisa2017$quad_sig,
  levels = c(1, 2, 3, 4, 5),
  labels = c(
    "Alto-Alto",
    "Baixo-Baixo",
    "Alto-Baixo",
    "Baixo-Alto",
    "Não Significativo"
  )
)


## ---- mapalisa2017 --------

pal <-
  colorFactor(
    palette = "magma",
    domain = lisa2017$quad_sig
  )

y = lisa2017$quad_sig

label_lisa2017 <- lisa2017 %>%
  mutate (pval= scales::number(pval, accuracy = 0.001, big.mark = ".", decimal.mark = ","))



mapa_lisa2017  <- leaflet(data = lisa2017) 

mapa_lisa2017 %>% 
  addPolygons (weight = 0.5, fillColor = ~pal(lisa2017$quad_sig),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               p-valor: </strong>%s",
                               label_lisa2017$name_muni, label_lisa2017$pval) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = lisa2017$quad_sig, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Significância LISA 2017")


## ---- analiseslisa2018 --------

#LISA LOCAL MORAN

lisa2018 <- banco_i_moran %>%
  filter(AnoNascimento == 2018)


locm <- localmoran(lisa2018$Prevalencia10000, ww)
lisa2018$Sgini <- scale(lisa2018$Prevalencia10000)
lisa2018$lag <- lag.listw(ww, lisa2018$Sgini)
lisa2018$pval <- locm[,5]

lisa2018$quad_sig <-
  ifelse(lisa2018$Sgini >= 0 & lisa2018$lag >= 0 & lisa2018$pval <= 0.05, 1,
         ifelse(lisa2018$Sgini <= 0 & lisa2018$lag <= 0 & lisa2018$pval <= 0.05, 2,
                ifelse(lisa2018$Sgini >= 0 & lisa2018$lag <= 0 & lisa2018$pval <= 0.05, 3,
                       ifelse(lisa2018$Sgini <= 0 & lisa2018$lag >= 0 & lisa2018$pval <= 0.05, 4, 5)
                )
         )
  )

breaks <- seq(1, 5, 1)
labels <- c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(lisa2018$quad_sig, breaks)
colors <- c("red", "blue", "lightpink", "skyblue2", "gray")

lisa2018$quad_sig <- factor(
  lisa2018$quad_sig,
  levels = c(1, 2, 3, 4, 5),
  labels = c(
    "Alto-Alto",
    "Baixo-Baixo",
    "Alto-Baixo",
    "Baixo-Alto",
    "Não Significativo"
  )
)


## ---- mapalisa2018 --------

pal <-
  colorFactor(
    palette = "magma",
    domain = lisa2018$quad_sig
  )

y = lisa2018$quad_sig

label_lisa2018 <- lisa2018 %>%
  mutate (pval= scales::number(pval, accuracy = 0.001, big.mark = ".", decimal.mark = ","))



mapa_lisa2018  <- leaflet(data = lisa2018) 

mapa_lisa2018 %>% 
  addPolygons (weight = 0.5, fillColor = ~pal(lisa2018$quad_sig),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               p-valor: </strong>%s",
                               label_lisa2018$name_muni, label_lisa2018$pval) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = lisa2018$quad_sig, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Significância LISA 2018")



## ---- analiseslisa2019 --------

#LISA LOCAL MORAN

lisa2019 <- banco_i_moran %>%
  filter(AnoNascimento == 2019)


locm <- localmoran(lisa2019$Prevalencia10000, ww)
lisa2019$Sgini <- scale(lisa2019$Prevalencia10000)
lisa2019$lag <- lag.listw(ww, lisa2019$Sgini)
lisa2019$pval <- locm[,5]

lisa2019$quad_sig <-
  ifelse(lisa2019$Sgini >= 0 & lisa2019$lag >= 0 & lisa2019$pval <= 0.05, 1,
         ifelse(lisa2019$Sgini <= 0 & lisa2019$lag <= 0 & lisa2019$pval <= 0.05, 2,
                ifelse(lisa2019$Sgini >= 0 & lisa2019$lag <= 0 & lisa2019$pval <= 0.05, 3,
                       ifelse(lisa2019$Sgini <= 0 & lisa2019$lag >= 0 & lisa2019$pval <= 0.05, 4, 5)
                )
         )
  )

breaks <- seq(1, 5, 1)
labels <- c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(lisa2019$quad_sig, breaks)
colors <- c("red", "blue", "lightpink", "skyblue2", "gray")

lisa2019$quad_sig <- factor(
  lisa2019$quad_sig,
  levels = c(1, 2, 3, 4, 5),
  labels = c(
    "Alto-Alto",
    "Baixo-Baixo",
    "Alto-Baixo",
    "Baixo-Alto",
    "Não Significativo"
  )
)


## ---- mapalisa2019 --------

pal <-
  colorFactor(
    palette = "magma",
    domain = lisa2019$quad_sig
  )

y = lisa2019$quad_sig

label_lisa2019 <- lisa2019 %>%
  mutate (pval= scales::number(pval, accuracy = 0.001, big.mark = ".", decimal.mark = ","))



mapa_lisa2019  <- leaflet(data = lisa2019) 

mapa_lisa2019 %>% 
  addPolygons (weight = 0.5, fillColor = ~pal(lisa2019$quad_sig),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               p-valor: </strong>%s",
                               label_lisa2019$name_muni, label_lisa2019$pval) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = lisa2019$quad_sig, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Significância LISA 2019")





## ---- analiseslisa2020 --------

#LISA LOCAL MORAN

lisa2020 <- banco_i_moran %>%
  filter(AnoNascimento == 2020)


locm <- localmoran(lisa2020$Prevalencia10000, ww)
lisa2020$Sgini <- scale(lisa2020$Prevalencia10000)
lisa2020$lag <- lag.listw(ww, lisa2020$Sgini)
lisa2020$pval <- locm[,5]

lisa2020$quad_sig <-
  ifelse(lisa2020$Sgini >= 0 & lisa2020$lag >= 0 & lisa2020$pval <= 0.05, 1,
         ifelse(lisa2020$Sgini <= 0 & lisa2020$lag <= 0 & lisa2020$pval <= 0.05, 2,
                ifelse(lisa2020$Sgini >= 0 & lisa2020$lag <= 0 & lisa2020$pval <= 0.05, 3,
                       ifelse(lisa2020$Sgini <= 0 & lisa2020$lag >= 0 & lisa2020$pval <= 0.05, 4, 5)
                )
         )
  )

breaks <- seq(1, 5, 1)
labels <- c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(lisa2020$quad_sig, breaks)
colors <- c("red", "blue", "lightpink", "skyblue2", "gray")

lisa2020$quad_sig <- factor(
  lisa2020$quad_sig,
  levels = c(1, 2, 3, 4, 5),
  labels = c(
    "Alto-Alto",
    "Baixo-Baixo",
    "Alto-Baixo",
    "Baixo-Alto",
    "Não Significativo"
  )
)


## ---- mapalisa2020 --------

pal <-
  colorFactor(
    palette = "magma",
    domain = lisa2020$quad_sig
  )

y = lisa2020$quad_sig

label_lisa2020 <- lisa2020 %>%
  mutate (pval= scales::number(pval, accuracy = 0.001, big.mark = ".", decimal.mark = ","))



mapa_lisa2020  <- leaflet(data = lisa2020) 

mapa_lisa2020 %>% 
  addPolygons (weight = 0.5, fillColor = ~pal(lisa2020$quad_sig),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               p-valor: </strong>%s",
                               label_lisa2020$name_muni, label_lisa2020$pval) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = lisa2020$quad_sig, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Significância LISA 2020")






### MORAN E LISA PARA O PERIODO TODO


## ---- analisesmoran1120 --------

### Municipios

#NASCIDOS VIVOS POR ANO
nv.1120.muni <- bancop1120 %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>% 
  count() %>% 
  rename(nasc_vivos = n)


#NV COM ANOMALIAS SELECIONADAS
nv.anom.1120.muni <- bancop1120 %>% 
  filter(str_detect(CID10Anomalia,"Q000|Q001|Q002|Q01|Q05|Q02|Q20|Q21|Q22|Q23|Q24|Q25|Q26|Q27|Q28|Q35|Q36|Q37|Q54|Q56|Q66|Q69|Q71|Q72|Q73|Q743|Q792|Q793|Q90")) %>% 
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>%
  count()


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.1120.muni <- nv.anom.1120.muni %>% 
  right_join(nv.1120.muni, nv.anom.1120.muni, by = c('CodigoMunicipioResidencia'))

prev.1120.muni <- prev.1120.muni %>% 
  mutate(n = replace_na(n, 0))

#calculando prevalencia
epiprev.1120.muni <-epi.prev(prev.1120.muni$n, prev.1120.muni$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
epiprev.1120.muni_2 <- epiprev.1120.muni[["ap"]]

#unindo os dados de prevalencia
prev.1120.muni <- prev.1120.muni %>% 
  bind_cols(epiprev.1120.muni_2) %>% 
  mutate(est = round(est, 2))

prev.1120.muni <- prev.1120.muni %>% 
  filter(CodigoMunicipioResidencia != 420000)


#MORAN

banco_i_moran1120 <- prev.1120.muni %>% 
  dplyr::select(CodigoMunicipioResidencia, est) %>%
  rename (Prevalencia10000 = est) %>% 
  rename (code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))



# unindo os dados com a base de mapa
banco_i_moran1120 <- dplyr::left_join(geoMunicipiosSC, banco_i_moran1120, by = "code_muni")



banco_i_moran1120_matriz = banco_i_moran1120 %>% 
  filter(abbrev_state == "SC")

w <- poly2nb(banco_i_moran1120_matriz$geom, row.names = banco_i_moran1120_matriz$name_muni)
ww <-  nb2listw(w, style = 'B') #faz a matriz de pesos 0 ou 1


banco_moranp1120 = banco_i_moran1120 %>% filter(abbrev_state == "SC")
moran1120=moran.mc(banco_moranp1120$Prevalencia10000, ww, nsim = 99999)

valores_teste_1120 =  data.frame(estatistica = moran1120[["statistic"]], 
                                 p_valor = moran1120[["p.value"]])

write.table(valores_teste_1120, file = "teste_moran1120_sc.txt", row.names = FALSE, col.names = FALSE)



## ---- analiseslisap1120 --------

#LISA LOCAL MORAN

lisap1120 <- banco_i_moran1120

locm <- localmoran(lisap1120$Prevalencia10000, ww)
lisap1120$Sgini <- scale(lisap1120$Prevalencia10000)
lisap1120$lag <- lag.listw(ww, lisap1120$Sgini)
lisap1120$pval <- locm[,5]

lisap1120$quad_sig <-
  ifelse(lisap1120$Sgini >= 0 & lisap1120$lag >= 0 & lisap1120$pval <= 0.05, 1,
         ifelse(lisap1120$Sgini <= 0 & lisap1120$lag <= 0 & lisap1120$pval <= 0.05, 2,
                ifelse(lisap1120$Sgini >= 0 & lisap1120$lag <= 0 & lisap1120$pval <= 0.05, 3,
                       ifelse(lisap1120$Sgini <= 0 & lisap1120$lag >= 0 & lisap1120$pval <= 0.05, 4, 5)
                )
         )
  )

breaks <- seq(1, 5, 1)
labels <- c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(lisap1120$quad_sig, breaks)
colors <- c("red", "blue", "lightpink", "skyblue2", "gray")

lisap1120$quad_sig <- factor(
  lisap1120$quad_sig,
  levels = c(1, 2, 3, 4, 5),
  labels = c(
    "Alto-Alto",
    "Baixo-Baixo",
    "Alto-Baixo",
    "Baixo-Alto",
    "Não Significativo"
  )
)


## ---- mapalisap1120 --------

pal <-
  colorFactor(
    palette = "magma",
    domain = lisap1120$quad_sig
  )

y = lisap1120$quad_sig

label_lisap1120 <- lisap1120 %>%
  mutate (pval= scales::number(pval, accuracy = 0.001, big.mark = ".", decimal.mark = ","))



mapa_lisap1120  <- leaflet(data = lisap1120) 

mapa_lisap1120 %>% 
  addPolygons (weight = 0.5, fillColor = ~pal(lisap1120$quad_sig),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               p-valor: </strong>%s",
                               label_lisap1120$name_muni, label_lisap1120$pval) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = lisap1120$quad_sig, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Significância LISA 2011-2020")








