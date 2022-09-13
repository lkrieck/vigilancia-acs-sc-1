### ANALISES SCAN

## ---- biblioteca_serie --------
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
library(rflexscan)



## ---- arquivos_mapasp1120 --------
###### ARQUIVOS MAPA

#abre lista com todos os dados do pacote - parece que geob br nao esta mais disponivel desde 23/01/2022
#datasets <- list_geobr()

#gerando as bases dos dados dos mapas
geoMunicipiosSC <- read_municipality(code_muni="SC", year=2020) %>% 
  st_transform(4326)

#	removendo ultimo digito do codigo do municipio SC
geoMunicipiosSC$code_muni= 
  substr(geoMunicipiosSC$code_muni,1,nchar(geoMunicipiosSC$code_muni)-1)


## ---- bancoscan --------

bancop1120 <- read_csv2("banco_limpeza.csv", locale = locale(encoding = "ISO8859-1"))


#CRIANDO BANCO SCAN

#NASCIDOS VIVOS POR MUNI
nv.muni <- bancop1120 %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>% 
  count() %>% 
  rename(nasc_vivos = n)


#GRUPOS ANOMALIAS

anom.gruposp1120 <- bancop1120 %>% 
  filter(str_detect(CID10Anomalia,"Q000|Q001|Q002|Q01|Q05|Q02|Q20|Q21|Q22|Q23|Q24|Q25|Q26|Q27|Q28|Q35|Q36|Q37|Q54|Q56|Q66|Q69|Q71|Q72|Q73|Q743|Q792|Q793|Q90")) %>% 
  mutate(GrupoCID10 = case_when(
    str_detect(CID10Anomalia,"Q000|Q001|Q002|Q01|Q05") ~ "Defeitos de tubo neural",
    str_detect(CID10Anomalia,"Q02") ~ "Microcefalia",
    str_detect(CID10Anomalia,"Q20|Q21|Q22|Q23|Q24|Q25|Q26|Q27|Q28") ~ "Cardiopatias congênitas",
    str_detect(CID10Anomalia,"Q35|Q36|Q37") ~ "Fendas orais",
    str_detect(CID10Anomalia,"Q54") ~ "Anomalias de órgãos genitais - Hipospádias",
    str_detect(CID10Anomalia,"Q56") ~ "Anomalias de órgãos genitais - Sexo indefinido",
    str_detect(CID10Anomalia,"Q66|Q69|Q71|Q72|Q73|Q743") ~ "Defeitos de membros",
    str_detect(CID10Anomalia,"Q792|Q793") ~ "Defeitos da parede abdominal",
    str_detect(CID10Anomalia,"Q90") ~ "Síndrome de Down"))


## ---- dadostodasanomalias --------

#NV COM ANOMALIAS SELECIONADAS
nv.anom.muni <- bancop1120 %>% 
  filter(str_detect(CID10Anomalia,"Q000|Q001|Q002|Q01|Q05|Q02|Q20|Q21|Q22|Q23|Q24|Q25|Q26|Q27|Q28|Q35|Q36|Q37|Q54|Q56|Q66|Q69|Q71|Q72|Q73|Q743|Q792|Q793|Q90")) %>% 
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>%
  count()


#unindo os dados de nascidos vivos e anomalias
banco_scan <- nv.anom.muni %>% 
  right_join(nv.muni, nv.anom.muni, by = c('CodigoMunicipioResidencia'))


banco_scan <- banco_scan %>%
  filter(CodigoMunicipioResidencia != 420000) %>% 
  mutate(n = replace_na(n, 0)) %>% 
  rename (code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))


#calculando valor esperado
banco_scan$valor_esperado <- banco_scan$nasc_vivos * sum(banco_scan$n)/sum(banco_scan$nasc_vivos)


# unindo os dados com a base de mapa
banco_scan_shape <- dplyr::left_join(geoMunicipiosSC, banco_scan, by = "code_muni")


banco_scan <- banco_scan_shape %>% 
  dplyr::select(code_muni, name_muni, n, nasc_vivos, valor_esperado)

banco_scan <- banco_scan %>% 
  st_drop_geometry()


#vizinhanca
w <- poly2nb(banco_scan_shape$geom, row.names = banco_scan_shape$name_muni)
ww <-  nb2listw(w, style = 'B') #faz a matriz de pesos 0 ou 1

nb <- ww[["neighbours"]]



##extraindo latitude e longitude
banco_scan$longitude <- as.numeric(gsub(".*?([-]*[0-9]+[.][0-9]+).*", "\\1", banco_scan_shape$geom))
banco_scan$latitude <- as.numeric(gsub(".* ([-]*[0-9]+[.][0-9]+).*", "\\1", banco_scan_shape$geom))


#convertendo em coordenadas x e y
coord <- data.frame(x=banco_scan$longitude, y=banco_scan$latitude)
coordinates(coord) <- c("x", "y")
proj4string(coord) <- CRS("+init=epsg:29101")
coord <- spTransform(coord, CRS("+init=epsg:29101"))



# run FleXScan
fls1 <- rflexscan(x = coord$x, y = coord$y,
                 observed = banco_scan$n,
                 expected = banco_scan$valor_esperado,
                 name = banco_scan$name_muni,
                 clustersize = 10,
                 nb = nb,
                 stattype = "ORIGINAL",
                 scanmethod = "CIRCULAR",
                 rantype = "POISSON",
                 simcount = 9999)


## ---- clustertodas --------

# display clusters

par(bty="n")
choropleth(banco_scan_shape["name_muni"], fls1, col = viridis(5, option = "A"), pval = 0.05, main = NULL, border = "azure4", bty="n")
legend("bottomleft", legend = 1:4, fill = viridis(5, option = "A"), bty = "n", title = "Ranking dos clusters") 

print(fls1$cluster[[1]])

print(fls1$cluster[[2]])

print(fls1$cluster[[3]])

print(fls1$cluster[[4]])

summary(fls1)


## ---- dadosmembros --------

#NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE RESIDENCIA
membros.muni <- anom.gruposp1120 %>% 
  filter(GrupoCID10 == "Defeitos de membros") %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>%
  count() 


#unindo os dados de nascidos vivos e anomalias
scan_membros <- membros.muni %>% 
  right_join(nv.muni, membros.muni, by = c('CodigoMunicipioResidencia'))


scan_membros <- scan_membros %>%
  filter(CodigoMunicipioResidencia != 420000) %>% 
  mutate(n = replace_na(n, 0)) %>% 
  rename (code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))


#calculando valor esperado
scan_membros$valor_esperado <- scan_membros$nasc_vivos * sum(scan_membros$n)/sum(scan_membros$nasc_vivos)


# unindo os dados com a base de mapa
scan_membros_shape <- dplyr::left_join(geoMunicipiosSC, scan_membros, by = "code_muni")


scan_membros <- scan_membros_shape %>% 
  dplyr::select(code_muni, name_muni, n, nasc_vivos, valor_esperado)

scan_membros <- scan_membros %>% 
  st_drop_geometry()

##row.names(scan_membros) <- scan_membros$name_muni



#vizinhanca
w <- poly2nb(scan_membros_shape$geom, row.names = scan_membros_shape$name_muni)
ww <-  nb2listw(w, style = 'B') #faz a matriz de pesos 0 ou 1

nb <- ww[["neighbours"]]



##extraindo latitude e longitude
scan_membros$longitude <- as.numeric(gsub(".*?([-]*[0-9]+[.][0-9]+).*", "\\1", scan_membros_shape$geom))
scan_membros$latitude <- as.numeric(gsub(".* ([-]*[0-9]+[.][0-9]+).*", "\\1", scan_membros_shape$geom))


#convertendo em coordenadas x e y
coord <- data.frame(x=scan_membros$longitude, y=scan_membros$latitude)
coordinates(coord) <- c("x", "y")
proj4string(coord) <- CRS("+init=epsg:29101")
coord <- spTransform(coord, CRS("+init=epsg:29101"))



# run FleXScan
fls2 <- rflexscan(x = coord$x, y = coord$y,
                 observed = scan_membros$n,
                 expected = scan_membros$valor_esperado,
                 name = scan_membros$name_muni,
                 clustersize = 10,
                 nb = nb,
                 stattype = "ORIGINAL",
                 scanmethod = "CIRCULAR",
                 rantype = "POISSON",
                 simcount = 9999)



## ---- clustermembros --------

# display clusters
par(bty="n")
choropleth(scan_membros_shape["name_muni"], fls2, col = viridis(5, option = "A"), pval = 0.05, main = NULL, border = "azure4", bty="n")
legend("bottomleft", legend = 1:2, fill = viridis(5, option = "A"), bty = "n", title = "Ranking dos clusters")


print(fls2$cluster[[1]])

print(fls2$cluster[[2]])

summary(fls2)



## ---- dadosmicrocefalia --------

#NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE RESIDENCIA
microcefalia.muni <- anom.gruposp1120 %>% 
  filter(GrupoCID10 == "Microcefalia") %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>%
  count() 


#unindo os dados de nascidos vivos e anomalias
scan_microcefalia <- microcefalia.muni %>% 
  right_join(nv.muni, microcefalia.muni, by = c('CodigoMunicipioResidencia'))


scan_microcefalia <- scan_microcefalia %>%
  filter(CodigoMunicipioResidencia != 420000) %>% 
  mutate(n = replace_na(n, 0)) %>% 
  rename (code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))


#calculando valor esperado
scan_microcefalia$valor_esperado <- scan_microcefalia$nasc_vivos * sum(scan_microcefalia$n)/sum(scan_microcefalia$nasc_vivos)


# unindo os dados com a base de mapa
scan_microcefalia_shape <- dplyr::left_join(geoMunicipiosSC, scan_microcefalia, by = "code_muni")


scan_microcefalia <- scan_microcefalia_shape %>% 
  dplyr::select(code_muni, name_muni, n, nasc_vivos, valor_esperado)

scan_microcefalia <- scan_microcefalia %>% 
  st_drop_geometry()

##row.names(scan_microcefalia) <- scan_microcefalia$name_muni



#vizinhanca
w <- poly2nb(scan_microcefalia_shape$geom, row.names = scan_microcefalia_shape$name_muni)
ww <-  nb2listw(w, style = 'B') #faz a matriz de pesos 0 ou 1

nb <- ww[["neighbours"]]



##extraindo latitude e longitude
scan_microcefalia$longitude <- as.numeric(gsub(".*?([-]*[0-9]+[.][0-9]+).*", "\\1", scan_microcefalia_shape$geom))
scan_microcefalia$latitude <- as.numeric(gsub(".* ([-]*[0-9]+[.][0-9]+).*", "\\1", scan_microcefalia_shape$geom))


#convertendo em coordenadas x e y
coord <- data.frame(x=scan_microcefalia$longitude, y=scan_microcefalia$latitude)
coordinates(coord) <- c("x", "y")
proj4string(coord) <- CRS("+init=epsg:29101")
coord <- spTransform(coord, CRS("+init=epsg:29101"))



# run FleXScan
fls3 <- rflexscan(x = coord$x, y = coord$y,
                 observed = scan_microcefalia$n,
                 expected = scan_microcefalia$valor_esperado,
                 name = scan_microcefalia$name_muni,
                 clustersize = 10,
                 nb = nb,
                 stattype = "ORIGINAL",
                 scanmethod = "CIRCULAR",
                 rantype = "POISSON",
                 simcount = 9999)



## ---- clustermicrocefalia --------

# display clusters
par(bty="n")
choropleth(scan_microcefalia_shape["name_muni"], fls3, col = viridis(5, option = "A"), rank = 1, main = NULL, border = "azure4", bty="n")
legend("bottomleft", legend = 1, fill = viridis(5, option = "A"), bty="n", title = "Ranking dos clusters")


print(fls3$cluster[[1]])

summary(fls3)



## ---- dadosdown --------

#NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE RESIDENCIA
down.muni <- anom.gruposp1120 %>% 
  filter(GrupoCID10 == "Síndrome de Down") %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>%
  count() 



#unindo os dados de nascidos vivos e anomalias
scan_down <- down.muni %>% 
  right_join(nv.muni, down.muni, by = c('CodigoMunicipioResidencia'))


scan_down <- scan_down %>%
  filter(CodigoMunicipioResidencia != 420000) %>% 
  mutate(n = replace_na(n, 0)) %>% 
  rename (code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))


#calculando valor esperado
scan_down$valor_esperado <- scan_down$nasc_vivos * sum(scan_down$n)/sum(scan_down$nasc_vivos)


# unindo os dados com a base de mapa
scan_down_shape <- dplyr::left_join(geoMunicipiosSC, scan_down, by = "code_muni")


scan_down <- scan_down_shape %>% 
  dplyr::select(code_muni, name_muni, n, nasc_vivos, valor_esperado)

scan_down <- scan_down %>% 
  st_drop_geometry()

##row.names(scan_down) <- scan_down$name_muni



#vizinhanca
w <- poly2nb(scan_down_shape$geom, row.names = scan_down_shape$name_muni)
ww <-  nb2listw(w, style = 'B') #faz a matriz de pesos 0 ou 1

nb <- ww[["neighbours"]]



##extraindo latitude e longitude
scan_down$longitude <- as.numeric(gsub(".*?([-]*[0-9]+[.][0-9]+).*", "\\1", scan_down_shape$geom))
scan_down$latitude <- as.numeric(gsub(".* ([-]*[0-9]+[.][0-9]+).*", "\\1", scan_down_shape$geom))


#convertendo em coordenadas x e y
coord <- data.frame(x=scan_down$longitude, y=scan_down$latitude)
coordinates(coord) <- c("x", "y")
proj4string(coord) <- CRS("+init=epsg:29101")
coord <- spTransform(coord, CRS("+init=epsg:29101"))



# run FleXScan
fls4 <- rflexscan(x = coord$x, y = coord$y,
                 observed = scan_down$n,
                 expected = scan_down$valor_esperado,
                 name = scan_down$name_muni,
                 clustersize = 10,
                 nb = nb,
                 stattype = "ORIGINAL",
                 scanmethod = "CIRCULAR",
                 rantype = "POISSON",
                 simcount = 9999)



## ---- clusterdown --------

# display clusters
par(bty="n")
choropleth(scan_down_shape["name_muni"], fls4, col = viridis(5, option = "A"), pval = 0.05, main = NULL, border = "azure4", bty="n")
legend("bottomleft", legend = 1, fill = viridis(5, option = "A"), bty="n", title = "Ranking dos clusters")

print(fls4$cluster[[1]])

summary(fls4)




## ---- dadostuboneural --------

#NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE RESIDENCIA
tubo.muni <- anom.gruposp1120 %>% 
  filter(GrupoCID10 == "Defeitos de tubo neural") %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>%
  count() 

#unindo os dados de nascidos vivos e anomalias
scan_tubo <- tubo.muni %>% 
  right_join(nv.muni, tubo.muni, by = c('CodigoMunicipioResidencia'))


scan_tubo <- scan_tubo %>%
  filter(CodigoMunicipioResidencia != 420000) %>% 
  mutate(n = replace_na(n, 0)) %>% 
  rename (code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))


#calculando valor esperado
scan_tubo$valor_esperado <- scan_tubo$nasc_vivos * sum(scan_tubo$n)/sum(scan_tubo$nasc_vivos)


# unindo os dados com a base de mapa
scan_tubo_shape <- dplyr::left_join(geoMunicipiosSC, scan_tubo, by = "code_muni")


scan_tubo <- scan_tubo_shape %>% 
  dplyr::select(code_muni, name_muni, n, nasc_vivos, valor_esperado)

scan_tubo <- scan_tubo %>% 
  st_drop_geometry()

##row.names(scan_tubo) <- scan_tubo$name_muni



#vizinhanca
w <- poly2nb(scan_tubo_shape$geom, row.names = scan_tubo_shape$name_muni)
ww <-  nb2listw(w, style = 'B') #faz a matriz de pesos 0 ou 1

nb <- ww[["neighbours"]]



##extraindo latitude e longitude
scan_tubo$longitude <- as.numeric(gsub(".*?([-]*[0-9]+[.][0-9]+).*", "\\1", scan_tubo_shape$geom))
scan_tubo$latitude <- as.numeric(gsub(".* ([-]*[0-9]+[.][0-9]+).*", "\\1", scan_tubo_shape$geom))


#convertendo em coordenadas x e y
coord <- data.frame(x=scan_tubo$longitude, y=scan_tubo$latitude)
coordinates(coord) <- c("x", "y")
proj4string(coord) <- CRS("+init=epsg:29101")
coord <- spTransform(coord, CRS("+init=epsg:29101"))



# run FleXScan
fls5 <- rflexscan(x = coord$x, y = coord$y,
                 observed = scan_tubo$n,
                 expected = scan_tubo$valor_esperado,
                 name = scan_tubo$name_muni,
                 clustersize = 10,
                 nb = nb,
                 stattype = "ORIGINAL",
                 scanmethod = "CIRCULAR",
                 rantype = "POISSON",
                 simcount = 9999)




## ---- clustertubo --------
# display clusters
par(bty="n")
choropleth(scan_tubo_shape["name_muni"], fls5, col = viridis(5, option = "A"), rank = 1, main = NULL, border = "azure4", bty="n")
legend("bottomleft", legend = 1, fill = viridis(5, option = "A"), bty="n", title = "Ranking dos clusters")

print(fls5$cluster[[1]])

summary(fls5)



## ---- dadosgenital --------

#NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE RESIDENCIA
genital.muni <- anom.gruposp1120 %>% 
  filter(GrupoCID10 == "Anomalias de órgãos genitais - Hipospádias" | GrupoCID10 == "Anomalias de órgãos genitais - Sexo indefinido") %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>%
  count() 

#unindo os dados de nascidos vivos e anomalias
scan_genital <- genital.muni %>% 
  right_join(nv.muni, genital.muni, by = c('CodigoMunicipioResidencia'))


scan_genital <- scan_genital %>%
  filter(CodigoMunicipioResidencia != 420000) %>% 
  mutate(n = replace_na(n, 0)) %>% 
  rename (code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))


#calculando valor esperado
scan_genital$valor_esperado <- scan_genital$nasc_vivos * sum(scan_genital$n)/sum(scan_genital$nasc_vivos)


# unindo os dados com a base de mapa
scan_genital_shape <- dplyr::left_join(geoMunicipiosSC, scan_genital, by = "code_muni")


scan_genital <- scan_genital_shape %>% 
  dplyr::select(code_muni, name_muni, n, nasc_vivos, valor_esperado)

scan_genital <- scan_genital %>% 
  st_drop_geometry()

##row.names(scan_genital) <- scan_genital$name_muni



#vizinhanca
w <- poly2nb(scan_genital_shape$geom, row.names = scan_genital_shape$name_muni)
ww <-  nb2listw(w, style = 'B') #faz a matriz de pesos 0 ou 1

nb <- ww[["neighbours"]]



##extraindo latitude e longitude
scan_genital$longitude <- as.numeric(gsub(".*?([-]*[0-9]+[.][0-9]+).*", "\\1", scan_genital_shape$geom))
scan_genital$latitude <- as.numeric(gsub(".* ([-]*[0-9]+[.][0-9]+).*", "\\1", scan_genital_shape$geom))


#convertendo em coordenadas x e y
coord <- data.frame(x=scan_genital$longitude, y=scan_genital$latitude)
coordinates(coord) <- c("x", "y")
proj4string(coord) <- CRS("+init=epsg:29101")
coord <- spTransform(coord, CRS("+init=epsg:29101"))



# run FleXScan
fls6 <- rflexscan(x = coord$x, y = coord$y,
                 observed = scan_genital$n,
                 expected = scan_genital$valor_esperado,
                 name = scan_genital$name_muni,
                 clustersize = 10,
                 nb = nb,
                 stattype = "ORIGINAL",
                 scanmethod = "CIRCULAR",
                 rantype = "POISSON",
                 simcount = 9999)




## ---- clustergenital --------

# display clusters
par(bty="n")
choropleth(scan_genital_shape["name_muni"], fls6, col = viridis(5, option = "A"), pval = 0.05, main = NULL, border = "azure4", bty="n")
legend("bottomleft", legend = 1:3, fill = viridis(5, option = "A"), bty="n", title = "Ranking dos clusters")


print(fls6$cluster[[1]])

print(fls6$cluster[[2]])

print(fls6$cluster[[3]])

summary(fls6)


## ---- dadoshipospadias --------

#NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE RESIDENCIA
hipospadias.muni <- anom.gruposp1120 %>% 
  filter(GrupoCID10 == "Anomalias de órgãos genitais - Hipospádias") %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>%
  count() 

#unindo os dados de nascidos vivos e anomalias
scan_hipospadias <- hipospadias.muni %>% 
  right_join(nv.muni, hipospadias.muni, by = c('CodigoMunicipioResidencia'))


scan_hipospadias <- scan_hipospadias %>%
  filter(CodigoMunicipioResidencia != 420000) %>% 
  mutate(n = replace_na(n, 0)) %>% 
  rename (code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))


#calculando valor esperado
scan_hipospadias$valor_esperado <- scan_hipospadias$nasc_vivos * sum(scan_hipospadias$n)/sum(scan_hipospadias$nasc_vivos)


# unindo os dados com a base de mapa
scan_hipospadias_shape <- dplyr::left_join(geoMunicipiosSC, scan_hipospadias, by = "code_muni")


scan_hipospadias <- scan_hipospadias_shape %>% 
  dplyr::select(code_muni, name_muni, n, nasc_vivos, valor_esperado)

scan_hipospadias <- scan_hipospadias %>% 
  st_drop_geometry()

##row.names(scan_hipospadias) <- scan_hipospadias$name_muni



#vizinhanca
w <- poly2nb(scan_hipospadias_shape$geom, row.names = scan_hipospadias_shape$name_muni)
ww <-  nb2listw(w, style = 'B') #faz a matriz de pesos 0 ou 1

nb <- ww[["neighbours"]]



##extraindo latitude e longitude
scan_hipospadias$longitude <- as.numeric(gsub(".*?([-]*[0-9]+[.][0-9]+).*", "\\1", scan_hipospadias_shape$geom))
scan_hipospadias$latitude <- as.numeric(gsub(".* ([-]*[0-9]+[.][0-9]+).*", "\\1", scan_hipospadias_shape$geom))


#convertendo em coordenadas x e y
coord <- data.frame(x=scan_hipospadias$longitude, y=scan_hipospadias$latitude)
coordinates(coord) <- c("x", "y")
proj4string(coord) <- CRS("+init=epsg:29101")
coord <- spTransform(coord, CRS("+init=epsg:29101"))



# run FleXScan
fls10 <- rflexscan(x = coord$x, y = coord$y,
                  observed = scan_hipospadias$n,
                  expected = scan_hipospadias$valor_esperado,
                  name = scan_hipospadias$name_muni,
                  clustersize = 10,
                  nb = nb,
                  stattype = "ORIGINAL",
                  scanmethod = "CIRCULAR",
                  rantype = "POISSON",
                  simcount = 9999)




## ---- clusterhipospadias --------

# display clusters
par(bty="n")
choropleth(scan_hipospadias_shape["name_muni"], fls10, col = viridis(5, option = "A"), pval = 0.05, main = NULL, border = "azure4", bty="n")
legend("bottomleft", legend = 1:3, fill = viridis(5, option = "A"), bty="n", title = "Ranking dos clusters")


print(fls10$cluster[[1]])

print(fls10$cluster[[2]])

print(fls10$cluster[[3]])

summary(fls10)



## ---- dadosSI --------

#NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE RESIDENCIA
SI.muni <- anom.gruposp1120 %>% 
  filter(GrupoCID10 == "Anomalias de órgãos genitais - Sexo indefinido") %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>%
  count() 

#unindo os dados de nascidos vivos e anomalias
scan_SI <- SI.muni %>% 
  right_join(nv.muni, SI.muni, by = c('CodigoMunicipioResidencia'))


scan_SI <- scan_SI %>%
  filter(CodigoMunicipioResidencia != 420000) %>% 
  mutate(n = replace_na(n, 0)) %>% 
  rename (code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))


#calculando valor esperado
scan_SI$valor_esperado <- scan_SI$nasc_vivos * sum(scan_SI$n)/sum(scan_SI$nasc_vivos)


# unindo os dados com a base de mapa
scan_SI_shape <- dplyr::left_join(geoMunicipiosSC, scan_SI, by = "code_muni")


scan_SI <- scan_SI_shape %>% 
  dplyr::select(code_muni, name_muni, n, nasc_vivos, valor_esperado)

scan_SI <- scan_SI %>% 
  st_drop_geometry()

##row.names(scan_SI) <- scan_SI$name_muni



#vizinhanca
w <- poly2nb(scan_SI_shape$geom, row.names = scan_SI_shape$name_muni)
ww <-  nb2listw(w, style = 'B') #faz a matriz de pesos 0 ou 1

nb <- ww[["neighbours"]]



##extraindo latitude e longitude
scan_SI$longitude <- as.numeric(gsub(".*?([-]*[0-9]+[.][0-9]+).*", "\\1", scan_SI_shape$geom))
scan_SI$latitude <- as.numeric(gsub(".* ([-]*[0-9]+[.][0-9]+).*", "\\1", scan_SI_shape$geom))


#convertendo em coordenadas x e y
coord <- data.frame(x=scan_SI$longitude, y=scan_SI$latitude)
coordinates(coord) <- c("x", "y")
proj4string(coord) <- CRS("+init=epsg:29101")
coord <- spTransform(coord, CRS("+init=epsg:29101"))



# run FleXScan
fls11 <- rflexscan(x = coord$x, y = coord$y,
                   observed = scan_SI$n,
                   expected = scan_SI$valor_esperado,
                   name = scan_SI$name_muni,
                   clustersize = 10,
                   nb = nb,
                   stattype = "ORIGINAL",
                   scanmethod = "CIRCULAR",
                   rantype = "POISSON",
                   simcount = 9999)




## ---- clusterSI --------

# display clusters
par(bty="n")
choropleth(scan_SI_shape["name_muni"], fls11, col = viridis(5, option = "A"), rank = 1, main = NULL, border = "azure4", bty="n")
legend("bottomleft", legend = 1, fill = viridis(5, option = "A"), bty="n", title = "Ranking dos clusters")


print(fls11$cluster[[1]])

summary(fls11)


## ---- dadoscardio--------


#NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE RESIDENCIA
cardio.muni <- anom.gruposp1120 %>% 
  filter(GrupoCID10 == "Cardiopatias congênitas") %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>%
  count() 


#unindo os dados de nascidos vivos e anomalias
scan_cardio <- cardio.muni %>% 
  right_join(nv.muni, cardio.muni, by = c('CodigoMunicipioResidencia'))


scan_cardio <- scan_cardio %>%
  filter(CodigoMunicipioResidencia != 420000) %>% 
  mutate(n = replace_na(n, 0)) %>% 
  rename (code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))


#calculando valor esperado
scan_cardio$valor_esperado <- scan_cardio$nasc_vivos * sum(scan_cardio$n)/sum(scan_cardio$nasc_vivos)


# unindo os dados com a base de mapa
scan_cardio_shape <- dplyr::left_join(geoMunicipiosSC, scan_cardio, by = "code_muni")


scan_cardio <- scan_cardio_shape %>% 
  dplyr::select(code_muni, name_muni, n, nasc_vivos, valor_esperado)

scan_cardio <- scan_cardio %>% 
  st_drop_geometry()

##row.names(scan_cardio) <- scan_cardio$name_muni



#vizinhanca
w <- poly2nb(scan_cardio_shape$geom, row.names = scan_cardio_shape$name_muni)
ww <-  nb2listw(w, style = 'B') #faz a matriz de pesos 0 ou 1

nb <- ww[["neighbours"]]



##extraindo latitude e longitude
scan_cardio$longitude <- as.numeric(gsub(".*?([-]*[0-9]+[.][0-9]+).*", "\\1", scan_cardio_shape$geom))
scan_cardio$latitude <- as.numeric(gsub(".* ([-]*[0-9]+[.][0-9]+).*", "\\1", scan_cardio_shape$geom))


#convertendo em coordenadas x e y
coord <- data.frame(x=scan_cardio$longitude, y=scan_cardio$latitude)
coordinates(coord) <- c("x", "y")
proj4string(coord) <- CRS("+init=epsg:29101")
coord <- spTransform(coord, CRS("+init=epsg:29101"))



# run FleXScan
fls7 <- rflexscan(x = coord$x, y = coord$y,
                 observed = scan_cardio$n,
                 expected = scan_cardio$valor_esperado,
                 name = scan_cardio$name_muni,
                 clustersize = 10,
                 nb = nb,
                 stattype = "ORIGINAL",
                 scanmethod = "CIRCULAR",
                 rantype = "POISSON",
                 simcount = 9999)


## ---- clustercardio --------

# display clusters
par(bty="n")
choropleth(scan_cardio_shape["name_muni"], fls7, col = viridis(5, option = "A"), pval = 0.05, main = NULL, border = "azure4", bty="n")
legend("bottomleft", legend = 1:2, fill = viridis(5, option = "A"), bty="n", title = "Ranking dos clusters")

print(fls7$cluster[[1]])

print(fls7$cluster[[2]])

summary(fls7)



## ---- dadosfendas--------

#NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE RESIDENCIA
fendas.muni <- anom.gruposp1120 %>% 
  filter(GrupoCID10 == "Fendas orais") %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>%
  count() 


#unindo os dados de nascidos vivos e anomalias
scan_fendas <- fendas.muni %>% 
  right_join(nv.muni, fendas.muni, by = c('CodigoMunicipioResidencia'))


scan_fendas <- scan_fendas %>%
  filter(CodigoMunicipioResidencia != 420000) %>% 
  mutate(n = replace_na(n, 0)) %>% 
  rename (code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))


#calculando valor esperado
scan_fendas$valor_esperado <- scan_fendas$nasc_vivos * sum(scan_fendas$n)/sum(scan_fendas$nasc_vivos)


# unindo os dados com a base de mapa
scan_fendas_shape <- dplyr::left_join(geoMunicipiosSC, scan_fendas, by = "code_muni")


scan_fendas <- scan_fendas_shape %>% 
  dplyr::select(code_muni, name_muni, n, nasc_vivos, valor_esperado)

scan_fendas <- scan_fendas %>% 
  st_drop_geometry()

##row.names(scan_fendas) <- scan_fendas$name_muni



#vizinhanca
w <- poly2nb(scan_fendas_shape$geom, row.names = scan_fendas_shape$name_muni)
ww <-  nb2listw(w, style = 'B') #faz a matriz de pesos 0 ou 1

nb <- ww[["neighbours"]]



##extraindo latitude e longitude
scan_fendas$longitude <- as.numeric(gsub(".*?([-]*[0-9]+[.][0-9]+).*", "\\1", scan_fendas_shape$geom))
scan_fendas$latitude <- as.numeric(gsub(".* ([-]*[0-9]+[.][0-9]+).*", "\\1", scan_fendas_shape$geom))


#convertendo em coordenadas x e y
coord <- data.frame(x=scan_fendas$longitude, y=scan_fendas$latitude)
coordinates(coord) <- c("x", "y")
proj4string(coord) <- CRS("+init=epsg:29101")
coord <- spTransform(coord, CRS("+init=epsg:29101"))



# run FleXScan
fls8 <- rflexscan(x = coord$x, y = coord$y,
                 observed = scan_fendas$n,
                 expected = scan_fendas$valor_esperado,
                 name = scan_fendas$name_muni,
                 clustersize = 10,
                 nb = nb,
                 stattype = "ORIGINAL",
                 scanmethod = "CIRCULAR",
                 rantype = "POISSON",
                 simcount = 9999)




## ---- clusterfendas --------

# display clusters
par(bty="n")
choropleth(scan_fendas_shape["name_muni"], fls8, col = viridis(5, option = "A"), rank = 1, main = NULL, border = "azure4", bty="n")
legend("bottomleft", legend = 1, fill = viridis(5, option = "A"), bty="n", title = "Ranking dos clusters")

print(fls8$cluster[[1]])

summary(fls8)


## ---- dadosabdominal--------

#NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE RESIDENCIA
abdominal.muni <- anom.gruposp1120 %>% 
  filter(GrupoCID10 == "Defeitos da parede abdominal") %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>%
  count() 


#unindo os dados de nascidos vivos e anomalias
scan_abdominal <- abdominal.muni %>% 
  right_join(nv.muni, abdominal.muni, by = c('CodigoMunicipioResidencia'))


scan_abdominal <- scan_abdominal %>%
  filter(CodigoMunicipioResidencia != 420000) %>% 
  mutate(n = replace_na(n, 0)) %>% 
  rename (code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))


#calculando valor esperado
scan_abdominal$valor_esperado <- scan_abdominal$nasc_vivos * sum(scan_abdominal$n)/sum(scan_abdominal$nasc_vivos)


# unindo os dados com a base de mapa
scan_abdominal_shape <- dplyr::left_join(geoMunicipiosSC, scan_abdominal, by = "code_muni")


scan_abdominal <- scan_abdominal_shape %>% 
  dplyr::select(code_muni, name_muni, n, nasc_vivos, valor_esperado)

scan_abdominal <- scan_abdominal %>% 
  st_drop_geometry()



#vizinhanca
w <- poly2nb(scan_abdominal_shape$geom, row.names = scan_abdominal_shape$name_muni)
ww <-  nb2listw(w, style = 'B') #faz a matriz de pesos 0 ou 1

nb <- ww[["neighbours"]]



##extraindo latitude e longitude
scan_abdominal$longitude <- as.numeric(gsub(".*?([-]*[0-9]+[.][0-9]+).*", "\\1", scan_abdominal_shape$geom))
scan_abdominal$latitude <- as.numeric(gsub(".* ([-]*[0-9]+[.][0-9]+).*", "\\1", scan_abdominal_shape$geom))


#convertendo em coordenadas x e y
coord <- data.frame(x=scan_abdominal$longitude, y=scan_abdominal$latitude)
coordinates(coord) <- c("x", "y")
proj4string(coord) <- CRS("+init=epsg:29101")
coord <- spTransform(coord, CRS("+init=epsg:29101"))



# run FleXScan
fls9 <- rflexscan(x = coord$x, y = coord$y,
                 observed = scan_abdominal$n,
                 expected = scan_abdominal$valor_esperado,
                 name = scan_abdominal$name_muni,
                 clustersize = 10,
                 nb = nb,
                 stattype = "ORIGINAL",
                 scanmethod = "CIRCULAR",
                 rantype = "POISSON",
                 simcount = 9999)



## ---- clusterabdominal --------

# display clusters
par(bty="n")
choropleth(scan_abdominal_shape["name_muni"], fls9, col = viridis(5, option = "A"), rank = 1, main = NULL, border = "azure4", bty="n")
legend("bottomleft", legend = 1, fill = viridis(5, option = "A"), bty="n", title = "Ranking dos clusters")

print(fls9$cluster[[1]])

summary(fls9)





