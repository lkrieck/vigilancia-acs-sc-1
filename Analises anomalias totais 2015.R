####  ANALISES ANOMALIAS TOTAIS 2015

#OBS dados de dn nova e antiga de dificil identificacao, ver com profa marcia questao do numero de cids na dn antiga
##OBS macroregiao e regiao é sobre municipio de residencia
## Perguntar pq diferenca entre numero de nascidos vivos


###
##########tentar filtras por nascidos com anomalias

#### Consertar dados dos estabelecimentos - tem estab no geo faltando

#numero nascimentos por anos 2015 a 2020
87589 + 88795 + 89899 + 93243 + 97231 + 95325 + 98372 + 99595 + 98091 + 97360  

## ---- biblioteca2015 --------
library(leaflet)
library(geobr)
library(ggplot2)
library(sf)
library(tidyverse)
library(viridis)
library(leaflegend)
library(epiR)
library(scales) 

## ---- arquivos_mapas2015 --------
###### ARQUIVOS MAPA

#abre lista com todos os dados do pacote - parece que geob br nao esta mais disponivel desde 23/01/2022
#datasets <- list_geobr()

#gerando as bases dos dados dos mapas
geoMunicipiosSC <- read_municipality(code_muni= "SC", year=2020) %>% 
  st_transform(4326)

geoMunicipiosTodos <- read_municipality(code_muni= "all", year=2020) %>% 
  st_transform(4326)

geoRegioesSC  <- read_health_region(year = 2013) %>% 
  st_transform(4326)

geoMacrorregioesSC <- read_health_region(year = 2013, macro = TRUE) %>% 
  st_transform(4326)

geoEstabeleTodos <- read_health_facilities(showProgress = TRUE) %>% 
  st_transform(4326)


#	removendo ultimo digito do codigo do municipio SC
geoMunicipiosSC$code_muni= 
  substr(geoMunicipiosSC$code_muni,1,nchar(geoMunicipiosSC$code_muni)-1)

#	removendo ultimo digito do codigo do municipio Brasil
geoMunicipiosTodos$code_muni= 
  substr(geoMunicipiosTodos$code_muni,1,nchar(geoMunicipiosTodos$code_muni)-1)


#	removendo ultimo digito do codigo da macrorregiao
geoMacrorregioesSC$code_health_marcroregion= 
  substr(geoMacrorregioesSC$code_health_marcroregion,1,nchar(geoMacrorregioesSC$code_health_marcroregion)-1)

#filtrando dados SC
geoRegioesSC  <- geoRegioesSC %>% 
  filter(abbrev_state == "SC")

geoMacrorregioesSC <- geoMacrorregioesSC %>%
  filter(abbrev_state == "SC")



## ---- analises2015 --------

banco2015 <- read_csv2("banco2015.csv", locale = locale(encoding = "ISO8859-1"))

##### ANALISES

###NASCIDOS VIVOS POR MUNICIPIO DE RESIDENCIA
#criando dados de nascidos vivos
nv.muni.resid2015 <- banco2015 %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>% 
  group_by(CodigoMunicipioResidencia) %>% 
  count() %>% 
  rename(code_muni = CodigoMunicipioResidencia) %>% 
  rename(nasc_vivos = n) %>% 
  mutate(code_muni = as.character(code_muni))



#NASCIDOS VIVOS POOR MACROREGIAO DE RESIDENCIA
#criando dados de nascidos vivos
nv.macro.resid2015 <- banco2015 %>%
  group_by(CodigoMacrorregiaoSaude, NumeroIdentificacao) %>%
  count() %>% 
  group_by(CodigoMacrorregiaoSaude) %>% 
  count() %>% 
  rename(code_health_marcroregion = CodigoMacrorregiaoSaude) %>% 
  rename(nasc_vivos = n) %>% 
  mutate(code_health_marcroregion = as.character(code_health_marcroregion))



#NASCIDOS VIVOS POR REGIAO DE RESIDENCIA
#criando dados de nascidos vivos
nv.reg.resid2015 <- banco2015 %>%
  group_by(CodigoRegiaoSaude, NumeroIdentificacao) %>%
  count() %>% 
  group_by(CodigoRegiaoSaude) %>% 
  count() %>% 
  rename(code_health_region = CodigoRegiaoSaude) %>% 
  rename(nasc_vivos = n) %>% 
  mutate(code_health_region = as.character(code_health_region))



#####NASCIDOS VIVOS POR MUNICÍPIO DE NASCIMENTO
#criando dados de nascidos vivos
nv.muni.nasc2015 <- banco2015 %>%
  group_by(CodigoMunicipioNascimento, NumeroIdentificacao) %>%
  count() %>% 
  group_by(CodigoMunicipioNascimento) %>% 
  count() %>% 
  rename(code_muni = CodigoMunicipioNascimento) %>% 
  rename(nasc_vivos = n) %>% 
  mutate(code_muni = as.character(code_muni))



#NASCIDOS VIVOS POR ESTABELECIMENTO
#criando dados de nascidos vivos
nv.estab2015 <- banco2015 %>%
  group_by(CodigoEstabelecimento, Estabelecimento, NumeroIdentificacao) %>%
  count() %>% 
  group_by(CodigoEstabelecimento, Estabelecimento) %>% 
  count() %>% 
  rename(code_cnes = CodigoEstabelecimento) %>% 
  rename(nasc_vivos = n) %>% 
  mutate(Estabelecimento = str_to_title(Estabelecimento))


###NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE RESIDENCIA
#criando dados de nascidos vivos
anomalia.muni.resid2015 <- banco2015 %>%
  filter(CID10Anomalia != "Ignorado", CID10Anomalia != "Não se aplica") %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>%
  count() %>% 
  rename(code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))



###NASCIDOS VIVOS COM ANOMALIAS POR MACRORREGIAO DE RESIDENCIA
#criando dados de nascidos vivos
anomalia.macro.resid2015 <- banco2015 %>%
  filter(CID10Anomalia != "Ignorado", CID10Anomalia != "Não se aplica") %>%
  group_by(CodigoMacrorregiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMacrorregiaoSaude) %>%
  count() %>% 
  rename(code_health_marcroregion = CodigoMacrorregiaoSaude) %>% 
  mutate(code_health_marcroregion = as.character(code_health_marcroregion))


###NASCIDOS VIVOS COM ANOMALIAS POR REGIAO DE RESIDENCIA
#criando dados de nascidos vivos
anomalia.reg.resid2015 <- banco2015 %>%
  filter(CID10Anomalia != "Ignorado", CID10Anomalia != "Não se aplica") %>%
  group_by(CodigoRegiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoRegiaoSaude) %>%
  count() %>% 
  rename(code_health_region = CodigoRegiaoSaude) %>% 
  mutate(code_health_region = as.character(code_health_region))


###NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE NASCIMENTO
#criando dados de nascidos vivos
anomalia.muni.nasc2015 <- banco2015 %>%
  filter(CID10Anomalia != "Ignorado", CID10Anomalia != "Não se aplica") %>%
  group_by(CodigoMunicipioNascimento, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioNascimento) %>%
  count() %>% 
  rename(code_muni = CodigoMunicipioNascimento) %>% 
  mutate(code_muni = as.character(code_muni))



###NASCIDOS VIVOS COM ANOMALIAS POR ESTABELECIMENTO
#criando dados de nascidos vivos
anomalia.estab2015 <- banco2015 %>%
  filter(CID10Anomalia != "Ignorado", CID10Anomalia != "Não se aplica") %>%
  group_by(CodigoEstabelecimento, Estabelecimento, CodigoMunicipioNascimento, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoEstabelecimento, Estabelecimento, CodigoMunicipioNascimento) %>%
  count() %>% 
  rename(code_cnes = CodigoEstabelecimento) %>% 
  mutate(Estabelecimento = str_to_title(Estabelecimento))




#PREVALENCIA POR MUNICIPIO DE RESIDENCIA DA MAE
#unindo os dados de nascidos vivos e anomalias
prev.muni.resid2015 <- anomalia.muni.resid2015 %>% 
  left_join(nv.muni.resid2015, anomalia.muni.resid2015, by = "code_muni")

#calculando prevalencia
epiprev.muni.resid2015 <-epi.prev(prev.muni.resid2015$n, prev.muni.resid2015$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
epiprev.muni.resid2015_2 <- epiprev.muni.resid2015[["ap"]]

#unindo os dados de prevalencia
prev.muni.resid2015 <- prev.muni.resid2015 %>% 
  bind_cols(epiprev.muni.resid2015_2) %>% 
  mutate(est = round(est, 2))



#PREVALENCIA POR MACRORREGIAO DE RESIDENCIA
#unindo os dados de nascidos vivos e anomalias
prev.macro.resid2015  <- anomalia.macro.resid2015 %>% 
  left_join(nv.macro.resid2015, anomalia.macro.resid2015, by = "code_health_marcroregion")

#calculando prevalencia
epiprev.macro.resid2015 <-epi.prev(prev.macro.resid2015$n, prev.macro.resid2015$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
epiprev.macro.resid2015_2 <- epiprev.macro.resid2015[["ap"]]

#unindo os dados de prevalencia
prev.macro.resid2015 <- prev.macro.resid2015 %>% 
  bind_cols(epiprev.macro.resid2015_2) %>% 
  mutate(est = round(est, 2))




#PREVALENCIA POR REGIAO DE RESIDENCIA
#unindo os dados de nascidos vivos e anomalias
prev.reg.resid2015 <- anomalia.reg.resid2015 %>% 
  left_join(nv.reg.resid2015, anomalia.reg.resid2015, by = "code_health_region")

#calculando prevalencia
epiprev.reg.resid2015 <-epi.prev(prev.reg.resid2015$n, prev.reg.resid2015$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

##criando objeto com prevalencias e IC
epiprev.reg.resid2015_2 <- epiprev.reg.resid2015[["ap"]]

#unindo os dados de prevalencia
prev.reg.resid2015 <- prev.reg.resid2015 %>% 
  bind_cols(epiprev.reg.resid2015_2) %>% 
  mutate(est = round(est, 2))



#PREVALENCIA POR MUNICIPIO DE NASCIMENTO
#unindo os dados de nascidos vivos e anomalias
prev.muni.nasc2015 <- anomalia.muni.nasc2015 %>% 
  left_join(nv.muni.nasc2015, anomalia.muni.nasc2015, by = "code_muni") %>% 
  filter(str_detect(code_muni,"42"))

#calculando prevalencia
epiprev.muni.nasc2015 <-epi.prev(prev.muni.nasc2015$n, prev.muni.nasc2015$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

##criando objeto com prevalencias e IC
epiprev.muni.nasc2015_2 <- epiprev.muni.nasc2015[["ap"]]

#unindo os dados de prevalencia
prev.muni.nasc2015 <- prev.muni.nasc2015 %>% 
  bind_cols(epiprev.muni.nasc2015_2) %>% 
  mutate(est = round(est, 2))



#PREVALENCIA POR ESTABELECIMENTO
#unindo os dados de nascidos vivos e anomalias
prev.estab2015 <- anomalia.estab2015 %>% 
  left_join(nv.estab2015, anomalia.estab2015, by = "code_cnes") %>% 
  filter(str_detect(CodigoMunicipioNascimento,"42")) %>% 
  filter(code_cnes != is.na(code_cnes)) %>% 
  mutate(Estabelecimento.x = NULL) %>% 
  rename(Estabelecimento = Estabelecimento.y)


#calculando prevalencia
epiprev.estab2015 <-epi.prev(prev.estab2015$n, prev.estab2015$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

##criando objeto com prevalencias e IC
epiprev.estab2015_2 <- epiprev.estab2015[["ap"]]

#unindo os dados de prevalencia
prev.estab2015 <- prev.estab2015 %>% 
  bind_cols(epiprev.estab2015_2) %>% 
  mutate(est = round(est, 2))




######### MAPAS

## ---- mapa_nv.muni.resid2015 --------
##### MAPA NASCIDOS VIVOS POR MUNICIPIO DE RESIDENCIA
#unindo os dados de nascidos vivos e dados do map
dados_nv.muni.resid2015 <- left_join(geoMunicipiosSC, nv.muni.resid2015, by = "code_muni")

#plotando o mapa
mapa_nv.muni.resid2015  <- leaflet(data = dados_nv.muni.resid2015) 

label_nv.muni.resid2015 <- dados_nv.muni.resid2015 %>%
  mutate (nasc_vivos = replace_na(nasc_vivos, 0))

colormagma_nv.muni.resid2015 = colorNumeric("magma", domain = dados_nv.muni.resid2015$nasc_vivos, alpha = TRUE, reverse =  TRUE)

mapa_nv.muni.resid2015 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_nv.muni.resid2015(nasc_vivos),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               Número de nascidos vivos: </strong>%s", 
                               label_nv.muni.resid2015$name_muni, label_nv.muni.resid2015$nasc_vivos) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>% 
  addLegend(position = "bottomleft", pal = colormagma_nv.muni.resid2015, values = ~nasc_vivos, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Número de nascidos vivos")


## ---- mapa_nv.macro.resid2015 --------
##### MAPA NASCIDOS VIVOS POR MACRORREGIAO DE RESIDENCIA
#unindo os dados de nascidos vivos e dados do map
dados_nv.macro.resid2015 <- left_join(geoMacrorregioesSC, nv.macro.resid2015, by = "code_health_marcroregion")

#plotando o mapa
mapa_nv.macro.resid2015  <- leaflet(data = dados_nv.macro.resid2015) 

label_nv.macro.resid2015 <- dados_nv.macro.resid2015 %>%
  mutate (nasc_vivos = replace_na(nasc_vivos, 0)) %>% 
  mutate(name_health_macroregion = str_to_title(name_health_macroregion))

label_nv.macro.resid2015["3", "name_health_macroregion"] <- "Meio Oeste E Serra Catarinense"

colormagma_nv.macro.resid2015 = colorNumeric("magma", domain = dados_nv.macro.resid2015$nasc_vivos, alpha = TRUE, reverse =  TRUE)

mapa_nv.macro.resid2015 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_nv.macro.resid2015(nasc_vivos),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Macrorregião de saúde: </strong>%s<br/><strong>
                               Número de nascidos vivos: </strong>%s", 
                               label_nv.macro.resid2015$name_health_macroregion, label_nv.macro.resid2015$nasc_vivos) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_nv.macro.resid2015, values = ~nasc_vivos, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Número de nascidos vivos")


## ---- mapa_nv.reg.resid2015 --------
##### MAPA NASCIDOS VIVOS POR REGIAO DE RESIDENCIA
#unindo os dados de nascidos vivos e dados do map
dados_nv.reg.resid2015 <- left_join(geoRegioesSC, nv.reg.resid2015, by = "code_health_region")

#plotando o mapa
mapa_nv.reg.resid2015  <- leaflet(data = dados_nv.reg.resid2015) 

label_nv.reg.resid2015 <- dados_nv.reg.resid2015 %>%
  mutate (nasc_vivos = replace_na(nasc_vivos, 0)) %>% 
  mutate(name_health_region = str_to_title(name_health_region))


colormagma_nv.reg.resid2015 = colorNumeric("magma", domain = dados_nv.reg.resid2015$nasc_vivos, alpha = TRUE, reverse =  TRUE)

mapa_nv.reg.resid2015 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_nv.reg.resid2015(nasc_vivos),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Região de saúde: </strong>%s<br/><strong>
                               Número de nascidos vivos: </strong>%s", 
                               label_nv.reg.resid2015$name_health_region, label_nv.reg.resid2015$nasc_vivos) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_nv.reg.resid2015, values = ~nasc_vivos, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Número de nascidos vivos")


## ---- mapa_nv.muni.nasc2015 --------
##### MAPA NASCIDOS VIVOS POR MUNICIPIO DE NASCIMENTO SC
#unindo os dados de nascidos vivos e dados do map
dados_nv.muni.nasc2015 <- left_join(geoMunicipiosSC, nv.muni.nasc2015, by = "code_muni")

#plotando o mapa
mapa_nv.muni.nasc2015  <- leaflet(data = dados_nv.muni.nasc2015) 

label_nv.muni.nasc2015 <- dados_nv.muni.nasc2015 %>%
  mutate (nasc_vivos = replace_na(nasc_vivos, 0))

colormagma_nv.muni.nasc2015 = colorNumeric("magma", domain = dados_nv.muni.nasc2015$nasc_vivos, alpha = TRUE, reverse =  TRUE)

mapa_nv.muni.nasc2015 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_nv.muni.nasc2015(nasc_vivos),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de nascimento: </strong>%s<br/><strong>
                               Número de nascidos vivos: </strong>%s", 
                               label_nv.muni.nasc2015$name_muni, label_nv.muni.nasc2015$nasc_vivos) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>% 
  addLegend(position = "bottomleft", pal = colormagma_nv.muni.nasc2015, values = ~nasc_vivos, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Número de nascidos vivos")



## ---- mapa_nv.muni.nascBR2015 --------  
##### MAPA NASCIDOS VIVOS POR MUNICIPIO DE NASCIMENTO BRASIL
#unindo os dados de nascidos vivos e dados do map
dados_nv.muni.nascBR2015 <- left_join(geoMunicipiosTodos, nv.muni.nasc2015, by = "code_muni")

#plotando o mapa
mapa_nv.muni.nascBR2015  <- leaflet(data = dados_nv.muni.nascBR2015) 

label_nv.muni.nascBR2015 <- dados_nv.muni.nascBR2015 %>%
  mutate (nasc_vivos = replace_na(nasc_vivos, 0))

colormagma_nv.muni.nascBR2015 = colorNumeric("magma", domain = dados_nv.muni.nascBR2015$nasc_vivos, alpha = TRUE, reverse =  TRUE)

mapa_nv.muni.nascBR2015 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_nv.muni.nascBR2015(nasc_vivos),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de nascimento: </strong>%s<br/><strong>
                               Número de nascidos vivos: </strong>%s", 
                               label_nv.muni.nascBR2015$name_muni, label_nv.muni.nascBR2015$nasc_vivos) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_nv.muni.nascBR2015, values = ~nasc_vivos, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Número de nascidos vivos")



## ---- mapa_nv.estab2015 -------- 
### MAPA NASCIDOS VIVOS POR ESTABELECIMENTO
## feito em http://dwilhelm89.github.io/Leaflet.StyleEditor/ -- redimensionar antes de usar
#criando lista de marcadores - so funcionou nessa ordem
marcadores_personalizados <- iconList(
  laranja = makeIcon("/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador laranja.png"),
  verde = makeIcon("/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador verde.png"),
  vermelho = makeIcon("/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador vermelho.png"))


# unindo os dados com a base de mapa
dados_nv.estab2015 <- dplyr::left_join(geoEstabeleTodos, nv.estab2015, by = "code_cnes")

#filtrando por estabelecimentos com anomalias
dados_nv.estab2015 <- dados_nv.estab2015 %>% 
  filter(nasc_vivos != is.na(nasc_vivos))

#criando coluna marcadores
dados_nv.estab2015<- dados_nv.estab2015 %>%
  mutate(Marcador = case_when(
    nasc_vivos <100 ~ "verde",
    nasc_vivos <1000 ~ "laranja",
    nasc_vivos >999 ~ "vermelho"))

#convertendo marcadores como factor
dados_nv.estab2015 <- dados_nv.estab2015 %>%
  mutate(Marcador=as.factor(Marcador))

#organizando nivel marcadores
levels(dados_nv.estab2015$Marcador) <- c("verde", "laranja", "vermelho")



#plotando o mapa  
label_nv.estab2015 <- sprintf("<p><strong>Estabelecimento: </strong>%s<br/>
                 <strong>Número de nascidos vivos: </strong>%s", dados_nv.estab2015$Estabelecimento,
                              dados_nv.estab2015$nasc_vivos) %>% lapply(htmltools::HTML)


mapa_nv.estab2015 <- leaflet(dados_nv.estab2015) %>% addTiles() 
mapa_nv.estab2015 %>% 
  addMarkers(icon = ~marcadores_personalizados[as.numeric(Marcador)],
             label = label_nv.estab2015,  
             labelOptions = labelOptions(textsize = "14px", opacity = 0.7, direction = "top"),
             options = markerOptions(riseOnHover = TRUE)) %>% 
  addLegendImage(mapa_nv.estab2015, images = c("/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador verde.png",
                                               "/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador laranja.png",
                                               "/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador vermelho.png"),
                 labels = c('1-99', '100-999', 'Maior ou igual a 1.000'), width = c(20, 25, 28), height = c(31, 34, 39),
                 title = htmltools::tags$div('Nascidos vivos por estabelecimento', style = 'font-size: 15px; text-align: center;'),
                 labelStyle = "font-size: 15px; vertical-align: middle;",
                 orientation = 'vertical',
                 position = 'bottomleft')


## ---- mapa_anomalia.muni.resid2015 --------
##### MAPA NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE RESIDENCIA
#unindo os dados de nascidos vivos e dados do map
dados_anomalia.muni.resid2015 <- left_join(geoMunicipiosSC, anomalia.muni.resid2015, by = "code_muni")

#plotando o mapa
mapa_anomalia.muni.resid2015  <- leaflet(data = dados_anomalia.muni.resid2015) 

label_anomalia.muni.resid2015 <- dados_anomalia.muni.resid2015 %>%
  mutate (n = replace_na(n, 0))

colormagma_anomalia.muni.resid2015 = colorNumeric("magma", domain = dados_anomalia.muni.resid2015$n, alpha = TRUE, reverse =  TRUE)

mapa_anomalia.muni.resid2015 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_anomalia.muni.resid2015(n),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               Número de nascidos vivos: </strong>%s", 
                               label_anomalia.muni.resid2015$name_muni, label_anomalia.muni.resid2015$n) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>% 
  addLegend(position = "bottomleft", pal = colormagma_anomalia.muni.resid2015, values = ~n, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Nascidos vivos <br> com anomalias")


## ---- mapa_anomalia.macro.resid2015 --------
##### MAPA NASCIDOS VIVOS COM ANOMALIAS POR MACRORREGIAO DE RESIDENCIA
#unindo os dados de nascidos vivos e dados do map
dados_anomalia.macro.resid2015 <- left_join(geoMacrorregioesSC, anomalia.macro.resid2015, by = "code_health_marcroregion")

#plotando o mapa
mapa_anomalia.macro.resid2015  <- leaflet(data = dados_anomalia.macro.resid2015) 

label_anomalia.macro.resid2015 <- dados_anomalia.macro.resid2015 %>%
  mutate (n = replace_na(n, 0)) %>% 
  mutate(name_health_macroregion = str_to_title(name_health_macroregion))

label_anomalia.macro.resid2015["3", "name_health_macroregion"] <- "Meio Oeste E Serra Catarinense"

colormagma_anomalia.macro.resid2015 = colorNumeric("magma", domain = dados_anomalia.macro.resid2015$n, alpha = TRUE, reverse =  TRUE)

mapa_anomalia.macro.resid2015 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_anomalia.macro.resid2015(n),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Macrorregião de saúde: </strong>%s<br/><strong>
                               Número de nascidos vivos: </strong>%s", 
                               label_anomalia.macro.resid2015$name_health_macroregion, label_anomalia.macro.resid2015$n) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_anomalia.macro.resid2015, values = ~n, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Nascidos vivos <br> com anomalias")


## ---- mapa_anomalia.reg.resid2015 --------
##### MAPA NASCIDOS VIVOS COM ANOMALIAS POR REGIAO DE RESIDENCIA
#unindo os dados de nascidos vivos e dados do map
dados_anomalia.reg.resid2015 <- left_join(geoRegioesSC, anomalia.reg.resid2015, by = "code_health_region")

#plotando o mapa
mapa_anomalia.reg.resid2015  <- leaflet(data = dados_anomalia.reg.resid2015) 

label_anomalia.reg.resid2015 <- dados_anomalia.reg.resid2015 %>%
  mutate (n = replace_na(n, 0)) %>% 
  mutate(name_health_region = str_to_title(name_health_region))


colormagma_anomalia.reg.resid2015 = colorNumeric("magma", domain = dados_anomalia.reg.resid2015$n, alpha = TRUE, reverse =  TRUE)

mapa_anomalia.reg.resid2015 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_anomalia.reg.resid2015(n),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Região de saúde: </strong>%s<br/><strong>
                               Número de nascidos vivos: </strong>%s", 
                               label_anomalia.reg.resid2015$name_health_region, label_anomalia.reg.resid2015$n) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_anomalia.reg.resid2015, values = ~n, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Nascidos vivos <br> com anomalias")


## ---- mapa_anomalia.muni.nasc2015 --------
##### MAPA NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE NASCIMENTO SC
#unindo os dados de nascidos vivos e dados do map
dados_anomalia.muni.nasc2015 <- left_join(geoMunicipiosSC, anomalia.muni.nasc2015, by = "code_muni")

#plotando o mapa
mapa_anomalia.muni.nasc2015  <- leaflet(data = dados_anomalia.muni.nasc2015) 

label_anomalia.muni.nasc2015 <- dados_anomalia.muni.nasc2015 %>%
  mutate (n = replace_na(n, 0))

colormagma_anomalia.muni.nasc2015 = colorNumeric("magma", domain = dados_anomalia.muni.nasc2015$n, alpha = TRUE, reverse =  TRUE)

mapa_anomalia.muni.nasc2015 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_anomalia.muni.nasc2015(n),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de nascimento: </strong>%s<br/><strong>
                               Número de nascidos vivos: </strong>%s", 
                               label_anomalia.muni.nasc2015$name_muni, label_anomalia.muni.nasc2015$n) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>% 
  addLegend(position = "bottomleft", pal = colormagma_anomalia.muni.nasc2015, values = ~n, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Nascidos vivos <br> com anomalias")



## ---- mapa_anomalia.muni.nascBR2015 --------
##### MAPA NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE NASCIMENTO BRASIL
#unindo os dados de nascidos vivos e dados do map
dados_anomalia.muni.nascBR2015 <- left_join(geoMunicipiosTodos, anomalia.muni.nasc2015, by = "code_muni")

#plotando o mapa
mapa_anomalia.muni.nascBR2015  <- leaflet(data = dados_anomalia.muni.nascBR2015) 

label_anomalia.muni.nascBR2015 <- dados_anomalia.muni.nascBR2015 %>%
  mutate (n = replace_na(n, 0))

colormagma_anomalia.muni.nascBR2015 = colorNumeric("magma", domain = dados_anomalia.muni.nascBR2015$n, alpha = TRUE, reverse =  TRUE)

mapa_anomalia.muni.nascBR2015 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_anomalia.muni.nascBR2015(n),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de nascimento: </strong>%s<br/><strong>
                               Número de nascidos vivos: </strong>%s", 
                               label_anomalia.muni.nascBR2015$name_muni, label_anomalia.muni.nascBR2015$n) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_anomalia.muni.nascBR2015, values = ~n, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Nascidos vivos <br> com anomalias")



## ---- mapa_anomalia.estab2015 --------
### MAPA NASCIDOS VIVOS COM ANOMALIAS POR ESTABELECIMENTO
## feito em http://dwilhelm89.github.io/Leaflet.StyleEditor/ -- redimensionar antes de usar
#criando lista de marcadores - so funcionou nessa ordem
marcadores_personalizados <- iconList(
  laranja = makeIcon("/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador laranja.png"),
  verde = makeIcon("/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador verde.png"),
  vermelho = makeIcon("/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador vermelho.png"))


# unindo os dados com a base de mapa
dados_anomalia.estab2015 <- dplyr::left_join(geoEstabeleTodos, anomalia.estab2015, by = "code_cnes")

#filtrando por estabelecimentos com anomalias
dados_anomalia.estab2015 <- dados_anomalia.estab2015 %>% 
  filter(n != is.na(n))

#criando coluna marcadores
dados_anomalia.estab2015<- dados_anomalia.estab2015 %>%
  mutate(Marcador = case_when(
    n <20 ~ "verde",
    n <40 ~ "laranja",
    n >39 ~ "vermelho"))

#coanomaliaertendo marcadores como factor
dados_anomalia.estab2015 <- dados_anomalia.estab2015 %>%
  mutate(Marcador=as.factor(Marcador))

#organizando nivel marcadores
levels(dados_anomalia.estab2015$Marcador) <- c("verde", "laranja", "vermelho")



#plotando o mapa  
label_anomalia.estab2015 <- sprintf("<p><strong>Estabelecimento: </strong>%s<br/>
                 <strong>Número de nascidos vivos: </strong>%s", dados_anomalia.estab2015$Estabelecimento,
                                    dados_anomalia.estab2015$n) %>% lapply(htmltools::HTML)


mapa_anomalia.estab2015 <- leaflet(dados_anomalia.estab2015) %>% addTiles() 
mapa_anomalia.estab2015 %>% 
  addMarkers(icon = ~marcadores_personalizados[as.numeric(Marcador)],
             label = label_anomalia.estab2015,  
             labelOptions = labelOptions(textsize = "14px", opacity = 0.7, direction = "top"),
             options = markerOptions(riseOnHover = TRUE)) %>% 
  addLegendImage(mapa_anomalia.estab2015, images = c("/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador verde.png",
                                                     "/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador laranja.png",
                                                     "/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador vermelho.png"),
                 labels = c('1-19', '20-39', 'Maior ou igual a 40'), width = c(20, 25, 28), height = c(31, 34, 39),
                 title = htmltools::tags$div('NV c/ AC por estabelecimento', style = 'font-size: 15px; text-align: center;'),
                 labelStyle = "font-size: 15px; vertical-align: middle;",
                 orientation = 'vertical',
                 position = 'bottomleft')




### PREVALENCIAS

## ---- mapa_prev.muni.resid2015 --------
### MAPA PREVALENCIA POR MUNICIPIOS DE RESIDENCIA DA MAE
# unindo os dados com a base de mapa
dados_prev.muni.resid2015 <- dplyr::left_join(geoMunicipiosSC, prev.muni.resid2015, by = "code_muni") 

#modificando decimal no label e susbtituindo NAs
label_prev.muni.resid2015 <- dados_prev.muni.resid2015 %>%
  mutate (est = replace_na(est, 0.00)) %>% 
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ","))

#plotando o mapa
mapa_prev.muni.resid2015  <- leaflet(data = dados_prev.muni.resid2015) 

colormagma_prev.muni.resid2015 = colorNumeric("magma", domain = dados_prev.muni.resid2015$est, alpha = TRUE, reverse =  TRUE)

mapa_prev.muni.resid2015 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_prev.muni.resid2015(est),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               Prevalência/10.000: </strong>%s", 
                               label_prev.muni.resid2015$name_muni, label_prev.muni.resid2015$est) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_prev.muni.resid2015, values = ~est, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Prevalência/10.000")


## ---- mapa_prev.macro.resid2015 --------
### MAPA PREVALENCIA POR MACRORREGIAO DE RESIDENCIA DA MAE
# unindo os dados com a base de mapa
dados_prev.macro.resid2015 <- dplyr::left_join(geoMacrorregioesSC, prev.macro.resid2015, by = "code_health_marcroregion") 

#modificando decimal no label e susbtituindo NAs
label_prev.macro.resid2015 <- dados_prev.macro.resid2015 %>%
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate(name_health_macroregion = str_to_title(name_health_macroregion))

label_prev.macro.resid2015["3", "name_health_macroregion"] <- "Meio Oeste E Serra Catarinense"

#plotando o mapa
mapa_prev.macro.resid2015  <- leaflet(data = dados_prev.macro.resid2015) 

colormagma_prev.macro.resid2015 = colorNumeric("magma", domain = dados_prev.macro.resid2015$est, alpha = TRUE, reverse =  TRUE)

mapa_prev.macro.resid2015 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_prev.macro.resid2015(est),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Macrorregião de saúde: </strong>%s<br/>
               <strong>Prevalência/10.000: </strong>%s", label_prev.macro.resid2015$name_health_macroregion, label_prev.macro.resid2015$est) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_prev.macro.resid2015, values = ~est, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Prevalência/10.000")



## ---- mapa_prev.reg.resid2015 --------
### MAPA PREVALENCIA POR REGIAO DE RESIDENCIA DA MAE
# unindo os dados com a base de mapa
dados_prev.reg.resid2015 <- dplyr::left_join(geoRegioesSC, prev.reg.resid2015, by = "code_health_region") 

#modificando decimal no label e susbtituindo NAs
label_prev.reg.resid2015 <- dados_prev.reg.resid2015 %>%
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ","))

#plotando o mapa
mapa_prev.reg.resid2015  <- leaflet(data = dados_prev.reg.resid2015) 

colormagma_prev.reg.resid2015 = colorNumeric("magma", domain = dados_prev.reg.resid2015$est, alpha = TRUE, reverse =  TRUE)

mapa_prev.reg.resid2015 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_prev.reg.resid2015(est),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Região de saúde: </strong>%s<br/><strong>
                               Prevalência/10.000: </strong>%s", 
                               label_prev.reg.resid2015$name_health_region, label_prev.reg.resid2015$est) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_prev.reg.resid2015, values = ~est, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Prevalência/10.000")



## ---- mapa_prev.muni.nasc2015 --------
### MAPA PREVALENCIA POR MUNICIPIOS DE NASCIMENTO
# unindo os dados com a base de mapa
dados_prev.muni.nasc2015 <- dplyr::left_join(geoMunicipiosSC, prev.muni.nasc2015, by = "code_muni") 

#substituindo NAs no label
label_prev.muni.nasc2015 <- dados_prev.muni.nasc2015 %>%
  mutate (est = replace_na(est, 0.00))

#modificando decimal no label 
label_prev.muni.nasc2015 <- label_prev.muni.nasc2015  %>%
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ","))


#plotando o mapa
mapa_prev.muni.nasc2015  <- leaflet(data = dados_prev.muni.nasc2015) 

colormagma_prev.muni.nasc2015 = colorNumeric("magma", domain = dados_prev.muni.nasc2015$est, alpha = TRUE, reverse =  TRUE)

mapa_prev.muni.nasc2015 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_prev.muni.nasc2015(est),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de nascimento: </strong>%s<br/><strong>
                               Prevalência/10.000: </strong>%s", 
                               label_prev.muni.nasc2015$name_muni, label_prev.muni.nasc2015$est) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_prev.muni.nasc2015, values = ~est, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Prevalência/10.000")


## ---- mapa_prev.estab2015 --------
### MAPA PREVALENCIAS POR ESTABELECIMENTO
#icones dos marcadores criados anteriormente
# unindo os dados com a base de mapa
dados_prev.estab2015 <- dplyr::left_join(geoEstabeleTodos, prev.estab2015, by = "code_cnes")

#filtrando por estabelecimentos com anomalias em santa catarina
dados_prev.estab2015 <- dados_prev.estab2015 %>% 
  filter(abbrev_state == "SC") %>% 
  filter(n != is.na(n))


#criando coluna marcadores
dados_prev.estab2015 <- dados_prev.estab2015 %>%
  mutate(Marcador = case_when(
    est <250 ~ "verde",
    est <400 ~ "laranja",
    est >399 ~ "vermelho"))

#convertendo marcadores como factor
dados_prev.estab2015 <- dados_prev.estab2015 %>%
  mutate(Marcador=as.factor(Marcador))

#organizando nivel marcadores
levels(dados_prev.estab2015$Marcador) <- c("verde", "laranja", "vermelho")


#criando label e modificando decimal
label_prev.estab2015 <- dados_prev.estab2015  %>%
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ","))


label_prev.estab2015 <- sprintf("<p><strong>Estabelecimento: </strong>%s<br/>
                 <strong>Prevalência/10.000: </strong>%s", label_prev.estab2015$Estabelecimento,
                                label_prev.estab2015$est) %>% lapply(htmltools::HTML)

#plotando o mapa
mapa_prev.estab2015 <- leaflet(dados_prev.estab2015) %>% addTiles() 
mapa_prev.estab2015 %>% 
  addMarkers(icon = ~marcadores_personalizados[as.numeric(Marcador)],
             label = label_prev.estab2015,  
             labelOptions = labelOptions(textsize = "14px", opacity = 0.7, direction = "top"),
             options = markerOptions(riseOnHover = TRUE)) %>% 
  addLegendImage(mapa_prev.estab2015, images = c("/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador verde.png",
                                                 "/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador laranja.png",
                                                 "/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador vermelho.png"),
                 labels = c('1-249', '250-399', 'Maior ou igual a 400'), width = c(20, 25, 28), height = c(31, 34, 39),
                 title = htmltools::tags$div('Prevalência/10.000', style = 'font-size: 15px; text-align: center;'),
                 labelStyle = "font-size: 15px; vertical-align: middle;",
                 orientation = 'vertical',
                 position = 'bottomleft')



## TABELAS


## ---- outros_arquivos --------


#	abrindo dados ibge municipios e transformando em objeto
# https://www.ibge.gov.br/explica/codigos-dos-municipios.php > Download da tabela de codigos
library(readxl)
municipios <- read_xls("RELATORIO_DTB_BRASIL_MUNICIPIO.xls")

#	manipulando e limpando dados dos municipios
municipios <- municipios %>%
  dplyr::select(Nome_UF, `Código Município Completo`, Nome_Município) %>%
  rename(UFNascimento = Nome_UF,
         CodigoMunicipioNascimento = `Código Município Completo`,
         MunicipioNascimento = Nome_Município)

#	removendo ultimo digito do codigo do municipio
municipios$CodigoMunicipioNascimento = 
  substr(municipios$CodigoMunicipioNascimento,1,nchar(municipios$CodigoMunicipioNascimento)-1)



#	duplicando objeto municipios para dados de residencia 
municipiosresidencia <- municipios

#	limpando dados residencia
municipiosresidencia <- municipiosresidencia %>%
  dplyr::select(CodigoMunicipioNascimento, MunicipioNascimento) %>%
  rename(CodigoMunicipioResidencia = CodigoMunicipioNascimento,
         MunicipioResidencia = MunicipioNascimento) %>%
  add_row(CodigoMunicipioResidencia="420000", MunicipioResidencia="Município Ignorado")



#Importando arquivo macrorregiao
#http://svs.aids.gov.br/dantps/cgiae/sinasc/documentacao/ > Arquivos DEF e CNV das Tabelas DN
macrorregioes <- read_fwf("sc_macsaud.cnv", fwf_empty("sc_macsaud.cnv"))

#	limpando dados macrorregioes
macrorregioes <- macrorregioes %>%
  dplyr::select(X4, X5, X6) %>%
  rename (CodigoMacrorregiaoSaude = X4,
          MacrorregiaoSaude = X5,
          CodigoMunicipioResidencia = X6) %>%
  slice(2:n()) %>%
  add_row(CodigoMacrorregiaoSaude = 4200, MacrorregiaoSaude = "Município Ignorado", CodigoMunicipioResidencia = 420000)

#	primeira letra maiuscula
macrorregioes$MacrorregiaoSaude = str_to_title(macrorregioes$MacrorregiaoSaude)



#	Importando arquivo regioes
#http://svs.aids.gov.br/dantps/cgiae/sinasc/documentacao/ > Arquivos DEF e CNV das Tabelas DN
regioes <- read_fwf("sc_regsaud.cnv", locale = locale(encoding = 'ISO8859-1'))

#	limpando dados regioes
regioes <- regioes %>%
  dplyr::select(X2, X3, X4) %>%
  rename (CodigoRegiaoSaude = X2,
          RegiaoSaude = X3,
          CodigoMunicipioResidencia = X4) %>%
  slice(2:n()) %>% #remove primeira linha
  slice(1:(n()-2)) %>% #remove ultimas duas linhas
  add_row(CodigoRegiaoSaude = 42000, RegiaoSaude = "Município Ignorado", CodigoMunicipioResidencia = "420000")

regioes["168", "CodigoMunicipioResidencia"] <- "420415"




## ---- tab_prev.muni.resid2015 --------
tab_prev.muni.resid2015 <- prev.muni.resid2015 %>%
  rename(CodigoMunicipioResidencia = code_muni) 

tab_prev.muni.resid2015 <- tab_prev.muni.resid2015 %>% 
  left_join(municipiosresidencia, tab_prev.muni.resid2015, by = "CodigoMunicipioResidencia") %>% 
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (lower = scales::number(lower, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (upper = scales::number(upper, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>%
  unite (col = IC95, lower:upper, sep = "-") %>%
  rename(NascidosVivosACs = n) %>% 
  rename(NascidosVivos = nasc_vivos) %>% 
  rename(Prevalencia10000 = est) %>% 
  relocate(MunicipioResidencia, .after = CodigoMunicipioResidencia)


## ---- tab_prev.macro.resid2015 --------
tab_prev.macro.resid2015 <- prev.macro.resid2015 %>%
  rename(CodigoMacrorregiaoSaude = code_health_marcroregion) %>% 
  mutate(CodigoMacrorregiaoSaude = as.numeric(CodigoMacrorregiaoSaude))

tab_prev.macro.resid2015 <- tab_prev.macro.resid2015 %>% 
  left_join(macrorregioes, tab_prev.macro.resid2015, by = "CodigoMacrorregiaoSaude") %>% 
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (lower = scales::number(lower, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (upper = scales::number(upper, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>%
  unite (col = IC95, lower:upper, sep = "-") %>%
  rename(NascidosVivosACs = n) %>% 
  rename(NascidosVivos = nasc_vivos) %>% 
  rename(Prevalencia10000 = est) %>% 
  relocate(MacrorregiaoSaude, .after = CodigoMacrorregiaoSaude) %>% 
  mutate (CodigoMunicipioResidencia = NULL) %>% 
  unique()


## ---- tab_prev.reg.resid2015 --------
tab_prev.reg.resid2015 <- prev.reg.resid2015 %>%
  rename(CodigoRegiaoSaude = code_health_region) %>% 
  mutate(CodigoRegiaoSaude = as.numeric(CodigoRegiaoSaude)) 

tab_prev.reg.resid2015 <- tab_prev.reg.resid2015 %>% 
  left_join(regioes, tab_prev.reg.resid2015, by = "CodigoRegiaoSaude") %>% 
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (lower = scales::number(lower, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (upper = scales::number(upper, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>%
  unite (col = IC95, lower:upper, sep = "-") %>%
  rename(NascidosVivosACs = n) %>% 
  rename(NascidosVivos = nasc_vivos) %>% 
  rename(Prevalencia10000 = est) %>% 
  relocate(RegiaoSaude, .after = CodigoRegiaoSaude) %>% 
  mutate (CodigoMunicipioResidencia = NULL) %>% 
  unique()


## ---- tab_prev.muni.nasc2015 --------
tab_prev.muni.nasc2015 <- prev.muni.nasc2015 %>%
  rename(CodigoMunicipioNascimento = code_muni) 

tab_prev.muni.nasc2015 <- tab_prev.muni.nasc2015 %>% 
  left_join(municipios, tab_prev.muni.nasc2015, by = "CodigoMunicipioNascimento") %>% 
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (lower = scales::number(lower, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (upper = scales::number(upper, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>%
  unite (col = IC95, lower:upper, sep = "-") %>%
  rename(NascidosVivosACs = n) %>% 
  rename(NascidosVivos = nasc_vivos) %>% 
  rename(Prevalencia10000 = est) %>% 
  relocate(MunicipioNascimento, .after = CodigoMunicipioNascimento) %>% 
  mutate (UFNascimento = NULL)


## ---- tab_prev.estab2015 --------
tab_prev.estab2015 <- prev.estab2015 %>%
  rename(CodigoEstabelecimento = code_cnes) %>% 
  mutate(CodigoEstabelecimento = as.character(CodigoEstabelecimento)) %>%
  mutate(CodigoEstabelecimento = str_pad(CodigoEstabelecimento, width = 7, side = "left", pad = "0"))


tab_prev.estab2015 <- tab_prev.estab2015 %>% 
  left_join(estabelecimentoC, tab_prev.estab2015, by = "CodigoEstabelecimento") %>% 
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (lower = scales::number(lower, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (upper = scales::number(upper, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>%
  unite (col = IC95, lower:upper, sep = "-") %>%
  rename(NascidosVivosACs = n) %>% 
  rename(NascidosVivos = nasc_vivos) %>% 
  rename(Prevalencia10000 = est) %>% 
  mutate(Estabelecimento.x = NULL) %>% 
  rename(Estabelecimento = Estabelecimento.y) %>% 
  relocate(Estabelecimento, .after = CodigoEstabelecimento) %>% 
  mutate(Estabelecimento = str_to_title(Estabelecimento))


tab_prev.estab2015 <- tab_prev.estab2015 %>%
  mutate(CodigoMunicipioNascimento = as.character(CodigoMunicipioNascimento)) %>% 
  left_join(municipios, tab_prev.estab2015, by = "CodigoMunicipioNascimento") %>% 
  relocate(MunicipioNascimento, .after = CodigoMunicipioNascimento) %>% 
  mutate (UFNascimento = NULL)



## ---- tab_muni.nasc2015 --------
tab_nv.muni.nasc2015 <- nv.muni.nasc2015 %>%
  rename(CodigoMunicipioNascimento = code_muni) 

tab_nv.muni.nasc2015 <- tab_nv.muni.nasc2015 %>% 
  left_join(municipios, tab_nv.muni.nasc2015, by = "CodigoMunicipioNascimento") %>% 
  rename(NascidosVivos = nasc_vivos) %>% 
  relocate(MunicipioNascimento, .after = CodigoMunicipioNascimento) %>%
  relocate(UFNascimento, .after = MunicipioNascimento)