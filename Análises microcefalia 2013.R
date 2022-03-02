#ANALISES MICROCEFALIA 2013


## ---- biblioteca2013 --------
library(leaflet)
library(geobr)
library(ggplot2)
library(sf)
library(tidyverse)
library(viridis)
library(leaflegend)
library(epiR)
library(scales) 


## ---- arquivos_mapas2013 --------
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



## ---- analises2013 --------

banco2013 <- read_csv2("banco2013.csv", locale = locale(encoding = "ISO8859-1"))

##### ANALISES

###NASCIDOS VIVOS POR MUNICIPIO DE RESIDENCIA
#criando dados de nascidos vivos
nv.muni.resid2013 <- banco2013 %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>% 
  group_by(CodigoMunicipioResidencia) %>% 
  count() %>% 
  rename(code_muni = CodigoMunicipioResidencia) %>% 
  rename(nasc_vivos = n) %>% 
  mutate(code_muni = as.character(code_muni))



#NASCIDOS VIVOS POOR MACROREGIAO DE RESIDENCIA
#criando dados de nascidos vivos
nv.macro.resid2013 <- banco2013 %>%
  group_by(CodigoMacrorregiaoSaude, NumeroIdentificacao) %>%
  count() %>% 
  group_by(CodigoMacrorregiaoSaude) %>% 
  count() %>% 
  rename(code_health_marcroregion = CodigoMacrorregiaoSaude) %>% 
  rename(nasc_vivos = n) %>% 
  mutate(code_health_marcroregion = as.character(code_health_marcroregion))



#NASCIDOS VIVOS POR REGIAO DE RESIDENCIA
#criando dados de nascidos vivos
nv.reg.resid2013 <- banco2013 %>%
  group_by(CodigoRegiaoSaude, NumeroIdentificacao) %>%
  count() %>% 
  group_by(CodigoRegiaoSaude) %>% 
  count() %>% 
  rename(code_health_region = CodigoRegiaoSaude) %>% 
  rename(nasc_vivos = n) %>% 
  mutate(code_health_region = as.character(code_health_region))



#####NASCIDOS VIVOS POR MUNICÍPIO DE NASCIMENTO
#criando dados de nascidos vivos
nv.muni.nasc2013 <- banco2013 %>%
  group_by(CodigoMunicipioNascimento, NumeroIdentificacao) %>%
  count() %>% 
  group_by(CodigoMunicipioNascimento) %>% 
  count() %>% 
  rename(code_muni = CodigoMunicipioNascimento) %>% 
  rename(nasc_vivos = n) %>% 
  mutate(code_muni = as.character(code_muni))



#NASCIDOS VIVOS POR ESTABELECIMENTO
#criando dados de nascidos vivos
nv.estab2013 <- banco2013 %>%
  group_by(CodigoEstabelecimento, NumeroIdentificacao) %>%
  count() %>% 
  group_by(CodigoEstabelecimento) %>% 
  count() %>% 
  rename(code_cnes = CodigoEstabelecimento) %>% 
  rename(nasc_vivos = n)


#criando variavel de grupo
anom.grupos2013 <- banco2013 %>% 
  filter(str_detect(CID10Anomalia,"Q000|Q001|Q002|Q01|Q05|Q02|Q20|Q21|Q22|Q23|Q24|Q25|Q26|Q27|Q28|Q35|Q36|Q37|Q54|Q56|Q66|Q69|Q71|Q72|Q73|Q743|Q792|Q793|Q90")) %>% 
  mutate(GrupoCID10 = case_when(
    str_detect(CID10Anomalia,"Q000|Q001|Q002|Q01|Q05") ~ "Defeitos de tubo neural",
    str_detect(CID10Anomalia,"Q02") ~ "Microcefalia",
    str_detect(CID10Anomalia,"Q20|Q21|Q22|Q23|Q24|Q25|Q26|Q27|Q28") ~ "Cardiopatias congênitas",
    str_detect(CID10Anomalia,"Q35|Q36|Q37") ~ "Fendas orais",
    str_detect(CID10Anomalia,"Q54|Q56") ~ "Anomalias de órgãos genitais",
    str_detect(CID10Anomalia,"Q66|Q69|Q71|Q72|Q73|Q743") ~ "Defeitos de membros",
    str_detect(CID10Anomalia,"Q792|Q793") ~ "Defeitos da parede microce",
    str_detect(CID10Anomalia,"Q90") ~ "Síndrome de Down"))



#NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE RESIDENCIA
#dados contagem de NASCIDOS VIVOS COM ANOMALIAS por municipios de residencia 2013 
microce.muni.resid2013 <- anom.grupos2013 %>% 
  filter(GrupoCID10 == "Microcefalia") %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>%
  count() %>% 
  rename(code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))


#NASCIDOS VIVOS COM ANOMALIAS POR MACRORREGIAO DE RESIDENCIA
#dados contagem de NASCIDOS VIVOS COM ANOMALIAS por macrorregiao de residencia 2013 
microce.macro.resid2013 <- anom.grupos2013 %>% 
  filter(GrupoCID10 == "Microcefalia") %>% 
  group_by(CodigoMacrorregiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMacrorregiaoSaude) %>% 
  count() %>%    
  rename(code_health_marcroregion = CodigoMacrorregiaoSaude) %>%
  mutate(code_health_marcroregion = as.character(code_health_marcroregion))



#NASCIDOS VIVOS COM ANOMALIAS POR REGIAO DE RESIDENCIA DA MAE
#dados contagem de NASCIDOS VIVOS COM ANOMALIAS por regiao de residencia 2013 
microce.reg.resid2013 <- anom.grupos2013 %>%  
  filter(GrupoCID10 == "Microcefalia") %>% 
  group_by(CodigoRegiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoRegiaoSaude) %>% 
  count() %>%  
  rename(code_health_region = CodigoRegiaoSaude) %>%  
  mutate(code_health_region = as.character(code_health_region)) 



#NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE NASCIMENTO
#dados contagem de NASCIDOS VIVOS COM ANOMALIAS por municipios de nascimento 
microce.muni.nasc2013 <- anom.grupos2013 %>% 
  filter(GrupoCID10 == "Microcefalia") %>%
  group_by(CodigoMunicipioNascimento, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioNascimento) %>%
  count() %>% 
  rename(code_muni = CodigoMunicipioNascimento) %>% 
  mutate(code_muni = as.character(code_muni)) 



#NASCIDOS VIVOS COM ANOMALIAS POR ESTABELECIMENTO
#dados contagem de NASCIDOS VIVOS COM ANOMALIAS por estabelecimento 
microce.estab2013 <- anom.grupos2013 %>% 
  filter(GrupoCID10 == "Microcefalia") %>%
  group_by(CodigoEstabelecimento, Estabelecimento, CodigoMunicipioNascimento, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoEstabelecimento, Estabelecimento, CodigoMunicipioNascimento) %>%
  count() %>% 
  rename(code_cnes = CodigoEstabelecimento) %>% 
  mutate(Estabelecimento = str_to_title(Estabelecimento))



#PREVALENCIA POR MUNICIPIO DE RESIDENCIA DA MAE
#unindo os dados de nascidos vivos e anomalias
prev.microce.muni.resid2013 <- microce.muni.resid2013 %>% 
  left_join(nv.muni.resid2013, microce.muni.resid2013, by = "code_muni")

#calculando prevalencia
epiprev.microce.muni.resid2013 <-epi.prev(prev.microce.muni.resid2013$n, prev.microce.muni.resid2013$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
epiprev.microce.muni.resid2013_2 <- epiprev.microce.muni.resid2013[["ap"]]

#unindo os dados de prevalencia
prev.microce.muni.resid2013 <- prev.microce.muni.resid2013 %>% 
  bind_cols(epiprev.microce.muni.resid2013_2) %>% 
  mutate(est = round(est, 2))



#PREVALENCIA POR MACRORREGIAO DE RESIDENCIA
#unindo os dados de nascidos vivos e anomalias
prev.microce.macro.resid2013  <- microce.macro.resid2013 %>% 
  left_join(nv.macro.resid2013, microce.macro.resid2013, by = "code_health_marcroregion")

#calculando prevalencia
epiprev.microce.macro.resid2013 <-epi.prev(prev.microce.macro.resid2013$n, prev.microce.macro.resid2013$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
epiprev.microce.macro.resid2013_2 <- epiprev.microce.macro.resid2013[["ap"]]

#unindo os dados de prevalencia
prev.microce.macro.resid2013 <- prev.microce.macro.resid2013 %>% 
  bind_cols(epiprev.microce.macro.resid2013_2) %>% 
  mutate(est = round(est, 2))



#PREVALENCIA POR REGIAO DE RESIDENCIA
#unindo os dados de nascidos vivos e anomalias
prev.microce.reg.resid2013 <- microce.reg.resid2013 %>% 
  left_join(nv.reg.resid2013, microce.reg.resid2013, by = "code_health_region")

#calculando prevalencia
epiprev.microce.reg.resid2013 <-epi.prev(prev.microce.reg.resid2013$n, prev.microce.reg.resid2013$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

##criando objeto com prevalencias e IC
epiprev.microce.reg.resid2013_2 <- epiprev.microce.reg.resid2013[["ap"]]

#unindo os dados de prevalencia
prev.microce.reg.resid2013 <- prev.microce.reg.resid2013 %>% 
  bind_cols(epiprev.microce.reg.resid2013_2) %>% 
  mutate(est = round(est, 2))



#PREVALENCIA POR MUNICIPIO DE NASCIMENTO
#unindo os dados de nascidos vivos e anomalias
prev.microce.muni.nasc2013 <- microce.muni.nasc2013 %>% 
  left_join(nv.muni.nasc2013, microce.muni.nasc2013, by = "code_muni") %>% 
  filter(str_detect(code_muni,"42"))

#calculando prevalencia
epiprev.microce.muni.nasc2013 <-epi.prev(prev.microce.muni.nasc2013$n, prev.microce.muni.nasc2013$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

##criando objeto com prevalencias e IC
epiprev.microce.muni.nasc2013_2 <- epiprev.microce.muni.nasc2013[["ap"]]

#unindo os dados de prevalencia
prev.microce.muni.nasc2013 <- prev.microce.muni.nasc2013 %>% 
  bind_cols(epiprev.microce.muni.nasc2013_2) %>% 
  mutate(est = round(est, 2))



#PREVALENCIA POR ESTABELECIMENTO
#unindo os dados de nascidos vivos e anomalias
prev.microce.estab2013 <- microce.estab2013 %>% 
  left_join(nv.estab2013, microce.estab2013, by = "code_cnes") %>% 
  filter(str_detect(CodigoMunicipioNascimento,"42")) %>% 
  filter(code_cnes != is.na(code_cnes)) 

#calculando prevalencia
epiprev.microce.estab2013 <-epi.prev(prev.microce.estab2013$n, prev.microce.estab2013$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

##criando objeto com prevalencias e IC
epiprev.microce.estab2013_2 <- epiprev.microce.estab2013[["ap"]]

#unindo os dados de prevalencia
prev.microce.estab2013 <- prev.microce.estab2013 %>% 
  bind_cols(epiprev.microce.estab2013_2) %>% 
  mutate(est = round(est, 2))




######### MAPAS

## ---- mapa_microce.muni.resid2013 --------
##MAPA ANOMALIAS SELECIONADAS POR MUNICIPIO DE RESIDENCIA
# unindo os dados com a base de mapa
dados_microce.muni.resid2013 <- dplyr::left_join(geoMunicipiosSC, microce.muni.resid2013, by = "code_muni")


#plotando o mapa
mapa_microce.muni.resid2013 <- leaflet(data = dados_microce.muni.resid2013) 

label_microce.muni.resid2013 <- dados_microce.muni.resid2013 %>%
  mutate (n = replace_na(n, 0))

colormagma_microce.muni.resid2013 = colorNumeric("magma", domain = dados_microce.muni.resid2013$n, alpha = TRUE, reverse =  TRUE)

mapa_microce.muni.resid2013 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_microce.muni.resid2013(n),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               Nascidos vivos com anomalias: </strong>%s", 
                               label_microce.muni.resid2013$name_muni, label_microce.muni.resid2013$n) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_microce.muni.resid2013, values = ~n, na.label = "Sem registro", opacity = 1, title = "Nascidos vivos <br> com anomalias")



## ---- mapa_microce.macro.resid2013 --------
##MAPA ANOMALIAS SELECIONADAS POR MACRORREGIAO DE RESIDENCIA
# unindo os dados com a base de mapa
dados_microce.macro.resid2013 <- dplyr::left_join(geoMacrorregioesSC, microce.macro.resid2013, by = "code_health_marcroregion")


#plotando o mapa
mapa_microce.macro.resid2013 <- leaflet(data = dados_microce.macro.resid2013) 

label_microce.macro.resid2013 <- dados_microce.macro.resid2013 %>%
  mutate (n = replace_na(n, 0)) %>% 
  mutate(name_health_macroregion = str_to_title(name_health_macroregion))

label_microce.macro.resid2013["3", "name_health_macroregion"] <- "Meio Oeste E Serra Catarinense"

colormagma_microce.macro.resid2013  = colorNumeric("magma", domain = dados_microce.macro.resid2013$n, alpha = TRUE, reverse =  TRUE)

mapa_microce.macro.resid2013 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_microce.macro.resid2013(n),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Macrorregião de saúde: </strong>%s<br/><strong>
                               Nascidos vivos com anomalias: </strong>%s", 
                               label_microce.macro.resid2013$name_health_macroregion, label_microce.macro.resid2013$n) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_microce.macro.resid2013, values = ~n, na.label = "Sem registro", opacity = 1, title = "Nascidos vivos <br> com anomalias")


## ---- mapa_microce.reg.resid2013 --------
##MAPA ANOMALIAS SELECIONADAS POR REGIAO DE RESIDENCIA
# unindo os dados com a base de mapa
dados_microce.reg.resid2013 <- dplyr::left_join(geoRegioesSC, microce.reg.resid2013, by = "code_health_region")


#plotando o mapa
mapa_microce.reg.resid2013 <- leaflet(data = dados_microce.reg.resid2013) 

label_microce.reg.resid2013 <- dados_microce.reg.resid2013 %>%
  mutate (n = replace_na(n, 0)) 

colormagma_microce.reg.resid2013 = colorNumeric("magma", domain = dados_microce.reg.resid2013$n, alpha = TRUE, reverse =  TRUE)

mapa_microce.reg.resid2013 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_microce.reg.resid2013(n),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Região de saúde: </strong>%s<br/><strong>
                               Nascidos vivos com anomalias: </strong>%s", 
                               label_microce.reg.resid2013$name_health_region, label_microce.reg.resid2013$n) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_microce.reg.resid2013, values = ~n, na.label = "Sem registro", opacity = 1, title = "Nascidos vivos <br> com anomalias")


## ---- mapa_microce.muni.nasc2013 --------
##MAPA ANOMALIAS SELECIONADAS POR MUNICIPIO DE NASCIMENTO SC
# unindo os dados com a base de mapa
dados_microce.muni.nasc2013 <- dplyr::left_join(geoMunicipiosSC, microce.muni.nasc2013, by = "code_muni")


#plotando o mapa
mapa_microce.muni.nasc2013 <- leaflet(data = dados_microce.muni.nasc2013) 

label_microce.muni.nasc2013 <- dados_microce.muni.nasc2013 %>%
  mutate (n = replace_na(n, 0))

colormagma_microce.muni.nasc2013 = colorNumeric("magma", domain = dados_microce.muni.nasc2013$n, alpha = TRUE, reverse =  TRUE)

mapa_microce.muni.nasc2013 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_microce.muni.nasc2013(n),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de Nascimento: </strong>%s<br/><strong>
                               Nascidos vivos com anomalias: </strong>%s", 
                               label_microce.muni.nasc2013$name_muni, label_microce.muni.nasc2013$n) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_microce.muni.nasc2013, values = ~n, na.label = "Sem registro", opacity = 1, title = "Nascidos vivos <br> com anomalias")


## ---- mapa_microce.estab2013 --------
### MAPA ANOMALIAS SELECIONADAS POR ESTABELECIMENTO
## feito em http://dwilhelm89.github.io/Leaflet.StyleEditor/ -- redimensionar antes de usar
#criando lista de marcadores - so funcionou nessa ordem
marcadores_personalizados <- iconList(
  verde = makeIcon("/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador verde.png"),
  laranja = makeIcon("/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador laranja.png"),
  vermelho = makeIcon("/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador vermelho.png"))

#laranja antes do verde

# unindo os dados com a base de mapa
dados_microce.estab2013 <- dplyr::left_join(geoEstabeleTodos, microce.estab2013, by = "code_cnes")

#filtrando por estabelecimentos com anomalias
dados_microce.estab2013 <- dados_microce.estab2013 %>% 
  filter(n != is.na(n))

#criando coluna marcadores
dados_microce.estab2013<- dados_microce.estab2013 %>%
  mutate(Marcador = case_when(
    n <2 ~ "verde"))

#n <5 ~ "laranja",    
#n >4 ~ "vermelho"))

#convertendo marcadores como factor
dados_microce.estab2013 <- dados_microce.estab2013 %>%
  mutate(Marcador=as.factor(Marcador))

#organizando nivel marcadores
#levels(dados_microce.estab2013$Marcador) <- c("verde", "laranja", "vermelho")


#plotando o mapa  
label_microce.estab2013 <- sprintf("<p><strong>Estabelecimento: </strong>%s<br/>
                 <strong>Nascidos vivos com anomalias: </strong>%s", dados_microce.estab2013$Estabelecimento,
                                   dados_microce.estab2013$n) %>% lapply(htmltools::HTML)


mapa_microce.estab2013 <- leaflet(dados_microce.estab2013) %>% addTiles() 
mapa_microce.estab2013 %>% 
  addMarkers(icon = ~marcadores_personalizados[as.numeric(Marcador)],
             label = label_microce.estab2013,  
             labelOptions = labelOptions(textsize = "14px", opacity = 0.7, direction = "top"),
             options = markerOptions(riseOnHover = TRUE)) %>% 
  addLegendImage(mapa_microce.estab2013, images = c("/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador verde.png"),
                 #"/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador laranja.png",
                 #"/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador vermelho.png"),
                 labels = '1', width = 20, height = 31,
                 title = htmltools::tags$div('NV c/ AC por estabelecimento', style = 'font-size: 15px; text-align: center;'),
                 labelStyle = "font-size: 15px; vertical-align: middle;",
                 orientation = 'vertical',
                 position = 'bottomleft')

#width = c(20, 25, 28), height = c(31, 34, 39)


### PREVALENCIAS


## ---- mapa_prev.microce.muni.resid2013 --------
### MAPA PREVALENCIA POR MUNICIPIOS DE RESIDENCIA DA MAE
# unindo os dados com a base de mapa
dados_prev.microce.muni.resid2013 <- dplyr::left_join(geoMunicipiosSC, prev.microce.muni.resid2013, by = "code_muni") 

#modificando decimal no label e susbtituindo NAs
label_prev.microce.muni.resid2013 <- dados_prev.microce.muni.resid2013 %>%
  mutate (est = replace_na(est, 0.00)) %>%
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ","))

#plotando o mapa
mapa_prev.microce.muni.resid2013  <- leaflet(data = dados_prev.microce.muni.resid2013) 

colormagma_prev.microce.muni.resid2013 = colorNumeric("magma", domain = dados_prev.microce.muni.resid2013$est, alpha = TRUE, reverse =  TRUE)

mapa_prev.microce.muni.resid2013 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_prev.microce.muni.resid2013(est),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               Prevalência/10.000: </strong>%s", 
                               label_prev.microce.muni.resid2013$name_muni, label_prev.microce.muni.resid2013$est) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_prev.microce.muni.resid2013, values = ~est, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Prevalência/10.000")


## ---- mapa_prev.microce.macro.resid2013 --------
### MAPA PREVALENCIA POR MACRORREGIAO DE RESIDENCIA DA MAE
# unindo os dados com a base de mapa
dados_prev.microce.macro.resid2013 <- dplyr::left_join(geoMacrorregioesSC, prev.microce.macro.resid2013, by = "code_health_marcroregion") 

#modificando decimal no label e susbtituindo NAs
label_prev.microce.macro.resid2013 <- dados_prev.microce.macro.resid2013 %>%
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate(name_health_macroregion = str_to_title(name_health_macroregion))

label_prev.microce.macro.resid2013["3", "name_health_macroregion"] <- "Meio Oeste E Serra Catarinense"

#plotando o mapa
mapa_prev.microce.macro.resid2013  <- leaflet(data = dados_prev.microce.macro.resid2013) 

colormagma_prev.microce.macro.resid2013 = colorNumeric("magma", domain = dados_prev.microce.macro.resid2013$est, alpha = TRUE, reverse =  TRUE)

mapa_prev.microce.macro.resid2013 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_prev.microce.macro.resid2013(est),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Macrorregião de saúde: </strong>%s<br/>
               <strong>Prevalência/10.000: </strong>%s", label_prev.microce.macro.resid2013$name_health_macroregion, label_prev.microce.macro.resid2013$est) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_prev.microce.macro.resid2013, values = ~est, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Prevalência/10.000")


## ---- mapa_prev.microce.reg.resid2013 --------
### MAPA PREVALENCIA POR REGIAO DE RESIDENCIA DA MAE
# unindo os dados com a base de mapa
dados_prev.microce.reg.resid2013 <- dplyr::left_join(geoRegioesSC, prev.microce.reg.resid2013, by = "code_health_region") 

#modificando decimal no label e susbtituindo NAs
label_prev.microce.reg.resid2013 <- dados_prev.microce.reg.resid2013 %>%
  mutate (est = replace_na(est, 0.00)) %>%
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ","))


#plotando o mapa
mapa_prev.microce.reg.resid2013  <- leaflet(data = dados_prev.microce.reg.resid2013) 

colormagma_prev.microce.reg.resid2013 = colorNumeric("magma", domain = dados_prev.microce.reg.resid2013$est, alpha = TRUE, reverse =  TRUE)

mapa_prev.microce.reg.resid2013 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_prev.microce.reg.resid2013(est),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Região de saúde: </strong>%s<br/><strong>
                               Prevalência/10.000: </strong>%s", 
                               label_prev.microce.reg.resid2013$name_health_region, label_prev.microce.reg.resid2013$est) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_prev.microce.reg.resid2013, values = ~est, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Prevalência/10.000")



## ---- mapa_prev.microce.muni.nasc2013 --------
### MAPA PREVALENCIA POR MUNICIPIOS DE NASCIMENTO
# unindo os dados com a base de mapa
dados_prev.microce.muni.nasc2013 <- dplyr::left_join(geoMunicipiosSC, prev.microce.muni.nasc2013, by = "code_muni") 

#substituindo NAs no label
label_prev.microce.muni.nasc2013 <- dados_prev.microce.muni.nasc2013 %>%
  mutate (est = replace_na(est, 0.00))

#modificando decimal no label 
label_prev.microce.muni.nasc2013 <- label_prev.microce.muni.nasc2013  %>%
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ","))


#plotando o mapa
mapa_prev.microce.muni.nasc2013  <- leaflet(data = dados_prev.microce.muni.nasc2013) 

colormagma_prev.microce.muni.nasc2013 = colorNumeric("magma", domain = dados_prev.microce.muni.nasc2013$est, alpha = TRUE, reverse =  TRUE)

mapa_prev.microce.muni.nasc2013 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_prev.microce.muni.nasc2013(est),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de nascimento: </strong>%s<br/><strong>
                               Prevalência/10.000: </strong>%s", 
                               label_prev.microce.muni.nasc2013$name_muni, label_prev.microce.muni.nasc2013$est) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_prev.microce.muni.nasc2013, values = ~est, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Prevalência/10.000")


## ---- mapa_prev.microce.estab2013 --------
### MAPA PREVALENCIAS POR ESTABELECIMENTO
## feito em http://dwilhelm89.github.io/Leaflet.StyleEditor/ -- redimensionar antes de usar
#criando lista de marcadores - so funcionou nessa ordem
marcadores_personalizados <- iconList(
  laranja = makeIcon("/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador laranja.png"),
  verde = makeIcon("/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador verde.png"),
  vermelho = makeIcon("/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador vermelho.png"))


# unindo os dados com a base de mapa
dados_prev.microce.estab2013 <- dplyr::left_join(geoEstabeleTodos, prev.microce.estab2013, by = "code_cnes")

#filtrando por estabelecimentos com anomalias em santa catarina
dados_prev.microce.estab2013 <- dados_prev.microce.estab2013 %>% 
  filter(abbrev_state == "SC") %>% 
  filter(n != is.na(n))


#criando coluna marcadores
dados_prev.microce.estab2013 <- dados_prev.microce.estab2013 %>%
  mutate(Marcador = case_when(
    est <3 ~ "verde",
    est <6 ~ "laranja",
    est >5 ~ "vermelho"))

#convertendo marcadores como factor
dados_prev.microce.estab2013 <- dados_prev.microce.estab2013 %>%
  mutate(Marcador=as.factor(Marcador))

#organizando nivel marcadores
levels(dados_prev.microce.estab2013$Marcador) <- c("verde", "laranja", "vermelho")


#criando label e modificando decimal
label_prev.microce.estab2013 <- dados_prev.microce.estab2013  %>%
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ","))


label_prev.microce.estab2013 <- sprintf("<p><strong>Estabelecimento: </strong>%s<br/>
                 <strong>Prevalência/10.000: </strong>%s", label_prev.microce.estab2013$Estabelecimento,
                                        label_prev.microce.estab2013$est) %>% lapply(htmltools::HTML)

#plotando o mapa
mapa_prev.microce.estab2013 <- leaflet(dados_prev.microce.estab2013) %>% addTiles() 
mapa_prev.microce.estab2013 %>% 
  addMarkers(icon = ~marcadores_personalizados[as.numeric(Marcador)],
             label = label_prev.microce.estab2013,  
             labelOptions = labelOptions(textsize = "14px", opacity = 0.7, direction = "top"),
             options = markerOptions(riseOnHover = TRUE)) %>% 
  addLegendImage(mapa_prev.microce.estab2013, images = c("/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador verde.png",
                                                         "/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador laranja.png",
                                                         "/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador vermelho.png"),
                 labels = c('1-2', '3-5', 'Maior ou igual a 6'), width = c(20, 25, 28), height = c(31, 34, 39),
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
  select(Nome_UF, `Código Município Completo`, Nome_Município) %>%
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
  select(CodigoMunicipioNascimento, MunicipioNascimento) %>%
  rename(CodigoMunicipioResidencia = CodigoMunicipioNascimento,
         MunicipioResidencia = MunicipioNascimento) %>%
  add_row(CodigoMunicipioResidencia="420000", MunicipioResidencia="Município Ignorado")



#Importando arquivo macrorregiao
#http://svs.aids.gov.br/dantps/cgiae/sinasc/documentacao/ > Arquivos DEF e CNV das Tabelas DN
macrorregioes <- read_fwf("sc_macsaud.cnv", fwf_empty("sc_macsaud.cnv"))

#	limpando dados macrorregioes
macrorregioes <- macrorregioes %>%
  select(X4, X5, X6) %>%
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
  select(X2, X3, X4) %>%
  rename (CodigoRegiaoSaude = X2,
          RegiaoSaude = X3,
          CodigoMunicipioResidencia = X4) %>%
  slice(2:n()) %>% #remove primeira linha
  slice(1:(n()-2)) %>% #remove ultimas duas linhas
  add_row(CodigoRegiaoSaude = 42000, RegiaoSaude = "Município Ignorado", CodigoMunicipioResidencia = "420000")

regioes["168", "CodigoMunicipioResidencia"] <- "420415"




## ---- tab_prev.microce.muni.resid2013 --------
tab_prev.microce.muni.resid2013 <- prev.microce.muni.resid2013 %>%
  rename(CodigoMunicipioResidencia = code_muni) 

tab_prev.microce.muni.resid2013 <- tab_prev.microce.muni.resid2013 %>% 
  left_join(municipiosresidencia, tab_prev.microce.muni.resid2013, by = "CodigoMunicipioResidencia") %>% 
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (lower = scales::number(lower, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (upper = scales::number(upper, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>%
  unite (col = IC95, lower:upper, sep = "-") %>%
  rename(NascidosVivosACs = n) %>% 
  rename(NascidosVivos = nasc_vivos) %>% 
  rename(Prevalencia10000 = est) %>% 
  relocate(MunicipioResidencia, .after = CodigoMunicipioResidencia)


## ---- tab_prev.microce.macro.resid2013 --------
tab_prev.microce.macro.resid2013 <- prev.microce.macro.resid2013 %>%
  rename(CodigoMacrorregiaoSaude = code_health_marcroregion) %>% 
  mutate(CodigoMacrorregiaoSaude = as.numeric(CodigoMacrorregiaoSaude))

tab_prev.microce.macro.resid2013 <- tab_prev.microce.macro.resid2013 %>% 
  left_join(macrorregioes, tab_prev.microce.macro.resid2013, by = "CodigoMacrorregiaoSaude") %>% 
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


## ---- tab_prev.microce.reg.resid2013 --------
tab_prev.microce.reg.resid2013 <- prev.microce.reg.resid2013 %>%
  rename(CodigoRegiaoSaude = code_health_region) %>% 
  mutate(CodigoRegiaoSaude = as.numeric(CodigoRegiaoSaude)) 

tab_prev.microce.reg.resid2013 <- tab_prev.microce.reg.resid2013 %>% 
  left_join(regioes, tab_prev.microce.reg.resid2013, by = "CodigoRegiaoSaude") %>% 
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


## ---- tab_prev.microce.muni.nasc2013 --------
tab_prev.microce.muni.nasc2013 <- prev.microce.muni.nasc2013 %>%
  rename(CodigoMunicipioNascimento = code_muni) 

tab_prev.microce.muni.nasc2013 <- tab_prev.microce.muni.nasc2013 %>% 
  left_join(municipios, tab_prev.microce.muni.nasc2013, by = "CodigoMunicipioNascimento") %>% 
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (lower = scales::number(lower, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (upper = scales::number(upper, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>%
  unite (col = IC95, lower:upper, sep = "-") %>%
  rename(NascidosVivosACs = n) %>% 
  rename(NascidosVivos = nasc_vivos) %>% 
  rename(Prevalencia10000 = est) %>% 
  relocate(MunicipioNascimento, .after = CodigoMunicipioNascimento) %>% 
  mutate (UFNascimento = NULL)


## ---- tab_prev.microce.estab2013 --------
tab_prev.microce.estab2013 <- prev.microce.estab2013 %>%
  rename(CodigoEstabelecimento = code_cnes) %>% 
  mutate(CodigoEstabelecimento = as.character(CodigoEstabelecimento)) %>%
  mutate(CodigoEstabelecimento = str_pad(CodigoEstabelecimento, width = 7, side = "left", pad = "0"))


tab_prev.microce.estab2013 <- tab_prev.microce.estab2013 %>% 
  left_join(estabelecimentoC, tab_prev.microce.estab2013, by = "CodigoEstabelecimento") %>% 
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


tab_prev.microce.estab2013 <- tab_prev.microce.estab2013 %>%
  mutate(CodigoMunicipioNascimento = as.character(CodigoMunicipioNascimento)) %>% 
  left_join(municipios, tab_prev.microce.estab2013, by = "CodigoMunicipioNascimento") %>% 
  relocate(MunicipioNascimento, .after = CodigoMunicipioNascimento) %>% 
  mutate (UFNascimento = NULL)



