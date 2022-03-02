#ANALISES DEFEITOS DA PAREDE ABDOMINAL 2015


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
  group_by(CodigoEstabelecimento, NumeroIdentificacao) %>%
  count() %>% 
  group_by(CodigoEstabelecimento) %>% 
  count() %>% 
  rename(code_cnes = CodigoEstabelecimento) %>% 
  rename(nasc_vivos = n)


#criando variavel de grupo
anom.grupos2015 <- banco2015 %>% 
  filter(str_detect(CID10Anomalia,"Q000|Q001|Q002|Q01|Q05|Q02|Q20|Q21|Q22|Q23|Q24|Q25|Q26|Q27|Q28|Q35|Q36|Q37|Q54|Q56|Q66|Q69|Q71|Q72|Q73|Q743|Q792|Q793|Q90")) %>% 
  mutate(GrupoCID10 = case_when(
    str_detect(CID10Anomalia,"Q000|Q001|Q002|Q01|Q05") ~ "Defeitos de tubo neural",
    str_detect(CID10Anomalia,"Q02") ~ "Microcefalia",
    str_detect(CID10Anomalia,"Q20|Q21|Q22|Q23|Q24|Q25|Q26|Q27|Q28") ~ "Cardiopatias congênitas",
    str_detect(CID10Anomalia,"Q35|Q36|Q37") ~ "Fendas orais",
    str_detect(CID10Anomalia,"Q54|Q56") ~ "Anomalias de órgãos genitais",
    str_detect(CID10Anomalia,"Q66|Q69|Q71|Q72|Q73|Q743") ~ "Defeitos de membros",
    str_detect(CID10Anomalia,"Q792|Q793") ~ "Defeitos da parede abdominal",
    str_detect(CID10Anomalia,"Q90") ~ "Síndrome de Down"))



#NUMERO NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE RESIDENCIA
#dados contagem de numero NASCIDOS VIVOS COM ANOMALIAS por municipios de residencia 2015 
abdominal.muni.resid2015 <- anom.grupos2015 %>% 
  filter(GrupoCID10 == "Defeitos da parede abdominal") %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>%
  count() %>% 
  rename(code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))


#NUMERO NASCIDOS VIVOS COM ANOMALIAS POR MACRORREGIAO DE RESIDENCIA
#dados contagem de numero NASCIDOS VIVOS COM ANOMALIAS por macrorregiao de residencia 2015 
abdominal.macro.resid2015 <- anom.grupos2015 %>% 
  filter(GrupoCID10 == "Defeitos da parede abdominal") %>%
  group_by(CodigoMacrorregiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMacrorregiaoSaude) %>% 
  count() %>%    
  rename(code_health_marcroregion = CodigoMacrorregiaoSaude) %>%
  mutate(code_health_marcroregion = as.character(code_health_marcroregion))



#NUMERO NASCIDOS VIVOS COM ANOMALIAS POR REGIAO DE RESIDENCIA DA MAE
#dados contagem de numero NASCIDOS VIVOS COM ANOMALIAS por regiao de residencia 2015 
abdominal.reg.resid2015 <- anom.grupos2015 %>%  
  filter(GrupoCID10 == "Defeitos da parede abdominal") %>%
  group_by(CodigoRegiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoRegiaoSaude) %>% 
  count() %>%  
  rename(code_health_region = CodigoRegiaoSaude) %>%  
  mutate(code_health_region = as.character(code_health_region)) 



#NUMERO NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE NASCIMENTO
#dados contagem de numero NASCIDOS VIVOS COM ANOMALIAS por municipios de nascimento 
abdominal.muni.nasc2015 <- anom.grupos2015 %>% 
  filter(GrupoCID10 == "Defeitos da parede abdominal") %>%
  group_by(CodigoMunicipioNascimento, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioNascimento) %>%
  count() %>% 
  rename(code_muni = CodigoMunicipioNascimento) %>% 
  mutate(code_muni = as.character(code_muni)) 



#NUMERO NASCIDOS VIVOS COM ANOMALIAS POR ESTABELECIMENTO
#dados contagem de numero NASCIDOS VIVOS COM ANOMALIAS por estabelecimento 
abdominal.estab2015 <- anom.grupos2015 %>% 
  filter(GrupoCID10 == "Defeitos da parede abdominal") %>%
  group_by(CodigoEstabelecimento, Estabelecimento, CodigoMunicipioNascimento, NumeroIdentificacao) %>%
  count() %>% 
  group_by(CodigoEstabelecimento, Estabelecimento, CodigoMunicipioNascimento) %>%
  count() %>% 
  rename(code_cnes = CodigoEstabelecimento) %>% 
  mutate(Estabelecimento = str_to_title(Estabelecimento))



#PREVALENCIA POR MUNICIPIO DE RESIDENCIA DA MAE
#unindo os dados de nascidos vivos e anomalias
prev.abdominal.muni.resid2015 <- abdominal.muni.resid2015 %>% 
  left_join(nv.muni.resid2015, abdominal.muni.resid2015, by = "code_muni")

#calculando prevalencia
epiprev.abdominal.muni.resid2015 <-epi.prev(prev.abdominal.muni.resid2015$n, prev.abdominal.muni.resid2015$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
epiprev.abdominal.muni.resid2015_2 <- epiprev.abdominal.muni.resid2015[["ap"]]

#unindo os dados de prevalencia
prev.abdominal.muni.resid2015 <- prev.abdominal.muni.resid2015 %>% 
  bind_cols(epiprev.abdominal.muni.resid2015_2) %>% 
  mutate(est = round(est, 2))



#PREVALENCIA POR MACRORREGIAO DE RESIDENCIA
#unindo os dados de nascidos vivos e anomalias
prev.abdominal.macro.resid2015  <- abdominal.macro.resid2015 %>% 
  left_join(nv.macro.resid2015, abdominal.macro.resid2015, by = "code_health_marcroregion")

#calculando prevalencia
epiprev.abdominal.macro.resid2015 <-epi.prev(prev.abdominal.macro.resid2015$n, prev.abdominal.macro.resid2015$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
epiprev.abdominal.macro.resid2015_2 <- epiprev.abdominal.macro.resid2015[["ap"]]

#unindo os dados de prevalencia
prev.abdominal.macro.resid2015 <- prev.abdominal.macro.resid2015 %>% 
  bind_cols(epiprev.abdominal.macro.resid2015_2) %>% 
  mutate(est = round(est, 2))



#PREVALENCIA POR REGIAO DE RESIDENCIA
#unindo os dados de nascidos vivos e anomalias
prev.abdominal.reg.resid2015 <- abdominal.reg.resid2015 %>% 
  left_join(nv.reg.resid2015, abdominal.reg.resid2015, by = "code_health_region")

#calculando prevalencia
epiprev.abdominal.reg.resid2015 <-epi.prev(prev.abdominal.reg.resid2015$n, prev.abdominal.reg.resid2015$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

##criando objeto com prevalencias e IC
epiprev.abdominal.reg.resid2015_2 <- epiprev.abdominal.reg.resid2015[["ap"]]

#unindo os dados de prevalencia
prev.abdominal.reg.resid2015 <- prev.abdominal.reg.resid2015 %>% 
  bind_cols(epiprev.abdominal.reg.resid2015_2) %>% 
  mutate(est = round(est, 2))



#PREVALENCIA POR MUNICIPIO DE NASCIMENTO
#unindo os dados de nascidos vivos e anomalias
prev.abdominal.muni.nasc2015 <- abdominal.muni.nasc2015 %>% 
  left_join(nv.muni.nasc2015, abdominal.muni.nasc2015, by = "code_muni") %>% 
  filter(str_detect(code_muni,"42"))

#calculando prevalencia
epiprev.abdominal.muni.nasc2015 <-epi.prev(prev.abdominal.muni.nasc2015$n, prev.abdominal.muni.nasc2015$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

##criando objeto com prevalencias e IC
epiprev.abdominal.muni.nasc2015_2 <- epiprev.abdominal.muni.nasc2015[["ap"]]

#unindo os dados de prevalencia
prev.abdominal.muni.nasc2015 <- prev.abdominal.muni.nasc2015 %>% 
  bind_cols(epiprev.abdominal.muni.nasc2015_2) %>% 
  mutate(est = round(est, 2))



#PREVALENCIA POR ESTABELECIMENTO
#unindo os dados de nascidos vivos e anomalias
prev.abdominal.estab2015 <- abdominal.estab2015 %>% 
  left_join(nv.estab2015, abdominal.estab2015, by = "code_cnes") %>% 
  filter(str_detect(CodigoMunicipioNascimento,"42")) %>% 
  filter(code_cnes != is.na(code_cnes)) 


#calculando prevalencia
epiprev.abdominal.estab2015 <-epi.prev(prev.abdominal.estab2015$n, prev.abdominal.estab2015$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

##criando objeto com prevalencias e IC
epiprev.abdominal.estab2015_2 <- epiprev.abdominal.estab2015[["ap"]]

#unindo os dados de prevalencia
prev.abdominal.estab2015 <- prev.abdominal.estab2015 %>% 
  bind_cols(epiprev.abdominal.estab2015_2) %>% 
  mutate(est = round(est, 2))




######### MAPAS

## ---- mapa_abdominal.muni.resid2015 --------
##MAPA ANOMALIAS SELECIONADAS POR MUNICIPIO DE RESIDENCIA
# unindo os dados com a base de mapa
dados_abdominal.muni.resid2015 <- dplyr::left_join(geoMunicipiosSC, abdominal.muni.resid2015, by = "code_muni")


#plotando o mapa
mapa_abdominal.muni.resid2015 <- leaflet(data = dados_abdominal.muni.resid2015) 

label_abdominal.muni.resid2015 <- dados_abdominal.muni.resid2015 %>%
  mutate (n = replace_na(n, 0))

colormagma_abdominal.muni.resid2015 = colorNumeric("magma", domain = dados_abdominal.muni.resid2015$n, alpha = TRUE, reverse =  TRUE)

mapa_abdominal.muni.resid2015 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_abdominal.muni.resid2015(n),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               Nascidos vivos com anomalias: </strong>%s", 
                               label_abdominal.muni.resid2015$name_muni, label_abdominal.muni.resid2015$n) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_abdominal.muni.resid2015, values = ~n, na.label = "Sem registro", opacity = 1, title = "Nascidos vivos <br> com anomalias")



## ---- mapa_abdominal.macro.resid2015 --------
##MAPA ANOMALIAS SELECIONADAS POR MACRORREGIAO DE RESIDENCIA
# unindo os dados com a base de mapa
dados_abdominal.macro.resid2015 <- dplyr::left_join(geoMacrorregioesSC, abdominal.macro.resid2015, by = "code_health_marcroregion")


#plotando o mapa
mapa_abdominal.macro.resid2015 <- leaflet(data = dados_abdominal.macro.resid2015) 

label_abdominal.macro.resid2015 <- dados_abdominal.macro.resid2015 %>%
  mutate (n = replace_na(n, 0)) %>% 
  mutate(name_health_macroregion = str_to_title(name_health_macroregion))

label_abdominal.macro.resid2015["3", "name_health_macroregion"] <- "Meio Oeste E Serra Catarinense"

colormagma_abdominal.macro.resid2015  = colorNumeric("magma", domain = dados_abdominal.macro.resid2015$n, alpha = TRUE, reverse =  TRUE)

mapa_abdominal.macro.resid2015 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_abdominal.macro.resid2015(n),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Macrorregião de saúde: </strong>%s<br/><strong>
                               Nascidos vivos com anomalias: </strong>%s", 
                               label_abdominal.macro.resid2015$name_health_macroregion, label_abdominal.macro.resid2015$n) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_abdominal.macro.resid2015, values = ~n, na.label = "Sem registro", opacity = 1, title = "Nascidos vivos <br> com anomalias")


## ---- mapa_abdominal.reg.resid2015 --------
##MAPA ANOMALIAS SELECIONADAS POR REGIAO DE RESIDENCIA
# unindo os dados com a base de mapa
dados_abdominal.reg.resid2015 <- dplyr::left_join(geoRegioesSC, abdominal.reg.resid2015, by = "code_health_region")


#plotando o mapa
mapa_abdominal.reg.resid2015 <- leaflet(data = dados_abdominal.reg.resid2015) 

label_abdominal.reg.resid2015 <- dados_abdominal.reg.resid2015 %>%
  mutate (n = replace_na(n, 0)) 

colormagma_abdominal.reg.resid2015 = colorNumeric("magma", domain = dados_abdominal.reg.resid2015$n, alpha = TRUE, reverse =  TRUE)

mapa_abdominal.reg.resid2015 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_abdominal.reg.resid2015(n),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Região de saúde: </strong>%s<br/><strong>
                               Nascidos vivos com anomalias: </strong>%s", 
                               label_abdominal.reg.resid2015$name_health_region, label_abdominal.reg.resid2015$n) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_abdominal.reg.resid2015, values = ~n, na.label = "Sem registro", opacity = 1, title = "Nascidos vivos <br> com anomalias")


## ---- mapa_abdominal.muni.nasc2015 --------
##MAPA ANOMALIAS SELECIONADAS POR MUNICIPIO DE NASCIMENTO SC
# unindo os dados com a base de mapa
dados_abdominal.muni.nasc2015 <- dplyr::left_join(geoMunicipiosSC, abdominal.muni.nasc2015, by = "code_muni")


#plotando o mapa
mapa_abdominal.muni.nasc2015 <- leaflet(data = dados_abdominal.muni.nasc2015) 

label_abdominal.muni.nasc2015 <- dados_abdominal.muni.nasc2015 %>%
  mutate (n = replace_na(n, 0))

colormagma_abdominal.muni.nasc2015 = colorNumeric("magma", domain = dados_abdominal.muni.nasc2015$n, alpha = TRUE, reverse =  TRUE)

mapa_abdominal.muni.nasc2015 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_abdominal.muni.nasc2015(n),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de Nascimento: </strong>%s<br/><strong>
                               Nascidos vivos com anomalias: </strong>%s", 
                               label_abdominal.muni.nasc2015$name_muni, label_abdominal.muni.nasc2015$n) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_abdominal.muni.nasc2015, values = ~n, na.label = "Sem registro", opacity = 1, title = "Nascidos vivos <br> com anomalias")


## ---- mapa_abdominal.estab2015 --------
### MAPA ANOMALIAS SELECIONADAS POR ESTABELECIMENTO
## feito em http://dwilhelm89.github.io/Leaflet.StyleEditor/ -- redimensionar antes de usar
#criando lista de marcadores - so funcionou nessa ordem
marcadores_personalizados <- iconList(
  laranja = makeIcon("/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador laranja.png"),
  verde = makeIcon("/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador verde.png"),
  vermelho = makeIcon("/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador vermelho.png"))



# unindo os dados com a base de mapa
dados_abdominal.estab2015 <- dplyr::left_join(geoEstabeleTodos, abdominal.estab2015, by = "code_cnes")

#filtrando por estabelecimentos com anomalias
dados_abdominal.estab2015 <- dados_abdominal.estab2015 %>% 
  filter(n != is.na(n))

#criando coluna marcadores
dados_abdominal.estab2015<- dados_abdominal.estab2015 %>%
  mutate(Marcador = case_when(
    n <2 ~ "verde",
    n <5 ~ "laranja",
    n >4 ~ "vermelho"))

#convertendo marcadores como factor
dados_abdominal.estab2015 <- dados_abdominal.estab2015 %>%
  mutate(Marcador=as.factor(Marcador))

#organizando nivel marcadores
levels(dados_abdominal.estab2015$Marcador) <- c("verde", "laranja", "vermelho")


#plotando o mapa  
label_abdominal.estab2015 <- sprintf("<p><strong>Estabelecimento: </strong>%s<br/>
                 <strong>Nascidos vivos com anomalias: </strong>%s", dados_abdominal.estab2015$Estabelecimento,
                                     dados_abdominal.estab2015$n) %>% lapply(htmltools::HTML)


mapa_abdominal.estab2015 <- leaflet(dados_abdominal.estab2015) %>% addTiles() 
mapa_abdominal.estab2015 %>% 
  addMarkers(icon = ~marcadores_personalizados[as.numeric(Marcador)],
             label = label_abdominal.estab2015,  
             labelOptions = labelOptions(textsize = "14px", opacity = 0.7, direction = "top"),
             options = markerOptions(riseOnHover = TRUE)) %>% 
  addLegendImage(mapa_abdominal.estab2015, images = c("/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador verde.png",
                                                      "/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador laranja.png",
                                                      "/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador vermelho.png"),
                 labels = c('1', '2-4', 'Maior ou igual a 5'), width = c(20, 25, 28), height = c(31, 34, 39),
                 title = htmltools::tags$div('NV c/ AC por estabelecimento', style = 'font-size: 15px; text-align: center;'),
                 labelStyle = "font-size: 15px; vertical-align: middle;",
                 orientation = 'vertical',
                 position = 'bottomleft')



### PREVALENCIAS


## ---- mapa_prev.abdominal.muni.resid2015 --------
### MAPA PREVALENCIA POR MUNICIPIOS DE RESIDENCIA DA MAE
# unindo os dados com a base de mapa
dados_prev.abdominal.muni.resid2015 <- dplyr::left_join(geoMunicipiosSC, prev.abdominal.muni.resid2015, by = "code_muni") 

#modificando decimal no label e susbtituindo NAs
label_prev.abdominal.muni.resid2015 <- dados_prev.abdominal.muni.resid2015 %>%
  mutate (est = replace_na(est, 0.00)) %>%
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ","))

#plotando o mapa
mapa_prev.abdominal.muni.resid2015  <- leaflet(data = dados_prev.abdominal.muni.resid2015) 

colormagma_prev.abdominal.muni.resid2015 = colorNumeric("magma", domain = dados_prev.abdominal.muni.resid2015$est, alpha = TRUE, reverse =  TRUE)

mapa_prev.abdominal.muni.resid2015 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_prev.abdominal.muni.resid2015(est),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               Prevalência/10.000: </strong>%s", 
                               label_prev.abdominal.muni.resid2015$name_muni, label_prev.abdominal.muni.resid2015$est) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_prev.abdominal.muni.resid2015, values = ~est, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Prevalência/10.000")


## ---- mapa_prev.abdominal.macro.resid2015 --------
### MAPA PREVALENCIA POR MACRORREGIAO DE RESIDENCIA DA MAE
# unindo os dados com a base de mapa
dados_prev.abdominal.macro.resid2015 <- dplyr::left_join(geoMacrorregioesSC, prev.abdominal.macro.resid2015, by = "code_health_marcroregion") 

#modificando decimal no label e susbtituindo NAs
label_prev.abdominal.macro.resid2015 <- dados_prev.abdominal.macro.resid2015 %>%
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate(name_health_macroregion = str_to_title(name_health_macroregion))

label_prev.abdominal.macro.resid2015["3", "name_health_macroregion"] <- "Meio Oeste E Serra Catarinense"

#plotando o mapa
mapa_prev.abdominal.macro.resid2015  <- leaflet(data = dados_prev.abdominal.macro.resid2015) 

colormagma_prev.abdominal.macro.resid2015 = colorNumeric("magma", domain = dados_prev.abdominal.macro.resid2015$est, alpha = TRUE, reverse =  TRUE)

mapa_prev.abdominal.macro.resid2015 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_prev.abdominal.macro.resid2015(est),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Macrorregião de saúde: </strong>%s<br/>
               <strong>Prevalência/10.000: </strong>%s", label_prev.abdominal.macro.resid2015$name_health_macroregion, label_prev.abdominal.macro.resid2015$est) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_prev.abdominal.macro.resid2015, values = ~est, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Prevalência/10.000")



## ---- mapa_prev.abdominal.reg.resid2015 --------
### MAPA PREVALENCIA POR REGIAO DE RESIDENCIA DA MAE
# unindo os dados com a base de mapa
dados_prev.abdominal.reg.resid2015 <- dplyr::left_join(geoRegioesSC, prev.abdominal.reg.resid2015, by = "code_health_region") 

#modificando decimal no label e susbtituindo NAs
label_prev.abdominal.reg.resid2015 <- dados_prev.abdominal.reg.resid2015 %>%
  mutate (est = replace_na(est, 0.00)) %>%
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ","))


#plotando o mapa
mapa_prev.abdominal.reg.resid2015  <- leaflet(data = dados_prev.abdominal.reg.resid2015) 

colormagma_prev.abdominal.reg.resid2015 = colorNumeric("magma", domain = dados_prev.abdominal.reg.resid2015$est, alpha = TRUE, reverse =  TRUE)

mapa_prev.abdominal.reg.resid2015 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_prev.abdominal.reg.resid2015(est),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Região de saúde: </strong>%s<br/><strong>
                               Prevalência/10.000: </strong>%s", 
                               label_prev.abdominal.reg.resid2015$name_health_region, label_prev.abdominal.reg.resid2015$est) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_prev.abdominal.reg.resid2015, values = ~est, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Prevalência/10.000")



## ---- mapa_prev.abdominal.muni.nasc2015 --------
### MAPA PREVALENCIA POR MUNICIPIOS DE NASCIMENTO
# unindo os dados com a base de mapa
dados_prev.abdominal.muni.nasc2015 <- dplyr::left_join(geoMunicipiosSC, prev.abdominal.muni.nasc2015, by = "code_muni") 

#substituindo NAs no label
label_prev.abdominal.muni.nasc2015 <- dados_prev.abdominal.muni.nasc2015 %>%
  mutate (est = replace_na(est, 0.00))

#modificando decimal no label 
label_prev.abdominal.muni.nasc2015 <- label_prev.abdominal.muni.nasc2015  %>%
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ","))


#plotando o mapa
mapa_prev.abdominal.muni.nasc2015  <- leaflet(data = dados_prev.abdominal.muni.nasc2015) 

colormagma_prev.abdominal.muni.nasc2015 = colorNumeric("magma", domain = dados_prev.abdominal.muni.nasc2015$est, alpha = TRUE, reverse =  TRUE)

mapa_prev.abdominal.muni.nasc2015 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_prev.abdominal.muni.nasc2015(est),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de nascimento: </strong>%s<br/><strong>
                               Prevalência/10.000: </strong>%s", 
                               label_prev.abdominal.muni.nasc2015$name_muni, label_prev.abdominal.muni.nasc2015$est) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_prev.abdominal.muni.nasc2015, values = ~est, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Prevalência/10.000")


## ---- mapa_prev.abdominal.estab2015 --------
### MAPA PREVALENCIAS POR ESTABELECIMENTO
#icones dos marcadores criados anteriormente
# unindo os dados com a base de mapa
dados_prev.abdominal.estab2015 <- dplyr::left_join(geoEstabeleTodos, prev.abdominal.estab2015, by = "code_cnes")

#filtrando por estabelecimentos com anomalias em santa catarina
dados_prev.abdominal.estab2015 <- dados_prev.abdominal.estab2015 %>% 
  filter(abbrev_state == "SC") %>% 
  filter(n != is.na(n))


#criando coluna marcadores
dados_prev.abdominal.estab2015 <- dados_prev.abdominal.estab2015 %>%
  mutate(Marcador = case_when(
    est <11 ~ "verde",
    est <51 ~ "laranja",
    est >50 ~ "vermelho"))

#convertendo marcadores como factor
dados_prev.abdominal.estab2015 <- dados_prev.abdominal.estab2015 %>%
  mutate(Marcador=as.factor(Marcador))

#organizando nivel marcadores
levels(dados_prev.abdominal.estab2015$Marcador) <- c("verde", "laranja", "vermelho")


#criando label e modificando decimal
label_prev.abdominal.estab2015 <- dados_prev.abdominal.estab2015  %>%
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ","))


label_prev.abdominal.estab2015 <- sprintf("<p><strong>Estabelecimento: </strong>%s<br/>
                 <strong>Prevalência/10.000: </strong>%s", label_prev.abdominal.estab2015$Estabelecimento,
                                          label_prev.abdominal.estab2015$est) %>% lapply(htmltools::HTML)

#plotando o mapa
mapa_prev.abdominal.estab2015 <- leaflet(dados_prev.abdominal.estab2015) %>% addTiles() 
mapa_prev.abdominal.estab2015 %>% 
  addMarkers(icon = ~marcadores_personalizados[as.numeric(Marcador)],
             label = label_prev.abdominal.estab2015,  
             labelOptions = labelOptions(textsize = "14px", opacity = 0.7, direction = "top"),
             options = markerOptions(riseOnHover = TRUE)) %>% 
  addLegendImage(mapa_prev.abdominal.estab2015, images = c("/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador verde.png",
                                                           "/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador laranja.png",
                                                           "/Users/Laysa/Google Drive/Doutorado PPGBM/Análises do banco/Marcador vermelho.png"),
                 labels = c('1-10', '10-50', 'Maior ou igual a 51'), width = c(20, 25, 28), height = c(31, 34, 39),
                 title = htmltools::tags$div('Prevalência/10.000', style = 'font-size: 15px; text-align: center;'),
                 labelStyle = "font-size: 15px; vertical-align: middle;",
                 orientation = 'vertical',
                 position = 'bottomleft')



## TABELAS


## ---- outros_arquivos --------

# abrindo arquivo estabelecimento e transformando em objeto
# http://svs.aids.gov.br/dantps/cgiae/sinasc/documentacao/ > Arquivos DEF e CNV das Tabelas DN 
library(readr)
estabel1 <- read_fwf("ESTABEL.cnv", fwf_empty("ESTABEL.cnv"),locale = locale(encoding = "ISO8859-1"))

# limpando arquivo estabelecimento
estabel1 <- estabel1 %>%
  mutate(X1 = NULL) %>% #remove primeira coluna
  slice(3:n()) %>% #remove duas primeiras linhas
  rename (Estabelecimento = X2,
          CodigoEstabelecimento = X3)


# abrindo arquivo estabelecimento e transformando em objeto
library(foreign)
estabel2 <- read.dbf("CNESDN18.dbf")

# limpando arquivo estabelecimento
estabel2 <- estabel2 %>%
  rename (Estabelecimento = DESCESTAB,
          CodigoEstabelecimento = CODESTAB)


#	abrindo arquivo estabelecimento e transformando em objeto
# http://cnes.datasus.gov.br/pages/downloads/arquivosBaseDados.jsp > Base_de_dados_cnes_202111.zip
estabel3 <- read_csv2("tbEstabelecimento202111.csv", locale = locale(encoding = "ISO8859-1"))

#	limpando arquivo estabelecimento
estabel3 <- estabel3 %>%
  select(CO_CNES, NO_FANTASIA) %>% 
  rename (Estabelecimento = NO_FANTASIA,
          CodigoEstabelecimento = CO_CNES)


# unindo os tres arquivos estabelecimentos
estabelecimentoA <- union(estabel3, estabel2)
estabelecimentoB <- union(estabelecimentoA, estabel1)

# removendo duplicatas
estabelecimentoC <- estabelecimentoB[order(estabelecimentoB$CodigoEstabelecimento,
                                           estabelecimentoB$Estabelecimento, decreasing=TRUE),]

estabelecimentoC <- estabelecimentoC[!duplicated(estabelecimentoC$CodigoEstabelecimento),
                                     fromLast = FALSE]



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




## ---- tab_prev.abdominal.muni.resid2015 --------
tab_prev.abdominal.muni.resid2015 <- prev.abdominal.muni.resid2015 %>%
  rename(CodigoMunicipioResidencia = code_muni) 

tab_prev.abdominal.muni.resid2015 <- tab_prev.abdominal.muni.resid2015 %>% 
  left_join(municipiosresidencia, tab_prev.abdominal.muni.resid2015, by = "CodigoMunicipioResidencia") %>% 
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (lower = scales::number(lower, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (upper = scales::number(upper, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>%
  unite (col = IC95, lower:upper, sep = "-") %>%
  rename(NascidosVivosACs = n) %>% 
  rename(NascidosVivos = nasc_vivos) %>% 
  rename(Prevalencia10000 = est) %>% 
  relocate(MunicipioResidencia, .after = CodigoMunicipioResidencia)


## ---- tab_prev.abdominal.macro.resid2015 --------
tab_prev.abdominal.macro.resid2015 <- prev.abdominal.macro.resid2015 %>%
  rename(CodigoMacrorregiaoSaude = code_health_marcroregion) %>% 
  mutate(CodigoMacrorregiaoSaude = as.numeric(CodigoMacrorregiaoSaude))

tab_prev.abdominal.macro.resid2015 <- tab_prev.abdominal.macro.resid2015 %>% 
  left_join(macrorregioes, tab_prev.abdominal.macro.resid2015, by = "CodigoMacrorregiaoSaude") %>% 
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


## ---- tab_prev.abdominal.reg.resid2015 --------
tab_prev.abdominal.reg.resid2015 <- prev.abdominal.reg.resid2015 %>%
  rename(CodigoRegiaoSaude = code_health_region) %>% 
  mutate(CodigoRegiaoSaude = as.numeric(CodigoRegiaoSaude)) 

tab_prev.abdominal.reg.resid2015 <- tab_prev.abdominal.reg.resid2015 %>% 
  left_join(regioes, tab_prev.abdominal.reg.resid2015, by = "CodigoRegiaoSaude") %>% 
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


## ---- tab_prev.abdominal.muni.nasc2015 --------
tab_prev.abdominal.muni.nasc2015 <- prev.abdominal.muni.nasc2015 %>%
  rename(CodigoMunicipioNascimento = code_muni) 

tab_prev.abdominal.muni.nasc2015 <- tab_prev.abdominal.muni.nasc2015 %>% 
  left_join(municipios, tab_prev.abdominal.muni.nasc2015, by = "CodigoMunicipioNascimento") %>% 
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (lower = scales::number(lower, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (upper = scales::number(upper, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>%
  unite (col = IC95, lower:upper, sep = "-") %>%
  rename(NascidosVivosACs = n) %>% 
  rename(NascidosVivos = nasc_vivos) %>% 
  rename(Prevalencia10000 = est) %>% 
  relocate(MunicipioNascimento, .after = CodigoMunicipioNascimento) %>% 
  mutate (UFNascimento = NULL)


## ---- tab_prev.abdominal.estab2015 --------
tab_prev.abdominal.estab2015 <- prev.abdominal.estab2015 %>%
  rename(CodigoEstabelecimento = code_cnes) %>% 
  mutate(CodigoEstabelecimento = as.character(CodigoEstabelecimento)) %>%
  mutate(CodigoEstabelecimento = str_pad(CodigoEstabelecimento, width = 7, side = "left", pad = "0"))


tab_prev.abdominal.estab2015 <- tab_prev.abdominal.estab2015 %>% 
  left_join(estabelecimentoC, tab_prev.abdominal.estab2015, by = "CodigoEstabelecimento") %>% 
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


tab_prev.abdominal.estab2015 <- tab_prev.abdominal.estab2015 %>%
  mutate(CodigoMunicipioNascimento = as.character(CodigoMunicipioNascimento)) %>% 
  left_join(municipios, tab_prev.abdominal.estab2015, by = "CodigoMunicipioNascimento") %>% 
  relocate(MunicipioNascimento, .after = CodigoMunicipioNascimento) %>% 
  mutate (UFNascimento = NULL)


