#ANALISES MICROCEFALIA PERIODO 2011-2019


## ---- bibliotecap1119 --------
library(leaflet)
library(geobr)
library(ggplot2)
library(sf)
library(tidyverse)
library(viridis)
library(leaflegend)
library(epiR)
library(scales) 


## ---- arquivos_mapasp1119 --------
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



## ---- analisesp1119 --------

bancop1119 <- read_csv2("banco_limpeza.csv", locale = locale(encoding = "ISO8859-1"))

bancop1119 <- bancop1119 %>% 
  filter(AnoNascimento != 2020)



##### ANALISES

###NASCIDOS VIVOS POR MUNICIPIO DE RESIDENCIA
#criando dados de nascidos vivos
nv.muni.residp1119 <- bancop1119 %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>% 
  group_by(CodigoMunicipioResidencia) %>% 
  count() %>% 
  rename(code_muni = CodigoMunicipioResidencia) %>% 
  rename(nasc_vivos = n) %>% 
  mutate(code_muni = as.character(code_muni))



#NASCIDOS VIVOS POOR MACROREGIAO DE RESIDENCIA
#criando dados de nascidos vivos
nv.macro.residp1119 <- bancop1119 %>%
  group_by(CodigoMacrorregiaoSaude, NumeroIdentificacao) %>%
  count() %>% 
  group_by(CodigoMacrorregiaoSaude) %>% 
  count() %>% 
  rename(code_health_marcroregion = CodigoMacrorregiaoSaude) %>% 
  rename(nasc_vivos = n) %>% 
  mutate(code_health_marcroregion = as.character(code_health_marcroregion))



#NASCIDOS VIVOS POR REGIAO DE RESIDENCIA
#criando dados de nascidos vivos
nv.reg.residp1119 <- bancop1119 %>%
  group_by(CodigoRegiaoSaude, NumeroIdentificacao) %>%
  count() %>% 
  group_by(CodigoRegiaoSaude) %>% 
  count() %>% 
  rename(code_health_region = CodigoRegiaoSaude) %>% 
  rename(nasc_vivos = n) %>% 
  mutate(code_health_region = as.character(code_health_region))



#####NASCIDOS VIVOS POR MUNIC??PIO DE NASCIMENTO
#criando dados de nascidos vivos
nv.muni.nascp1119 <- bancop1119 %>%
  group_by(CodigoMunicipioNascimento, NumeroIdentificacao) %>%
  count() %>% 
  group_by(CodigoMunicipioNascimento) %>% 
  count() %>% 
  rename(code_muni = CodigoMunicipioNascimento) %>% 
  rename(nasc_vivos = n) %>% 
  mutate(code_muni = as.character(code_muni))



#NASCIDOS VIVOS POR ESTABELECIMENTO
#criando dados de nascidos vivos
nv.estabp1119 <- bancop1119 %>%
  group_by(CodigoEstabelecimento, NumeroIdentificacao) %>%
  count() %>% 
  group_by(CodigoEstabelecimento) %>% 
  count() %>% 
  rename(code_cnes = CodigoEstabelecimento) %>% 
  rename(nasc_vivos = n)


#criando variavel de grupo
anom.gruposp1119 <- bancop1119 %>% 
  filter(str_detect(CID10Anomalia,"Q000|Q001|Q002|Q01|Q05|Q02|Q20|Q21|Q22|Q23|Q24|Q25|Q26|Q27|Q28|Q35|Q36|Q37|Q54|Q56|Q66|Q69|Q71|Q72|Q73|Q743|Q792|Q793|Q90")) %>% 
  mutate(GrupoCID10 = case_when(
    str_detect(CID10Anomalia,"Q000|Q001|Q002|Q01|Q05") ~ "Defeitos de tubo neural",
    str_detect(CID10Anomalia,"Q02") ~ "Microcefalia",
    str_detect(CID10Anomalia,"Q20|Q21|Q22|Q23|Q24|Q25|Q26|Q27|Q28") ~ "Cardiopatias cong??nitas",
    str_detect(CID10Anomalia,"Q35|Q36|Q37") ~ "Fendas orais",
    str_detect(CID10Anomalia,"Q54|Q56") ~ "Anomalias de ??rg??os genitais",
    str_detect(CID10Anomalia,"Q66|Q69|Q71|Q72|Q73|Q743") ~ "Defeitos de membros",
    str_detect(CID10Anomalia,"Q792|Q793") ~ "Defeitos da parede abdominal",
    str_detect(CID10Anomalia,"Q90") ~ "S??ndrome de Down"))



#NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE RESIDENCIA
#dados contagem de NASCIDOS VIVOS COM ANOMALIAS por municipios de residencia p1119 
microce.muni.residp1119 <- anom.gruposp1119 %>% 
  filter(GrupoCID10 == "Microcefalia") %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>%
  count() %>% 
  rename(code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))


#NASCIDOS VIVOS COM ANOMALIAS POR MACRORREGIAO DE RESIDENCIA
#dados contagem de NASCIDOS VIVOS COM ANOMALIAS por macrorregiao de residencia p1119 
microce.macro.residp1119 <- anom.gruposp1119 %>% 
  filter(GrupoCID10 == "Microcefalia") %>% 
  group_by(CodigoMacrorregiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMacrorregiaoSaude) %>% 
  count() %>%    
  rename(code_health_marcroregion = CodigoMacrorregiaoSaude) %>%
  mutate(code_health_marcroregion = as.character(code_health_marcroregion))



#NASCIDOS VIVOS COM ANOMALIAS POR REGIAO DE RESIDENCIA DA MAE
#dados contagem de NASCIDOS VIVOS COM ANOMALIAS por regiao de residencia p1119 
microce.reg.residp1119 <- anom.gruposp1119 %>%  
  filter(GrupoCID10 == "Microcefalia") %>% 
  group_by(CodigoRegiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoRegiaoSaude) %>% 
  count() %>%  
  rename(code_health_region = CodigoRegiaoSaude) %>%  
  mutate(code_health_region = as.character(code_health_region)) 



#NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE NASCIMENTO
#dados contagem de NASCIDOS VIVOS COM ANOMALIAS por municipios de nascimento 
microce.muni.nascp1119 <- anom.gruposp1119 %>% 
  filter(GrupoCID10 == "Microcefalia") %>%
  group_by(CodigoMunicipioNascimento, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioNascimento) %>%
  count() %>% 
  rename(code_muni = CodigoMunicipioNascimento) %>% 
  mutate(code_muni = as.character(code_muni)) 



#NASCIDOS VIVOS COM ANOMALIAS POR ESTABELECIMENTO
#dados contagem de NASCIDOS VIVOS COM ANOMALIAS por estabelecimento 
microce.estabp1119 <- anom.gruposp1119 %>% 
  filter(GrupoCID10 == "Microcefalia") %>%
  group_by(CodigoEstabelecimento, Estabelecimento, CodigoMunicipioNascimento, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoEstabelecimento, Estabelecimento, CodigoMunicipioNascimento) %>%
  count() %>% 
  rename(code_cnes = CodigoEstabelecimento) %>% 
  mutate(Estabelecimento = str_to_title(Estabelecimento))



#PREVALENCIA POR MUNICIPIO DE RESIDENCIA DA MAE
#unindo os dados de nascidos vivos e anomalias
prev.microce.muni.residp1119 <- microce.muni.residp1119 %>% 
  left_join(nv.muni.residp1119, microce.muni.residp1119, by = "code_muni")

#calculando prevalencia
epiprev.microce.muni.residp1119 <-epi.prev(prev.microce.muni.residp1119$n, prev.microce.muni.residp1119$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
epiprev.microce.muni.residp1119_2 <- epiprev.microce.muni.residp1119[["ap"]]

#unindo os dados de prevalencia
prev.microce.muni.residp1119 <- prev.microce.muni.residp1119 %>% 
  bind_cols(epiprev.microce.muni.residp1119_2) %>% 
  mutate(est = round(est, 2))



#PREVALENCIA POR MACRORREGIAO DE RESIDENCIA
#unindo os dados de nascidos vivos e anomalias
prev.microce.macro.residp1119  <- microce.macro.residp1119 %>% 
  left_join(nv.macro.residp1119, microce.macro.residp1119, by = "code_health_marcroregion")

#calculando prevalencia
epiprev.microce.macro.residp1119 <-epi.prev(prev.microce.macro.residp1119$n, prev.microce.macro.residp1119$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
epiprev.microce.macro.residp1119_2 <- epiprev.microce.macro.residp1119[["ap"]]

#unindo os dados de prevalencia
prev.microce.macro.residp1119 <- prev.microce.macro.residp1119 %>% 
  bind_cols(epiprev.microce.macro.residp1119_2) %>% 
  mutate(est = round(est, 2))



#PREVALENCIA POR REGIAO DE RESIDENCIA
#unindo os dados de nascidos vivos e anomalias
prev.microce.reg.residp1119 <- microce.reg.residp1119 %>% 
  left_join(nv.reg.residp1119, microce.reg.residp1119, by = "code_health_region")

#calculando prevalencia
epiprev.microce.reg.residp1119 <-epi.prev(prev.microce.reg.residp1119$n, prev.microce.reg.residp1119$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

##criando objeto com prevalencias e IC
epiprev.microce.reg.residp1119_2 <- epiprev.microce.reg.residp1119[["ap"]]

#unindo os dados de prevalencia
prev.microce.reg.residp1119 <- prev.microce.reg.residp1119 %>% 
  bind_cols(epiprev.microce.reg.residp1119_2) %>% 
  mutate(est = round(est, 2))



#PREVALENCIA POR MUNICIPIO DE NASCIMENTO
#unindo os dados de nascidos vivos e anomalias
prev.microce.muni.nascp1119 <- microce.muni.nascp1119 %>% 
  left_join(nv.muni.nascp1119, microce.muni.nascp1119, by = "code_muni") %>% 
  filter(str_detect(code_muni,"42"))

#calculando prevalencia
epiprev.microce.muni.nascp1119 <-epi.prev(prev.microce.muni.nascp1119$n, prev.microce.muni.nascp1119$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

##criando objeto com prevalencias e IC
epiprev.microce.muni.nascp1119_2 <- epiprev.microce.muni.nascp1119[["ap"]]

#unindo os dados de prevalencia
prev.microce.muni.nascp1119 <- prev.microce.muni.nascp1119 %>% 
  bind_cols(epiprev.microce.muni.nascp1119_2) %>% 
  mutate(est = round(est, 2))



#PREVALENCIA POR ESTABELECIMENTO
#unindo os dados de nascidos vivos e anomalias
prev.microce.estabp1119 <- microce.estabp1119 %>% 
  left_join(nv.estabp1119, microce.estabp1119, by = "code_cnes") %>% 
  filter(str_detect(CodigoMunicipioNascimento,"42")) %>% 
  filter(code_cnes != is.na(code_cnes)) 

#calculando prevalencia
epiprev.microce.estabp1119 <-epi.prev(prev.microce.estabp1119$n, prev.microce.estabp1119$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

##criando objeto com prevalencias e IC
epiprev.microce.estabp1119_2 <- epiprev.microce.estabp1119[["ap"]]

#unindo os dados de prevalencia
prev.microce.estabp1119 <- prev.microce.estabp1119 %>% 
  bind_cols(epiprev.microce.estabp1119_2) %>% 
  mutate(est = round(est, 2))




######### MAPAS

## ---- mapa_microce.muni.residp1119 --------
##MAPA ANOMALIAS SELECIONADAS POR MUNICIPIO DE RESIDENCIA
# unindo os dados com a base de mapa
dados_microce.muni.residp1119 <- dplyr::left_join(geoMunicipiosSC, microce.muni.residp1119, by = "code_muni")


#plotando o mapa
mapa_microce.muni.residp1119 <- leaflet(data = dados_microce.muni.residp1119) 

label_microce.muni.residp1119 <- dados_microce.muni.residp1119 %>%
  mutate (n = replace_na(n, 0))

colormagma_microce.muni.residp1119 = colorNumeric("magma", domain = dados_microce.muni.residp1119$n, alpha = TRUE, reverse =  TRUE)

mapa_microce.muni.residp1119 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_microce.muni.residp1119(n),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Munic??pio de resid??ncia: </strong>%s<br/><strong>
                               Nascidos vivos com anomalias: </strong>%s", 
                               label_microce.muni.residp1119$name_muni, label_microce.muni.residp1119$n) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_microce.muni.residp1119, values = ~n, na.label = "Sem registro", opacity = 1, title = "Nascidos vivos <br> com anomalias")



## ---- mapa_microce.macro.residp1119 --------
##MAPA ANOMALIAS SELECIONADAS POR MACRORREGIAO DE RESIDENCIA
# unindo os dados com a base de mapa
dados_microce.macro.residp1119 <- dplyr::left_join(geoMacrorregioesSC, microce.macro.residp1119, by = "code_health_marcroregion")


#plotando o mapa
mapa_microce.macro.residp1119 <- leaflet(data = dados_microce.macro.residp1119) 

label_microce.macro.residp1119 <- dados_microce.macro.residp1119 %>%
  mutate (n = replace_na(n, 0)) %>% 
  mutate(name_health_macroregion = str_to_title(name_health_macroregion))

label_microce.macro.residp1119["3", "name_health_macroregion"] <- "Meio Oeste E Serra Catarinense"

colormagma_microce.macro.residp1119  = colorNumeric("magma", domain = dados_microce.macro.residp1119$n, alpha = TRUE, reverse =  TRUE)

mapa_microce.macro.residp1119 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_microce.macro.residp1119(n),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Macrorregi??o de sa??de: </strong>%s<br/><strong>
                               Nascidos vivos com anomalias: </strong>%s", 
                               label_microce.macro.residp1119$name_health_macroregion, label_microce.macro.residp1119$n) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_microce.macro.residp1119, values = ~n, na.label = "Sem registro", opacity = 1, title = "Nascidos vivos <br> com anomalias")


## ---- mapa_microce.reg.residp1119 --------
##MAPA ANOMALIAS SELECIONADAS POR REGIAO DE RESIDENCIA
# unindo os dados com a base de mapa
dados_microce.reg.residp1119 <- dplyr::left_join(geoRegioesSC, microce.reg.residp1119, by = "code_health_region")


#plotando o mapa
mapa_microce.reg.residp1119 <- leaflet(data = dados_microce.reg.residp1119) 

label_microce.reg.residp1119 <- dados_microce.reg.residp1119 %>%
  mutate (n = replace_na(n, 0)) 

colormagma_microce.reg.residp1119 = colorNumeric("magma", domain = dados_microce.reg.residp1119$n, alpha = TRUE, reverse =  TRUE)

mapa_microce.reg.residp1119 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_microce.reg.residp1119(n),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Regi??o de sa??de: </strong>%s<br/><strong>
                               Nascidos vivos com anomalias: </strong>%s", 
                               label_microce.reg.residp1119$name_health_region, label_microce.reg.residp1119$n) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_microce.reg.residp1119, values = ~n, na.label = "Sem registro", opacity = 1, title = "Nascidos vivos <br> com anomalias")


## ---- mapa_microce.muni.nascp1119 --------
##MAPA ANOMALIAS SELECIONADAS POR MUNICIPIO DE NASCIMENTO SC
# unindo os dados com a base de mapa
dados_microce.muni.nascp1119 <- dplyr::left_join(geoMunicipiosSC, microce.muni.nascp1119, by = "code_muni")


#plotando o mapa
mapa_microce.muni.nascp1119 <- leaflet(data = dados_microce.muni.nascp1119) 

label_microce.muni.nascp1119 <- dados_microce.muni.nascp1119 %>%
  mutate (n = replace_na(n, 0))

colormagma_microce.muni.nascp1119 = colorNumeric("magma", domain = dados_microce.muni.nascp1119$n, alpha = TRUE, reverse =  TRUE)

mapa_microce.muni.nascp1119 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_microce.muni.nascp1119(n),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Munic??pio de Nascimento: </strong>%s<br/><strong>
                               Nascidos vivos com anomalias: </strong>%s", 
                               label_microce.muni.nascp1119$name_muni, label_microce.muni.nascp1119$n) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_microce.muni.nascp1119, values = ~n, na.label = "Sem registro", opacity = 1, title = "Nascidos vivos <br> com anomalias")



## ---- mapa_microce.estabp1119 --------
### MAPA ANOMALIAS SELECIONADAS POR ESTABELECIMENTO
## feito em http://dwilhelm89.github.io/Leaflet.StyleEditor/ -- redimensionar antes de usar
#criando lista de marcadores - so funcionou nessa ordem
marcadores_personalizados <- iconList(
  laranja = makeIcon("/Users/Laysa/Google Drive/Doutorado PPGBM/An??lises do banco/Marcador laranja.png"),
  verde = makeIcon("/Users/Laysa/Google Drive/Doutorado PPGBM/An??lises do banco/Marcador verde.png"),
  vermelho = makeIcon("/Users/Laysa/Google Drive/Doutorado PPGBM/An??lises do banco/Marcador vermelho.png"))



# unindo os dados com a base de mapa
dados_microce.estabp1119 <- dplyr::left_join(geoEstabeleTodos, microce.estabp1119, by = "code_cnes")

#filtrando por estabelecimentos com anomalias
dados_microce.estabp1119 <- dados_microce.estabp1119 %>% 
  filter(n != is.na(n))

#criando coluna marcadores
dados_microce.estabp1119<- dados_microce.estabp1119 %>%
  mutate(Marcador = case_when(
    n <3 ~ "verde",
    n <5 ~ "laranja",
    n >4 ~ "vermelho"))

#convertendo marcadores como factor
dados_microce.estabp1119 <- dados_microce.estabp1119 %>%
  mutate(Marcador=as.factor(Marcador))

#organizando nivel marcadores
levels(dados_microce.estabp1119$Marcador) <- c("verde", "laranja", "vermelho")


#plotando o mapa  
label_microce.estabp1119 <- sprintf("<p><strong>Estabelecimento: </strong>%s<br/>
                 <strong>Nascidos vivos com anomalias: </strong>%s", dados_microce.estabp1119$Estabelecimento,
                                   dados_microce.estabp1119$n) %>% lapply(htmltools::HTML)


mapa_microce.estabp1119 <- leaflet(dados_microce.estabp1119) %>% addTiles() 
mapa_microce.estabp1119 %>% 
  addMarkers(icon = ~marcadores_personalizados[as.numeric(Marcador)],
             label = label_microce.estabp1119,  
             labelOptions = labelOptions(textsize = "14px", opacity = 0.7, direction = "top"),
             options = markerOptions(riseOnHover = TRUE)) %>% 
  addLegendImage(mapa_microce.estabp1119, images = c("/Users/Laysa/Google Drive/Doutorado PPGBM/An??lises do banco/Marcador verde.png",
                                                    "/Users/Laysa/Google Drive/Doutorado PPGBM/An??lises do banco/Marcador laranja.png",
                                                    "/Users/Laysa/Google Drive/Doutorado PPGBM/An??lises do banco/Marcador vermelho.png"),
                 labels = c('1-2', '3-4', 'Maior ou igual a 5'), width = c(20, 25, 28), height = c(31, 34, 39),
                 title = htmltools::tags$div('NV c/ AC por estabelecimento', style = 'font-size: 15px; text-align: center;'),
                 labelStyle = "font-size: 15px; vertical-align: middle;",
                 orientation = 'vertical',
                 position = 'bottomleft')


### PREVALENCIAS


## ---- mapa_prev.microce.muni.residp1119 --------
### MAPA PREVALENCIA POR MUNICIPIOS DE RESIDENCIA DA MAE
# unindo os dados com a base de mapa
dados_prev.microce.muni.residp1119 <- dplyr::left_join(geoMunicipiosSC, prev.microce.muni.residp1119, by = "code_muni") 

#modificando decimal no label e susbtituindo NAs
label_prev.microce.muni.residp1119 <- dados_prev.microce.muni.residp1119 %>%
  mutate (est = replace_na(est, 0.00)) %>%
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ","))

#plotando o mapa
mapa_prev.microce.muni.residp1119  <- leaflet(data = dados_prev.microce.muni.residp1119) 

colormagma_prev.microce.muni.residp1119 = colorNumeric("magma", domain = dados_prev.microce.muni.residp1119$est, alpha = TRUE, reverse =  TRUE)

mapa_prev.microce.muni.residp1119 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_prev.microce.muni.residp1119(est),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Munic??pio de resid??ncia: </strong>%s<br/><strong>
                               Preval??ncia/10.000: </strong>%s", 
                               label_prev.microce.muni.residp1119$name_muni, label_prev.microce.muni.residp1119$est) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_prev.microce.muni.residp1119, values = ~est, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Preval??ncia/10.000")


## ---- mapa_prev.microce.macro.residp1119 --------
### MAPA PREVALENCIA POR MACRORREGIAO DE RESIDENCIA DA MAE
# unindo os dados com a base de mapa
dados_prev.microce.macro.residp1119 <- dplyr::left_join(geoMacrorregioesSC, prev.microce.macro.residp1119, by = "code_health_marcroregion") 

#modificando decimal no label e susbtituindo NAs
label_prev.microce.macro.residp1119 <- dados_prev.microce.macro.residp1119 %>%
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate(name_health_macroregion = str_to_title(name_health_macroregion))

label_prev.microce.macro.residp1119["3", "name_health_macroregion"] <- "Meio Oeste E Serra Catarinense"

#plotando o mapa
mapa_prev.microce.macro.residp1119  <- leaflet(data = dados_prev.microce.macro.residp1119) 

colormagma_prev.microce.macro.residp1119 = colorNumeric("magma", domain = dados_prev.microce.macro.residp1119$est, alpha = TRUE, reverse =  TRUE)

mapa_prev.microce.macro.residp1119 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_prev.microce.macro.residp1119(est),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Macrorregi??o de sa??de: </strong>%s<br/>
               <strong>Preval??ncia/10.000: </strong>%s", label_prev.microce.macro.residp1119$name_health_macroregion, label_prev.microce.macro.residp1119$est) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_prev.microce.macro.residp1119, values = ~est, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Preval??ncia/10.000")


## ---- mapa_prev.microce.reg.residp1119 --------
### MAPA PREVALENCIA POR REGIAO DE RESIDENCIA DA MAE
# unindo os dados com a base de mapa
dados_prev.microce.reg.residp1119 <- dplyr::left_join(geoRegioesSC, prev.microce.reg.residp1119, by = "code_health_region") 

#modificando decimal no label e susbtituindo NAs
label_prev.microce.reg.residp1119 <- dados_prev.microce.reg.residp1119 %>%
  mutate (est = replace_na(est, 0.00)) %>%
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ","))


#plotando o mapa
mapa_prev.microce.reg.residp1119  <- leaflet(data = dados_prev.microce.reg.residp1119) 

colormagma_prev.microce.reg.residp1119 = colorNumeric("magma", domain = dados_prev.microce.reg.residp1119$est, alpha = TRUE, reverse =  TRUE)

mapa_prev.microce.reg.residp1119 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_prev.microce.reg.residp1119(est),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Regi??o de sa??de: </strong>%s<br/><strong>
                               Preval??ncia/10.000: </strong>%s", 
                               label_prev.microce.reg.residp1119$name_health_region, label_prev.microce.reg.residp1119$est) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_prev.microce.reg.residp1119, values = ~est, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Preval??ncia/10.000")



## ---- mapa_prev.microce.muni.nascp1119 --------
### MAPA PREVALENCIA POR MUNICIPIOS DE NASCIMENTO
# unindo os dados com a base de mapa
dados_prev.microce.muni.nascp1119 <- dplyr::left_join(geoMunicipiosSC, prev.microce.muni.nascp1119, by = "code_muni") 

#substituindo NAs no label
label_prev.microce.muni.nascp1119 <- dados_prev.microce.muni.nascp1119 %>%
  mutate (est = replace_na(est, 0.00))

#modificando decimal no label 
label_prev.microce.muni.nascp1119 <- label_prev.microce.muni.nascp1119  %>%
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ","))


#plotando o mapa
mapa_prev.microce.muni.nascp1119  <- leaflet(data = dados_prev.microce.muni.nascp1119) 

colormagma_prev.microce.muni.nascp1119 = colorNumeric("magma", domain = dados_prev.microce.muni.nascp1119$est, alpha = TRUE, reverse =  TRUE)

mapa_prev.microce.muni.nascp1119 %>% 
  addPolygons (weight = 0.5, fillColor = ~colormagma_prev.microce.muni.nascp1119(est),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Munic??pio de nascimento: </strong>%s<br/><strong>
                               Preval??ncia/10.000: </strong>%s", 
                               label_prev.microce.muni.nascp1119$name_muni, label_prev.microce.muni.nascp1119$est) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = colormagma_prev.microce.muni.nascp1119, values = ~est, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Preval??ncia/10.000")



## ---- mapa_prev.microce.estabp1119 --------
### MAPA PREVALENCIAS POR ESTABELECIMENTO
#icones dos marcadores criados anteriormente
# unindo os dados com a base de mapa
dados_prev.microce.estabp1119 <- dplyr::left_join(geoEstabeleTodos, prev.microce.estabp1119, by = "code_cnes")

#filtrando por estabelecimentos com anomalias em santa catarina
dados_prev.microce.estabp1119 <- dados_prev.microce.estabp1119 %>% 
  filter(abbrev_state == "SC") %>% 
  filter(n != is.na(n))


#criando coluna marcadores
dados_prev.microce.estabp1119 <- dados_prev.microce.estabp1119 %>%
  mutate(Marcador = case_when(
    est <2 ~ "verde",
    est <4 ~ "laranja",
    est >5 ~ "vermelho"))

#convertendo marcadores como factor
dados_prev.microce.estabp1119 <- dados_prev.microce.estabp1119 %>%
  mutate(Marcador=as.factor(Marcador))

#organizando nivel marcadores
levels(dados_prev.microce.estabp1119$Marcador) <- c("verde", "laranja", "vermelho")


#criando label e modificando decimal
label_prev.microce.estabp1119 <- dados_prev.microce.estabp1119  %>%
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ","))


label_prev.microce.estabp1119 <- sprintf("<p><strong>Estabelecimento: </strong>%s<br/>
                 <strong>Preval??ncia/10.000: </strong>%s", label_prev.microce.estabp1119$Estabelecimento,
                                        label_prev.microce.estabp1119$est) %>% lapply(htmltools::HTML)

#plotando o mapa
mapa_prev.microce.estabp1119 <- leaflet(dados_prev.microce.estabp1119) %>% addTiles() 
mapa_prev.microce.estabp1119 %>% 
  addMarkers(icon = ~marcadores_personalizados[as.numeric(Marcador)],
             label = label_prev.microce.estabp1119,  
             labelOptions = labelOptions(textsize = "14px", opacity = 0.7, direction = "top"),
             options = markerOptions(riseOnHover = TRUE)) %>% 
  addLegendImage(mapa_prev.microce.estabp1119, images = c("/Users/Laysa/Google Drive/Doutorado PPGBM/An??lises do banco/Marcador verde.png",
                                                         "/Users/Laysa/Google Drive/Doutorado PPGBM/An??lises do banco/Marcador laranja.png",
                                                         "/Users/Laysa/Google Drive/Doutorado PPGBM/An??lises do banco/Marcador vermelho.png"),
                 labels = c('1-2', '2-4', 'Maior ou igual a 5'), width = c(20, 25, 28), height = c(31, 34, 39),
                 title = htmltools::tags$div('Preval??ncia/10.000', style = 'font-size: 15px; text-align: center;'),
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
  select(Nome_UF, `C??digo Munic??pio Completo`, Nome_Munic??pio) %>%
  rename(UFNascimento = Nome_UF,
         CodigoMunicipioNascimento = `C??digo Munic??pio Completo`,
         MunicipioNascimento = Nome_Munic??pio)

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
  add_row(CodigoMunicipioResidencia="420000", MunicipioResidencia="Munic??pio Ignorado")



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
  add_row(CodigoMacrorregiaoSaude = 4200, MacrorregiaoSaude = "Munic??pio Ignorado", CodigoMunicipioResidencia = 420000)

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
  add_row(CodigoRegiaoSaude = 42000, RegiaoSaude = "Munic??pio Ignorado", CodigoMunicipioResidencia = "420000")

regioes["168", "CodigoMunicipioResidencia"] <- "420415"




## ---- tab_prev.microce.muni.residp1119 --------
tab_prev.microce.muni.residp1119 <- prev.microce.muni.residp1119 %>%
  rename(CodigoMunicipioResidencia = code_muni) 

tab_prev.microce.muni.residp1119 <- tab_prev.microce.muni.residp1119 %>% 
  left_join(municipiosresidencia, tab_prev.microce.muni.residp1119, by = "CodigoMunicipioResidencia") %>% 
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (lower = scales::number(lower, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (upper = scales::number(upper, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>%
  unite (col = IC95, lower:upper, sep = "-") %>%
  rename(NascidosVivosACs = n) %>% 
  rename(NascidosVivos = nasc_vivos) %>% 
  rename(Prevalencia10000 = est) %>% 
  relocate(MunicipioResidencia, .after = CodigoMunicipioResidencia)


## ---- tab_prev.microce.macro.residp1119 --------
tab_prev.microce.macro.residp1119 <- prev.microce.macro.residp1119 %>%
  rename(CodigoMacrorregiaoSaude = code_health_marcroregion) %>% 
  mutate(CodigoMacrorregiaoSaude = as.numeric(CodigoMacrorregiaoSaude))

tab_prev.microce.macro.residp1119 <- tab_prev.microce.macro.residp1119 %>% 
  left_join(macrorregioes, tab_prev.microce.macro.residp1119, by = "CodigoMacrorregiaoSaude") %>% 
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


## ---- tab_prev.microce.reg.residp1119 --------
tab_prev.microce.reg.residp1119 <- prev.microce.reg.residp1119 %>%
  rename(CodigoRegiaoSaude = code_health_region) %>% 
  mutate(CodigoRegiaoSaude = as.numeric(CodigoRegiaoSaude)) 

tab_prev.microce.reg.residp1119 <- tab_prev.microce.reg.residp1119 %>% 
  left_join(regioes, tab_prev.microce.reg.residp1119, by = "CodigoRegiaoSaude") %>% 
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


## ---- tab_prev.microce.muni.nascp1119 --------
tab_prev.microce.muni.nascp1119 <- prev.microce.muni.nascp1119 %>%
  rename(CodigoMunicipioNascimento = code_muni) 

tab_prev.microce.muni.nascp1119 <- tab_prev.microce.muni.nascp1119 %>% 
  left_join(municipios, tab_prev.microce.muni.nascp1119, by = "CodigoMunicipioNascimento") %>% 
  mutate (est = scales::number(est, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (lower = scales::number(lower, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (upper = scales::number(upper, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>%
  unite (col = IC95, lower:upper, sep = "-") %>%
  rename(NascidosVivosACs = n) %>% 
  rename(NascidosVivos = nasc_vivos) %>% 
  rename(Prevalencia10000 = est) %>% 
  relocate(MunicipioNascimento, .after = CodigoMunicipioNascimento) %>% 
  mutate (UFNascimento = NULL)


## ---- tab_prev.microce.estabp1119 --------

estabelecimentoC <- read_csv2("estabelecimentoC.csv", locale = locale(encoding = "ISO8859-1"))

tab_prev.microce.estabp1119 <- prev.microce.estabp1119 %>%
  rename(CodigoEstabelecimento = code_cnes) %>% 
  mutate(CodigoEstabelecimento = as.character(CodigoEstabelecimento)) %>%
  mutate(CodigoEstabelecimento = str_pad(CodigoEstabelecimento, width = 7, side = "left", pad = "0"))


tab_prev.microce.estabp1119 <- tab_prev.microce.estabp1119 %>% 
  left_join(estabelecimentoC, tab_prev.microce.estabp1119, by = "CodigoEstabelecimento") %>% 
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


tab_prev.microce.estabp1119 <- tab_prev.microce.estabp1119 %>%
  mutate(CodigoMunicipioNascimento = as.character(CodigoMunicipioNascimento)) %>% 
  left_join(municipios, tab_prev.microce.estabp1119, by = "CodigoMunicipioNascimento") %>% 
  relocate(MunicipioNascimento, .after = CodigoMunicipioNascimento) %>% 
  mutate (UFNascimento = NULL)



