### SUAVIZAÇÃO ESPACIAL

## ---- biblioteca_suavizacao --------

library(CARBayes)
library(leaflet)
library(tidyverse)
library(viridis)
library(geobr)
library(sf)
library(leaflegend)
library(scales)
library(ggplot2)
library(readr)
library(spdep)

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


## ---- bancoSIR --------

bancop1120 <- read_csv2("banco_limpeza.csv", locale = locale(encoding = "ISO8859-1"))


#CRIANDO BANCO SIR

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


## ---- dadostodasanomaliasbym --------

#NV COM ANOMALIAS SELECIONADAS
nv.anom.muni <- bancop1120 %>% 
  filter(str_detect(CID10Anomalia,"Q000|Q001|Q002|Q01|Q05|Q02|Q20|Q21|Q22|Q23|Q24|Q25|Q26|Q27|Q28|Q35|Q36|Q37|Q54|Q56|Q66|Q69|Q71|Q72|Q73|Q743|Q792|Q793|Q90")) %>% 
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>%
  count()


#unindo os dados de nascidos vivos e anomalias
banco_SIR <- nv.anom.muni %>% 
  right_join(nv.muni, nv.anom.muni, by = c('CodigoMunicipioResidencia'))


banco_SIR <- banco_SIR %>%
  filter(CodigoMunicipioResidencia != 420000) %>% 
  mutate(n = replace_na(n, 0)) %>% 
  rename (code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))


#calculando valor esperado
banco_SIR$valor_esperado <- banco_SIR$nasc_vivos * sum(banco_SIR$n)/sum(banco_SIR$nasc_vivos)


#calculando SIR
banco_SIR$SIR <- banco_SIR$n/banco_SIR$valor_esperado


# unindo os dados com a base de mapa
banco_SIR_shape <- dplyr::left_join(geoMunicipiosSC, banco_SIR, by = "code_muni")


### BYM MODEL

# Create binary, first-order, adjacency spatial weights matrix
W <- nb2mat(poly2nb(banco_SIR_shape), style = "B")

# MCMC parameters
M.burnin <- 10000       # Number of burn-in iterations (discarded)
M <- 5000               # Number of iterations retained

# Fit BYM model using CARBayes
set.seed(1)          # For reproducability
MCMC <- S.CARbym(
  formula = banco_SIR_shape$n ~ 1 + offset(log(banco_SIR_shape$valor_esperado)), 
  data = banco_SIR_shape,
  family = "poisson",
  W = W,
  burnin = M.burnin,
  n.sample = M.burnin + M,    # Total iterations
  verbose = FALSE
)

# Broad summary of results
print(MCMC$summary.results)



# Compute posterior SIR
y.fit <- MCMC$samples$fitted
SIR <- t(t(y.fit)/banco_SIR_shape$valor_esperado)


# Add summary statistics of the posterior SIR to map
banco_SIR_shape$SIR.50 <- apply(SIR, 2, median)
banco_SIR_shape$SIR.025 <- apply(SIR, 2, quantile, 0.025)
banco_SIR_shape$SIR.975 <- apply(SIR, 2, quantile, 0.975)
banco_SIR_shape$PP <- apply(SIR, 2, function(x) length(which(x > 1)))/M


## ---- mapabymtodas --------

pal <-
  colorNumeric(
    palette = "magma",
    domain = banco_SIR_shape$SIR.50,
    alpha = TRUE, reverse =  TRUE
  )


label_banco_SIR <- banco_SIR_shape %>%
  mutate (valor_esperado= scales::number(valor_esperado, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR= scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.50= scales::number(SIR.50, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>%   
  mutate (SIR.025= scales::number(SIR.025, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.975= scales::number(SIR.975, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (PP= scales::number(PP, accuracy = 0.001, big.mark = ".", decimal.mark = ","))


mapaSIRtodas <- leaflet(data = banco_SIR_shape) 

mapaSIRtodas %>%
  addPolygons (weight = 0.5, fillColor = ~pal(banco_SIR_shape$SIR.50),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<strong> %s </strong> <br/> Observado: %s <br/> Esperado: %s <br/>
        SIR: %s <br/>RR: %s (%s, %s) <br/>PP: %s",
                               label_banco_SIR$name_muni, label_banco_SIR$n, label_banco_SIR$valor_esperado,
                               label_banco_SIR$SIR, label_banco_SIR$SIR.50, label_banco_SIR$SIR.025, label_banco_SIR$SIR.975,
                               label_banco_SIR$PP) %>% 
                 lapply(htmltools::HTML),
              labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = banco_SIR_shape$SIR.50, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Risco Relativo")


## ---- tabelatodas --------

tabela_todas <- banco_SIR_shape %>% 
  st_drop_geometry()

tabela_todas <- tabela_todas %>% 
  dplyr::select (-code_state, -abbrev_state, -name_state, -code_region, -name_region) %>%  
  mutate (Prevalencia10000 = n/nasc_vivos*10^4) %>% 
  rename (CodigoMunicipioResidencia = code_muni) %>% 
  rename (MunicipioResidencia = name_muni) %>%
  rename (NascidosVivos = nasc_vivos) %>%
  rename (NascidosVivosAnomalia = n) %>%
  rename (ValorEsperado = valor_esperado) %>%
  rename (RR_SIR.50 = SIR.50)

tabela_todas <- tabela_todas %>% 
  mutate (ValorEsperado = scales::number(ValorEsperado, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR = scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (RR_SIR.50 = scales::number(RR_SIR.50, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.025 = scales::number(SIR.025, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.975 = scales::number(SIR.975, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (PP = scales::number(PP, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (Prevalencia10000 = scales::number(Prevalencia10000, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) 



## ---- dadosbymmembros --------

#NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE RESIDENCIA
membros.muni <- anom.gruposp1120 %>% 
  filter(GrupoCID10 == "Defeitos de membros") %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>%
  count() 


#unindo os dados de nascidos vivos e anomalias
membros_SIR <- membros.muni %>% 
  right_join(nv.muni, membros.muni, by = c('CodigoMunicipioResidencia'))


membros_SIR <- membros_SIR %>%
  filter(CodigoMunicipioResidencia != 420000) %>% 
  mutate(n = replace_na(n, 0)) %>% 
  rename (code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))


#calculando valor esperado
membros_SIR$valor_esperado <- membros_SIR$nasc_vivos * sum(membros_SIR$n)/sum(membros_SIR$nasc_vivos)


#calculando SIR
membros_SIR$SIR <- membros_SIR$n/membros_SIR$valor_esperado


# unindo os dados com a base de mapa
membros_SIR_shape <- dplyr::left_join(geoMunicipiosSC, membros_SIR, by = "code_muni")



#mapa


pal <-
  colorNumeric(
    palette = "magma",
    domain = membros_SIR_shape$SIR,
    alpha = TRUE, reverse =  TRUE
  )


y = membros_SIR_shape$SIR

label_membros_SIR <- membros_SIR_shape %>%
  mutate (SIR= scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ","))




mapaSIRmembros  <- leaflet(data = membros_SIR_shape) 

mapaSIRmembros %>%
  addPolygons (weight = 0.5, fillColor = ~pal(membros_SIR_shape$SIR),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               SIR: </strong>%s",
                               label_membros_SIR$name_muni, label_membros_SIR$SIR) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = membros_SIR_shape$SIR, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "SIR")



### BYM MODEL

# Create binary, first-order, adjacency spatial weights matrix
W <- nb2mat(poly2nb(membros_SIR_shape), style = "B")

# MCMC parameters
M.burnin <- 10000       # Number of burn-in iterations (discarded)
M <- 5000               # Number of iterations retained

# Fit BYM model using CARBayes
set.seed(1)          # For reproducability
MCMC <- S.CARbym(
  formula = membros_SIR_shape$n ~ 1 + offset(log(membros_SIR_shape$valor_esperado)), 
  data = membros_SIR_shape,
  family = "poisson",
  W = W,
  burnin = M.burnin,
  n.sample = M.burnin + M,    # Total iterations
  verbose = FALSE
)

# Broad summary of results
print(MCMC$summary.results)



# Compute posterior SIR
y.fit <- MCMC$samples$fitted
SIR <- t(t(y.fit)/membros_SIR_shape$valor_esperado)


# Add summary statistics of the posterior SIR to map
membros_SIR_shape$SIR.50 <- apply(SIR, 2, median)
membros_SIR_shape$SIR.025 <- apply(SIR, 2, quantile, 0.025)
membros_SIR_shape$SIR.975 <- apply(SIR, 2, quantile, 0.975)
membros_SIR_shape$PP <- apply(SIR, 2, function(x) length(which(x > 1)))/M



## ---- mapabymmembros --------

pal <-
  colorNumeric(
    palette = "magma",
    domain = membros_SIR_shape$SIR.50,
    alpha = TRUE, reverse =  TRUE
  )


label_membros_SIR <- membros_SIR_shape %>%
  mutate (valor_esperado= scales::number(valor_esperado, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR= scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.50= scales::number(SIR.50, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>%   
  mutate (SIR.025= scales::number(SIR.025, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.975= scales::number(SIR.975, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (PP= scales::number(PP, accuracy = 0.001, big.mark = ".", decimal.mark = ","))


mapaSIRmembros  <- leaflet(data = membros_SIR_shape) 

mapaSIRmembros %>%
  addPolygons (weight = 0.5, fillColor = ~pal(membros_SIR_shape$SIR.50),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<strong> %s </strong> <br/> Observado: %s <br/> Esperado: %s <br/>
        SIR: %s <br/>RR: %s (%s, %s) <br/>PP: %s",
                               label_membros_SIR$name_muni, label_membros_SIR$n, label_membros_SIR$valor_esperado,
                               label_membros_SIR$SIR, label_membros_SIR$SIR.50, label_membros_SIR$SIR.025, label_membros_SIR$SIR.975,
                               label_membros_SIR$PP) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = membros_SIR_shape$SIR.50, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Risco Relativo")



## ---- tabelamembros --------

tabela_membros <- membros_SIR_shape %>% 
  st_drop_geometry()

tabela_membros <- tabela_membros %>% 
  dplyr::select (-code_state, -abbrev_state, -name_state, -code_region, -name_region) %>%  
  mutate (Prevalencia10000 = n/nasc_vivos*10^4) %>% 
  rename (CodigoMunicipioResidencia = code_muni) %>% 
  rename (MunicipioResidencia = name_muni) %>%
  rename (NascidosVivos = nasc_vivos) %>%
  rename (NascidosVivosAnomalia = n) %>%
  rename (ValorEsperado = valor_esperado) %>%
  rename (RR_SIR.50 = SIR.50)

tabela_membros <- tabela_membros %>% 
  mutate (ValorEsperado = scales::number(ValorEsperado, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR = scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (RR_SIR.50 = scales::number(RR_SIR.50, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.025 = scales::number(SIR.025, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.975 = scales::number(SIR.975, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (PP = scales::number(PP, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (Prevalencia10000 = scales::number(Prevalencia10000, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) 





## ---- dadosbymtubo --------

#NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE RESIDENCIA
tubo.muni <- anom.gruposp1120 %>% 
  filter(GrupoCID10 == "Defeitos de tubo neural") %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>%
  count() 


#unindo os dados de nascidos vivos e anomalias
tubo_SIR <- tubo.muni %>% 
  right_join(nv.muni, tubo.muni, by = c('CodigoMunicipioResidencia'))


tubo_SIR <- tubo_SIR %>%
  filter(CodigoMunicipioResidencia != 420000) %>% 
  mutate(n = replace_na(n, 0)) %>% 
  rename (code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))


#calculando valor esperado
tubo_SIR$valor_esperado <- tubo_SIR$nasc_vivos * sum(tubo_SIR$n)/sum(tubo_SIR$nasc_vivos)


#calculando SIR
tubo_SIR$SIR <- tubo_SIR$n/tubo_SIR$valor_esperado


# unindo os dados com a base de mapa
tubo_SIR_shape <- dplyr::left_join(geoMunicipiosSC, tubo_SIR, by = "code_muni")



#mapa


pal <-
  colorNumeric(
    palette = "magma",
    domain = tubo_SIR_shape$SIR,
    alpha = TRUE, reverse =  TRUE
  )


y = tubo_SIR_shape$SIR

label_tubo_SIR <- tubo_SIR_shape %>%
  mutate (SIR= scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ","))




mapaSIRtubo  <- leaflet(data = tubo_SIR_shape) 

mapaSIRtubo %>%
  addPolygons (weight = 0.5, fillColor = ~pal(tubo_SIR_shape$SIR),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               SIR: </strong>%s",
                               label_tubo_SIR$name_muni, label_tubo_SIR$SIR) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = tubo_SIR_shape$SIR, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "SIR")



### BYM MODEL

# Create binary, first-order, adjacency spatial weights matrix
W <- nb2mat(poly2nb(tubo_SIR_shape), style = "B")

# MCMC parameters
M.burnin <- 10000       # Number of burn-in iterations (discarded)
M <- 5000               # Number of iterations retained

# Fit BYM model using CARBayes
set.seed(1)          # For reproducability
MCMC <- S.CARbym(
  formula = tubo_SIR_shape$n ~ 1 + offset(log(tubo_SIR_shape$valor_esperado)), 
  data = tubo_SIR_shape,
  family = "poisson",
  W = W,
  burnin = M.burnin,
  n.sample = M.burnin + M,    # Total iterations
  verbose = FALSE
)

# Broad summary of results
print(MCMC$summary.results)



# Compute posterior SIR
y.fit <- MCMC$samples$fitted
SIR <- t(t(y.fit)/tubo_SIR_shape$valor_esperado)


# Add summary statistics of the posterior SIR to map
tubo_SIR_shape$SIR.50 <- apply(SIR, 2, median)
tubo_SIR_shape$SIR.025 <- apply(SIR, 2, quantile, 0.025)
tubo_SIR_shape$SIR.975 <- apply(SIR, 2, quantile, 0.975)
tubo_SIR_shape$PP <- apply(SIR, 2, function(x) length(which(x > 1)))/M


## ---- mapabymtubo --------

pal <-
  colorNumeric(
    palette = "magma",
    domain = tubo_SIR_shape$SIR.50,
    alpha = TRUE, reverse =  TRUE
  )


label_tubo_SIR <- tubo_SIR_shape %>%
  mutate (valor_esperado= scales::number(valor_esperado, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR= scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.50= scales::number(SIR.50, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>%   
  mutate (SIR.025= scales::number(SIR.025, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.975= scales::number(SIR.975, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (PP= scales::number(PP, accuracy = 0.001, big.mark = ".", decimal.mark = ","))


mapaSIRtubo  <- leaflet(data = tubo_SIR_shape) 

mapaSIRtubo %>%
  addPolygons (weight = 0.5, fillColor = ~pal(tubo_SIR_shape$SIR.50),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<strong> %s </strong> <br/> Observado: %s <br/> Esperado: %s <br/>
        SIR: %s <br/>RR: %s (%s, %s) <br/>PP: %s",
                               label_tubo_SIR$name_muni, label_tubo_SIR$n, label_tubo_SIR$valor_esperado,
                               label_tubo_SIR$SIR, label_tubo_SIR$SIR.50, label_tubo_SIR$SIR.025, label_tubo_SIR$SIR.975,
                               label_tubo_SIR$PP) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = tubo_SIR_shape$SIR.50, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Risco Relativo")



## ---- tabelatubo --------

tabela_tubo <- tubo_SIR_shape %>% 
  st_drop_geometry()

tabela_tubo <- tabela_tubo %>% 
  dplyr::select (-code_state, -abbrev_state, -name_state, -code_region, -name_region) %>%  
  mutate (Prevalencia10000 = n/nasc_vivos*10^4) %>% 
  rename (CodigoMunicipioResidencia = code_muni) %>% 
  rename (MunicipioResidencia = name_muni) %>%
  rename (NascidosVivos = nasc_vivos) %>%
  rename (NascidosVivosAnomalia = n) %>%
  rename (ValorEsperado = valor_esperado) %>%
  rename (RR_SIR.50 = SIR.50)

tabela_tubo <- tabela_tubo %>% 
  mutate (ValorEsperado = scales::number(ValorEsperado, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR = scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (RR_SIR.50 = scales::number(RR_SIR.50, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.025 = scales::number(SIR.025, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.975 = scales::number(SIR.975, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (PP = scales::number(PP, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (Prevalencia10000 = scales::number(Prevalencia10000, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) 



## ---- dadosbymmicrocefalia--------

#NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE RESIDENCIA
microcefalia.muni <- anom.gruposp1120 %>% 
  filter(GrupoCID10 == "Microcefalia") %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>%
  count() 


#unindo os dados de nascidos vivos e anomalias
microcefalia_SIR <- microcefalia.muni %>% 
  right_join(nv.muni, microcefalia.muni, by = c('CodigoMunicipioResidencia'))


microcefalia_SIR <- microcefalia_SIR %>%
  filter(CodigoMunicipioResidencia != 420000) %>% 
  mutate(n = replace_na(n, 0)) %>% 
  rename (code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))


#calculando valor esperado
microcefalia_SIR$valor_esperado <- microcefalia_SIR$nasc_vivos * sum(microcefalia_SIR$n)/sum(microcefalia_SIR$nasc_vivos)


#calculando SIR
microcefalia_SIR$SIR <- microcefalia_SIR$n/microcefalia_SIR$valor_esperado


# unindo os dados com a base de mapa
microcefalia_SIR_shape <- dplyr::left_join(geoMunicipiosSC, microcefalia_SIR, by = "code_muni")



#mapa


pal <-
  colorNumeric(
    palette = "magma",
    domain = microcefalia_SIR_shape$SIR,
    alpha = TRUE, reverse =  TRUE
  )


y = microcefalia_SIR_shape$SIR

label_microcefalia_SIR <- microcefalia_SIR_shape %>%
  mutate (SIR= scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ","))




mapaSIRmicrocefalia  <- leaflet(data = microcefalia_SIR_shape) 

mapaSIRmicrocefalia %>%
  addPolygons (weight = 0.5, fillColor = ~pal(microcefalia_SIR_shape$SIR),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               SIR: </strong>%s",
                               label_microcefalia_SIR$name_muni, label_microcefalia_SIR$SIR) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = microcefalia_SIR_shape$SIR, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "SIR")



### BYM MODEL

# Create binary, first-order, adjacency spatial weights matrix
W <- nb2mat(poly2nb(microcefalia_SIR_shape), style = "B")

# MCMC parameters
M.burnin <- 10000       # Number of burn-in iterations (discarded)
M <- 5000               # Number of iterations retained

# Fit BYM model using CARBayes
set.seed(1)          # For reproducability
MCMC <- S.CARbym(
  formula = microcefalia_SIR_shape$n ~ 1 + offset(log(microcefalia_SIR_shape$valor_esperado)), 
  data = microcefalia_SIR_shape,
  family = "poisson",
  W = W,
  burnin = M.burnin,
  n.sample = M.burnin + M,    # Total iterations
  verbose = FALSE
)

# Broad summary of results
print(MCMC$summary.results)



# Compute posterior SIR
y.fit <- MCMC$samples$fitted
SIR <- t(t(y.fit)/microcefalia_SIR_shape$valor_esperado)


# Add summary statistics of the posterior SIR to map
microcefalia_SIR_shape$SIR.50 <- apply(SIR, 2, median)
microcefalia_SIR_shape$SIR.025 <- apply(SIR, 2, quantile, 0.025)
microcefalia_SIR_shape$SIR.975 <- apply(SIR, 2, quantile, 0.975)
microcefalia_SIR_shape$PP <- apply(SIR, 2, function(x) length(which(x > 1)))/M


## ---- mapabymmicrocefalia --------

pal <-
  colorNumeric(
    palette = "magma",
    domain = microcefalia_SIR_shape$SIR.50,
    alpha = TRUE, reverse =  TRUE
  )


label_microcefalia_SIR <- microcefalia_SIR_shape %>%
  mutate (valor_esperado= scales::number(valor_esperado, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR= scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.50= scales::number(SIR.50, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>%   
  mutate (SIR.025= scales::number(SIR.025, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.975= scales::number(SIR.975, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (PP= scales::number(PP, accuracy = 0.001, big.mark = ".", decimal.mark = ","))


mapaSIRmicrocefalia  <- leaflet(data = microcefalia_SIR_shape) 

mapaSIRmicrocefalia %>%
  addPolygons (weight = 0.5, fillColor = ~pal(microcefalia_SIR_shape$SIR.50),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<strong> %s </strong> <br/> Observado: %s <br/> Esperado: %s <br/>
        SIR: %s <br/>RR: %s (%s, %s) <br/>PP: %s",
                               label_microcefalia_SIR$name_muni, label_microcefalia_SIR$n, label_microcefalia_SIR$valor_esperado,
                               label_microcefalia_SIR$SIR, label_microcefalia_SIR$SIR.50, label_microcefalia_SIR$SIR.025, label_microcefalia_SIR$SIR.975,
                               label_microcefalia_SIR$PP) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = microcefalia_SIR_shape$SIR.50, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Risco Relativo")





## ---- tabelamicrocefalia --------

tabela_microcefalia <- microcefalia_SIR_shape %>% 
  st_drop_geometry()

tabela_microcefalia <- tabela_microcefalia %>% 
  dplyr::select (-code_state, -abbrev_state, -name_state, -code_region, -name_region) %>%  
  mutate (Prevalencia10000 = n/nasc_vivos*10^4) %>% 
  rename (CodigoMunicipioResidencia = code_muni) %>% 
  rename (MunicipioResidencia = name_muni) %>%
  rename (NascidosVivos = nasc_vivos) %>%
  rename (NascidosVivosAnomalia = n) %>%
  rename (ValorEsperado = valor_esperado) %>%
  rename (RR_SIR.50 = SIR.50)

tabela_microcefalia <- tabela_microcefalia %>% 
  mutate (ValorEsperado = scales::number(ValorEsperado, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR = scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (RR_SIR.50 = scales::number(RR_SIR.50, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.025 = scales::number(SIR.025, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.975 = scales::number(SIR.975, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (PP = scales::number(PP, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (Prevalencia10000 = scales::number(Prevalencia10000, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) 




## ---- dadosbymcardio--------

#NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE RESIDENCIA
cardio.muni <- anom.gruposp1120 %>% 
  filter(GrupoCID10 == "Cardiopatias congênitas") %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>%
  count() 


#unindo os dados de nascidos vivos e anomalias
cardio_SIR <- cardio.muni %>% 
  right_join(nv.muni, cardio.muni, by = c('CodigoMunicipioResidencia'))


cardio_SIR <- cardio_SIR %>%
  filter(CodigoMunicipioResidencia != 420000) %>% 
  mutate(n = replace_na(n, 0)) %>% 
  rename (code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))


#calculando valor esperado
cardio_SIR$valor_esperado <- cardio_SIR$nasc_vivos * sum(cardio_SIR$n)/sum(cardio_SIR$nasc_vivos)


#calculando SIR
cardio_SIR$SIR <- cardio_SIR$n/cardio_SIR$valor_esperado


# unindo os dados com a base de mapa
cardio_SIR_shape <- dplyr::left_join(geoMunicipiosSC, cardio_SIR, by = "code_muni")



#mapa


pal <-
  colorNumeric(
    palette = "magma",
    domain = cardio_SIR_shape$SIR,
    alpha = TRUE, reverse =  TRUE
  )


y = cardio_SIR_shape$SIR

label_cardio_SIR <- cardio_SIR_shape %>%
  mutate (SIR= scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ","))




mapaSIRcardio  <- leaflet(data = cardio_SIR_shape) 

mapaSIRcardio %>%
  addPolygons (weight = 0.5, fillColor = ~pal(cardio_SIR_shape$SIR),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               SIR: </strong>%s",
                               label_cardio_SIR$name_muni, label_cardio_SIR$SIR) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = cardio_SIR_shape$SIR, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "SIR")



### BYM MODEL

# Create binary, first-order, adjacency spatial weights matrix
W <- nb2mat(poly2nb(cardio_SIR_shape), style = "B")

# MCMC parameters
M.burnin <- 10000       # Number of burn-in iterations (discarded)
M <- 5000               # Number of iterations retained

# Fit BYM model using CARBayes
set.seed(1)          # For reproducability
MCMC <- S.CARbym(
  formula = cardio_SIR_shape$n ~ 1 + offset(log(cardio_SIR_shape$valor_esperado)), 
  data = cardio_SIR_shape,
  family = "poisson",
  W = W,
  burnin = M.burnin,
  n.sample = M.burnin + M,    # Total iterations
  verbose = FALSE
)

# Broad summary of results
print(MCMC$summary.results)



# Compute posterior SIR
y.fit <- MCMC$samples$fitted
SIR <- t(t(y.fit)/cardio_SIR_shape$valor_esperado)


# Add summary statistics of the posterior SIR to map
cardio_SIR_shape$SIR.50 <- apply(SIR, 2, median)
cardio_SIR_shape$SIR.025 <- apply(SIR, 2, quantile, 0.025)
cardio_SIR_shape$SIR.975 <- apply(SIR, 2, quantile, 0.975)
cardio_SIR_shape$PP <- apply(SIR, 2, function(x) length(which(x > 1)))/M

## ---- mapabymcardio --------


pal <-
  colorNumeric(
    palette = "magma",
    domain = cardio_SIR_shape$SIR.50,
    alpha = TRUE, reverse =  TRUE
  )


label_cardio_SIR <- cardio_SIR_shape %>%
  mutate (valor_esperado= scales::number(valor_esperado, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR= scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.50= scales::number(SIR.50, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>%   
  mutate (SIR.025= scales::number(SIR.025, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.975= scales::number(SIR.975, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (PP= scales::number(PP, accuracy = 0.001, big.mark = ".", decimal.mark = ","))


mapaSIRcardio  <- leaflet(data = cardio_SIR_shape) 

mapaSIRcardio %>%
  addPolygons (weight = 0.5, fillColor = ~pal(cardio_SIR_shape$SIR.50),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<strong> %s </strong> <br/> Observado: %s <br/> Esperado: %s <br/>
        SIR: %s <br/>RR: %s (%s, %s) <br/>PP: %s",
                               label_cardio_SIR$name_muni, label_cardio_SIR$n, label_cardio_SIR$valor_esperado,
                               label_cardio_SIR$SIR, label_cardio_SIR$SIR.50, label_cardio_SIR$SIR.025, label_cardio_SIR$SIR.975,
                               label_cardio_SIR$PP) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = cardio_SIR_shape$SIR.50, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Risco Relativo")




## ---- tabelacardio --------

tabela_cardio <- cardio_SIR_shape %>% 
  st_drop_geometry()

tabela_cardio <- tabela_cardio %>% 
  dplyr::select (-code_state, -abbrev_state, -name_state, -code_region, -name_region) %>%  
  mutate (Prevalencia10000 = n/nasc_vivos*10^4) %>% 
  rename (CodigoMunicipioResidencia = code_muni) %>% 
  rename (MunicipioResidencia = name_muni) %>%
  rename (NascidosVivos = nasc_vivos) %>%
  rename (NascidosVivosAnomalia = n) %>%
  rename (ValorEsperado = valor_esperado) %>%
  rename (RR_SIR.50 = SIR.50)

tabela_cardio <- tabela_cardio %>% 
  mutate (ValorEsperado = scales::number(ValorEsperado, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR = scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (RR_SIR.50 = scales::number(RR_SIR.50, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.025 = scales::number(SIR.025, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.975 = scales::number(SIR.975, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (PP = scales::number(PP, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (Prevalencia10000 = scales::number(Prevalencia10000, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) 



## ---- dadosbymfendas--------

#NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE RESIDENCIA
fendas.muni <- anom.gruposp1120 %>% 
  filter(GrupoCID10 == "Fendas orais") %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>%
  count() 


#unindo os dados de nascidos vivos e anomalias
fendas_SIR <- fendas.muni %>% 
  right_join(nv.muni, fendas.muni, by = c('CodigoMunicipioResidencia'))


fendas_SIR <- fendas_SIR %>%
  filter(CodigoMunicipioResidencia != 420000) %>% 
  mutate(n = replace_na(n, 0)) %>% 
  rename (code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))


#calculando valor esperado
fendas_SIR$valor_esperado <- fendas_SIR$nasc_vivos * sum(fendas_SIR$n)/sum(fendas_SIR$nasc_vivos)


#calculando SIR
fendas_SIR$SIR <- fendas_SIR$n/fendas_SIR$valor_esperado


# unindo os dados com a base de mapa
fendas_SIR_shape <- dplyr::left_join(geoMunicipiosSC, fendas_SIR, by = "code_muni")



#mapa


pal <-
  colorNumeric(
    palette = "magma",
    domain = fendas_SIR_shape$SIR,
    alpha = TRUE, reverse =  TRUE
  )


y = fendas_SIR_shape$SIR

label_fendas_SIR <- fendas_SIR_shape %>%
  mutate (SIR= scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ","))




mapaSIRfendas  <- leaflet(data = fendas_SIR_shape) 

mapaSIRfendas %>%
  addPolygons (weight = 0.5, fillColor = ~pal(fendas_SIR_shape$SIR),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               SIR: </strong>%s",
                               label_fendas_SIR$name_muni, label_fendas_SIR$SIR) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = fendas_SIR_shape$SIR, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "SIR")



### BYM MODEL

# Create binary, first-order, adjacency spatial weights matrix
W <- nb2mat(poly2nb(fendas_SIR_shape), style = "B")

# MCMC parameters
M.burnin <- 10000       # Number of burn-in iterations (discarded)
M <- 5000               # Number of iterations retained

# Fit BYM model using CARBayes
set.seed(1)          # For reproducability
MCMC <- S.CARbym(
  formula = fendas_SIR_shape$n ~ 1 + offset(log(fendas_SIR_shape$valor_esperado)), 
  data = fendas_SIR_shape,
  family = "poisson",
  W = W,
  burnin = M.burnin,
  n.sample = M.burnin + M,    # Total iterations
  verbose = FALSE
)

# Broad summary of results
print(MCMC$summary.results)



# Compute posterior SIR
y.fit <- MCMC$samples$fitted
SIR <- t(t(y.fit)/fendas_SIR_shape$valor_esperado)


# Add summary statistics of the posterior SIR to map
fendas_SIR_shape$SIR.50 <- apply(SIR, 2, median)
fendas_SIR_shape$SIR.025 <- apply(SIR, 2, quantile, 0.025)
fendas_SIR_shape$SIR.975 <- apply(SIR, 2, quantile, 0.975)
fendas_SIR_shape$PP <- apply(SIR, 2, function(x) length(which(x > 1)))/M


## ---- mapabymfendas --------

pal <-
  colorNumeric(
    palette = "magma",
    domain = fendas_SIR_shape$SIR.50,
    alpha = TRUE, reverse =  TRUE
  )


label_fendas_SIR <- fendas_SIR_shape %>%
  mutate (valor_esperado= scales::number(valor_esperado, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR= scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.50= scales::number(SIR.50, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>%   
  mutate (SIR.025= scales::number(SIR.025, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.975= scales::number(SIR.975, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (PP= scales::number(PP, accuracy = 0.001, big.mark = ".", decimal.mark = ","))



mapaSIRfendas  <- leaflet(data = fendas_SIR_shape) 

mapaSIRfendas %>%
  addPolygons (weight = 0.5, fillColor = ~pal(fendas_SIR_shape$SIR.50),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<strong> %s </strong> <br/> Observado: %s <br/> Esperado: %s <br/>
        SIR: %s <br/>RR: %s (%s, %s) <br/>PP: %s",
                               label_fendas_SIR$name_muni, label_fendas_SIR$n, label_fendas_SIR$valor_esperado,
                               label_fendas_SIR$SIR, label_fendas_SIR$SIR.50, label_fendas_SIR$SIR.025, label_fendas_SIR$SIR.975,
                               label_fendas_SIR$PP) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = fendas_SIR_shape$SIR.50, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Risco Relativo")



## ---- tabelafendas --------

tabela_fendas <- fendas_SIR_shape %>% 
  st_drop_geometry()

tabela_fendas <- tabela_fendas %>% 
  dplyr::select (-code_state, -abbrev_state, -name_state, -code_region, -name_region) %>%  
  mutate (Prevalencia10000 = n/nasc_vivos*10^4) %>% 
  rename (CodigoMunicipioResidencia = code_muni) %>% 
  rename (MunicipioResidencia = name_muni) %>%
  rename (NascidosVivos = nasc_vivos) %>%
  rename (NascidosVivosAnomalia = n) %>%
  rename (ValorEsperado = valor_esperado) %>%
  rename (RR_SIR.50 = SIR.50)

tabela_fendas <- tabela_fendas %>% 
  mutate (ValorEsperado = scales::number(ValorEsperado, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR = scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (RR_SIR.50 = scales::number(RR_SIR.50, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.025 = scales::number(SIR.025, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.975 = scales::number(SIR.975, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (PP = scales::number(PP, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (Prevalencia10000 = scales::number(Prevalencia10000, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) 


## ---- dadosbymgenital--------

#NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE RESIDENCIA
genital.muni <- anom.gruposp1120 %>% 
  filter(GrupoCID10 == "Anomalias de órgãos genitais - Hipospádias" | GrupoCID10 == "Anomalias de órgãos genitais - Sexo indefinido") %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>%
  count() 


#unindo os dados de nascidos vivos e anomalias
genital_SIR <- genital.muni %>% 
  right_join(nv.muni, genital.muni, by = c('CodigoMunicipioResidencia'))


genital_SIR <- genital_SIR %>%
  filter(CodigoMunicipioResidencia != 420000) %>% 
  mutate(n = replace_na(n, 0)) %>% 
  rename (code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))


#calculando valor esperado
genital_SIR$valor_esperado <- genital_SIR$nasc_vivos * sum(genital_SIR$n)/sum(genital_SIR$nasc_vivos)


#calculando SIR
genital_SIR$SIR <- genital_SIR$n/genital_SIR$valor_esperado


# unindo os dados com a base de mapa
genital_SIR_shape <- dplyr::left_join(geoMunicipiosSC, genital_SIR, by = "code_muni")



#mapa


pal <-
  colorNumeric(
    palette = "magma",
    domain = genital_SIR_shape$SIR,
    alpha = TRUE, reverse =  TRUE
  )


y = genital_SIR_shape$SIR

label_genital_SIR <- genital_SIR_shape %>%
  mutate (SIR= scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ","))




mapaSIRgenital  <- leaflet(data = genital_SIR_shape) 

mapaSIRgenital %>%
  addPolygons (weight = 0.5, fillColor = ~pal(genital_SIR_shape$SIR),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               SIR: </strong>%s",
                               label_genital_SIR$name_muni, label_genital_SIR$SIR) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = genital_SIR_shape$SIR, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "SIR")



### BYM MODEL

# Create binary, first-order, adjacency spatial weights matrix
W <- nb2mat(poly2nb(genital_SIR_shape), style = "B")

# MCMC parameters
M.burnin <- 10000       # Number of burn-in iterations (discarded)
M <- 5000               # Number of iterations retained

# Fit BYM model using CARBayes
set.seed(1)          # For reproducability
MCMC <- S.CARbym(
  formula = genital_SIR_shape$n ~ 1 + offset(log(genital_SIR_shape$valor_esperado)), 
  data = genital_SIR_shape,
  family = "poisson",
  W = W,
  burnin = M.burnin,
  n.sample = M.burnin + M,    # Total iterations
  verbose = FALSE
)

# Broad summary of results
print(MCMC$summary.results)



# Compute posterior SIR
y.fit <- MCMC$samples$fitted
SIR <- t(t(y.fit)/genital_SIR_shape$valor_esperado)


# Add summary statistics of the posterior SIR to map
genital_SIR_shape$SIR.50 <- apply(SIR, 2, median)
genital_SIR_shape$SIR.025 <- apply(SIR, 2, quantile, 0.025)
genital_SIR_shape$SIR.975 <- apply(SIR, 2, quantile, 0.975)
genital_SIR_shape$PP <- apply(SIR, 2, function(x) length(which(x > 1)))/M

## ---- mapabymgenital --------


pal <-
  colorNumeric(
    palette = "magma",
    domain = genital_SIR_shape$SIR.50,
    alpha = TRUE, reverse =  TRUE
  )


label_genital_SIR <- genital_SIR_shape %>%
  mutate (valor_esperado= scales::number(valor_esperado, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR= scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.50= scales::number(SIR.50, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>%   
  mutate (SIR.025= scales::number(SIR.025, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.975= scales::number(SIR.975, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (PP= scales::number(PP, accuracy = 0.001, big.mark = ".", decimal.mark = ","))



mapaSIRgenital  <- leaflet(data = genital_SIR_shape) 

mapaSIRgenital %>%
  addPolygons (weight = 0.5, fillColor = ~pal(genital_SIR_shape$SIR.50),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<strong> %s </strong> <br/> Observado: %s <br/> Esperado: %s <br/>
        SIR: %s <br/>RR: %s (%s, %s) <br/>PP: %s",
                               label_genital_SIR$name_muni, label_genital_SIR$n, label_genital_SIR$valor_esperado,
                               label_genital_SIR$SIR, label_genital_SIR$SIR.50, label_genital_SIR$SIR.025, label_genital_SIR$SIR.975,
                               label_genital_SIR$PP) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = genital_SIR_shape$SIR.50, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Risco Relativo")




## ---- tabelagenital --------

tabela_genital <- genital_SIR_shape %>% 
  st_drop_geometry()

tabela_genital <- tabela_genital %>% 
  dplyr::select (-code_state, -abbrev_state, -name_state, -code_region, -name_region) %>%  
  mutate (Prevalencia10000 = n/nasc_vivos*10^4) %>% 
  rename (CodigoMunicipioResidencia = code_muni) %>% 
  rename (MunicipioResidencia = name_muni) %>%
  rename (NascidosVivos = nasc_vivos) %>%
  rename (NascidosVivosAnomalia = n) %>%
  rename (ValorEsperado = valor_esperado) %>%
  rename (RR_SIR.50 = SIR.50)

tabela_genital <- tabela_genital %>% 
  mutate (ValorEsperado = scales::number(ValorEsperado, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR = scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (RR_SIR.50 = scales::number(RR_SIR.50, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.025 = scales::number(SIR.025, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.975 = scales::number(SIR.975, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (PP = scales::number(PP, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (Prevalencia10000 = scales::number(Prevalencia10000, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) 




## ---- dadosbymhipospadias--------

#NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE RESIDENCIA
hipospadias.muni <- anom.gruposp1120 %>% 
  filter(GrupoCID10 == "Anomalias de órgãos genitais - Hipospádias") %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>%
  count() 


#unindo os dados de nascidos vivos e anomalias
hipospadias_SIR <- hipospadias.muni %>% 
  right_join(nv.muni, hipospadias.muni, by = c('CodigoMunicipioResidencia'))


hipospadias_SIR <- hipospadias_SIR %>%
  filter(CodigoMunicipioResidencia != 420000) %>% 
  mutate(n = replace_na(n, 0)) %>% 
  rename (code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))


#calculando valor esperado
hipospadias_SIR$valor_esperado <- hipospadias_SIR$nasc_vivos * sum(hipospadias_SIR$n)/sum(hipospadias_SIR$nasc_vivos)


#calculando SIR
hipospadias_SIR$SIR <- hipospadias_SIR$n/hipospadias_SIR$valor_esperado


# unindo os dados com a base de mapa
hipospadias_SIR_shape <- dplyr::left_join(geoMunicipiosSC, hipospadias_SIR, by = "code_muni")



#mapa


pal <-
  colorNumeric(
    palette = "magma",
    domain = hipospadias_SIR_shape$SIR,
    alpha = TRUE, reverse =  TRUE
  )


y = hipospadias_SIR_shape$SIR

label_hipospadias_SIR <- hipospadias_SIR_shape %>%
  mutate (SIR= scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ","))




mapaSIRhipospadias  <- leaflet(data = hipospadias_SIR_shape) 

mapaSIRhipospadias %>%
  addPolygons (weight = 0.5, fillColor = ~pal(hipospadias_SIR_shape$SIR),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               SIR: </strong>%s",
                               label_hipospadias_SIR$name_muni, label_hipospadias_SIR$SIR) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = hipospadias_SIR_shape$SIR, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "SIR")



### BYM MODEL

# Create binary, first-order, adjacency spatial weights matrix
W <- nb2mat(poly2nb(hipospadias_SIR_shape), style = "B")

# MCMC parameters
M.burnin <- 10000       # Number of burn-in iterations (discarded)
M <- 5000               # Number of iterations retained

# Fit BYM model using CARBayes
set.seed(1)          # For reproducability
MCMC <- S.CARbym(
  formula = hipospadias_SIR_shape$n ~ 1 + offset(log(hipospadias_SIR_shape$valor_esperado)), 
  data = hipospadias_SIR_shape,
  family = "poisson",
  W = W,
  burnin = M.burnin,
  n.sample = M.burnin + M,    # Total iterations
  verbose = FALSE
)

# Broad summary of results
print(MCMC$summary.results)



# Compute posterior SIR
y.fit <- MCMC$samples$fitted
SIR <- t(t(y.fit)/hipospadias_SIR_shape$valor_esperado)


# Add summary statistics of the posterior SIR to map
hipospadias_SIR_shape$SIR.50 <- apply(SIR, 2, median)
hipospadias_SIR_shape$SIR.025 <- apply(SIR, 2, quantile, 0.025)
hipospadias_SIR_shape$SIR.975 <- apply(SIR, 2, quantile, 0.975)
hipospadias_SIR_shape$PP <- apply(SIR, 2, function(x) length(which(x > 1)))/M

## ---- mapabymhipospadias --------


pal <-
  colorNumeric(
    palette = "magma",
    domain = hipospadias_SIR_shape$SIR.50,
    alpha = TRUE, reverse =  TRUE
  )


label_hipospadias_SIR <- hipospadias_SIR_shape %>%
  mutate (valor_esperado= scales::number(valor_esperado, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR= scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.50= scales::number(SIR.50, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>%   
  mutate (SIR.025= scales::number(SIR.025, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.975= scales::number(SIR.975, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (PP= scales::number(PP, accuracy = 0.001, big.mark = ".", decimal.mark = ","))



mapaSIRhipospadias  <- leaflet(data = hipospadias_SIR_shape) 

mapaSIRhipospadias %>%
  addPolygons (weight = 0.5, fillColor = ~pal(hipospadias_SIR_shape$SIR.50),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<strong> %s </strong> <br/> Observado: %s <br/> Esperado: %s <br/>
        SIR: %s <br/>RR: %s (%s, %s) <br/>PP: %s",
                               label_hipospadias_SIR$name_muni, label_hipospadias_SIR$n, label_hipospadias_SIR$valor_esperado,
                               label_hipospadias_SIR$SIR, label_hipospadias_SIR$SIR.50, label_hipospadias_SIR$SIR.025, label_hipospadias_SIR$SIR.975,
                               label_hipospadias_SIR$PP) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = hipospadias_SIR_shape$SIR.50, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Risco Relativo")




## ---- tabelahipospadias --------

tabela_hipospadias <- hipospadias_SIR_shape %>% 
  st_drop_geometry()

tabela_hipospadias <- tabela_hipospadias %>% 
  dplyr::select (-code_state, -abbrev_state, -name_state, -code_region, -name_region) %>%  
  mutate (Prevalencia10000 = n/nasc_vivos*10^4) %>% 
  rename (CodigoMunicipioResidencia = code_muni) %>% 
  rename (MunicipioResidencia = name_muni) %>%
  rename (NascidosVivos = nasc_vivos) %>%
  rename (NascidosVivosAnomalia = n) %>%
  rename (ValorEsperado = valor_esperado) %>%
  rename (RR_SIR.50 = SIR.50)

tabela_hipospadias <- tabela_hipospadias %>% 
  mutate (ValorEsperado = scales::number(ValorEsperado, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR = scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (RR_SIR.50 = scales::number(RR_SIR.50, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.025 = scales::number(SIR.025, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.975 = scales::number(SIR.975, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (PP = scales::number(PP, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (Prevalencia10000 = scales::number(Prevalencia10000, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) 



## ---- dadosbymSI--------

#NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE RESIDENCIA
SI.muni <- anom.gruposp1120 %>% 
  filter(GrupoCID10 == "Anomalias de órgãos genitais - Sexo indefinido") %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>%
  count() 


#unindo os dados de nascidos vivos e anomalias
SI_SIR <- SI.muni %>% 
  right_join(nv.muni, SI.muni, by = c('CodigoMunicipioResidencia'))


SI_SIR <- SI_SIR %>%
  filter(CodigoMunicipioResidencia != 420000) %>% 
  mutate(n = replace_na(n, 0)) %>% 
  rename (code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))


#calculando valor esperado
SI_SIR$valor_esperado <- SI_SIR$nasc_vivos * sum(SI_SIR$n)/sum(SI_SIR$nasc_vivos)


#calculando SIR
SI_SIR$SIR <- SI_SIR$n/SI_SIR$valor_esperado


# unindo os dados com a base de mapa
SI_SIR_shape <- dplyr::left_join(geoMunicipiosSC, SI_SIR, by = "code_muni")



#mapa


pal <-
  colorNumeric(
    palette = "magma",
    domain = SI_SIR_shape$SIR,
    alpha = TRUE, reverse =  TRUE
  )


y = SI_SIR_shape$SIR

label_SI_SIR <- SI_SIR_shape %>%
  mutate (SIR= scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ","))




mapaSIRSI  <- leaflet(data = SI_SIR_shape) 

mapaSIRSI %>%
  addPolygons (weight = 0.5, fillColor = ~pal(SI_SIR_shape$SIR),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               SIR: </strong>%s",
                               label_SI_SIR$name_muni, label_SI_SIR$SIR) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = SI_SIR_shape$SIR, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "SIR")



### BYM MODEL

# Create binary, first-order, adjacency spatial weights matrix
W <- nb2mat(poly2nb(SI_SIR_shape), style = "B")

# MCMC parameters
M.burnin <- 10000       # Number of burn-in iterations (discarded)
M <- 5000               # Number of iterations retained

# Fit BYM model using CARBayes
set.seed(1)          # For reproducability
MCMC <- S.CARbym(
  formula = SI_SIR_shape$n ~ 1 + offset(log(SI_SIR_shape$valor_esperado)), 
  data = SI_SIR_shape,
  family = "poisson",
  W = W,
  burnin = M.burnin,
  n.sample = M.burnin + M,    # Total iterations
  verbose = FALSE
)

# Broad summary of results
print(MCMC$summary.results)



# Compute posterior SIR
y.fit <- MCMC$samples$fitted
SIR <- t(t(y.fit)/SI_SIR_shape$valor_esperado)


# Add summary statistics of the posterior SIR to map
SI_SIR_shape$SIR.50 <- apply(SIR, 2, median)
SI_SIR_shape$SIR.025 <- apply(SIR, 2, quantile, 0.025)
SI_SIR_shape$SIR.975 <- apply(SIR, 2, quantile, 0.975)
SI_SIR_shape$PP <- apply(SIR, 2, function(x) length(which(x > 1)))/M

## ---- mapabymSI --------


pal <-
  colorNumeric(
    palette = "magma",
    domain = SI_SIR_shape$SIR.50,
    alpha = TRUE, reverse =  TRUE
  )


label_SI_SIR <- SI_SIR_shape %>%
  mutate (valor_esperado= scales::number(valor_esperado, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR= scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.50= scales::number(SIR.50, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>%   
  mutate (SIR.025= scales::number(SIR.025, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.975= scales::number(SIR.975, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (PP= scales::number(PP, accuracy = 0.001, big.mark = ".", decimal.mark = ","))



mapaSIRSI  <- leaflet(data = SI_SIR_shape) 

mapaSIRSI %>%
  addPolygons (weight = 0.5, fillColor = ~pal(SI_SIR_shape$SIR.50),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<strong> %s </strong> <br/> Observado: %s <br/> Esperado: %s <br/>
        SIR: %s <br/>RR: %s (%s, %s) <br/>PP: %s",
                               label_SI_SIR$name_muni, label_SI_SIR$n, label_SI_SIR$valor_esperado,
                               label_SI_SIR$SIR, label_SI_SIR$SIR.50, label_SI_SIR$SIR.025, label_SI_SIR$SIR.975,
                               label_SI_SIR$PP) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = SI_SIR_shape$SIR.50, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Risco Relativo")




## ---- tabelaSI --------

tabela_SI <- SI_SIR_shape %>% 
  st_drop_geometry()

tabela_SI <- tabela_SI %>% 
  dplyr::select (-code_state, -abbrev_state, -name_state, -code_region, -name_region) %>%  
  mutate (Prevalencia10000 = n/nasc_vivos*10^4) %>% 
  rename (CodigoMunicipioResidencia = code_muni) %>% 
  rename (MunicipioResidencia = name_muni) %>%
  rename (NascidosVivos = nasc_vivos) %>%
  rename (NascidosVivosAnomalia = n) %>%
  rename (ValorEsperado = valor_esperado) %>%
  rename (RR_SIR.50 = SIR.50)

tabela_SI <- tabela_SI %>% 
  mutate (ValorEsperado = scales::number(ValorEsperado, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR = scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (RR_SIR.50 = scales::number(RR_SIR.50, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.025 = scales::number(SIR.025, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.975 = scales::number(SIR.975, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (PP = scales::number(PP, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (Prevalencia10000 = scales::number(Prevalencia10000, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) 






## ---- dadosbymabdominal--------

#NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE RESIDENCIA
abdominal.muni <- anom.gruposp1120 %>% 
  filter(GrupoCID10 == "Defeitos da parede abdominal") %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>%
  count() 


#unindo os dados de nascidos vivos e anomalias
abdominal_SIR <- abdominal.muni %>% 
  right_join(nv.muni, abdominal.muni, by = c('CodigoMunicipioResidencia'))


abdominal_SIR <- abdominal_SIR %>%
  filter(CodigoMunicipioResidencia != 420000) %>% 
  mutate(n = replace_na(n, 0)) %>% 
  rename (code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))


#calculando valor esperado
abdominal_SIR$valor_esperado <- abdominal_SIR$nasc_vivos * sum(abdominal_SIR$n)/sum(abdominal_SIR$nasc_vivos)


#calculando SIR
abdominal_SIR$SIR <- abdominal_SIR$n/abdominal_SIR$valor_esperado


# unindo os dados com a base de mapa
abdominal_SIR_shape <- dplyr::left_join(geoMunicipiosSC, abdominal_SIR, by = "code_muni")



#mapa


pal <-
  colorNumeric(
    palette = "magma",
    domain = abdominal_SIR_shape$SIR,
    alpha = TRUE, reverse =  TRUE
  )


y = abdominal_SIR_shape$SIR

label_abdominal_SIR <- abdominal_SIR_shape %>%
  mutate (SIR= scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ","))




mapaSIRabdominal  <- leaflet(data = abdominal_SIR_shape) 

mapaSIRabdominal %>%
  addPolygons (weight = 0.5, fillColor = ~pal(abdominal_SIR_shape$SIR),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               SIR: </strong>%s",
                               label_abdominal_SIR$name_muni, label_abdominal_SIR$SIR) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = abdominal_SIR_shape$SIR, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "SIR")



### BYM MODEL

# Create binary, first-order, adjacency spatial weights matrix
W <- nb2mat(poly2nb(abdominal_SIR_shape), style = "B")

# MCMC parameters
M.burnin <- 10000       # Number of burn-in iterations (discarded)
M <- 5000               # Number of iterations retained

# Fit BYM model using CARBayes
set.seed(1)          # For reproducability
MCMC <- S.CARbym(
  formula = abdominal_SIR_shape$n ~ 1 + offset(log(abdominal_SIR_shape$valor_esperado)), 
  data = abdominal_SIR_shape,
  family = "poisson",
  W = W,
  burnin = M.burnin,
  n.sample = M.burnin + M,    # Total iterations
  verbose = FALSE
)

# Broad summary of results
print(MCMC$summary.results)



# Compute posterior SIR
y.fit <- MCMC$samples$fitted
SIR <- t(t(y.fit)/abdominal_SIR_shape$valor_esperado)


# Add summary statistics of the posterior SIR to map
abdominal_SIR_shape$SIR.50 <- apply(SIR, 2, median)
abdominal_SIR_shape$SIR.025 <- apply(SIR, 2, quantile, 0.025)
abdominal_SIR_shape$SIR.975 <- apply(SIR, 2, quantile, 0.975)
abdominal_SIR_shape$PP <- apply(SIR, 2, function(x) length(which(x > 1)))/M


## ---- mapabymabdominal --------

pal <-
  colorNumeric(
    palette = "magma",
    domain = abdominal_SIR_shape$SIR.50,
    alpha = TRUE, reverse =  TRUE
  )


label_abdominal_SIR <- abdominal_SIR_shape %>%
  mutate (valor_esperado= scales::number(valor_esperado, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR= scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.50= scales::number(SIR.50, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>%   
  mutate (SIR.025= scales::number(SIR.025, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.975= scales::number(SIR.975, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (PP= scales::number(PP, accuracy = 0.001, big.mark = ".", decimal.mark = ","))


mapaSIRabdominal  <- leaflet(data = abdominal_SIR_shape) 

mapaSIRabdominal %>%
  addPolygons (weight = 0.5, fillColor = ~pal(abdominal_SIR_shape$SIR.50),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<strong> %s </strong> <br/> Observado: %s <br/> Esperado: %s <br/>
        SIR: %s <br/>RR: %s (%s, %s) <br/>PP: %s",
                               label_abdominal_SIR$name_muni, label_abdominal_SIR$n, label_abdominal_SIR$valor_esperado,
                               label_abdominal_SIR$SIR, label_abdominal_SIR$SIR.50, label_abdominal_SIR$SIR.025, label_abdominal_SIR$SIR.975,
                               label_abdominal_SIR$PP) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = abdominal_SIR_shape$SIR.50, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Risco Relativo")




## ---- tabelaabdominal --------

tabela_abdominal <- abdominal_SIR_shape %>% 
  st_drop_geometry()

tabela_abdominal <- tabela_abdominal %>% 
  dplyr::select (-code_state, -abbrev_state, -name_state, -code_region, -name_region) %>%  
  mutate (Prevalencia10000 = n/nasc_vivos*10^4) %>% 
  rename (CodigoMunicipioResidencia = code_muni) %>% 
  rename (MunicipioResidencia = name_muni) %>%
  rename (NascidosVivos = nasc_vivos) %>%
  rename (NascidosVivosAnomalia = n) %>%
  rename (ValorEsperado = valor_esperado) %>%
  rename (RR_SIR.50 = SIR.50)

tabela_abdominal <- tabela_abdominal %>% 
  mutate (ValorEsperado = scales::number(ValorEsperado, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR = scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (RR_SIR.50 = scales::number(RR_SIR.50, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.025 = scales::number(SIR.025, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.975 = scales::number(SIR.975, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (PP = scales::number(PP, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (Prevalencia10000 = scales::number(Prevalencia10000, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) 




## ---- dadosbymdown--------

#NASCIDOS VIVOS COM ANOMALIAS POR MUNICIPIO DE RESIDENCIA
down.muni <- anom.gruposp1120 %>% 
  filter(GrupoCID10 == "Síndrome de Down") %>%
  group_by(CodigoMunicipioResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(CodigoMunicipioResidencia) %>%
  count() 


#unindo os dados de nascidos vivos e anomalias
down_SIR <- down.muni %>% 
  right_join(nv.muni, down.muni, by = c('CodigoMunicipioResidencia'))


down_SIR <- down_SIR %>%
  filter(CodigoMunicipioResidencia != 420000) %>% 
  mutate(n = replace_na(n, 0)) %>% 
  rename (code_muni = CodigoMunicipioResidencia) %>% 
  mutate(code_muni = as.character(code_muni))


#calculando valor esperado
down_SIR$valor_esperado <- down_SIR$nasc_vivos * sum(down_SIR$n)/sum(down_SIR$nasc_vivos)


#calculando SIR
down_SIR$SIR <- down_SIR$n/down_SIR$valor_esperado


# unindo os dados com a base de mapa
down_SIR_shape <- dplyr::left_join(geoMunicipiosSC, down_SIR, by = "code_muni")



#mapa


pal <-
  colorNumeric(
    palette = "magma",
    domain = down_SIR_shape$SIR,
    alpha = TRUE, reverse =  TRUE
  )


y = down_SIR_shape$SIR

label_down_SIR <- down_SIR_shape %>%
  mutate (SIR= scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ","))




mapaSIRdown  <- leaflet(data = down_SIR_shape) 

mapaSIRdown %>%
  addPolygons (weight = 0.5, fillColor = ~pal(down_SIR_shape$SIR),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<p><strong>Município de residência: </strong>%s<br/><strong>
                               SIR: </strong>%s",
                               label_down_SIR$name_muni, label_down_SIR$SIR) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = down_SIR_shape$SIR, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "SIR")



### BYM MODEL

# Create binary, first-order, adjacency spatial weights matrix
W <- nb2mat(poly2nb(down_SIR_shape), style = "B")

# MCMC parameters
M.burnin <- 10000       # Number of burn-in iterations (discarded)
M <- 5000               # Number of iterations retained

# Fit BYM model using CARBayes
set.seed(1)          # For reproducability
MCMC <- S.CARbym(
  formula = down_SIR_shape$n ~ 1 + offset(log(down_SIR_shape$valor_esperado)), 
  data = down_SIR_shape,
  family = "poisson",
  W = W,
  burnin = M.burnin,
  n.sample = M.burnin + M,    # Total iterations
  verbose = FALSE
)

# Broad summary of results
print(MCMC$summary.results)



# Compute posterior SIR
y.fit <- MCMC$samples$fitted
SIR <- t(t(y.fit)/down_SIR_shape$valor_esperado)


# Add summary statistics of the posterior SIR to map
down_SIR_shape$SIR.50 <- apply(SIR, 2, median)
down_SIR_shape$SIR.025 <- apply(SIR, 2, quantile, 0.025)
down_SIR_shape$SIR.975 <- apply(SIR, 2, quantile, 0.975)
down_SIR_shape$PP <- apply(SIR, 2, function(x) length(which(x > 1)))/M


## ---- mapabymdown --------

pal <-
  colorNumeric(
    palette = "magma",
    domain = down_SIR_shape$SIR.50,
    alpha = TRUE, reverse =  TRUE
  )


label_down_SIR <- down_SIR_shape %>%
  mutate (valor_esperado= scales::number(valor_esperado, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR= scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.50= scales::number(SIR.50, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>%   
  mutate (SIR.025= scales::number(SIR.025, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.975= scales::number(SIR.975, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (PP= scales::number(PP, accuracy = 0.001, big.mark = ".", decimal.mark = ","))


mapaSIRdown  <- leaflet(data = down_SIR_shape) 

mapaSIRdown %>%
  addPolygons (weight = 0.5, fillColor = ~pal(down_SIR_shape$SIR.50),
               color = "black", fillOpacity = 1.0,
               smoothFactor = 1,
               highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE),
               label = sprintf("<strong> %s </strong> <br/> Observado: %s <br/> Esperado: %s <br/>
        SIR: %s <br/>RR: %s (%s, %s) <br/>PP: %s",
                               label_down_SIR$name_muni, label_down_SIR$n, label_down_SIR$valor_esperado,
                               label_down_SIR$SIR, label_down_SIR$SIR.50, label_down_SIR$SIR.025, label_down_SIR$SIR.975,
                               label_down_SIR$PP) %>% 
                 lapply(htmltools::HTML),
               labelOptions = labelOptions(textsize = "14px",
                                           opacity = 0.8, direction = "bottom")) %>%
  addLegend(position = "bottomleft", pal = pal, values = down_SIR_shape$SIR.50, na.label = "Sem registro", opacity = 1, labFormat = labelFormat(big.mark = "."), title = "Risco Relativo")



## ---- tabeladown --------

tabela_down <- down_SIR_shape %>% 
  st_drop_geometry()

tabela_down <- tabela_down %>% 
  dplyr::select (-code_state, -abbrev_state, -name_state, -code_region, -name_region) %>%  
  mutate (Prevalencia10000 = n/nasc_vivos*10^4) %>% 
  rename (CodigoMunicipioResidencia = code_muni) %>% 
  rename (MunicipioResidencia = name_muni) %>%
  rename (NascidosVivos = nasc_vivos) %>%
  rename (NascidosVivosAnomalia = n) %>%
  rename (ValorEsperado = valor_esperado) %>%
  rename (RR_SIR.50 = SIR.50)

tabela_down <- tabela_down %>% 
  mutate (ValorEsperado = scales::number(ValorEsperado, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR = scales::number(SIR, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (RR_SIR.50 = scales::number(RR_SIR.50, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.025 = scales::number(SIR.025, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (SIR.975 = scales::number(SIR.975, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (PP = scales::number(PP, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (Prevalencia10000 = scales::number(Prevalencia10000, accuracy = 0.001, big.mark = ".", decimal.mark = ",")) 








