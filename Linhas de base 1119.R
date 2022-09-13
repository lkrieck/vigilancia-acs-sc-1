
## SERIES TEMPORAIS

## ---- biblioteca_linha --------
library(ggplot2)
library(tidyverse)
library(viridis)
library(epiR)
library(scales)
library(readr)



## ---- analises_linha1119 --------
##### ANALISES

bancop1119 <- read_csv2("banco_limpeza.csv", locale = locale(encoding = "ISO8859-1")) 

bancop1119 <- bancop1119 %>% 
  filter(AnoNascimento != 2020)




bancop1119$UFResidencia <- "SC"


#NASCIDOS VIVOS POR ANO
nv.ano1119 <- bancop1119 %>%
  group_by(UFResidencia, NumeroIdentificacao) %>%
  count() %>% 
  group_by(UFResidencia) %>%
  count() %>% 
  rename(nasc_vivos = n) 



#NV COM ANOMALIAS SELECIONADAS
nv.anom.ano1119 <- bancop1119 %>% 
  filter(str_detect(CID10Anomalia,"Q000|Q001|Q002|Q01|Q05|Q02|Q20|Q21|Q22|Q23|Q24|Q25|Q26|Q27|Q28|Q35|Q36|Q37|Q54|Q56|Q66|Q69|Q71|Q72|Q73|Q743|Q792|Q793|Q90")) %>% 
  group_by(UFResidencia, NumeroIdentificacao) %>%
  count() %>% 
  group_by(UFResidencia) %>%
  count()


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.ano1119 <- nv.anom.ano1119 %>% 
  left_join(nv.ano1119, nv.anom.ano1119, by = 'UFResidencia')

#calculando prevalencia
epiprev.ano1119 <-epi.prev(prev.ano1119$n, prev.ano1119$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
epiprev.ano1119_2 <- epiprev.ano1119[["ap"]]

#unindo os dados de prevalencia
prev.ano1119 <- prev.ano1119 %>% 
  bind_cols(epiprev.ano1119_2) %>% 
  mutate(est = round(est, 2))



#GRUPOS
#criando variavel de grupo
banco.anom.grupos1119 <- bancop1119 %>% 
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



#NV TUBO NEURAL
nv.tuboneural.ano1119 <- banco.anom.grupos1119 %>%
  filter(GrupoCID10 == "Defeitos de tubo neural") %>%
  group_by(UFResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(UFResidencia) %>%
  count()


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.tuboneural.ano1119 <- nv.tuboneural.ano1119 %>% 
  left_join(nv.ano1119, nv.tuboneural.ano1119, by = 'UFResidencia')

#calculando prevalencia
epiprev.tuboneural.ano1119 <-epi.prev(prev.tuboneural.ano1119$n, prev.tuboneural.ano1119$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.tuboneural.ano1119_2 <- epiprev.tuboneural.ano1119[["ap"]]

#unindo os dados de prevalencia
prev.tuboneural.ano1119 <- prev.tuboneural.ano1119 %>% 
  bind_cols(prev.tuboneural.ano1119_2) %>% 
  mutate(est = round(est, 2))



#NV MICROCEFALIA
nv.microce.ano1119 <- banco.anom.grupos1119 %>%
  filter(GrupoCID10 == "Microcefalia") %>%
  group_by(UFResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(UFResidencia) %>%
  count() 


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.microce.ano1119 <- nv.microce.ano1119 %>% 
  left_join(nv.ano1119, nv.microce.ano1119, by = 'UFResidencia')

#calculando prevalencia
epiprev.microce.ano1119 <-epi.prev(prev.microce.ano1119$n, prev.microce.ano1119$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.microce.ano1119_2 <- epiprev.microce.ano1119[["ap"]]

#unindo os dados de prevalencia
prev.microce.ano1119 <- prev.microce.ano1119 %>% 
  bind_cols(prev.microce.ano1119_2) %>% 
  mutate(est = round(est, 2))



#NV CARDIOPATIA 
nv.cardio.ano1119 <- banco.anom.grupos1119 %>%
  filter(GrupoCID10 == "Cardiopatias congênitas") %>%
  group_by(UFResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(UFResidencia) %>%
  count()


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.cardio.ano1119 <- nv.cardio.ano1119 %>% 
  left_join(nv.ano1119, nv.cardio.ano1119, by = 'UFResidencia')

#calculando prevalencia
epiprev.cardio.ano1119 <-epi.prev(prev.cardio.ano1119$n, prev.cardio.ano1119$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.cardio.ano1119_2 <- epiprev.cardio.ano1119[["ap"]]

#unindo os dados de prevalencia
prev.cardio.ano1119 <- prev.cardio.ano1119 %>% 
  bind_cols(prev.cardio.ano1119_2) %>% 
  mutate(est = round(est, 2))




#NV FENDAS
nv.fendas.ano1119 <- banco.anom.grupos1119 %>%
  filter(GrupoCID10 == "Fendas orais") %>%
  group_by(UFResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(UFResidencia) %>%
  count()

#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.fendas.ano1119 <- nv.fendas.ano1119 %>% 
  left_join(nv.ano1119, nv.fendas.ano1119, by = 'UFResidencia')

#calculando prevalencia
epiprev.fendas.ano1119 <-epi.prev(prev.fendas.ano1119$n, prev.fendas.ano1119$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.fendas.ano1119_2 <- epiprev.fendas.ano1119[["ap"]]

#unindo os dados de prevalencia
prev.fendas.ano1119 <- prev.fendas.ano1119 %>% 
  bind_cols(prev.fendas.ano1119_2) %>% 
  mutate(est = round(est, 2))




#NV GENITAIS
nv.genitais.ano1119 <- banco.anom.grupos1119 %>%
  filter(GrupoCID10 == "Anomalias de órgãos genitais - Hipospádias" | GrupoCID10 == "Anomalias de órgãos genitais - Sexo indefinido") %>%
  group_by(UFResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(UFResidencia) %>%
  count()

#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.genitais.ano1119 <- nv.genitais.ano1119 %>% 
  left_join(nv.ano1119, nv.genitais.ano1119, by = 'UFResidencia')

#calculando prevalencia
epiprev.genitais.ano1119 <-epi.prev(prev.genitais.ano1119$n, prev.genitais.ano1119$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.genitais.ano1119_2 <- epiprev.genitais.ano1119[["ap"]]

#unindo os dados de prevalencia
prev.genitais.ano1119 <- prev.genitais.ano1119 %>% 
  bind_cols(prev.genitais.ano1119_2) %>% 
  mutate(est = round(est, 2))




#NV MEMBROS
nv.membros.ano1119 <- banco.anom.grupos1119 %>%
  filter(GrupoCID10 == "Defeitos de membros") %>%
  group_by(UFResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(UFResidencia) %>%
  count()


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.membros.ano1119 <- nv.membros.ano1119 %>% 
  left_join(nv.ano1119, nv.membros.ano1119, by = 'UFResidencia')

#calculando prevalencia
epiprev.membros.ano1119 <-epi.prev(prev.membros.ano1119$n, prev.membros.ano1119$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.membros.ano1119_2 <- epiprev.membros.ano1119[["ap"]]

#unindo os dados de prevalencia
prev.membros.ano1119 <- prev.membros.ano1119 %>% 
  bind_cols(prev.membros.ano1119_2) %>% 
  mutate(est = round(est, 2))



#NV DEFEITOS ABDOMINAIS
nv.abdominal.ano1119 <- banco.anom.grupos1119 %>%
  filter(GrupoCID10 == "Defeitos da parede abdominal") %>%
  group_by(UFResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(UFResidencia) %>%
  count()

#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.abdominal.ano1119 <- nv.abdominal.ano1119 %>% 
  left_join(nv.ano1119, nv.abdominal.ano1119, by = 'UFResidencia')

#calculando prevalencia
epiprev.abdominal.ano1119 <-epi.prev(prev.abdominal.ano1119$n, prev.abdominal.ano1119$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.abdominal.ano1119_2 <- epiprev.abdominal.ano1119[["ap"]]

#unindo os dados de prevalencia
prev.abdominal.ano1119 <- prev.abdominal.ano1119 %>% 
  bind_cols(prev.abdominal.ano1119_2) %>% 
  mutate(est = round(est, 2))



#NV SÍNDROME DE DOWN
nv.down.ano1119 <- banco.anom.grupos1119 %>%
  filter(GrupoCID10 == "Síndrome de Down") %>%
  group_by(UFResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(UFResidencia) %>%
  count()


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.down.ano1119 <- nv.down.ano1119 %>% 
  left_join(nv.ano1119, nv.down.ano1119, by = 'UFResidencia')

#calculando prevalencia
epiprev.down.ano1119 <-epi.prev(prev.down.ano1119$n, prev.down.ano1119$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.down.ano1119_2 <- epiprev.down.ano1119[["ap"]]

#unindo os dados de prevalencia
prev.down.ano1119 <- prev.down.ano1119 %>% 
  bind_cols(prev.down.ano1119_2) %>% 
  mutate(est = round(est, 2))



#NV HIPOSPADIAS
nv.hipospadias.ano1119 <- banco.anom.grupos1119 %>%
  filter(GrupoCID10 == "Anomalias de órgãos genitais - Hipospádias") %>%
  group_by(UFResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(UFResidencia) %>%
  count()

#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.hipospadias.ano1119 <- nv.hipospadias.ano1119 %>% 
  left_join(nv.ano1119, nv.hipospadias.ano1119, by = 'UFResidencia')

#calculando prevalencia
epiprev.hipospadias.ano1119 <-epi.prev(prev.hipospadias.ano1119$n, prev.hipospadias.ano1119$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.hipospadias.ano1119_2 <- epiprev.hipospadias.ano1119[["ap"]]

#unindo os dados de prevalencia
prev.hipospadias.ano1119 <- prev.hipospadias.ano1119 %>% 
  bind_cols(prev.hipospadias.ano1119_2) %>% 
  mutate(est = round(est, 2))


#NV SI
nv.SI.ano1119 <- banco.anom.grupos1119 %>%
  filter(GrupoCID10 == "Anomalias de órgãos genitais - Sexo indefinido") %>%
  group_by(UFResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(UFResidencia) %>%
  count()

#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.SI.ano1119 <- nv.SI.ano1119 %>% 
  left_join(nv.ano1119, nv.SI.ano1119, by = 'UFResidencia')

#calculando prevalencia
epiprev.SI.ano1119 <-epi.prev(prev.SI.ano1119$n, prev.SI.ano1119$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.SI.ano1119_2 <- epiprev.SI.ano1119[["ap"]]

#unindo os dados de prevalencia
prev.SI.ano1119 <- prev.SI.ano1119 %>% 
  bind_cols(prev.SI.ano1119_2) %>% 
  mutate(est = round(est, 2))




#GRAFICOS

#TABELAS
#SELECIONADAS
tab_prev.ano1119 <- prev.ano1119 %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.ano1119$Variavel <- "Todas anomalias selecionadas"


#TUBO NEURAL
tab_prev.tuboneural.ano1119 <- prev.tuboneural.ano1119 %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.tuboneural.ano1119$Variavel <- "Defeitos de tubo neural"


#MICROCEFALIA
tab_prev.microce.ano1119 <- prev.microce.ano1119 %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.microce.ano1119$Variavel <- "Microcefalia"


#CARDIOPATIA
tab_prev.cardio.ano1119 <- prev.cardio.ano1119 %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.cardio.ano1119$Variavel <- "Cardiopatias congênitas"


#GENITAIS
tab_prev.genitais.ano1119 <- prev.genitais.ano1119 %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.genitais.ano1119$Variavel <- "Anomalias de órgãos genitais"

#HIPOSPADIAS
tab_prev.hipospadias.ano1119 <- prev.hipospadias.ano1119 %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.hipospadias.ano1119$Variavel <- "Anomalias de órgãos genitais - Hipospádias"

#SI
tab_prev.SI.ano1119 <- prev.SI.ano1119 %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.SI.ano1119$Variavel <- "Anomalias de órgãos genitais - Sexo indefinido"

#MEMBROS
tab_prev.membros.ano1119 <- prev.membros.ano1119 %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.membros.ano1119$Variavel <- "Defeitos de membros"


#FENDAS
tab_prev.fendas.ano1119 <- prev.fendas.ano1119 %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.fendas.ano1119$Variavel <- "Fendas orais"


#ABDOMINAL
tab_prev.abdominal.ano1119 <- prev.abdominal.ano1119 %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.abdominal.ano1119$Variavel <- "Defeitos da parede abdominal"



#DOWN
tab_prev.down.ano1119 <- prev.down.ano1119 %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.down.ano1119$Variavel <- "Síndrome de Down"


tab_grafico.lb1119 <- tab_prev.ano1119 %>% 
  rbind(tab_prev.tuboneural.ano1119) %>%
  rbind(tab_prev.cardio.ano1119) %>%
  rbind(tab_prev.membros.ano1119) %>%
  rbind(tab_prev.fendas.ano1119) %>%
  rbind(tab_prev.genitais.ano1119) %>%
  rbind(tab_prev.hipospadias.ano1119) %>%
  rbind(tab_prev.SI.ano1119) %>%
  rbind(tab_prev.abdominal.ano1119) %>%
  rbind(tab_prev.microce.ano1119) %>%
  rbind(tab_prev.down.ano1119)


## ---- tabela_linha.de.base1119 --------
tabela_linha.de.base1119 <- tab_grafico.lb1119 %>% 
  mutate (NascidosVivos = scales::number(NascidosVivos, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (Prevalencia10000 = scales::number(Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>%
  mutate (lower= scales::number(lower, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>%
  mutate (upper = scales::number(upper, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>%
  unite (col = IC95, lower:upper, sep = "-")
