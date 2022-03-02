## SERIES TEMPORAIS

## ---- biblioteca_linha --------
library(ggplot2)
library(tidyverse)
library(viridis)
library(epiR)
library(scales)
library(readr)



## ---- analises_linha --------
##### ANALISES

bancop1119 <- read_csv2("banco_limpeza.csv", locale = locale(encoding = "ISO8859-1")) %>% 
  filter(AnoNascimento != 2020)


bancop1119$UFResidencia <- "SC"


#NASCIDOS VIVOS POR ANO
nv.ano <- bancop1119 %>%
  group_by(UFResidencia, NumeroIdentificacao) %>%
  count() %>% 
  group_by(UFResidencia) %>%
  count() %>% 
  rename(nasc_vivos = n) 



#NV COM ANOMALIAS SELECIONADAS
nv.anom.ano <- bancop1119 %>% 
  filter(str_detect(CID10Anomalia,"Q000|Q001|Q002|Q01|Q05|Q02|Q20|Q21|Q22|Q23|Q24|Q25|Q26|Q27|Q28|Q35|Q36|Q37|Q54|Q56|Q66|Q69|Q71|Q72|Q73|Q743|Q792|Q793|Q90")) %>% 
  group_by(UFResidencia, NumeroIdentificacao) %>%
  count() %>% 
  group_by(UFResidencia) %>%
  count()


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.ano <- nv.anom.ano %>% 
  left_join(nv.ano, nv.anom.ano, by = 'UFResidencia')

#calculando prevalencia
epiprev.ano <-epi.prev(prev.ano$n, prev.ano$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
epiprev.ano_2 <- epiprev.ano[["ap"]]

#unindo os dados de prevalencia
prev.ano <- prev.ano %>% 
  bind_cols(epiprev.ano_2) %>% 
  mutate(est = round(est, 2))



#GRUPOS
#criando variavel de grupo
banco.anom.grupos <- bancop1119 %>% 
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



#NV TUBO NEURAL
nv.tuboneural.ano <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Defeitos de tubo neural") %>%
  group_by(UFResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(UFResidencia) %>%
  count()


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.tuboneural.ano <- nv.tuboneural.ano %>% 
  left_join(nv.ano, nv.tuboneural.ano, by = 'UFResidencia')

#calculando prevalencia
epiprev.tuboneural.ano <-epi.prev(prev.tuboneural.ano$n, prev.tuboneural.ano$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.tuboneural.ano_2 <- epiprev.tuboneural.ano[["ap"]]

#unindo os dados de prevalencia
prev.tuboneural.ano <- prev.tuboneural.ano %>% 
  bind_cols(prev.tuboneural.ano_2) %>% 
  mutate(est = round(est, 2))



#NV MICROCEFALIA
nv.microce.ano <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Microcefalia") %>%
  group_by(UFResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(UFResidencia) %>%
  count() 


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.microce.ano <- nv.microce.ano %>% 
  left_join(nv.ano, nv.microce.ano, by = 'UFResidencia')

#calculando prevalencia
epiprev.microce.ano <-epi.prev(prev.microce.ano$n, prev.microce.ano$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.microce.ano_2 <- epiprev.microce.ano[["ap"]]

#unindo os dados de prevalencia
prev.microce.ano <- prev.microce.ano %>% 
  bind_cols(prev.microce.ano_2) %>% 
  mutate(est = round(est, 2))



#NV CARDIOPATIA 
nv.cardio.ano <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Cardiopatias congênitas") %>%
  group_by(UFResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(UFResidencia) %>%
  count()


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.cardio.ano <- nv.cardio.ano %>% 
  left_join(nv.ano, nv.cardio.ano, by = 'UFResidencia')

#calculando prevalencia
epiprev.cardio.ano <-epi.prev(prev.cardio.ano$n, prev.cardio.ano$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.cardio.ano_2 <- epiprev.cardio.ano[["ap"]]

#unindo os dados de prevalencia
prev.cardio.ano <- prev.cardio.ano %>% 
  bind_cols(prev.cardio.ano_2) %>% 
  mutate(est = round(est, 2))




#NV FENDAS
nv.fendas.ano <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Fendas orais") %>%
  group_by(UFResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(UFResidencia) %>%
  count()

#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.fendas.ano <- nv.fendas.ano %>% 
  left_join(nv.ano, nv.fendas.ano, by = 'UFResidencia')

#calculando prevalencia
epiprev.fendas.ano <-epi.prev(prev.fendas.ano$n, prev.fendas.ano$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.fendas.ano_2 <- epiprev.fendas.ano[["ap"]]

#unindo os dados de prevalencia
prev.fendas.ano <- prev.fendas.ano %>% 
  bind_cols(prev.fendas.ano_2) %>% 
  mutate(est = round(est, 2))




#NV GENITAIS
nv.genitais.ano <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Anomalias de órgãos genitais") %>%
  group_by(UFResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(UFResidencia) %>%
  count()

#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.genitais.ano <- nv.genitais.ano %>% 
  left_join(nv.ano, nv.genitais.ano, by = 'UFResidencia')

#calculando prevalencia
epiprev.genitais.ano <-epi.prev(prev.genitais.ano$n, prev.genitais.ano$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.genitais.ano_2 <- epiprev.genitais.ano[["ap"]]

#unindo os dados de prevalencia
prev.genitais.ano <- prev.genitais.ano %>% 
  bind_cols(prev.genitais.ano_2) %>% 
  mutate(est = round(est, 2))




#NV MEMBROS
nv.membros.ano <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Defeitos de membros") %>%
  group_by(UFResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(UFResidencia) %>%
  count()


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.membros.ano <- nv.membros.ano %>% 
  left_join(nv.ano, nv.membros.ano, by = 'UFResidencia')

#calculando prevalencia
epiprev.membros.ano <-epi.prev(prev.membros.ano$n, prev.membros.ano$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.membros.ano_2 <- epiprev.membros.ano[["ap"]]

#unindo os dados de prevalencia
prev.membros.ano <- prev.membros.ano %>% 
  bind_cols(prev.membros.ano_2) %>% 
  mutate(est = round(est, 2))



#NV DEFEITOS ABDOMINAIS
nv.abdominal.ano <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Defeitos da parede abdominal") %>%
  group_by(UFResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(UFResidencia) %>%
  count()

#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.abdominal.ano <- nv.abdominal.ano %>% 
  left_join(nv.ano, nv.abdominal.ano, by = 'UFResidencia')

#calculando prevalencia
epiprev.abdominal.ano <-epi.prev(prev.abdominal.ano$n, prev.abdominal.ano$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.abdominal.ano_2 <- epiprev.abdominal.ano[["ap"]]

#unindo os dados de prevalencia
prev.abdominal.ano <- prev.abdominal.ano %>% 
  bind_cols(prev.abdominal.ano_2) %>% 
  mutate(est = round(est, 2))



#NV SÍNDROME DE DOWN
nv.down.ano <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Síndrome de Down") %>%
  group_by(UFResidencia, NumeroIdentificacao) %>%
  count() %>%
  group_by(UFResidencia) %>%
  count()


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.down.ano <- nv.down.ano %>% 
  left_join(nv.ano, nv.down.ano, by = 'UFResidencia')

#calculando prevalencia
epiprev.down.ano <-epi.prev(prev.down.ano$n, prev.down.ano$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.down.ano_2 <- epiprev.down.ano[["ap"]]

#unindo os dados de prevalencia
prev.down.ano <- prev.down.ano %>% 
  bind_cols(prev.down.ano_2) %>% 
  mutate(est = round(est, 2))



#GRAFICOS

#TABELAS
#SELECIONADAS
tab_prev.ano <- prev.ano %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.ano$Variavel <- "Todas anomalias selecionadas"


#TUBO NEURAL
tab_prev.tuboneural.ano <- prev.tuboneural.ano %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.tuboneural.ano$Variavel <- "Defeitos de tubo neural"


#MICROCEFALIA
tab_prev.microce.ano <- prev.microce.ano %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.microce.ano$Variavel <- "Microcefalia"


#CARDIOPATIA
tab_prev.cardio.ano <- prev.cardio.ano %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.cardio.ano$Variavel <- "Cardiopatias congênitas"


#GENITAIS
tab_prev.genitais.ano <- prev.genitais.ano %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.genitais.ano$Variavel <- "Anomalias de órgãos genitais"


#MEMBROS
tab_prev.membros.ano <- prev.membros.ano %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.membros.ano$Variavel <- "Defeitos de membros"


#FENDAS
tab_prev.fendas.ano <- prev.fendas.ano %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.fendas.ano$Variavel <- "Fendas orais"


#ABDOMINAL
tab_prev.abdominal.ano <- prev.abdominal.ano %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.abdominal.ano$Variavel <- "Defeitos da parede abdominal"



#DOWN
tab_prev.down.ano <- prev.down.ano %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.down.ano$Variavel <- "Síndrome de Down"


tab_grafico.lb <- tab_prev.ano %>% 
  rbind(tab_prev.tuboneural.ano) %>%
  rbind(tab_prev.cardio.ano) %>%
  rbind(tab_prev.membros.ano) %>%
  rbind(tab_prev.fendas.ano) %>%
  rbind(tab_prev.genitais.ano) %>%
  rbind(tab_prev.abdominal.ano) %>%
  rbind(tab_prev.microce.ano) %>%
  rbind(tab_prev.down.ano)


## ---- tabela_linha.de.base --------
tabela_linha.de.base <- tab_grafico.lb %>% 
  mutate (NascidosVivos = scales::number(NascidosVivos, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (Prevalencia10000 = scales::number(Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>%
  mutate (lower= scales::number(lower, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>%
  mutate (upper = scales::number(upper, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>%
  unite (col = IC95, lower:upper, sep = "-")
