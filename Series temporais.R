## SERIES TEMPORAIS

## ---- biblioteca_serie --------
library(ggplot2)
library(tidyverse)
library(viridis)
library(epiR)
library(scales)
library(readr)



## ---- analisestotal --------
##### ANALISES

bancop1120 <- read_csv2("banco_limpeza.csv", locale = locale(encoding = "ISO8859-1"))


#NASCIDOS VIVOS POR ANO
nv.ano <- bancop1120 %>%
  group_by(AnoNascimento, NumeroIdentificacao) %>%
  count() %>% 
  group_by(AnoNascimento) %>% 
  count() %>% 
  rename(nasc_vivos = n)


#NV COM ANOMALIAS SELECIONADAS
nv.anom.ano <- bancop1120 %>% 
  filter(str_detect(CID10Anomalia,"Q000|Q001|Q002|Q01|Q05|Q02|Q20|Q21|Q22|Q23|Q24|Q25|Q26|Q27|Q28|Q35|Q36|Q37|Q54|Q56|Q66|Q69|Q71|Q72|Q73|Q743|Q792|Q793|Q90")) %>% 
  group_by(AnoNascimento, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento) %>%
  count()


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.ano <- nv.anom.ano %>% 
  left_join(nv.ano, nv.anom.ano, by = 'AnoNascimento')

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
banco.anom.grupos <- bancop1120 %>% 
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
nv.tuboneural.ano <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Defeitos de tubo neural") %>%
  group_by(AnoNascimento, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento) %>%
  count()


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.tuboneural.ano <- nv.tuboneural.ano %>% 
  left_join(nv.ano, nv.tuboneural.ano, by = 'AnoNascimento')

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
  group_by(AnoNascimento, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento) %>%
  count() 


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.microce.ano <- nv.microce.ano %>% 
  left_join(nv.ano, nv.microce.ano, by = 'AnoNascimento')

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
  group_by(AnoNascimento, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento) %>%
  count()


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.cardio.ano <- nv.cardio.ano %>% 
  left_join(nv.ano, nv.cardio.ano, by = 'AnoNascimento')

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
  group_by(AnoNascimento, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento) %>%
  count()

#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.fendas.ano <- nv.fendas.ano %>% 
  left_join(nv.ano, nv.fendas.ano, by = 'AnoNascimento')

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
  filter(GrupoCID10 == "Anomalias de órgãos genitais - Hipospádias" | GrupoCID10 == "Anomalias de órgãos genitais - Sexo indefinido") %>%
  group_by(AnoNascimento, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento) %>%
  count()

#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.genitais.ano <- nv.genitais.ano %>% 
  left_join(nv.ano, nv.genitais.ano, by = 'AnoNascimento')

#calculando prevalencia
epiprev.genitais.ano <-epi.prev(prev.genitais.ano$n, prev.genitais.ano$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.genitais.ano_2 <- epiprev.genitais.ano[["ap"]]

#unindo os dados de prevalencia
prev.genitais.ano <- prev.genitais.ano %>% 
  bind_cols(prev.genitais.ano_2) %>% 
  mutate(est = round(est, 2))



#NV HIPOSPADIA
nv.hipospadias.ano <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Anomalias de órgãos genitais - Hipospádias") %>%
  group_by(AnoNascimento, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento) %>%
  count()

#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.hipospadias.ano <- nv.hipospadias.ano %>% 
  left_join(nv.ano, nv.hipospadias.ano, by = 'AnoNascimento')

#calculando prevalencia
epiprev.hipospadias.ano <-epi.prev(prev.hipospadias.ano$n, prev.hipospadias.ano$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.hipospadias.ano_2 <- epiprev.hipospadias.ano[["ap"]]

#unindo os dados de prevalencia
prev.hipospadias.ano <- prev.hipospadias.ano %>% 
  bind_cols(prev.hipospadias.ano_2) %>% 
  mutate(est = round(est, 2))




#NV SEXO INDEFINIDO
nv.SI.ano <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Anomalias de órgãos genitais - Sexo indefinido") %>%
  group_by(AnoNascimento, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento) %>%
  count()

#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.SI.ano <- nv.SI.ano %>% 
  left_join(nv.ano, nv.SI.ano, by = 'AnoNascimento')

#calculando prevalencia
epiprev.SI.ano <-epi.prev(prev.SI.ano$n, prev.SI.ano$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.SI.ano_2 <- epiprev.SI.ano[["ap"]]

#unindo os dados de prevalencia
prev.SI.ano <- prev.SI.ano %>% 
  bind_cols(prev.SI.ano_2) %>% 
  mutate(est = round(est, 2))




#NV MEMBROS
nv.membros.ano <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Defeitos de membros") %>%
  group_by(AnoNascimento, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento) %>%
  count()


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.membros.ano <- nv.membros.ano %>% 
  left_join(nv.ano, nv.membros.ano, by = 'AnoNascimento')

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
  group_by(AnoNascimento, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento) %>%
  count()

#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.abdominal.ano <- nv.abdominal.ano %>% 
  left_join(nv.ano, nv.abdominal.ano, by = 'AnoNascimento')

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
  group_by(AnoNascimento, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento) %>%
  count()


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.down.ano <- nv.down.ano %>% 
  left_join(nv.ano, nv.down.ano, by = 'AnoNascimento')

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



#HIPOSPADIAS
tab_prev.hipospadias.ano <- prev.hipospadias.ano %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.hipospadias.ano$Variavel <- "Anomalias de órgãos genitais - Hipospádias"


#SEXO INDEFINIDO
tab_prev.SI.ano <- prev.SI.ano %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.SI.ano$Variavel <- "Anomalias de órgãos genitais - Sexo indefinido"



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


tab_grafico <- tab_prev.ano %>% 
  rbind(tab_prev.tuboneural.ano) %>%
  rbind(tab_prev.cardio.ano) %>%
  rbind(tab_prev.membros.ano) %>%
  rbind(tab_prev.fendas.ano) %>%
  rbind(tab_prev.genitais.ano) %>%
  rbind(tab_prev.hipospadias.ano) %>%
  rbind(tab_prev.SI.ano) %>%
  rbind(tab_prev.abdominal.ano) %>%
  rbind(tab_prev.microce.ano) %>%
  rbind(tab_prev.down.ano)


label_grafico_total <- tab_grafico %>% 
  mutate (NascidosVivos = scales::number(NascidosVivos, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (Prevalencia10000 = scales::number(Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ","))


## ---- grafico.total.nv --------

#Grafico nascidos vivos
ggplot(tab_grafico, aes(x = AnoNascimento, y = NascidosVivos)) +
  geom_line(size=1, color = "purple") +
  geom_point(size=2.5, color = "purple") +
  geom_text(aes(label = label_grafico_total$NascidosVivos), hjust = 0.5,  vjust = -1.25, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(y = "Nascidos Vivos", x = "Ano de Nascimento")


## ---- grafico.total.nv.ac --------

#Grafico faced wrap com todos nv com anomalias

ggplot(tab_grafico, aes(x = AnoNascimento, y = NascidosVivosACs, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=5) +
  geom_text(aes(label = tab_grafico$NascidosVivosACs), hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~., ncol = 3, scales = "free_y")




## ---- grafico.total.prev --------
#Grafico faced wrap com todas prevalencias
ggplot(tab_grafico, aes(x = AnoNascimento, y = Prevalencia10000, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(label = label_grafico_total$Prevalencia10000), hjust = 0.5,  vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 70, by = 2)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 



## ---- tabela.total --------
tabela_grafico.total <- tab_grafico %>% 
  rename(GrupoAnomalias = Variavel) %>% 
  mutate (Prevalencia10000 = scales::number(Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (lower = scales::number(lower, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (upper = scales::number(upper, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>%
  unite (col = IC95, lower:upper, sep = "-")




## ---- analisesmacro --------

### MACRORREGIAO

#NASCIDOS VIVOS POR ANO
nv.ano.macro <- bancop1120 %>%
  group_by(AnoNascimento, MacrorregiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  filter (MacrorregiaoSaude != "Município Ignorado") %>% 
  group_by(AnoNascimento, MacrorregiaoSaude) %>% 
  count() %>% 
  rename(nasc_vivos = n)


#NV COM ANOMALIAS SELECIONADAS
nv.anom.ano.macro <- bancop1120 %>% 
  filter(str_detect(CID10Anomalia,"Q000|Q001|Q002|Q01|Q05|Q02|Q20|Q21|Q22|Q23|Q24|Q25|Q26|Q27|Q28|Q35|Q36|Q37|Q54|Q56|Q66|Q69|Q71|Q72|Q73|Q743|Q792|Q793|Q90")) %>% 
  group_by(AnoNascimento, MacrorregiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento, MacrorregiaoSaude) %>%
  count()


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.ano.macro <- nv.anom.ano.macro %>% 
  left_join(nv.ano.macro, nv.anom.ano.macro, by = c('AnoNascimento', 'MacrorregiaoSaude'))

#calculando prevalencia
epiprev.ano.macro <-epi.prev(prev.ano.macro$n, prev.ano.macro$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
epiprev.ano.macro_2 <- epiprev.ano.macro[["ap"]]

#unindo os dados de prevalencia
prev.ano.macro <- prev.ano.macro %>% 
  bind_cols(epiprev.ano.macro_2) %>% 
  mutate(est = round(est, 2))



#GRUPOS
#NV TUBO NEURAL
nv.tuboneural.ano.macro <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Defeitos de tubo neural") %>%
  group_by(AnoNascimento, MacrorregiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento, MacrorregiaoSaude) %>%
  count()


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.tuboneural.ano.macro <- nv.tuboneural.ano.macro %>% 
  left_join(nv.ano.macro, nv.tuboneural.ano.macro, by = c('AnoNascimento', 'MacrorregiaoSaude'))

#calculando prevalencia
epiprev.tuboneural.ano.macro <-epi.prev(prev.tuboneural.ano.macro$n, prev.tuboneural.ano.macro$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.tuboneural.ano.macro_2 <- epiprev.tuboneural.ano.macro[["ap"]]

#unindo os dados de prevalencia
prev.tuboneural.ano.macro <- prev.tuboneural.ano.macro %>% 
  bind_cols(prev.tuboneural.ano.macro_2) %>% 
  mutate(est = round(est, 2))



#NV MICROCEFALIA
nv.microce.ano.macro <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Microcefalia") %>%
  group_by(AnoNascimento, MacrorregiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento, MacrorregiaoSaude) %>%
  count() 


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.microce.ano.macro <- nv.microce.ano.macro %>% 
  left_join(nv.ano.macro, nv.microce.ano.macro, by = c('AnoNascimento', 'MacrorregiaoSaude'))

#calculando prevalencia
epiprev.microce.ano.macro <-epi.prev(prev.microce.ano.macro$n, prev.microce.ano.macro$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.microce.ano.macro_2 <- epiprev.microce.ano.macro[["ap"]]

#unindo os dados de prevalencia
prev.microce.ano.macro <- prev.microce.ano.macro %>% 
  bind_cols(prev.microce.ano.macro_2) %>% 
  mutate(est = round(est, 2))



#NV CARDIOPATIA 
nv.cardio.ano.macro <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Cardiopatias congênitas") %>%
  group_by(AnoNascimento, MacrorregiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento, MacrorregiaoSaude) %>%
  count()


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.cardio.ano.macro <- nv.cardio.ano.macro %>% 
  left_join(nv.ano.macro, nv.cardio.ano.macro, by = c('AnoNascimento', 'MacrorregiaoSaude'))

#calculando prevalencia
epiprev.cardio.ano.macro <-epi.prev(prev.cardio.ano.macro$n, prev.cardio.ano.macro$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.cardio.ano.macro_2 <- epiprev.cardio.ano.macro[["ap"]]

#unindo os dados de prevalencia
prev.cardio.ano.macro <- prev.cardio.ano.macro %>% 
  bind_cols(prev.cardio.ano.macro_2) %>% 
  mutate(est = round(est, 2))




#NV FENDAS
nv.fendas.ano.macro <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Fendas orais") %>%
  group_by(AnoNascimento, MacrorregiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento, MacrorregiaoSaude) %>%
  count()

#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.fendas.ano.macro <- nv.fendas.ano.macro %>% 
  left_join(nv.ano.macro, nv.fendas.ano.macro, by = c('AnoNascimento', 'MacrorregiaoSaude'))

#calculando prevalencia
epiprev.fendas.ano.macro <-epi.prev(prev.fendas.ano.macro$n, prev.fendas.ano.macro$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.fendas.ano.macro_2 <- epiprev.fendas.ano.macro[["ap"]]

#unindo os dados de prevalencia
prev.fendas.ano.macro <- prev.fendas.ano.macro %>% 
  bind_cols(prev.fendas.ano.macro_2) %>% 
  mutate(est = round(est, 2))




#NV GENITAIS
nv.genitais.ano.macro <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Anomalias de órgãos genitais - Hipospádias" | GrupoCID10 == "Anomalias de órgãos genitais - Sexo indefinido") %>%
  group_by(AnoNascimento, MacrorregiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento, MacrorregiaoSaude) %>%
  count()

#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.genitais.ano.macro <- nv.genitais.ano.macro %>% 
  left_join(nv.ano.macro, nv.genitais.ano.macro, by = c('AnoNascimento', 'MacrorregiaoSaude'))

#calculando prevalencia
epiprev.genitais.ano.macro <-epi.prev(prev.genitais.ano.macro$n, prev.genitais.ano.macro$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.genitais.ano.macro_2 <- epiprev.genitais.ano.macro[["ap"]]

#unindo os dados de prevalencia
prev.genitais.ano.macro <- prev.genitais.ano.macro %>% 
  bind_cols(prev.genitais.ano.macro_2) %>% 
  mutate(est = round(est, 2))





#NV HIPOSPADIAS
nv.hipospadias.ano.macro <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Anomalias de órgãos genitais - Hipospádias") %>%
  group_by(AnoNascimento, MacrorregiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento, MacrorregiaoSaude) %>%
  count()

#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.hipospadias.ano.macro <- nv.hipospadias.ano.macro %>% 
  left_join(nv.ano.macro, nv.hipospadias.ano.macro, by = c('AnoNascimento', 'MacrorregiaoSaude'))

#calculando prevalencia
epiprev.hipospadias.ano.macro <-epi.prev(prev.hipospadias.ano.macro$n, prev.hipospadias.ano.macro$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.hipospadias.ano.macro_2 <- epiprev.hipospadias.ano.macro[["ap"]]

#unindo os dados de prevalencia
prev.hipospadias.ano.macro <- prev.hipospadias.ano.macro %>% 
  bind_cols(prev.hipospadias.ano.macro_2) %>% 
  mutate(est = round(est, 2))




#NV SEXO INDEFINIDO
nv.SI.ano.macro <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Anomalias de órgãos genitais - Sexo indefinido") %>%
  group_by(AnoNascimento, MacrorregiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento, MacrorregiaoSaude) %>%
  count()

#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.SI.ano.macro <- nv.SI.ano.macro %>% 
  left_join(nv.ano.macro, nv.SI.ano.macro, by = c('AnoNascimento', 'MacrorregiaoSaude'))

#calculando prevalencia
epiprev.SI.ano.macro <-epi.prev(prev.SI.ano.macro$n, prev.SI.ano.macro$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.SI.ano.macro_2 <- epiprev.SI.ano.macro[["ap"]]

#unindo os dados de prevalencia
prev.SI.ano.macro <- prev.SI.ano.macro %>% 
  bind_cols(prev.SI.ano.macro_2) %>% 
  mutate(est = round(est, 2))




#NV MEMBROS
nv.membros.ano.macro <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Defeitos de membros") %>%
  group_by(AnoNascimento, MacrorregiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento, MacrorregiaoSaude) %>%
  count()


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.membros.ano.macro <- nv.membros.ano.macro %>% 
  left_join(nv.ano.macro, nv.membros.ano.macro, by = c('AnoNascimento', 'MacrorregiaoSaude'))

#calculando prevalencia
epiprev.membros.ano.macro <-epi.prev(prev.membros.ano.macro$n, prev.membros.ano.macro$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.membros.ano.macro_2 <- epiprev.membros.ano.macro[["ap"]]

#unindo os dados de prevalencia
prev.membros.ano.macro <- prev.membros.ano.macro %>% 
  bind_cols(prev.membros.ano.macro_2) %>% 
  mutate(est = round(est, 2))



#NV DEFEITOS ABDOMINAIS
nv.abdominal.ano.macro <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Defeitos da parede abdominal") %>%
  group_by(AnoNascimento, MacrorregiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento, MacrorregiaoSaude) %>%
  count()

#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.abdominal.ano.macro <- nv.abdominal.ano.macro %>% 
  left_join(nv.ano.macro, nv.abdominal.ano.macro, by = c('AnoNascimento', 'MacrorregiaoSaude'))

#calculando prevalencia
epiprev.abdominal.ano.macro <-epi.prev(prev.abdominal.ano.macro$n, prev.abdominal.ano.macro$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.abdominal.ano.macro_2 <- epiprev.abdominal.ano.macro[["ap"]]

#unindo os dados de prevalencia
prev.abdominal.ano.macro <- prev.abdominal.ano.macro %>% 
  bind_cols(prev.abdominal.ano.macro_2) %>% 
  mutate(est = round(est, 2))



#NV SÍNDROME DE DOWN
nv.down.ano.macro <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Síndrome de Down") %>%
  group_by(AnoNascimento, MacrorregiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento, MacrorregiaoSaude) %>%
  count()


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.down.ano.macro <- nv.down.ano.macro %>% 
  left_join(nv.ano.macro, nv.down.ano.macro, by = c('AnoNascimento', 'MacrorregiaoSaude'))

#calculando prevalencia
epiprev.down.ano.macro <-epi.prev(prev.down.ano.macro$n, prev.down.ano.macro$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.down.ano.macro_2 <- epiprev.down.ano.macro[["ap"]]

#unindo os dados de prevalencia
prev.down.ano.macro <- prev.down.ano.macro %>% 
  bind_cols(prev.down.ano.macro_2) %>% 
  mutate(est = round(est, 2))



#GRAFICOS

#TABELAS
#SELECIONADAS
tab_prev.ano.macro <- prev.ano.macro %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.ano.macro$Variavel <- "Todas anomalias selecionadas"


#TUBO NEURAL
tab_prev.tuboneural.ano.macro <- prev.tuboneural.ano.macro %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.tuboneural.ano.macro$Variavel <- "Defeitos de tubo neural"


#MICROCEFALIA
tab_prev.microce.ano.macro <- prev.microce.ano.macro %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.microce.ano.macro$Variavel <- "Microcefalia"


#CARDIOPATIA
tab_prev.cardio.ano.macro <- prev.cardio.ano.macro %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.cardio.ano.macro$Variavel <- "Cardiopatias congênitas"


#GENITAIS
tab_prev.genitais.ano.macro <- prev.genitais.ano.macro %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.genitais.ano.macro$Variavel <- "Anomalias de órgãos genitais"


#HIPOSPADIAS
tab_prev.hipospadias.ano.macro <- prev.hipospadias.ano.macro %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.hipospadias.ano.macro$Variavel <- "Anomalias de órgãos genitais - Hipospádias"


#SEXO INDEFINIDO
tab_prev.SI.ano.macro <- prev.SI.ano.macro %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.SI.ano.macro$Variavel <- "Anomalias de órgãos genitais - Sexo indefinido"



#MEMBROS
tab_prev.membros.ano.macro <- prev.membros.ano.macro %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.membros.ano.macro$Variavel <- "Defeitos de membros"


#FENDAS
tab_prev.fendas.ano.macro <- prev.fendas.ano.macro %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.fendas.ano.macro$Variavel <- "Fendas orais"


#ABDOMINAL
tab_prev.abdominal.ano.macro <- prev.abdominal.ano.macro %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.abdominal.ano.macro$Variavel <- "Defeitos da parede abdominal"



#DOWN
tab_prev.down.ano.macro <- prev.down.ano.macro %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.down.ano.macro$Variavel <- "Síndrome de Down"


tab_grafico.macro <- tab_prev.ano.macro %>% 
  rbind(tab_prev.tuboneural.ano.macro) %>%
  rbind(tab_prev.cardio.ano.macro) %>%
  rbind(tab_prev.membros.ano.macro) %>%
  rbind(tab_prev.fendas.ano.macro) %>%
  rbind(tab_prev.genitais.ano.macro) %>%
  rbind(tab_prev.hipospadias.ano.macro) %>%
  rbind(tab_prev.SI.ano.macro) %>%
  rbind(tab_prev.abdominal.ano.macro) %>%
  rbind(tab_prev.microce.ano.macro) %>%
  rbind(tab_prev.down.ano.macro) %>% 
  mutate (MacrorregiaoSaude = recode(MacrorregiaoSaude, "Alto Vale Do Itajai" = "Vale do Itajaí"))


## ---- grafico.macro.nv --------
#Grafico faced wrap com todos nv
ggplot(tab_grafico.macro, aes(x = AnoNascimento, y = NascidosVivos, color = MacrorregiaoSaude, group = MacrorregiaoSaude)) +
  geom_line(size=1) +
  geom_point(size=2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Macrorregião de Saúde", y = "Nascidos Vivos", x = "Ano de Nascimento") +
  facet_wrap(MacrorregiaoSaude ~., ncol = 3, scales = "free_y") 



## ---- grafico.macro.nv.avi --------

##MACRO Vale do Itajaí
#Grafico nascidos vivos
ggplot(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Vale do Itajaí",], aes(x = AnoNascimento, y = NascidosVivos)) +
  geom_line(size=1, color = "purple") +
  geom_point(size=2.5, color = "purple") +
  geom_text(aes(y = tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Vale do Itajaí", "NascidosVivos"]$NascidosVivos,
                label = scales::number(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Vale do Itajaí", "NascidosVivos"]$NascidosVivos, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5,  vjust = -1.25, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(y = "Nascidos Vivos", x = "Ano de Nascimento")


## ---- grafico.macro.nv.ac.avi --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Vale do Itajaí",], aes(x = AnoNascimento, y = NascidosVivosACs, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Vale do Itajaí", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 


## ---- grafico.macro.prev.avi --------
#Grafico faced wrap com todas prevalencias
ggplot(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Vale do Itajaí",], aes(x = AnoNascimento, y = Prevalencia10000, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Vale do Itajaí", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Vale do Itajaí", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 5)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 


## ---- grafico.macro.nv.fri --------
##MACRO Foz Do Rio Itajai
#Grafico nascidos vivos
ggplot(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Foz Do Rio Itajai",], aes(x = AnoNascimento, y = NascidosVivos)) +
  geom_line(size=1, color = "purple") +
  geom_point(size=2.5, color = "purple") +
  geom_text(aes(y = tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Foz Do Rio Itajai", "NascidosVivos"]$NascidosVivos,
                label = scales::number(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Foz Do Rio Itajai", "NascidosVivos"]$NascidosVivos, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5,  vjust = -1.25, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(y = "Nascidos Vivos", x = "Ano de Nascimento")


## ---- grafico.macro.nv.ac.fri --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Foz Do Rio Itajai",], aes(x = AnoNascimento, y = NascidosVivosACs, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Foz Do Rio Itajai", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 


## ---- grafico.macro.prev.fri --------
#Grafico faced wrap com todas prevalencias
ggplot(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Foz Do Rio Itajai",], aes(x = AnoNascimento, y = Prevalencia10000, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Foz Do Rio Itajai", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Foz Do Rio Itajai", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 5)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 



## ---- grafico.macro.nv.gf --------
##MACRO Grande Florianopolis
#Grafico nascidos vivos
ggplot(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Grande Florianopolis",], aes(x = AnoNascimento, y = NascidosVivos)) +
  geom_line(size=1, color = "purple") +
  geom_point(size=2.5, color = "purple") +
  geom_text(aes(y = tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Grande Florianopolis", "NascidosVivos"]$NascidosVivos,
                label = scales::number(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Grande Florianopolis", "NascidosVivos"]$NascidosVivos, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5,  vjust = -1.25, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(y = "Nascidos Vivos", x = "Ano de Nascimento")


## ---- grafico.macro.nv.ac.gf --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Grande Florianopolis",], aes(x = AnoNascimento, y = NascidosVivosACs, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Grande Florianopolis", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 


## ---- grafico.macro.prev.gf --------
#Grafico faced wrap com todas prevalencias
ggplot(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Grande Florianopolis",], aes(x = AnoNascimento, y = Prevalencia10000, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Grande Florianopolis", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Grande Florianopolis", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 150, by = 5)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 



## ---- grafico.macro.nv.go --------
##MACRO Grande Oeste
#Grafico nascidos vivos
ggplot(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Grande Oeste",], aes(x = AnoNascimento, y = NascidosVivos)) +
  geom_line(size=1, color = "purple") +
  geom_point(size=2.5, color = "purple") +
  geom_text(aes(y = tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Grande Oeste", "NascidosVivos"]$NascidosVivos,
                label = scales::number(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Grande Oeste", "NascidosVivos"]$NascidosVivos, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5,  vjust = -1.25, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(y = "Nascidos Vivos", x = "Ano de Nascimento")


## ---- grafico.macro.nv.ac.go --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Grande Oeste",], aes(x = AnoNascimento, y = NascidosVivosACs, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Grande Oeste", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 


## ---- grafico.macro.prev.go --------
#Grafico faced wrap com todas prevalencias
ggplot(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Grande Oeste",], aes(x = AnoNascimento, y = Prevalencia10000, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Grande Oeste", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Grande Oeste", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 150, by = 5)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 



## ---- grafico.macro.nv.mosc --------
##MACRO Meio Oeste E Serra Catarinense
#Grafico nascidos vivos
ggplot(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Meio Oeste E Serra Catarinense",], aes(x = AnoNascimento, y = NascidosVivos)) +
  geom_line(size=1, color = "purple") +
  geom_point(size=2.5, color = "purple") +
  geom_text(aes(y = tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Meio Oeste E Serra Catarinense", "NascidosVivos"]$NascidosVivos,
                label = scales::number(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Meio Oeste E Serra Catarinense", "NascidosVivos"]$NascidosVivos, accuracy = 1, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5,  vjust = -1.25, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(y = "Nascidos Vivos", x = "Ano de Nascimento")


## ---- grafico.macro.nv.ac.mosc --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Meio Oeste E Serra Catarinense",], aes(x = AnoNascimento, y = NascidosVivosACs, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Meio Oeste E Serra Catarinense", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 


## ---- grafico.macro.prev.mosc --------
#Grafico faced wrap com todas prevalencias
ggplot(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Meio Oeste E Serra Catarinense",], aes(x = AnoNascimento, y = Prevalencia10000, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Meio Oeste E Serra Catarinense", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Meio Oeste E Serra Catarinense", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 150, by = 5)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 




## ---- grafico.macro.nv.pnn --------
##MACRO Planalto Norte E Nordeste
#Grafico nascidos vivos
ggplot(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Planalto Norte E Nordeste",], aes(x = AnoNascimento, y = NascidosVivos)) +
  geom_line(size=1, color = "purple") +
  geom_point(size=2.5, color = "purple") +
  geom_text(aes(y = tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Planalto Norte E Nordeste", "NascidosVivos"]$NascidosVivos,
                label = scales::number(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Planalto Norte E Nordeste", "NascidosVivos"]$NascidosVivos, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5,  vjust = -1.25, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(y = "Nascidos Vivos", x = "Ano de Nascimento")


## ---- grafico.macro.nv.ac.pnn --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Planalto Norte E Nordeste",], aes(x = AnoNascimento, y = NascidosVivosACs, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Planalto Norte E Nordeste", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 


## ---- grafico.macro.prev.pnn --------
#Grafico faced wrap com todas prevalencias
ggplot(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Planalto Norte E Nordeste",], aes(x = AnoNascimento, y = Prevalencia10000, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Planalto Norte E Nordeste", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Planalto Norte E Nordeste", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 80, by = 5)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 


## ---- grafico.macro.nv.sul --------
##MACRO SUL
#Grafico nascidos vivos
ggplot(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Sul",], aes(x = AnoNascimento, y = NascidosVivos)) +
  geom_line(size=1, color = "purple") +
  geom_point(size=2.5, color = "purple") +
  geom_text(aes(y = tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Sul", "NascidosVivos"]$NascidosVivos,
                label = scales::number(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Sul", "NascidosVivos"]$NascidosVivos, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5,  vjust = -1.25, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(y = "Nascidos Vivos", x = "Ano de Nascimento")


## ---- grafico.macro.nv.ac.sul --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Sul",], aes(x = AnoNascimento, y = NascidosVivosACs, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Sul", "NascidosVivosACs"]$NascidosVivosACs),
                hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 


## ---- grafico.macro.prev.sul --------
#Grafico faced wrap com todas prevalencias
ggplot(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Sul",], aes(x = AnoNascimento, y = Prevalencia10000, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Sul", "Prevalencia10000"]$Prevalencia10000,
            label = scales::number(tab_grafico.macro[tab_grafico.macro$MacrorregiaoSaude == "Sul", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 5)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 
  


## ---- tabela.grafico.macro --------
tabela_grafico.macro <- tab_grafico.macro %>% 
  rename(GrupoAnomalias = Variavel) %>% 
  mutate (Prevalencia10000 = scales::number(Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (lower = scales::number(lower, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (upper = scales::number(upper, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>%
  unite (col = IC95, lower:upper, sep = "-")



## ---- analisesreg --------

### REGIAO

#NASCIDOS VIVOS POR ANO
nv.ano.reg <- bancop1120 %>%
  group_by(AnoNascimento, RegiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  filter (RegiaoSaude != "Município Ignorado") %>% 
  group_by(AnoNascimento, RegiaoSaude) %>% 
  count() %>% 
  rename(nasc_vivos = n)


#NV COM ANOMALIAS SELECIONADAS
nv.anom.ano.reg <- bancop1120 %>% 
  filter(str_detect(CID10Anomalia,"Q000|Q001|Q002|Q01|Q05|Q02|Q20|Q21|Q22|Q23|Q24|Q25|Q26|Q27|Q28|Q35|Q36|Q37|Q54|Q56|Q66|Q69|Q71|Q72|Q73|Q743|Q792|Q793|Q90")) %>% 
  group_by(AnoNascimento, RegiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento, RegiaoSaude) %>%
  count()


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.ano.reg <- nv.anom.ano.reg %>% 
  left_join(nv.ano.reg, nv.anom.ano.reg, by = c('AnoNascimento', 'RegiaoSaude'))

#calculando prevalencia
epiprev.ano.reg <-epi.prev(prev.ano.reg$n, prev.ano.reg$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
epiprev.ano.reg_2 <- epiprev.ano.reg[["ap"]]

#unindo os dados de prevalencia
prev.ano.reg <- prev.ano.reg %>% 
  bind_cols(epiprev.ano.reg_2) %>% 
  mutate(est = round(est, 2))



#GRUPOS

#NV TUBO NEURAL
nv.tuboneural.ano.reg <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Defeitos de tubo neural") %>%
  group_by(AnoNascimento, RegiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento, RegiaoSaude) %>%
  count()


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.tuboneural.ano.reg <- nv.tuboneural.ano.reg %>% 
  left_join(nv.ano.reg, nv.tuboneural.ano.reg, by = c('AnoNascimento', 'RegiaoSaude'))

#calculando prevalencia
epiprev.tuboneural.ano.reg <-epi.prev(prev.tuboneural.ano.reg$n, prev.tuboneural.ano.reg$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.tuboneural.ano.reg_2 <- epiprev.tuboneural.ano.reg[["ap"]]

#unindo os dados de prevalencia
prev.tuboneural.ano.reg <- prev.tuboneural.ano.reg %>% 
  bind_cols(prev.tuboneural.ano.reg_2) %>% 
  mutate(est = round(est, 2))



#NV MICROCEFALIA
nv.microce.ano.reg <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Microcefalia") %>%
  group_by(AnoNascimento, RegiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento, RegiaoSaude) %>%
  count() 


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.microce.ano.reg <- nv.microce.ano.reg %>% 
  left_join(nv.ano.reg, nv.microce.ano.reg, by = c('AnoNascimento', 'RegiaoSaude'))

#calculando prevalencia
epiprev.microce.ano.reg <-epi.prev(prev.microce.ano.reg$n, prev.microce.ano.reg$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.microce.ano.reg_2 <- epiprev.microce.ano.reg[["ap"]]

#unindo os dados de prevalencia
prev.microce.ano.reg <- prev.microce.ano.reg %>% 
  bind_cols(prev.microce.ano.reg_2) %>% 
  mutate(est = round(est, 2))



#NV CARDIOPATIA 
nv.cardio.ano.reg <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Cardiopatias congênitas") %>%
  group_by(AnoNascimento, RegiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento, RegiaoSaude) %>%
  count()


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.cardio.ano.reg <- nv.cardio.ano.reg %>% 
  left_join(nv.ano.reg, nv.cardio.ano.reg, by = c('AnoNascimento', 'RegiaoSaude'))

#calculando prevalencia
epiprev.cardio.ano.reg <-epi.prev(prev.cardio.ano.reg$n, prev.cardio.ano.reg$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.cardio.ano.reg_2 <- epiprev.cardio.ano.reg[["ap"]]

#unindo os dados de prevalencia
prev.cardio.ano.reg <- prev.cardio.ano.reg %>% 
  bind_cols(prev.cardio.ano.reg_2) %>% 
  mutate(est = round(est, 2))




#NV FENDAS
nv.fendas.ano.reg <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Fendas orais") %>%
  group_by(AnoNascimento, RegiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento, RegiaoSaude) %>%
  count()

#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.fendas.ano.reg <- nv.fendas.ano.reg %>% 
  left_join(nv.ano.reg, nv.fendas.ano.reg, by = c('AnoNascimento', 'RegiaoSaude'))

#calculando prevalencia
epiprev.fendas.ano.reg <-epi.prev(prev.fendas.ano.reg$n, prev.fendas.ano.reg$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.fendas.ano.reg_2 <- epiprev.fendas.ano.reg[["ap"]]

#unindo os dados de prevalencia
prev.fendas.ano.reg <- prev.fendas.ano.reg %>% 
  bind_cols(prev.fendas.ano.reg_2) %>% 
  mutate(est = round(est, 2))




#NV GENITAIS
nv.genitais.ano.reg <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Anomalias de órgãos genitais - Hipospádias" | GrupoCID10 == "Anomalias de órgãos genitais - Sexo indefinido") %>%
  group_by(AnoNascimento, RegiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento, RegiaoSaude) %>%
  count()

#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.genitais.ano.reg <- nv.genitais.ano.reg %>% 
  left_join(nv.ano.reg, nv.genitais.ano.reg, by = c('AnoNascimento', 'RegiaoSaude'))

#calculando prevalencia
epiprev.genitais.ano.reg <-epi.prev(prev.genitais.ano.reg$n, prev.genitais.ano.reg$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.genitais.ano.reg_2 <- epiprev.genitais.ano.reg[["ap"]]

#unindo os dados de prevalencia
prev.genitais.ano.reg <- prev.genitais.ano.reg %>% 
  bind_cols(prev.genitais.ano.reg_2) %>% 
  mutate(est = round(est, 2))





#NV HIPOSPADIAS
nv.hipospadias.ano.reg <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Anomalias de órgãos genitais - Hipospádias") %>%
  group_by(AnoNascimento, RegiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento, RegiaoSaude) %>%
  count()

#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.hipospadias.ano.reg <- nv.hipospadias.ano.reg %>% 
  left_join(nv.ano.reg, nv.hipospadias.ano.reg, by = c('AnoNascimento', 'RegiaoSaude'))

#calculando prevalencia
epiprev.hipospadias.ano.reg <-epi.prev(prev.hipospadias.ano.reg$n, prev.hipospadias.ano.reg$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.hipospadias.ano.reg_2 <- epiprev.hipospadias.ano.reg[["ap"]]

#unindo os dados de prevalencia
prev.hipospadias.ano.reg <- prev.hipospadias.ano.reg %>% 
  bind_cols(prev.hipospadias.ano.reg_2) %>% 
  mutate(est = round(est, 2))



#NV SEXO INDEFINIDO
nv.SI.ano.reg <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Anomalias de órgãos genitais - Sexo indefinido") %>%
  group_by(AnoNascimento, RegiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento, RegiaoSaude) %>%
  count()

#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.SI.ano.reg <- nv.SI.ano.reg %>% 
  left_join(nv.ano.reg, nv.SI.ano.reg, by = c('AnoNascimento', 'RegiaoSaude'))

#calculando prevalencia
epiprev.SI.ano.reg <-epi.prev(prev.SI.ano.reg$n, prev.SI.ano.reg$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.SI.ano.reg_2 <- epiprev.SI.ano.reg[["ap"]]

#unindo os dados de prevalencia
prev.SI.ano.reg <- prev.SI.ano.reg %>% 
  bind_cols(prev.SI.ano.reg_2) %>% 
  mutate(est = round(est, 2))




#NV MEMBROS
nv.membros.ano.reg <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Defeitos de membros") %>%
  group_by(AnoNascimento, RegiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento, RegiaoSaude) %>%
  count()


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.membros.ano.reg <- nv.membros.ano.reg %>% 
  left_join(nv.ano.reg, nv.membros.ano.reg, by = c('AnoNascimento', 'RegiaoSaude'))

#calculando prevalencia
epiprev.membros.ano.reg <-epi.prev(prev.membros.ano.reg$n, prev.membros.ano.reg$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.membros.ano.reg_2 <- epiprev.membros.ano.reg[["ap"]]

#unindo os dados de prevalencia
prev.membros.ano.reg <- prev.membros.ano.reg %>% 
  bind_cols(prev.membros.ano.reg_2) %>% 
  mutate(est = round(est, 2))



#NV DEFEITOS ABDOMINAIS
nv.abdominal.ano.reg <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Defeitos da parede abdominal") %>%
  group_by(AnoNascimento, RegiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento, RegiaoSaude) %>%
  count()

#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.abdominal.ano.reg <- nv.abdominal.ano.reg %>% 
  left_join(nv.ano.reg, nv.abdominal.ano.reg, by = c('AnoNascimento', 'RegiaoSaude'))

#calculando prevalencia
epiprev.abdominal.ano.reg <-epi.prev(prev.abdominal.ano.reg$n, prev.abdominal.ano.reg$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.abdominal.ano.reg_2 <- epiprev.abdominal.ano.reg[["ap"]]

#unindo os dados de prevalencia
prev.abdominal.ano.reg <- prev.abdominal.ano.reg %>% 
  bind_cols(prev.abdominal.ano.reg_2) %>% 
  mutate(est = round(est, 2))



#NV SÍNDROME DE DOWN
nv.down.ano.reg <- banco.anom.grupos %>%
  filter(GrupoCID10 == "Síndrome de Down") %>%
  group_by(AnoNascimento, RegiaoSaude, NumeroIdentificacao) %>%
  count() %>%
  group_by(AnoNascimento, RegiaoSaude) %>%
  count()


#Prevalencia
#unindo os dados de nascidos vivos e anomalias
prev.down.ano.reg <- nv.down.ano.reg %>% 
  left_join(nv.ano.reg, nv.down.ano.reg, by = c('AnoNascimento', 'RegiaoSaude'))

#calculando prevalencia
epiprev.down.ano.reg <-epi.prev(prev.down.ano.reg$n, prev.down.ano.reg$nasc_vivos, se = 0.95, sp = 0.95, units = 10000)

#criando objeto com prevalencias e IC
prev.down.ano.reg_2 <- epiprev.down.ano.reg[["ap"]]

#unindo os dados de prevalencia
prev.down.ano.reg <- prev.down.ano.reg %>% 
  bind_cols(prev.down.ano.reg_2) %>% 
  mutate(est = round(est, 2))



#GRAFICOS

#TABELAS
#SELECIONADAS
tab_prev.ano.reg <- prev.ano.reg %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.ano.reg$Variavel <- "Todas anomalias selecionadas"


#TUBO NEURAL
tab_prev.tuboneural.ano.reg <- prev.tuboneural.ano.reg %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.tuboneural.ano.reg$Variavel <- "Defeitos de tubo neural"


#MICROCEFALIA
tab_prev.microce.ano.reg <- prev.microce.ano.reg %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.microce.ano.reg$Variavel <- "Microcefalia"


#CARDIOPATIA
tab_prev.cardio.ano.reg <- prev.cardio.ano.reg %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.cardio.ano.reg$Variavel <- "Cardiopatias congênitas"


#GENITAIS
tab_prev.genitais.ano.reg <- prev.genitais.ano.reg %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.genitais.ano.reg$Variavel <- "Anomalias de órgãos genitais"



#HIPOSPADIAS
tab_prev.hipospadias.ano.reg <- prev.hipospadias.ano.reg %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.hipospadias.ano.reg$Variavel <- "Anomalias de órgãos genitais - Hipospádias"


#SEXO INDEFINIDO
tab_prev.SI.ano.reg <- prev.SI.ano.reg %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.SI.ano.reg$Variavel <- "Anomalias de órgãos genitais - Sexo indefinido"



#MEMBROS
tab_prev.membros.ano.reg <- prev.membros.ano.reg %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.membros.ano.reg$Variavel <- "Defeitos de membros"


#FENDAS
tab_prev.fendas.ano.reg <- prev.fendas.ano.reg %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.fendas.ano.reg$Variavel <- "Fendas orais"


#ABDOMINAL
tab_prev.abdominal.ano.reg <- prev.abdominal.ano.reg %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.abdominal.ano.reg$Variavel <- "Defeitos da parede abdominal"



#DOWN
tab_prev.down.ano.reg <- prev.down.ano.reg %>% 
  rename (NascidosVivosACs = n) %>% 
  rename (NascidosVivos = nasc_vivos) %>% 
  rename (Prevalencia10000 = est)

tab_prev.down.ano.reg$Variavel <- "Síndrome de Down"


tab_grafico.reg <- tab_prev.ano.reg %>% 
  rbind(tab_prev.tuboneural.ano.reg) %>%
  rbind(tab_prev.cardio.ano.reg) %>%
  rbind(tab_prev.membros.ano.reg) %>%
  rbind(tab_prev.fendas.ano.reg) %>%
  rbind(tab_prev.genitais.ano.reg) %>%
  rbind(tab_prev.hipospadias.ano.reg) %>%
  rbind(tab_prev.SI.ano.reg) %>%
  rbind(tab_prev.abdominal.ano.reg) %>%
  rbind(tab_prev.microce.ano.reg) %>%
  rbind(tab_prev.down.ano.reg)


## ---- grafico.reg.nv --------
#Grafico faced wrap com todos nv
ggplot(tab_grafico.reg, aes(x = AnoNascimento, y = NascidosVivos, color = RegiaoSaude, group = RegiaoSaude)) +
  geom_line(size=1) +
  geom_point(size=2.5) +
  scale_x_continuous(breaks=2011:2020,labels=11:20) +
  labs(color = "Região de Saúde", y = "Nascidos Vivos", x = "Ano de Nascimento") +
  facet_wrap(RegiaoSaude ~., nrow = 4, scales = "free_y") 



## ---- grafico.reg.nv.auc --------
##REGIAO Alto Uruguai Catarinense
#Grafico nascidos vivos
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Alto Uruguai Catarinense",], aes(x = AnoNascimento, y = NascidosVivos)) +
  geom_line(size=1, color = "purple") +
  geom_point(size=2.5, color = "purple") +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Alto Uruguai Catarinense", "NascidosVivos"]$NascidosVivos,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Alto Uruguai Catarinense", "NascidosVivos"]$NascidosVivos, accuracy = 1, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5,  vjust = -1.25, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(y = "Nascidos Vivos", x = "Ano de Nascimento")


## ---- grafico.reg.nv.ac.auc --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Alto Uruguai Catarinense",], aes(x = AnoNascimento, y = NascidosVivosACs, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Alto Uruguai Catarinense", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_y_continuous(breaks = seq(from = 0, to = 20, by = 2)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~., ncol = 3, scales = "free_y") 


## ---- grafico.reg.prev.auc --------
#Grafico faced wrap com todas prevalencias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Alto Uruguai Catarinense",], aes(x = AnoNascimento, y = Prevalencia10000, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Alto Uruguai Catarinense", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Alto Uruguai Catarinense", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 170, by = 10)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 



## ---- grafico.reg.nv.avi --------
##REGIAO Alto Vale do Itajaí
#Grafico nascidos vivos
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Alto Vale do Itajaí",], aes(x = AnoNascimento, y = NascidosVivos)) +
  geom_line(size=1, color = "purple") +
  geom_point(size=2.5, color = "purple") +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Alto Vale do Itajaí", "NascidosVivos"]$NascidosVivos,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Alto Vale do Itajaí", "NascidosVivos"]$NascidosVivos, accuracy = 1, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5,  vjust = -1.25, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(y = "Nascidos Vivos", x = "Ano de Nascimento")


## ---- grafico.reg.nv.ac.avi --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Alto Vale do Itajaí",], aes(x = AnoNascimento, y = NascidosVivosACs, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Alto Vale do Itajaí", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_y_continuous(breaks = seq(from = 0, to = 20, by = 2)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 



## ---- grafico.reg.prev.avi --------
#Grafico faced wrap com todas prevalencias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Alto Vale do Itajaí",], aes(x = AnoNascimento, y = Prevalencia10000, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Alto Vale do Itajaí", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Alto Vale do Itajaí", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 170, by = 10)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 


## ---- grafico.reg.nv.avrp --------
##REGIAO Alto Vale do R.do Peixe
#Grafico nascidos vivos
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Alto Vale do R.do Peixe",], aes(x = AnoNascimento, y = NascidosVivos)) +
  geom_line(size=1, color = "purple") +
  geom_point(size=2.5, color = "purple") +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Alto Vale do R.do Peixe", "NascidosVivos"]$NascidosVivos,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Alto Vale do R.do Peixe", "NascidosVivos"]$NascidosVivos, accuracy = 1, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5,  vjust = -1.25, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(y = "Nascidos Vivos", x = "Ano de Nascimento")


## ---- grafico.reg.nv.ac.avrp --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Alto Vale do R.do Peixe",], aes(x = AnoNascimento, y = NascidosVivosACs, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Alto Vale do R.do Peixe", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_y_continuous(breaks = seq(from = 0, to = 20, by = 2)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 


## ---- grafico.reg.prev.avrp --------
#Grafico faced wrap com todas prevalencias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Alto Vale do R.do Peixe",], aes(x = AnoNascimento, y = Prevalencia10000, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Alto Vale do R.do Peixe", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Alto Vale do R.do Peixe", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 170, by = 10)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 



## ---- grafico.reg.nv.carb --------
##REGIAO Carbonífera
#Grafico nascidos vivos
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Carbonífera",], aes(x = AnoNascimento, y = NascidosVivos)) +
  geom_line(size=1, color = "purple") +
  geom_point(size=2.5, color = "purple") +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Carbonífera", "NascidosVivos"]$NascidosVivos,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Carbonífera", "NascidosVivos"]$NascidosVivos, accuracy = 1, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5,  vjust = -1.25, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(y = "Nascidos Vivos", x = "Ano de Nascimento")



## ---- grafico.reg.nv.ac.carb --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Carbonífera",], aes(x = AnoNascimento, y = NascidosVivosACs, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Carbonífera", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_y_continuous(breaks = seq(from = 0, to = 20, by = 2)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 


## ---- grafico.reg.prev.carb --------
#Grafico faced wrap com todas prevalencias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Carbonífera",], aes(x = AnoNascimento, y = Prevalencia10000, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Carbonífera", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Carbonífera", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 170, by = 10)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 




## ---- grafico.reg.nv.eo --------
##REGIAO Extremo Oeste
#Grafico nascidos vivos
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Extremo Oeste",], aes(x = AnoNascimento, y = NascidosVivos)) +
  geom_line(size=1, color = "purple") +
  geom_point(size=2.5, color = "purple") +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Extremo Oeste", "NascidosVivos"]$NascidosVivos,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Extremo Oeste", "NascidosVivos"]$NascidosVivos, accuracy = 1, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5,  vjust = -1.25, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(y = "Nascidos Vivos", x = "Ano de Nascimento")



## ---- grafico.reg.nv.ac.eo --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Extremo Oeste",], aes(x = AnoNascimento, y = NascidosVivosACs, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Extremo Oeste", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_y_continuous(breaks = seq(from = 0, to = 20, by = 2)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 



## ---- grafico.reg.prev.eo --------
#Grafico faced wrap com todas prevalencias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Extremo Oeste",], aes(x = AnoNascimento, y = Prevalencia10000, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Extremo Oeste", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Extremo Oeste", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 170, by = 10)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 



## ---- grafico.reg.nv.esc --------
##REGIAO Extremo Sul Catarinense
#Grafico nascidos vivos
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Extremo Sul Catarinense",], aes(x = AnoNascimento, y = NascidosVivos)) +
  geom_line(size=1, color = "purple") +
  geom_point(size=2.5, color = "purple") +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Extremo Sul Catarinense", "NascidosVivos"]$NascidosVivos,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Extremo Sul Catarinense", "NascidosVivos"]$NascidosVivos, accuracy = 1, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5,  vjust = -1.25, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(y = "Nascidos Vivos", x = "Ano de Nascimento")



## ---- grafico.reg.nv.ac.esc --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Extremo Sul Catarinense",], aes(x = AnoNascimento, y = NascidosVivosACs, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Extremo Sul Catarinense", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_y_continuous(breaks = seq(from = 0, to = 20, by = 2)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 



## ---- grafico.reg.prev.esc --------
#Grafico faced wrap com todas prevalencias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Extremo Sul Catarinense",], aes(x = AnoNascimento, y = Prevalencia10000, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Extremo Sul Catarinense", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Extremo Sul Catarinense", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 170, by = 10)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 




## ---- grafico.reg.nv.fri --------
##REGIAO Foz do Rio Itajaí
#Grafico nascidos vivos
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Foz do Rio Itajaí",], aes(x = AnoNascimento, y = NascidosVivos)) +
  geom_line(size=1, color = "purple") +
  geom_point(size=2.5, color = "purple") +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Foz do Rio Itajaí", "NascidosVivos"]$NascidosVivos,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Foz do Rio Itajaí", "NascidosVivos"]$NascidosVivos, accuracy = 1, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5,  vjust = -1.25, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(y = "Nascidos Vivos", x = "Ano de Nascimento")


## ---- grafico.reg.nv.ac.fri --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Foz do Rio Itajaí",], aes(x = AnoNascimento, y = NascidosVivosACs, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Foz do Rio Itajaí", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_y_continuous(breaks = seq(from = 0, to = 20, by = 2)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 



## ---- grafico.reg.prev.fri --------
#Grafico faced wrap com todas prevalencias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Foz do Rio Itajaí",], aes(x = AnoNascimento, y = Prevalencia10000, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Foz do Rio Itajaí", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Foz do Rio Itajaí", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 170, by = 10)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 



## ---- grafico.reg.nv.gf --------
##REGIAO Grande Florianópolis
#Grafico nascidos vivos
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Grande Florianópolis",], aes(x = AnoNascimento, y = NascidosVivos)) +
  geom_line(size=1, color = "purple") +
  geom_point(size=2.5, color = "purple") +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Grande Florianópolis", "NascidosVivos"]$NascidosVivos,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Grande Florianópolis", "NascidosVivos"]$NascidosVivos, accuracy = 1, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5,  vjust = -1.25, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(y = "Nascidos Vivos", x = "Ano de Nascimento")



## ---- grafico.reg.nv.ac.gf --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Grande Florianópolis",], aes(x = AnoNascimento, y = NascidosVivosACs, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Grande Florianópolis", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_y_continuous(breaks = seq(from = 0, to = 20, by = 2)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 



## ---- grafico.reg.prev.gf --------
#Grafico faced wrap com todas prevalencias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Grande Florianópolis",], aes(x = AnoNascimento, y = Prevalencia10000, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Grande Florianópolis", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Grande Florianópolis", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 170, by = 10)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 



## ---- grafico.reg.nv.lag --------
##REGIAO Laguna
#Grafico nascidos vivos
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Laguna",], aes(x = AnoNascimento, y = NascidosVivos)) +
  geom_line(size=1, color = "purple") +
  geom_point(size=2.5, color = "purple") +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Laguna", "NascidosVivos"]$NascidosVivos,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Laguna", "NascidosVivos"]$NascidosVivos, accuracy = 1, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5,  vjust = -1.25, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(y = "Nascidos Vivos", x = "Ano de Nascimento")



## ---- grafico.reg.nv.ac.lag --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Laguna",], aes(x = AnoNascimento, y = NascidosVivosACs, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Laguna", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_y_continuous(breaks = seq(from = 0, to = 20, by = 2)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 



## ---- grafico.reg.prev.lag --------
#Grafico faced wrap com todas prevalencias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Laguna",], aes(x = AnoNascimento, y = Prevalencia10000, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Laguna", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Laguna", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 170, by = 10)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 




## ---- grafico.reg.nv.mvi --------
##REGIAO Médio Vale do Itajaí
#Grafico nascidos vivos
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Médio Vale do Itajaí",], aes(x = AnoNascimento, y = NascidosVivos)) +
  geom_line(size=1, color = "purple") +
  geom_point(size=2.5, color = "purple") +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Médio Vale do Itajaí", "NascidosVivos"]$NascidosVivos,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Médio Vale do Itajaí", "NascidosVivos"]$NascidosVivos, accuracy = 1, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5,  vjust = -1.25, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(y = "Nascidos Vivos", x = "Ano de Nascimento")



## ---- grafico.reg.nv.ac.mvi --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Médio Vale do Itajaí",], aes(x = AnoNascimento, y = NascidosVivosACs, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Médio Vale do Itajaí", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_y_continuous(breaks = seq(from = 0, to = 20, by = 2)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 



## ---- grafico.reg.nv.prev.mvi --------
#Grafico faced wrap com todas prevalencias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Médio Vale do Itajaí",], aes(x = AnoNascimento, y = Prevalencia10000, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Médio Vale do Itajaí", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Médio Vale do Itajaí", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 170, by = 10)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 




## ---- grafico.reg.nv.mo --------
##REGIAO Meio Oeste
#Grafico nascidos vivos
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Meio Oeste",], aes(x = AnoNascimento, y = NascidosVivos)) +
  geom_line(size=1, color = "purple") +
  geom_point(size=2.5, color = "purple") +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Meio Oeste", "NascidosVivos"]$NascidosVivos,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Meio Oeste", "NascidosVivos"]$NascidosVivos, accuracy = 1, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5,  vjust = -1.25, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(y = "Nascidos Vivos", x = "Ano de Nascimento")



## ---- grafico.reg.nv.ac.mo --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Meio Oeste",], aes(x = AnoNascimento, y = NascidosVivosACs, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Meio Oeste", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_y_continuous(breaks = seq(from = 0, to = 20, by = 2)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 



## ---- grafico.reg.prev.mo --------
#Grafico faced wrap com todas prevalencias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Meio Oeste",], aes(x = AnoNascimento, y = Prevalencia10000, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Meio Oeste", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Meio Oeste", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 170, by = 10)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 




## ---- grafico.reg.nv.nord --------
##REGIAO Nordeste
#Grafico nascidos vivos
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Nordeste",], aes(x = AnoNascimento, y = NascidosVivos)) +
  geom_line(size=1, color = "purple") +
  geom_point(size=2.5, color = "purple") +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Nordeste", "NascidosVivos"]$NascidosVivos,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Nordeste", "NascidosVivos"]$NascidosVivos, accuracy = 1, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5,  vjust = -1.25, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(y = "Nascidos Vivos", x = "Ano de Nascimento")



## ---- grafico.reg.nv.ac.nord --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Nordeste",], aes(x = AnoNascimento, y = NascidosVivosACs, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Nordeste", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_y_continuous(breaks = seq(from = 0, to = 20, by = 2)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 



## ---- grafico.reg.prev.nord --------
#Grafico faced wrap com todas prevalencias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Nordeste",], aes(x = AnoNascimento, y = Prevalencia10000, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Nordeste", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Nordeste", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 170, by = 10)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 



## ---- grafico.reg.nv.oeste --------
##REGIAO Oeste
#Grafico nascidos vivos
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Oeste",], aes(x = AnoNascimento, y = NascidosVivos)) +
  geom_line(size=1, color = "purple") +
  geom_point(size=2.5, color = "purple") +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Oeste", "NascidosVivos"]$NascidosVivos,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Oeste", "NascidosVivos"]$NascidosVivos, accuracy = 1, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5,  vjust = -1.25, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(y = "Nascidos Vivos", x = "Ano de Nascimento")


## ---- grafico.reg.nv.ac.oeste --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Oeste",], aes(x = AnoNascimento, y = NascidosVivosACs, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Oeste", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_y_continuous(breaks = seq(from = 0, to = 20, by = 2)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 



## ---- grafico.reg.prev.oeste --------
#Grafico faced wrap com todas prevalencias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Oeste",], aes(x = AnoNascimento, y = Prevalencia10000, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Oeste", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Oeste", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 170, by = 10)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~.,ncol = 3, scales = "free_y") 




## ---- grafico.reg.nv.pn --------
##REGIAO Planalto Norte
#Grafico nascidos vivos
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Planalto Norte",], aes(x = AnoNascimento, y = NascidosVivos)) +
  geom_line(size=1, color = "purple") +
  geom_point(size=2.5, color = "purple") +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Planalto Norte", "NascidosVivos"]$NascidosVivos,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Planalto Norte", "NascidosVivos"]$NascidosVivos, accuracy = 1, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5,  vjust = -1.25, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(y = "Nascidos Vivos", x = "Ano de Nascimento")




## ---- grafico.reg.nv.ac.pn --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Planalto Norte",], aes(x = AnoNascimento, y = NascidosVivosACs, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Planalto Norte", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_y_continuous(breaks = seq(from = 0, to = 20, by = 2)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~., ncol = 3, scales = "free_y") 



## ---- grafico.reg.prev.pn --------
#Grafico faced wrap com todas prevalencias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Planalto Norte",], aes(x = AnoNascimento, y = Prevalencia10000, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Planalto Norte", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Planalto Norte", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 170, by = 10)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~., ncol = 3, scales = "free_y") 



## ---- grafico.reg.nv.sc --------
##REGIAO Serra Catarinense
#Grafico nascidos vivos
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Serra Catarinense",], aes(x = AnoNascimento, y = NascidosVivos)) +
  geom_line(size=1, color = "purple") +
  geom_point(size=2.5, color = "purple") +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Serra Catarinense", "NascidosVivos"]$NascidosVivos,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Serra Catarinense", "NascidosVivos"]$NascidosVivos, accuracy = 1, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5,  vjust = -1.25, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(y = "Nascidos Vivos", x = "Ano de Nascimento")



## ---- grafico.reg.nv.ac.sc --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Serra Catarinense",], aes(x = AnoNascimento, y = NascidosVivosACs, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Serra Catarinense", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_y_continuous(breaks = seq(from = 0, to = 20, by = 2)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~., ncol = 3, scales = "free_y") 



## ---- grafico.reg.prev.sc --------
#Grafico faced wrap com todas prevalencias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Serra Catarinense",], aes(x = AnoNascimento, y = Prevalencia10000, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Serra Catarinense", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Serra Catarinense", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 170, by = 10)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~., ncol = 3, scales = "free_y") 




## ---- grafico.reg.nv.xanx --------
##REGIAO Xanxerê
#Grafico nascidos vivos
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Xanxerê",], aes(x = AnoNascimento, y = NascidosVivos)) +
  geom_line(size=1, color = "purple") +
  geom_point(size=2.5, color = "purple") +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Xanxerê", "NascidosVivos"]$NascidosVivos,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Xanxerê", "NascidosVivos"]$NascidosVivos, accuracy = 1, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5,  vjust = -1.25, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(y = "Nascidos Vivos", x = "Ano de Nascimento")



## ---- grafico.reg.nv.ac.xanx --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Xanxerê",], aes(x = AnoNascimento, y = NascidosVivosACs, color = Variavel, group = Variavel)) +
  geom_line(size=1) +
  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Xanxerê", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_y_continuous(breaks = seq(from = 0, to = 20, by = 2)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~., ncol = 3, scales = "free_y") 



## ---- grafico.reg.prev.xanx --------
#Grafico faced wrap com todas prevalencias
ggplot(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Xanxerê",], aes(x = AnoNascimento, y = Prevalencia10000, color = Variavel, group = Variavel)) +
  geom_line(size=1) +

  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Xanxerê", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$RegiaoSaude == "Xanxerê", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 170, by = 10)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Grupos de Anomalias", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(Variavel ~., ncol = 3, scales = "free_y") 



## ---- tabela.grafico.reg --------
tabela_grafico.reg <- tab_grafico.reg %>% 
  rename(GrupoAnomalias = Variavel) %>% 
  mutate (Prevalencia10000 = scales::number(Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (lower = scales::number(lower, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>% 
  mutate (upper = scales::number(upper, accuracy = 0.01, big.mark = ".", decimal.mark = ",")) %>%
  unite (col = IC95, lower:upper, sep = "-")





#POR GRUPOS DE ANOMALIAS

#MACRORREGIAO

#NV COM ACS

## ---- grafico.macro.nv.ac.dtn --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.macro[tab_grafico.macro$Variavel == "Defeitos de tubo neural",], aes(x = AnoNascimento, y = NascidosVivosACs, color = MacrorregiaoSaude, group = MacrorregiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.macro[tab_grafico.macro$Variavel == "Defeitos de tubo neural", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Macrorregião de Saúde", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(MacrorregiaoSaude ~., scales = "free_y") 


## ---- grafico.macro.nv.ac.micro --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.macro[tab_grafico.macro$Variavel == "Microcefalia",], aes(x = AnoNascimento, y = NascidosVivosACs, color = MacrorregiaoSaude, group = MacrorregiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.macro[tab_grafico.macro$Variavel == "Microcefalia", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Macrorregião de Saúde", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(MacrorregiaoSaude ~., scales = "free_y") 


## ---- grafico.macro.nv.ac.cardio --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.macro[tab_grafico.macro$Variavel == "Cardiopatias congênitas",], aes(x = AnoNascimento, y = NascidosVivosACs, color = MacrorregiaoSaude, group = MacrorregiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.macro[tab_grafico.macro$Variavel == "Cardiopatias congênitas", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Macrorregião de Saúde", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(MacrorregiaoSaude ~., scales = "free_y") 


## ---- grafico.macro.nv.ac.fendas --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.macro[tab_grafico.macro$Variavel == "Fendas orais",], aes(x = AnoNascimento, y = NascidosVivosACs, color = MacrorregiaoSaude, group = MacrorregiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.macro[tab_grafico.macro$Variavel == "Fendas orais", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Macrorregião de Saúde", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(MacrorregiaoSaude ~., scales = "free_y") 


## ---- grafico.macro.nv.ac.genitais --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.macro[tab_grafico.macro$Variavel == "Anomalias de órgãos genitais",], aes(x = AnoNascimento, y = NascidosVivosACs, color = MacrorregiaoSaude, group = MacrorregiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.macro[tab_grafico.macro$Variavel == "Anomalias de órgãos genitais", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Macrorregião de Saúde", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(MacrorregiaoSaude ~., scales = "free_y") 



## ---- grafico.macro.nv.ac.membros --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.macro[tab_grafico.macro$Variavel == "Defeitos de membros",], aes(x = AnoNascimento, y = NascidosVivosACs, color = MacrorregiaoSaude, group = MacrorregiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.macro[tab_grafico.macro$Variavel == "Defeitos de membros", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Macrorregião de Saúde", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(MacrorregiaoSaude ~., scales = "free_y") 



## ---- grafico.macro.nv.ac.abdominal --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.macro[tab_grafico.macro$Variavel == "Defeitos da parede abdominal",], aes(x = AnoNascimento, y = NascidosVivosACs, color = MacrorregiaoSaude, group = MacrorregiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.macro[tab_grafico.macro$Variavel == "Defeitos da parede abdominal", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Macrorregião de Saúde", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(MacrorregiaoSaude ~., scales = "free_y") 


## ---- grafico.macro.nv.ac.down --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.macro[tab_grafico.macro$Variavel == "Síndrome de Down",], aes(x = AnoNascimento, y = NascidosVivosACs, color = MacrorregiaoSaude, group = MacrorregiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.macro[tab_grafico.macro$Variavel == "Síndrome de Down", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Macrorregião de Saúde", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(MacrorregiaoSaude ~., scales = "free_y")


## ---- grafico.macro.nv.ac.todas --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.macro[tab_grafico.macro$Variavel == "Todas anomalias selecionadas",], aes(x = AnoNascimento, y = NascidosVivosACs, color = MacrorregiaoSaude, group = MacrorregiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.macro[tab_grafico.macro$Variavel == "Todas anomalias selecionadas", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Macrorregião de Saúde", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(MacrorregiaoSaude ~., scales = "free_y")


#PREVALENCIAS

## ---- grafico.macro.prev.dtn --------
#Grafico faced wrap com todos prevalencia
ggplot(tab_grafico.macro[tab_grafico.macro$Variavel == "Defeitos de tubo neural",], aes(x = AnoNascimento, y = Prevalencia10000, color = MacrorregiaoSaude, group = MacrorregiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.macro[tab_grafico.macro$Variavel == "Defeitos de tubo neural", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.macro[tab_grafico.macro$Variavel == "Defeitos de tubo neural", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 20, by = 2)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Macrorregião de Saúde", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(MacrorregiaoSaude ~., scales = "free_y") 


## ---- grafico.macro.prev.micro --------
#Grafico faced wrap com todos prevalencia
ggplot(tab_grafico.macro[tab_grafico.macro$Variavel == "Microcefalia",], aes(x = AnoNascimento, y = Prevalencia10000, color = MacrorregiaoSaude, group = MacrorregiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.macro[tab_grafico.macro$Variavel == "Microcefalia", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.macro[tab_grafico.macro$Variavel == "Microcefalia", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 20, by = 2)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Macrorregião de Saúde", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(MacrorregiaoSaude ~., scales = "free_y") 



## ---- grafico.macro.prev.cardio --------
#Grafico faced wrap com todos prevalencia
ggplot(tab_grafico.macro[tab_grafico.macro$Variavel == "Cardiopatias congênitas",], aes(x = AnoNascimento, y = Prevalencia10000, color = MacrorregiaoSaude, group = MacrorregiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.macro[tab_grafico.macro$Variavel == "Cardiopatias congênitas", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.macro[tab_grafico.macro$Variavel == "Cardiopatias congênitas", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 40, by = 2)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Macrorregião de Saúde", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(MacrorregiaoSaude ~., scales = "free_y")  



## ---- grafico.macro.prev.fendas --------
#Grafico faced wrap com todos prevalencia
ggplot(tab_grafico.macro[tab_grafico.macro$Variavel == "Fendas orais",], aes(x = AnoNascimento, y = Prevalencia10000, color = MacrorregiaoSaude, group = MacrorregiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.macro[tab_grafico.macro$Variavel == "Fendas orais", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.macro[tab_grafico.macro$Variavel == "Fendas orais", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 40, by = 2)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Macrorregião de Saúde", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(MacrorregiaoSaude ~., scales = "free_y") 



## ---- grafico.macro.prev.genitais --------
#Grafico faced wrap com todos prevalencia
ggplot(tab_grafico.macro[tab_grafico.macro$Variavel == "Anomalias de órgãos genitais",], aes(x = AnoNascimento, y = Prevalencia10000, color = MacrorregiaoSaude, group = MacrorregiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.macro[tab_grafico.macro$Variavel == "Anomalias de órgãos genitais", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.macro[tab_grafico.macro$Variavel == "Anomalias de órgãos genitais", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 40, by = 2)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Macrorregião de Saúde", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(MacrorregiaoSaude ~., scales = "free_y") 


## ---- grafico.macro.prev.hipospadias --------
#Grafico faced wrap com todos prevalencia
ggplot(tab_grafico.macro[tab_grafico.macro$Variavel == "Anomalias de órgãos genitais - Hipospádias",], aes(x = AnoNascimento, y = Prevalencia10000, color = MacrorregiaoSaude, group = MacrorregiaoSaude)) +
  geom_line(size=1) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.macro[tab_grafico.macro$Variavel == "Anomalias de órgãos genitais - Hipospádias", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.macro[tab_grafico.macro$Variavel == "Anomalias de órgãos genitais - Hipospádias", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 40, by = 2)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Macrorregião de Saúde", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(MacrorregiaoSaude ~., scales = "free_y") 


## ---- grafico.macro.prev.SI --------
#Grafico faced wrap com todos prevalencia
ggplot(tab_grafico.macro[tab_grafico.macro$Variavel == "Anomalias de órgãos genitais - Sexo indefinido",], aes(x = AnoNascimento, y = Prevalencia10000, color = MacrorregiaoSaude, group = MacrorregiaoSaude)) +
  geom_line(size=1) +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.macro[tab_grafico.macro$Variavel == "Anomalias de órgãos genitais - Sexo indefinido", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.macro[tab_grafico.macro$Variavel == "Anomalias de órgãos genitais - Sexo indefinido", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 40, by = 2)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Macrorregião de Saúde", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(MacrorregiaoSaude ~., scales = "free_y") 




## ---- grafico.macro.prev.membros --------
#Grafico faced wrap com todos prevalencia
ggplot(tab_grafico.macro[tab_grafico.macro$Variavel == "Defeitos de membros",], aes(x = AnoNascimento, y = Prevalencia10000, color = MacrorregiaoSaude, group = MacrorregiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.macro[tab_grafico.macro$Variavel == "Defeitos de membros", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.macro[tab_grafico.macro$Variavel == "Defeitos de membros", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 60, by = 5)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Macrorregião de Saúde", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(MacrorregiaoSaude ~., scales = "free_y") 



## ---- grafico.macro.prev.abdominal --------
#Grafico faced wrap com todos prevalencia
ggplot(tab_grafico.macro[tab_grafico.macro$Variavel == "Defeitos da parede abdominal",], aes(x = AnoNascimento, y = Prevalencia10000, color = MacrorregiaoSaude, group = MacrorregiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.macro[tab_grafico.macro$Variavel == "Defeitos da parede abdominal", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.macro[tab_grafico.macro$Variavel == "Defeitos da parede abdominal", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 60, by = 2)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Macrorregião de Saúde", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(MacrorregiaoSaude ~., scales = "free_y")  


## ---- grafico.macro.prev.down --------
#Grafico faced wrap com todos prevalencia
ggplot(tab_grafico.macro[tab_grafico.macro$Variavel == "Síndrome de Down",], aes(x = AnoNascimento, y = Prevalencia10000, color = MacrorregiaoSaude, group = MacrorregiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.macro[tab_grafico.macro$Variavel == "Síndrome de Down", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.macro[tab_grafico.macro$Variavel == "Síndrome de Down", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 60, by = 2)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Macrorregião de Saúde", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(MacrorregiaoSaude ~., scales = "free_y")  



## ---- grafico.macro.prev.todas --------
#Grafico faced wrap com todos prevalencia
ggplot(tab_grafico.macro[tab_grafico.macro$Variavel == "Todas anomalias selecionadas",], aes(x = AnoNascimento, y = Prevalencia10000, color = MacrorregiaoSaude, group = MacrorregiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.macro[tab_grafico.macro$Variavel == "Todas anomalias selecionadas", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.macro[tab_grafico.macro$Variavel == "Todas anomalias selecionadas", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 150, by = 5)) +
  scale_x_continuous(breaks=2011:2020,labels=2011:2020) +
  labs(color = "Macrorregião de Saúde", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(MacrorregiaoSaude ~., scales = "free_y") 






#REGIAO

#NV COM ACS

## ---- grafico.reg.nv.ac.dtn --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.reg[tab_grafico.reg$Variavel == "Defeitos de tubo neural",], aes(x = AnoNascimento, y = NascidosVivosACs, color = RegiaoSaude, group = RegiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.reg[tab_grafico.reg$Variavel == "Defeitos de tubo neural", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=11:20) +
  labs(color = "Região de Saúde", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(RegiaoSaude ~., nrow = 4, scales = "free_y") 


## ---- grafico.reg.nv.ac.micro --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.reg[tab_grafico.reg$Variavel == "Microcefalia",], aes(x = AnoNascimento, y = NascidosVivosACs, color = RegiaoSaude, group = RegiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.reg[tab_grafico.reg$Variavel == "Microcefalia", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=11:20) +
  labs(color = "Região de Saúde", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(RegiaoSaude ~., nrow = 4, scales = "free_y") 


## ---- grafico.reg.nv.ac.cardio --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.reg[tab_grafico.reg$Variavel == "Cardiopatias congênitas",], aes(x = AnoNascimento, y = NascidosVivosACs, color = RegiaoSaude, group = RegiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.reg[tab_grafico.reg$Variavel == "Cardiopatias congênitas", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=11:20) +
  labs(color = "Região de Saúde", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(RegiaoSaude ~., nrow = 4, scales = "free_y") 


## ---- grafico.reg.nv.ac.fendas --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.reg[tab_grafico.reg$Variavel == "Fendas orais",], aes(x = AnoNascimento, y = NascidosVivosACs, color = RegiaoSaude, group = RegiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.reg[tab_grafico.reg$Variavel == "Fendas orais", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=11:20) +
  labs(color = "Região de Saúde", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(RegiaoSaude ~., nrow = 4, scales = "free_y") 


## ---- grafico.reg.nv.ac.genitais --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.reg[tab_grafico.reg$Variavel == "Anomalias de órgãos genitais",], aes(x = AnoNascimento, y = NascidosVivosACs, color = RegiaoSaude, group = RegiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.reg[tab_grafico.reg$Variavel == "Anomalias de órgãos genitais", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=11:20) +
  labs(color = "Região de Saúde", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(RegiaoSaude ~., nrow = 4, scales = "free_y") 



## ---- grafico.reg.nv.ac.membros --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.reg[tab_grafico.reg$Variavel == "Defeitos de membros",], aes(x = AnoNascimento, y = NascidosVivosACs, color = RegiaoSaude, group = RegiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.reg[tab_grafico.reg$Variavel == "Defeitos de membros", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=11:20) +
  labs(color = "Região de Saúde", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(RegiaoSaude ~., nrow = 4, scales = "free_y") 



## ---- grafico.reg.nv.ac.abdominal --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.reg[tab_grafico.reg$Variavel == "Defeitos da parede abdominal",], aes(x = AnoNascimento, y = NascidosVivosACs, color = RegiaoSaude, group = RegiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.reg[tab_grafico.reg$Variavel == "Defeitos da parede abdominal", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=11:20) +
  labs(color = "Região de Saúde", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(RegiaoSaude ~., nrow = 4, scales = "free_y") 


## ---- grafico.reg.nv.ac.down --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.reg[tab_grafico.reg$Variavel == "Síndrome de Down",], aes(x = AnoNascimento, y = NascidosVivosACs, color = RegiaoSaude, group = RegiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.reg[tab_grafico.reg$Variavel == "Síndrome de Down", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=11:20) +
  labs(color = "Região de Saúde", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(RegiaoSaude ~., nrow = 4, scales = "free_y")


## ---- grafico.reg.nv.ac.todas --------
#Grafico faced wrap com todos nv com anomalias
ggplot(tab_grafico.reg[tab_grafico.reg$Variavel == "Todas anomalias selecionadas",], aes(x = AnoNascimento, y = NascidosVivosACs, color = RegiaoSaude, group = RegiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=5) +
  geom_text(aes(label = tab_grafico.reg[tab_grafico.reg$Variavel == "Todas anomalias selecionadas", "NascidosVivosACs"]$NascidosVivosACs),
            hjust = 0.5,  vjust = 0.5, color = "black", size = 2.5) +
  scale_x_continuous(breaks=2011:2020,labels=11:20) +
  labs(color = "Região de Saúde", y = "Nascidos Vivos c/ ACs", x = "Ano de Nascimento") +
  facet_wrap(RegiaoSaude ~., nrow = 4, scales = "free_y")


#PREVALENCIAS

## ---- grafico.reg.prev.dtn --------
#Grafico faced wrap com todos prevalencia
ggplot(tab_grafico.reg[tab_grafico.reg$Variavel == "Defeitos de tubo neural",], aes(x = AnoNascimento, y = Prevalencia10000, color = RegiaoSaude, group = RegiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$Variavel == "Defeitos de tubo neural", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$Variavel == "Defeitos de tubo neural", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 5)) +
  scale_x_continuous(breaks=2011:2020,labels=11:20) +
  labs(color = "Região de Saúde", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(RegiaoSaude ~., nrow = 4, scales = "free_y") 


## ---- grafico.reg.prev.micro --------
#Grafico faced wrap com todos prevalencia
ggplot(tab_grafico.reg[tab_grafico.reg$Variavel == "Microcefalia",], aes(x = AnoNascimento, y = Prevalencia10000, color = RegiaoSaude, group = RegiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$Variavel == "Microcefalia", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$Variavel == "Microcefalia", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 5)) +
  scale_x_continuous(breaks=2011:2020,labels=11:20) +
  labs(color = "Região de Saúde", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(RegiaoSaude ~., nrow = 4, scales = "free_y") 



## ---- grafico.reg.prev.cardio --------
#Grafico faced wrap com todos prevalencia
ggplot(tab_grafico.reg[tab_grafico.reg$Variavel == "Cardiopatias congênitas",], aes(x = AnoNascimento, y = Prevalencia10000, color = RegiaoSaude, group = RegiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$Variavel == "Cardiopatias congênitas", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$Variavel == "Cardiopatias congênitas", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 5)) +
  scale_x_continuous(breaks=2011:2020,labels=11:20) +
  labs(color = "Região de Saúde", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(RegiaoSaude ~., nrow = 4, scales = "free_y")  



## ---- grafico.reg.prev.fendas --------
#Grafico faced wrap com todos prevalencia
ggplot(tab_grafico.reg[tab_grafico.reg$Variavel == "Fendas orais",], aes(x = AnoNascimento, y = Prevalencia10000, color = RegiaoSaude, group = RegiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$Variavel == "Fendas orais", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$Variavel == "Fendas orais", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 5)) +
  scale_x_continuous(breaks=2011:2020,labels=11:20) +
  labs(color = "Região de Saúde", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(RegiaoSaude ~., nrow = 4, scales = "free_y") 



## ---- grafico.reg.prev.genitais --------
#Grafico faced wrap com todos prevalencia
ggplot(tab_grafico.reg[tab_grafico.reg$Variavel == "Anomalias de órgãos genitais",], aes(x = AnoNascimento, y = Prevalencia10000, color = RegiaoSaude, group = RegiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$Variavel == "Anomalias de órgãos genitais", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$Variavel == "Anomalias de órgãos genitais", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 5)) +
  scale_x_continuous(breaks=2011:2020,labels=11:20) +
  labs(color = "Região de Saúde", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(RegiaoSaude ~., nrow = 4, scales = "free_y") 




## ---- grafico.reg.prev.membros --------
#Grafico faced wrap com todos prevalencia
ggplot(tab_grafico.reg[tab_grafico.reg$Variavel == "Defeitos de membros",], aes(x = AnoNascimento, y = Prevalencia10000, color = RegiaoSaude, group = RegiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$Variavel == "Defeitos de membros", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$Variavel == "Defeitos de membros", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 10)) +
  scale_x_continuous(breaks=2011:2020,labels=11:20) +
  labs(color = "Região de Saúde", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(RegiaoSaude ~., nrow = 4, scales = "free_y") 



## ---- grafico.reg.prev.abdominal --------
#Grafico faced wrap com todos prevalencia
ggplot(tab_grafico.reg[tab_grafico.reg$Variavel == "Defeitos da parede abdominal",], aes(x = AnoNascimento, y = Prevalencia10000, color = RegiaoSaude, group = RegiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$Variavel == "Defeitos da parede abdominal", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$Variavel == "Defeitos da parede abdominal", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 5)) +
  scale_x_continuous(breaks=2011:2020,labels=11:20) +
  labs(color = "Região de Saúde", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(RegiaoSaude ~., nrow = 4, scales = "free_y")  


## ---- grafico.reg.prev.down --------
#Grafico faced wrap com todos prevalencia
ggplot(tab_grafico.reg[tab_grafico.reg$Variavel == "Síndrome de Down",], aes(x = AnoNascimento, y = Prevalencia10000, color = RegiaoSaude, group = RegiaoSaude)) +
  geom_line(size=1) +

  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$Variavel == "Síndrome de Down", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$Variavel == "Síndrome de Down", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 5)) +
  scale_x_continuous(breaks=2011:2020,labels=11:20) +
  labs(color = "Região de Saúde", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(RegiaoSaude ~., nrow = 4, scales = "free_y")  



## ---- grafico.reg.prev.todas --------
#Grafico faced wrap com todos prevalencia
ggplot(tab_grafico.reg[tab_grafico.reg$Variavel == "Todas anomalias selecionadas",], aes(x = AnoNascimento, y = Prevalencia10000, color = RegiaoSaude, group = RegiaoSaude)) +
  geom_line(size=1) +
  
  geom_point(size=2.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5,
                position=position_dodge(0.05), size = 0.5) +
  geom_text(aes(y = tab_grafico.reg[tab_grafico.reg$Variavel == "Todas anomalias selecionadas", "Prevalencia10000"]$Prevalencia10000,
                label = scales::number(tab_grafico.reg[tab_grafico.reg$Variavel == "Todas anomalias selecionadas", "Prevalencia10000"]$Prevalencia10000, accuracy = 0.01, big.mark = ".", decimal.mark = ",")),
            hjust = 0.5, vjust = -1.25, color = "black", size = 3) +
  scale_y_continuous(breaks = seq(from = 0, to = 200, by = 10)) +
  scale_x_continuous(breaks=2011:2020,labels=11:20) +
  labs(color = "Região de Saúde", y = "Prevalência/10.000", x = "Ano de Nascimento") +
  facet_wrap(RegiaoSaude ~., nrow = 4, scales = "free_y") 
