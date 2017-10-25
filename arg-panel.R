library(foreign)
library(tidyverse)
library(plm)
library(statar)

setwd("F:/Archivos/Computadora CEDLAS/Curso-Quito-2014-Docentes/4-Paneles/arg-datalib")

lista<-c("2008_q01","2008_q02", "2008_q03", "2008_q04", "2009_q01", "2009_q02",
          "2009_q03", "2009_q04", "2010_q01", "2010_q02", "2010_q03", "2010_q04")

# Lectura de las bases
eph<-list()
for(i in lista){
  eph[[i]] <- tbl_df(read.dta(paste("arg_",i,"_ephc.dta", sep="")))
  eph[[i]] <- mutate(eph[[i]], idp_t = i)
}

# row bind (R) == append (Stata)
base <- bind_rows(eph)
rm(eph)

# Duplicates report
nrow(distinct(base, idi_codusu, idi_nro_hogar, idi_componente, idi_ano4, idi_trimestre))


# Identificador del hogar

# idi_codusu     = Código para distinguir viviendas
# idi_nro_hogar  = Código para distinguir hogares
# idi_componente = Número de componente
base <- base %>% group_by(idi_codusu, idi_nro_hogar)
idp_h <- group_indices(base)
base <- cbind(data.frame(base), idp_h)
rm(idp_h)

# Identificador de los individuos 
base <- base %>% group_by(idi_componente)
idp_i <- group_indices(base)
base <- cbind(data.frame(base), idp_i)
rm(idp_i)

# Duplicados en terminos de idp_t idp_h idp_i
nrow(distinct(base, idp_t, idp_h, idp_i))

# Cuenta número de veces que se repite una observación
base <- base %>% mutate(idp_ind = paste(idp_h, idp_i, sep="_"))
base <- base %>% group_by(idp_ind) %>% mutate(nrep=n())

# Como máximo debería aparecer 4 veces
ungroup(base) %>% tab(nrep)

# idp_match: dummy para individuos con más de una observación
base <- ungroup(base) %>% mutate(idp_match = ifelse(nrep>1,1,0))
base %>% tab(idp_match)

# Identifico cohortes
# Identifico el primer período donde aparece cada familia
base <- base %>% group_by(idp_h) %>% mutate(idp_p = min(idp_t))
base <- ungroup(base)

# Replicar cuadro
base_m <- base %>% filter(idp_match==1)
base_m %>% tab(idp_p, idp_t)

base_m4 <- base %>% filter(nrep == 4) 
base_m4 %>% tab(idp_p, idp_t)

# replicamos cuadro del .ppt
base <- base %>%
  mutate(anot = paste(ano, "_q0", as.numeric(trimestre), sep = ""))

tab_ppt <- base %>% filter(idp_match==1 & nrep==4) %>%
  tab(idp_p, anot) %>% 
  select(-c(Percent, Cum)) %>%
  spread(anot, n) %>%
  print

########## CONTROLES #################

# Individuos que no matchean
base <- base %>% mutate(cohp = ifelse(idp_match==0,0,1))

# Individuos con inconsistencias en la edad
base <- base %>% 
  group_by(nrep, idp_h, idp_i) %>% 
  mutate(difedad = edad - dplyr::lag(edad, order_by = idp_t)) %>%
  ungroup

base %>% tab(difedad)

base$idp_t <- as.numeric(base$idp_t)
base <- base %>% group_by(nrep, idp_h, idp_i) %>% 
  mutate(difperiodo = idp_t-lag(idp_t)) %>%
  ungroup

base %>% tab(difperiodo)

# Cambios pequeños
# Edad menor a lo largo del tiempo
#1 año (-) de lo que corresponde respecto de la observacion anterior
base$cohp <- replace(base$cohp, base$difedad >= (-1) & 
                     base$difedad<0, 20)

# Edad creció mas de lo que debería
# 1 año (+) de lo que corresponde respecto de la observacion anterior
base$cohp <- replace(base$cohp, base$difedad==2 & 
                     base$difperiodo<4,30)
base$cohp <- replace(base$cohp, base$difedad==3 & 
                     base$difperiodo>=4 &
                     base$difperiodo<=5,30)

# Cambios grandes	
# Cambios grandes en la edad
# La edad es demasiado dispar respecto de la observacion anterior
base$cohp <- replace(base$cohp, base$difedad<(-1), 700)
base$cohp <- replace(base$cohp, base$difedad>=3 & 
                     is.na(base$difedad)==FALSE & 
                     base$difperiodo<4, 700)
base$cohp <- replace(base$cohp, base$difedad>=4 & 
                     is.na(base$difedad)==FALSE & 
                     base$difperiodo>=4 & 
                     base$difperiodo<=5, 700)

# Matching en períodos muy dispares (en argentina el 
# máximo debería ser 5 por que son 6 trimestres)
base$cohp <- replace(base$cohp, base$difperiodo>6 & 
                     is.na(base$difperiodo)==FALSE,800)

# Cambios de sexo
base <- base %>% mutate(sexo=ifelse(hombre == "Hombre", 1,0))
base <- base %>% group_by(idp_h, idp_i, nrep) %>% 
  mutate(aux1 = sexo-dplyr::lag(sexo, order_by=idp_t)) %>%
  ungroup

base$cohp <- replace(base$cohp, base$aux1!=0 & 
                     is.na(base$aux1)==FALSE &
                     base$cohp==1, 900)
base$cohp <- replace(base$cohp, base$aux1!=0 & 
                     is.na(base$aux1)==FALSE &
                     base$cohp>1 & 
                     base$cohp<800, 901)
base <-base %>% select(-aux1)

# Vemos % de coherentes
ungroup(base) %>% tab(cohp)

# Solo nos quedamos con individuos que están más de una ves
base <- base %>% mutate(nocoherente=ifelse(cohp!=1,1,0))
base <- base %>% group_by(idp_h, idp_i) %>%
  mutate(siemprecoherente = sum(nocoherente)) %>%
  ungroup

base <- base %>% filter(siemprecoherente==0)

# Nos quedamos con paneles completos
base <- base %>% filter(nrep==4 & hogarsec=="No")

# Borro variables auxiliares
base <- base %>% select(-c(cohp, difedad, difperiodo, nocoherente,
                      siemprecoherente)) 
base <- base %>% group_by(idp_h, idp_i, idp_t, idp_p, idi_ano4, 
                        idi_trimestre, idp_match)

# Guardamos el panel
save(base, file = "panel-arg.RData")

rm(list=ls(all=TRUE))
