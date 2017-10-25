library(foreign)
library(tidyverse)
library(plm)
library(statar)
library(boxr)

# Fetch data base from box.com
box_fetch(dir_id = 40959786926)

# read files
files <- c("2008_q01","2008_q02", "2008_q03", "2008_q04", "2009_q01", "2009_q02",
         "2009_q03", "2009_q04", "2010_q01", "2010_q02", "2010_q03", "2010_q04")

eph <- list()
for(i in files){
  eph[[i]] <- read.dta(paste("arg_",i,"_ephc.dta", sep=""))
  eph[[i]] <- mutate(eph[[i]], idp_t = i)
}

# row bind (R) == append (Stata)
base <- bind_rows(eph)
rm(eph)

# Duplicates report
nrow(distinct(base, idi_codusu, idi_nro_hogar, idi_componente, idi_ano4, idi_trimestre))


# Household ID

# idi_codusu     = Homes ID
# idi_nro_hogar  = Household ID
# idi_componente = Household memeber ID
base <- base %>% 
  group_by(idi_codusu, idi_nro_hogar)
idp_h <- group_indices(base)
base <- cbind(data.frame(base), idp_h)
rm(idp_h)

# Individual ID
base <- base %>% 
  group_by(idi_componente)
idp_i <- group_indices(base)
base <- cbind(data.frame(base), idp_i)
rm(idp_i)

# Duplicates for idp_t, idp_h and idp_i
nrow(distinct(base, idp_t, idp_h, idp_i))

# counts number of times an observation is repeated

base <- base %>% 
  mutate(idp_ind = paste(idp_h, idp_i, sep="_")) %>%
  group_by(idp_ind) %>% 
  mutate(nrep=n())

# Should repeat no more than four times
ungroup(base) %>% 
  tab(nrep)

# idp_match: dummy for individuals with more than one observation
base <- ungroup(base) %>% 
mutate(idp_match = ifelse(nrep>1,1,0))
base %>% 
  tab(idp_match)

# Cohort IDs
# ID for the first period a HH appears
base <- base %>% 
  group_by(idp_h) %>% 
  mutate(idp_p = min(idp_t)) %>%
  ungroup()

# Replicate rotating panel table
base_m <- base %>% 
  filter(idp_match==1)
base_m %>% 
  tab(idp_p, idp_t)

base_m4 <- base %>% 
  filter(nrep == 4) 
base_m4 %>% 
  tab(idp_p, idp_t)

# replication of rotating panel figure (año = year, trimestre = quarter)
base <- base %>%
  mutate(anot = paste(ano, "_q0", as.numeric(trimestre), sep = ""))

rp_figure <- base %>% 
  filter(idp_match==1 & nrep==4) %>%
  tab(idp_p, anot) %>% 
  select(-c(Percent, Cum.)) %>%
  spread(anot, Freq.) %>%
  print

########## CONTROLS #################

# non-mathing individuals
base <- base %>% 
  mutate(cohp = ifelse(idp_match==0,0,1))

# age-inconsistent individuals
base <- base %>% 
  group_by(nrep, idp_h, idp_i) %>% 
  mutate(difedad = edad - dplyr::lag(edad, order_by = idp_t)) %>%
  ungroup

base %>% 
  tab(difedad)

base$idp_t <- as.numeric(base$idp_t)
base <- base %>% 
  group_by(nrep, idp_h, idp_i) %>% 
  mutate(difperiodo = idp_t-lag(idp_t)) %>%
  ungroup

base %>% tab(difperiodo)

# Small changes
# more than one age older w/r to previous year
base$cohp <- replace(base$cohp, base$difedad >= (-1) & 
                       base$difedad<0, 20)

# Age grew more than expected
base$cohp <- replace(base$cohp, base$difedad==2 & 
                       base$difperiodo<4,30)
base$cohp <- replace(base$cohp, base$difedad==3 & 
                       base$difperiodo>=4 &
                       base$difperiodo<=5,30)

# Large changes
# age grew more than expected by large margins
base$cohp <- replace(base$cohp, base$difedad<(-1), 700)
base$cohp <- replace(base$cohp, base$difedad>=3 & 
                       is.na(base$difedad)==FALSE & 
                       base$difperiodo<4, 700)
base$cohp <- replace(base$cohp, base$difedad>=4 & 
                       is.na(base$difedad)==FALSE & 
                       base$difperiodo>=4 & 
                       base$difperiodo<=5, 700)

# Incoherent period matching (max should be 5)
base$cohp <- replace(base$cohp, base$difperiodo>6 & 
                       is.na(base$difperiodo)==FALSE,800)

# sex changes (Hombre = male, Mujer = female)
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

# % coherent observations
ungroup(base) %>% tab(cohp)

# keep individuals that are repeated more than once
base <- base %>% mutate(nocoherente=ifelse(cohp!=1,1,0))
base <- base %>% group_by(idp_h, idp_i) %>%
  mutate(siemprecoherente = sum(nocoherente)) %>%
  ungroup

base <- base %>% filter(siemprecoherente==0)

# keep full panels only
base <- base %>% filter(nrep==4 & hogarsec=="No")

# delete auxiliary variables
base <- base %>% select(-c(cohp, difedad, difperiodo, nocoherente,
                           siemprecoherente)) 
base <- base %>% group_by(idp_h, idp_i, idp_t, idp_p, idi_ano4, 
                          idi_trimestre, idp_match)

# Save panel
save(base, file = "panel-arg.RData")

rm(list=ls(all=TRUE))
