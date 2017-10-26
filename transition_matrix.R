library(questionr)
library(foreign)
library(tidyverse)
library(sandwich)
library(lmtest)
library(stargazer)
library(tables)
library(statar)
library(plm)
library(broom)

# loads data base
load("panel-arg.RData")
panel_arg <- base
rm(base)
summary(panel_arg)

# Identify indivifuals without retirement benefits (informal)
panel_arg <- panel_arg %>% 
  mutate(informal = as.numeric(djubila == "No"))
panel_arg$informal <- replace(panel_arg$informal,
                            panel_arg$djubila == "Si",0)

# condact: occupational status 
panel_arg <- panel_arg %>%
  mutate(condact=rep(4, length(ocupado)))
panel_arg$condact <- replace(panel_arg$condact, 
                           panel_arg$ocupado=="Ocupado", 1)
panel_arg$condact <- replace(panel_arg$condact, 
                           panel_arg$desocupa=="Desocupado", 2)
panel_arg$condact <- replace(panel_arg$condact, 
                           panel_arg$desocupa=="No desocupado" & 
                             panel_arg$ocupado=="No ocupado", 3)
panel_arg$condact[panel_arg$condact == 4] <- NA
panel_arg$condact <- factor(panel_arg$condact, 
                          labels=c("Ocupado", 
                                   "Desocupado", 
                                   "Inactivo"))
# recode labor status
panel_arg <- panel_arg %>%
  mutate(reloc = rep(0, length(relab)))

panel_arg$reloc <- replace(panel_arg$reloc, 
                         panel_arg$relab=="Empleador" | 
                           panel_arg$relab=="Cuentapropista", 1)

panel_arg$reloc <- replace(panel_arg$reloc, 
                         panel_arg$relab=="Asalariado" & 
                           panel_arg$informal==0, 2)
panel_arg$reloc <- replace(panel_arg$reloc, 
                         panel_arg$relab=="Asalariado" & 
                           panel_arg$informal==1, 3)
panel_arg$reloc <- replace(panel_arg$reloc, 
                         panel_arg$relab=="Desocupado", 4)
panel_arg$reloc <- replace(panel_arg$reloc, 
                         panel_arg$condact=="Inactivo", 5)
panel_arg$reloc[panel_arg$reloc == 0] <- NA
panel_arg$reloc <- factor(panel_arg$reloc, 
                        labels=c("Cuentapropista", 
                                 "Asal. Formal", 
                                 "Asal. Informal", 
                                 "Desocupado", 
                                 "Inactivo"))

# keep only labor status , per capita income quantile and weight variables
panel_alt <- panel_arg %>% 
  group_by(idp_h, idp_i) %>%
  select(c(idp_h, idp_i, idp_t, informal, idp_p, reloc, condact, qipcf, pondera))

panel1 <- panel_alt %>%
  data.frame() %>%
  reshape(direction="wide", idvar=c("idp_h", "idp_i"), 
          timevar="idp_t", 
          v.names=c("condact", "pondera", "informal", "reloc", "qipcf"))
rm(panel_alt)

# labor status
tab1 <- panel1 %>%
  filter(idp_p == "2008_q04") %$%
  questionr::wtd.table(condact.4, condact.9, weights = pondera.9) %>%
  prop.table(., 1) %>%
  latex(., file = "tab1.tex")

# labor condition
tab2 <- panel1 %>%
  filter(idp_p == "2008_q04") %$%
  questionr::wtd.table(reloc.4, reloc.9, weights = pondera.9) %>%
  prop.table(., 1) %>%
  latex(., file = "tab2.tex")

# informal labor
tab3 <- panel1 %>%
  mutate(informal.4 = factor(informal.4, labels = c("No", "Si")),
         informal.9 = factor(informal.9, labels = c("No", "Si"))) %>%
  filter(idp_p == "2008_q04") %$%
  questionr::wtd.table(informal.4, informal.9, weights = pondera.9) %>%
  prop.table(., 1) %>%
  latex(., file = "tab3.tex")

# per capita income quintiles
tab4 <- panel1 %>%
  filter(idp_p == "2008_q04") %$%
  questionr::wtd.table(qipcf.4, qipcf.9, weights = pondera.9) %>%
  prop.table(., 1) %>%
  latex(., file = "tab4.tex")


#--- Transition matrices for all periods

##(1) unemployed in t --> emp, unemp or inactive in t+1?

# loop for periods 1 to 7
panel1 <- panel1 %>%
  mutate(idp_pp = rep(NA, length(idp_p)))
panelr<-list()

# Matriz con resultados
cuad1 <- data.frame(matrix(NA, ncol=3, nrow=7))

idp_p<-c("2008_q01", "2008_q02", "2008_q03", "2008_q04",
         "2009_q01", "2009_q02", "2009_q03")

for(i in 1:7){
  panel1$idp_pp <- replace(panel1$idp_pp, panel1$idp_p==idp_p[i],i)
  panelr[[i]]<-panel1 %>%
    filter(panel1[,i+3] == "Desocupado" & panel1[,64]== i)
  cuad1[i,] <- questionr::wtd.table(panelr[[i]][,i+8], weights=panelr[[i]][,i+20])
}

id <- seq(1,7,1)
cuad1 <- cbind(id, cuad1)

# variblae names
names(cuad1) <- c("id", "Ocupado", "Desocupado", "Inactivo")
suma <- rowSums(cuad1)

tab5 <- rbind(cuad1[1,-1]/suma[1], cuad1[2,-1]/suma[2], cuad1[3,-1]/suma[3],
              cuad1[4,-1]/suma[4], cuad1[5,-1]/suma[5], cuad1[6,-1]/suma[6],
              cuad1[7,-1]/suma[7]) 
tab5 <- cbind(id, tab5)

plot1 <- tab5 %>%
  gather(condicion, part, Ocupado:Inactivo) %>%
  mutate(part = round(part*100, digits = 2)) %>%
  ggplot(aes(x = factor(id), y = part, fill = factor(condicion))) + 
  geom_bar(stat = "identity", colour = "black") +
  geom_text(aes(label=part), position ="stack", hjust=0.5, vjust=3) + 
  xlab("Período") +  ylab("Porciento") + theme_bw() + 
  scale_colour_discrete(name="", 
                        labels=c("Ocupado", "Desocupado", "Inactivo"))+
  theme(legend.position="bottom", legend.direction="horizontal")+
  ggtitle("Transición de desocupados")+
  theme(plot.title = element_text(lineheight=.8, face="bold", size=18))+
  scale_fill_discrete("Condición laboral")
plot1

##(2) IDEM (1) with relab

# cohort loop

panelr2 <- list()

# Matriz con resultados
cuad2 <- data.frame(matrix(0, ncol=5, nrow=7))
for(i in 1:7){
  panelr2[[i]] <- panel1 %>%
    filter(panel1[,i+27]=="Desocupado" & panel1[,64]==i)
  cuad2[i,] <- questionr::wtd.table(panelr2[[i]][,i+32], weights=panelr2[[i]][,i+20])
}

cuad2 <- cbind(id, cuad2)

# Renombramos las variables
names(cuad2) <- c("id", "Cuentapropista", "Asalariado formal", "Asalariado informal",  "Desocupado", "Inactivos")
suma <- rowSums(cuad2)
tab6 <- rbind(cuad2[1,2:6]/suma[1],cuad2[2,2:6]/suma[2], 
              cuad2[3,2:6]/suma[3],cuad2[4,2:6]/suma[4], 
              cuad2[5,2:6]/suma[5],cuad2[6,2:6]/suma[6], 
              cuad2[7,2:6]/suma[7]) 
tab6 <- cbind(id, tab6)

plot2 <- tab6 %>%
  gather(condicion, part, Cuentapropista:Inactivos) %>%
  mutate(part = round(part*100, digits = 2)) %>%
  ggplot(aes(x = factor(id), y = part, fill = factor(condicion))) + 
  geom_bar(stat = "identity", colour = "black") +
  geom_text(aes(label=part), position ="stack", hjust=0.5, vjust=3) + 
  xlab("Período") +  ylab("Porciento") + theme_bw() + 
  scale_colour_discrete(name="", 
                        labels=c("Cuentapropista", "Asalariado formal", "Asalariado informal",  "Desocupado", "Inactivos"))+
  theme(legend.position="bottom", legend.direction="horizontal")+
  ggtitle("Transición del desempleo, ARG")+
  theme(plot.title = element_text(lineheight=.8, face="bold", size=18))+
  scale_fill_discrete("Categoría ocupacional")
plot2


# regression analysis
reg1 <- lm(informal~hombre+edad+I(edad^2)+factor(empresa)+factor(qipcf), 
         data=panel_arg)
summary(reg1)
reg1r <- coeftest(reg1, vcovHC(reg1, type="HC0"))
reg1r

panel2 <- na.omit(select(panel_arg, c(idp_h,idp_i,idp_t,
                                    informal,hombre,edad,empresa,qipcf)))
nrow(distinct(panel2, idp_h, idp_i, idp_t))

panel2 <- panel2 %>%
  mutate(id = paste(idp_h, idp_i, sep="_"))

panel2 <- plm.data(panel2, c("id", "idp_t"))
reg2 <- plm(informal~hombre+edad+I(edad^2)+
            factor(empresa)+factor(qipcf), 
           data=panel2, model="between", na.exclude=TRUE)
summary(reg2)

stargazer(reg1,  digits = 4, out="reg1.tex", 
          digit.separator=".", decimal.mark=",")

lm.probs <- predict(reg1, type="response")
lm.pred <- rep(0,length(lm.probs))
lm.pred[lm.probs>0.5] <- 1
conf <- table(lm.pred,panel2$informal)
a <- rowSums(conf)
b <- colSums(conf)