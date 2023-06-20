#pacotes
library(haven)
library(lavaan)
library(dplyr)
library(psych)
library(xlsx)


######carregar o banco de dados
banco <- read_sav("Banco_BFI2_888.sav")


##################### MODELOS #############################
##modelo AFC sem controle desbalanceado
##Para essa análise foram selecionados os 5 primeiros itens positivos e o primeiro negativo de cada dimensão
modelAFCDesbalSemCrt <- 
"f1 =~ NA*it1  + it2  + it3  + it4  + it5 + it12
 f1 ~~ 1*f1
 f2 =~ NA*it13  + it14  + it15  + it16  + it17 + it24 
 f2 ~~ 1*f2
 f3 =~ NA*it25  + it26  + it27  + it28  + it29 + it36 
 f3 ~~ 1*f3
 f4 =~ NA*it37  + it38  + it40  + it41  + it42 + it48 
 f4 ~~ 1*f4
 f5 =~ NA*it49  + it50  + it51  + it52  + it53 + it60 
 f5 ~~ 1*f5"

##modelo AFC sem controle balanceado
##foram utilizados os 3 primeiros pares de itens de cada dimensão
modelAFCBalancSemCrt <- 
"f1 =~ NA*it1  + it2  + it3  + it10  + it11 + it12
 f1 ~~ 1*f1
 f2 =~ NA*it13  + it14  + it15  + it22  + it23 + it24 
 f2 ~~ 1*f2
 f3 =~ NA*it25  + it26  + it27  + it34  + it35 + it36 
 f3 ~~ 1*f3
 f4 =~ NA*it37  + it38  + it40  + it46  + it47 + it48 
 f4 ~~ 1*f4
 f5 =~ NA*it49  + it50  + it51  + it58  + it59 + it60 
 f5 ~~ 1*f5"

##modelo AFC desbalanceada com MIMIC
modelAFCDesbalAqMIMIC <- 
"f1 =~ NA*it1  + it2  + it3  + it4  + it5 + it12
 f1 ~~ 1*f1
 f2 =~ NA*it13  + it14  + it15  + it16  + it17 + it24 
 f2 ~~ 1*f2
 f3 =~ NA*it25  + it26  + it27  + it28  + it29 + it36 
 f3 ~~ 1*f3
 f4 =~ NA*it37  + it38  + it40  + it41  + it42 + it48 
 f4 ~~ 1*f4
 f5 =~ NA*it49  + it50  + it51  + it52  + it53 + it60 
 f5 ~~ 1*f5
it1  + it2  + it3  + it4  + it5 + it12 ~ AqMIMICdesb
it13  + it14  + it15  + it16  + it17 + it24  ~ AqMIMICdesb
it25  + it26  + it27  + it28  + it29 + it36 ~ AqMIMICdesb
it37  + it38  + it40  + it41  + it42 + it48 ~ AqMIMICdesb
it49  + it50  + it51  + it52  + it53 + it60 ~ AqMIMICdesb
AqMIMICdesb ~~ 0*f1 + 0*f2 + 0*f3 + 0*f4 + 0*f5"

##modelo AFC Balanceada com MIMIC
modelAFCBalAqMIMIC <- 
"f1 =~ NA*it1  + it2  + it3  + it10  + it11 + it12
 f1 ~~ 1*f1
 f2 =~ NA*it13  + it14  + it15  + it22  + it23 + it24 
 f2 ~~ 1*f2
 f3 =~ NA*it25  + it26  + it27  + it34  + it35 + it36 
 f3 ~~ 1*f3
 f4 =~ NA*it37  + it38  + it40  + it46  + it47 + it48 
 f4 ~~ 1*f4
 f5 =~ NA*it49  + it50  + it51  + it58  + it59 + it60 
 f5 ~~ 1*f5
it1  + it2  + it3  + it10  + it11 + it12 ~ AqMIMICbal
it13  + it14  + it15  + it22  + it23 + it24 ~ AqMIMICbal
it25  + it26  + it27  + it34  + it35 + it36 ~ AqMIMICbal
it37  + it38  + it40  + it46  + it47 + it48 ~ AqMIMICbal
it49  + it50  + it51  + it58  + it59 + it60 ~ AqMIMICbal
AqMIMICbal ~~ 0*f1 + 0*f2 + 0*f3 + 0*f4 + 0*f5"


##modelo AFC com interceptos randomicos desbalanceada
modelAFCDesbalIR <- 
"f1 =~ NA*it1  + it2  + it3  + it4  + it5 + it12
 f1 ~~ 1*f1
 f2 =~ NA*it13  + it14  + it15  + it16  + it17 + it24 
 f2 ~~ 1*f2
 f3 =~ NA*it25  + it26  + it27  + it28  + it29 + it36 
 f3 ~~ 1*f3
 f4 =~ NA*it37  + it38  + it40  + it41  + it42 + it48 
 f4 ~~ 1*f4
 f5 =~ NA*it49  + it50  + it51  + it52  + it53 + it60 
 f5 ~~ 1*f5
aq =~ 1*it1  + 1*it2  + 1*it3  + 1*it4  + 1*it5 + 1*it12 +
      1*it13  + 1*it14  + 1*it15  + 1*it16  + 1*it17 + 1*it24 +
      1*it25  + 1*it26  + 1*it27  + 1*it28  + 1*it29 + 1*it36 +
      1*it37  + 1*it38  + 1*it40  + 1*it41  + 1*it42 + 1*it48 +
      1*it49  + 1*it50  + 1*it51  + 1*it52  + 1*it53 + 1*it60 
aq ~~ 0*f1 + 0*f2 + 0*f3 + 0*f4 + 0*f5"


##modelo AFC com interceptos randomicos balanceada
modelAFCBalIR <- 
"f1 =~ NA*it1  + it2  + it3  + it10  + it11 + it12
 f1 ~~ 1*f1
 f2 =~ NA*it13  + it14  + it15  + it22  + it23 + it24 
 f2 ~~ 1*f2
 f3 =~ NA*it25  + it26  + it27  + it34  + it35 + it36 
 f3 ~~ 1*f3
 f4 =~ NA*it37  + it38  + it40  + it46  + it47 + it48 
 f4 ~~ 1*f4
 f5 =~ NA*it49  + it50  + it51  + it58  + it59 + it60 
 f5 ~~ 1*f5
aq =~ 1*it1  + 1*it2  + 1*it3  + 1*it10  + 1*it11 + 1*it12 +
      1*it13  + 1*it14  + 1*it15  + 1*it22  + 1*it23 + 1*it24 +
      1*it25  + 1*it26  + 1*it27  + 1*it34  + 1*it35 + 1*it36 +
      1*it37  + 1*it38  + 1*it40  + 1*it46  + 1*it47 + 1*it48 +
      1*it49  + 1*it50  + 1*it51  + 1*it58  + 1*it59 + 1*it60  
aq ~~ 0*f1 + 0*f2 + 0*f3 + 0*f4 + 0*f5"

#calcular os indicadores de aquiescencia para o MIMIC
banco$AqMIMICdesb <- apply(banco[,c(1, 12, 13, 24, 25, 36, 37, 48, 49, 60)],1, mean)
banco$AqMIMICbal  <- apply(banco[,c(1:3, 10:12, 13:15, 22:24, 25:27, 34:36, 37:38, 40, 46:48, 49:51, 58:60)],1, mean)

##################### ANÁLISES #############################
#balanceadas 
out_AFCBalSemCon<-sem(modelAFCBalancSemCrt, data=banco, ordered = names(banco))
out_AFCBalMIMIC<-sem(modelAFCBalAqMIMIC, data=banco, ordered = names(banco))
out_AFCBalIR<-sem(modelAFCBalIR, data=banco, ordered = names(banco))

#analise desbalanceada 
out_AFCDesSemCon<-sem(modelAFCDesbalSemCrt, data=banco, ordered = names(banco))
out_AFCDesMIMIC<-sem(modelAFCDesbalAqMIMIC, data=banco, ordered = names(banco))
out_AFCDesIR<-sem(modelAFCDesbalIR, data=banco, ordered = names(banco))

##################### RECUPERAR PARÂMETROS #############################
#recuperar as cargas
ParEst_BalSemCon<-parameterestimates(out_AFCBalSemCon) %>% filter(.,op=="=~")
ParEst_BalMIMIC<-parameterestimates(out_AFCBalMIMIC) %>% filter(.,op=="=~")
ParEst_BalIR<-parameterestimates(out_AFCBalIR) %>% filter(.,op=="=~") %>% filter(.,se>0)
ParEst_DesSemCon<-parameterestimates(out_AFCDesSemCon) %>% filter(.,op=="=~")
ParEst_DesMIMIC<-parameterestimates(out_AFCDesMIMIC) %>% filter(.,op=="=~")
ParEst_DesIR<-parameterestimates(out_AFCDesIR) %>% filter(.,op=="=~") %>% filter(.,se>0)

#selecionar apenas as estimativas
EST_ParEst_BalSemCon<-ParEst_BalSemCon$est %>% as.data.frame() %>% round(2)
EST_ParEst_BalMIMIC<-ParEst_BalMIMIC$est %>% as.data.frame() %>% round(2)
EST_ParEst_BalIR<-ParEst_BalIR$est %>% as.data.frame() %>% round(2)
EST_ParEst_DesSemCon<-ParEst_DesSemCon$est %>% as.data.frame() %>% round(2)
EST_ParEst_DesMIMIC<-ParEst_DesMIMIC$est %>% as.data.frame() %>% round(2)
EST_ParEst_DesIR<-ParEst_DesIR$est %>% as.data.frame() %>% round(2)

#recuperar a variância da aquiescência
ParEstAqu_BalMIMIC<-parameterestimates(out_AFCBalMIMIC) %>% filter(.,op=="~~") %>% filter(.,lhs=="AqMIMICbal")
ParEstAqu_BalIR<-parameterestimates(out_AFCBalIR) %>% filter(.,op=="~~") %>% filter(.,lhs=="aq")
ParEstAqu_DesMIMIC<-parameterestimates(out_AFCDesMIMIC) %>% filter(.,op=="~~") %>% filter(.,lhs=="AqMIMICdesb")
ParEstAqu_DesIR<-parameterestimates(out_AFCDesIR) %>% filter(.,op=="~~") %>% filter(.,lhs=="aq")

#selecionar apenas as estimativas
EST_ParEstAqu_BalMIMIC<-ParEstAqu_BalMIMIC$est %>% as.data.frame() %>% round(2)
EST_ParEstAqu_BalIR<-ParEstAqu_BalIR$est %>% as.data.frame() %>% round(2)
EST_ParEstAqu_DesMIMIC<-ParEstAqu_DesMIMIC$est %>% as.data.frame() %>% round(2)
EST_ParEstAqu_DesIR<-ParEstAqu_DesIR$est %>% as.data.frame() %>% round(2)

#recuperar os índices de ajuste
indices_ajuste_BalSemCon<-
  fitMeasures(out_AFCBalSemCon, c("chisq.scaled", "df.scaled", "pvalue.scaled", 
                                  "cfi.scaled", "tli.scaled", "rmsea.scaled")) %>%
  round(2) %>% as.data.frame()

indices_ajuste_BalMIMIC<-
  fitMeasures(out_AFCBalMIMIC, c("chisq.scaled", "df.scaled", "pvalue.scaled", 
                                  "cfi.scaled", "tli.scaled", "rmsea.scaled")) %>%
  round(2) %>% as.data.frame()

indices_ajuste_BalIR<-
  fitMeasures(out_AFCBalIR, c("chisq.scaled", "df.scaled", "pvalue.scaled", 
                                  "cfi.scaled", "tli.scaled", "rmsea.scaled")) %>%
  round(2) %>% as.data.frame()

indices_ajuste_DesSemCon<-
  fitMeasures(out_AFCDesSemCon, c("chisq.scaled", "df.scaled", "pvalue.scaled", 
                                  "cfi.scaled", "tli.scaled", "rmsea.scaled")) %>%
  round(2) %>% as.data.frame()

indices_ajuste_DesMIMIC<-
  fitMeasures(out_AFCDesMIMIC, c("chisq.scaled", "df.scaled", "pvalue.scaled", 
                                  "cfi.scaled", "tli.scaled", "rmsea.scaled")) %>%
  round(2) %>% as.data.frame()

indices_ajuste_DesIR<-
  fitMeasures(out_AFCDesIR, c("chisq.scaled", "df.scaled", "pvalue.scaled", 
                                  "cfi.scaled", "tli.scaled", "rmsea.scaled")) %>%
  round(2) %>% as.data.frame()


##################### SALVAR RESULTADOS #############################

write.csv2(EST_ParEst_BalSemCon, file="EST_ParEst_BalSemCon.csv")
write.csv2(EST_ParEst_BalMIMIC, file="EST_ParEst_BalMIMIC.csv")
write.csv2(EST_ParEst_BalIR, file="EST_ParEst_BalIR.csv")
write.csv2(EST_ParEst_DesSemCon, file="EST_ParEst_DesSemCon.csv")
write.csv2(EST_ParEst_DesMIMIC, file="EST_ParEst_DesMIMIC.csv")
write.csv2(EST_ParEst_DesIR, file="EST_ParEst_DesIR.csv")

write.csv2(EST_ParEstAqu_BalMIMIC, file="EST_ParEstAqu_BalMIMIC.csv")
write.csv2(EST_ParEstAqu_BalIR, file="EST_ParEstAqu_BalIR.csv")
write.csv2(EST_ParEstAqu_DesMIMIC, file="EST_ParEstAqu_DesMIMIC.csv")
write.csv2(EST_ParEstAqu_DesIR, file="EST_ParEstAqu_DesIR.csv")

write.csv2(indices_ajuste_BalSemCon, file="indices_ajuste_BalSemCon.csv")
write.csv2(indices_ajuste_BalMIMIC, file="indices_ajuste_BalMIMIC.csv")
write.csv2(indices_ajuste_BalIR, file="indices_ajuste_BalIR.csv")
write.csv2(indices_ajuste_DesSemCon, file="indices_ajuste_DesSemCon.csv")
write.csv2(indices_ajuste_DesMIMIC, file="indices_ajuste_DesMIMIC.csv")
write.csv2(indices_ajuste_DesIR, file="indices_ajuste_DesIR.csv")