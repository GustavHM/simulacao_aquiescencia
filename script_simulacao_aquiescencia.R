#pacotes
library(xlsx)
library(dplyr)
library(MASS)
library(lavaan)
library(foreach)
library(doParallel)

######################### SIMULAR OS DADOS ###################################
#importar a planilha de condicoes
condit<-read.xlsx("condicoes.xlsx",1)

#numero de replicacoes
nrepl<-500

#numero de itens
nitem<-60

#codigo completo para simulacao, analises e armazenamento dos resultados
for(c in 1:18) {
  # informar o minimo e máximo das cargas
  ldmin<-condit[c,2]
  ldmax<-condit[c,3]
  #informar a variância de aquiescência
  varAqu<-condit[c,4]
  #informar o tamanho da amostra
  sample<-condit[c,5]
  
  #thresholds aleatorios
  set.seed(2020)
  thld1Vet<-round(runif((nitem/2), min=-2, max=.5),3)
  thld2Vet<-round(thld1Vet +.5,3)
  thld3Vet<-round(thld1Vet + 1,3)
  thld4Vet<-round(thld1Vet + 1.5,3)
  #cargas aleatorias
  set.seed(2020)
  loadPos1<-round(runif((nitem/2), min=ldmin, max=ldmax),3)
  loadNeg1<-loadPos1*-1
  
  # modelo para simular os dados
  model_Sim<-
    paste(
      "f1 =~ ",loadPos1[1],"*it1 + ",loadPos1[2],"*it2 + ",loadPos1[3],"*it3 + ",
      loadPos1[4],"*it4 + ",loadPos1[5],"*it5 + ",loadPos1[6],"*it6 + ",
      loadNeg1[1],"*it31 + ",loadNeg1[2],"*it32 + ",loadNeg1[3],"*it33 + ",
      loadNeg1[4],"*it34 + ",loadNeg1[5],"*it35 + ",loadNeg1[6],"*it36 
     f1 ~~ 1*f1
     f2 =~ ",loadPos1[7],"*it7 + ",loadPos1[8],"*it8 + ",loadPos1[9],"*it9 + ",
      loadPos1[10],"*it10 + ",loadPos1[11],"*it11 + ",loadPos1[12],"*it12 + ", 
      loadNeg1[7],"*it37 + ",loadNeg1[8],"*it38 + ",loadNeg1[9],"*it39 + ",
      loadNeg1[10],"*it40 + ",loadNeg1[11],"*it41 + ",loadNeg1[12],"*it42  
     f2 ~~ 1*f2
     f3 =~ ",loadPos1[13],"*it13 + ",loadPos1[14],"*it14 + ",loadPos1[15],"*it15 + ",
      loadPos1[16],"*it16 + ",loadPos1[17],"*it17 + ",loadPos1[18],"*it18 + ", 
      loadNeg1[13],"*it43 + ",loadNeg1[14],"*it44 + ",loadNeg1[15],"*it45 + ",
      loadNeg1[16],"*it46 + ",loadNeg1[17],"*it47 + ",loadNeg1[18],"*it48  
     f3 ~~ 1*f3
     f4 =~ ",loadPos1[19],"*it19 +",loadPos1[20],"*it20 + ",loadPos1[21],"*it21 + ",
      loadPos1[22],"*it22 + ",loadPos1[23],"*it23 + ",loadPos1[24],"*it24 + ",
      loadNeg1[19],"*it49 + ",loadNeg1[20],"*it50 + ",loadNeg1[21],"*it51 + ",
      loadNeg1[22],"*it52 + ",loadNeg1[23],"*it53 + ",loadNeg1[24],"*it54 
     f4 ~~ 1*f4
     f5 =~ ",loadPos1[25],"*it25 + ",loadPos1[26],"*it26 + ",loadPos1[27],"*it27 + ",
      loadPos1[28],"*it28 + ",loadPos1[29],"*it29 + ",loadPos1[30],"*it30 + ", 
      loadNeg1[25],"*it55 + ",loadNeg1[26],"*it56 + ",loadNeg1[27],"*it57 + ",
      loadNeg1[28],"*it58 + ",loadNeg1[29],"*it59 + ",loadNeg1[30],"*it60 
     f5 ~~ 1*f5
IR =~    
1*it1  + 1*it2  + 1*it3  + 1*it4  + 1*it5  + 1*it6  + 1*it7  + 1*it8  + 1*it9  + 1*it10 + 
1*it11 + 1*it12 + 1*it13 + 1*it14 + 1*it15 + 1*it16 + 1*it17 + 1*it18 + 1*it19 + 1*it20 +
1*it21 + 1*it22 + 1*it23 + 1*it24 + 1*it25 + 1*it26 + 1*it27 + 1*it28 + 1*it29 + 1*it30 +
1*it31 + 1*it32 + 1*it33 + 1*it34 + 1*it35 + 1*it36 + 1*it37 + 1*it38 + 1*it39 + 1*it40 +
1*it41 + 1*it42 + 1*it43 + 1*it44 + 1*it45 + 1*it46 + 1*it47 + 1*it48 + 1*it49 + 1*it50 +
1*it51 + 1*it52 + 1*it53 + 1*it54 + 1*it55 + 1*it56 + 1*it57 + 1*it58 + 1*it59 + 1*it60
IR ~~ ", varAqu, "*IR
IR ~~  0*f1 + 0*f2 + 0*f3 + 0*f4 + 0*f5
f1 ~~  .43*f2 + .35*f3 + .44*f4 + .26*f5
f2 ~~  .57*f3 + .59*f4 + .42*f5
f3 ~~  .51*f4 + .37*f5
f4 ~~  .52*f5 
it1 |", thld1Vet[1],"*t1 + ", thld2Vet[1],"*t2 + ", thld3Vet[1],"*t3 + ", thld4Vet[1],"*t4
it2 |", thld1Vet[2],"*t1 + ", thld2Vet[2],"*t2 + ", thld3Vet[2],"*t3 + ", thld4Vet[2],"*t4
it3 |", thld1Vet[3],"*t1 + ", thld2Vet[3],"*t2 + ", thld3Vet[3],"*t3 + ", thld4Vet[3],"*t4
it4 |", thld1Vet[4],"*t1 + ", thld2Vet[4],"*t2 + ", thld3Vet[4],"*t3 + ", thld4Vet[4],"*t4
it5 |", thld1Vet[5],"*t1 + ", thld2Vet[5],"*t2 + ", thld3Vet[5],"*t3 + ", thld4Vet[5],"*t4
it6 |", thld1Vet[6],"*t1 + ", thld2Vet[6],"*t2 + ", thld3Vet[6],"*t3 + ", thld4Vet[6],"*t4
it7 |", thld1Vet[7],"*t1 + ", thld2Vet[7],"*t2 + ", thld3Vet[7],"*t3 + ", thld4Vet[7],"*t4
it8 |", thld1Vet[8],"*t1 + ", thld2Vet[8],"*t2 + ", thld3Vet[8],"*t3 + ", thld4Vet[8],"*t4
it9 |", thld1Vet[9],"*t1 + ", thld2Vet[9],"*t2 + ", thld3Vet[9],"*t3 + ", thld4Vet[9],"*t4
it10 |", thld1Vet[10],"*t1 + ", thld2Vet[10],"*t2 + ", thld3Vet[10],"*t3 + ", thld4Vet[10],"*t4
it11 |", thld1Vet[11],"*t1 + ", thld2Vet[11],"*t2 + ", thld3Vet[11],"*t3 + ", thld4Vet[11],"*t4
it12 |", thld1Vet[12],"*t1 + ", thld2Vet[12],"*t2 + ", thld3Vet[12],"*t3 + ", thld4Vet[12],"*t4
it13 |", thld1Vet[13],"*t1 + ", thld2Vet[13],"*t2 + ", thld3Vet[13],"*t3 + ", thld4Vet[13],"*t4
it14 |", thld1Vet[14],"*t1 + ", thld2Vet[14],"*t2 + ", thld3Vet[14],"*t3 + ", thld4Vet[14],"*t4
it15 |", thld1Vet[15],"*t1 + ", thld2Vet[15],"*t2 + ", thld3Vet[15],"*t3 + ", thld4Vet[15],"*t4
it16 |", thld1Vet[16],"*t1 + ", thld2Vet[16],"*t2 + ", thld3Vet[16],"*t3 + ", thld4Vet[16],"*t4
it17 |", thld1Vet[17],"*t1 + ", thld2Vet[17],"*t2 + ", thld3Vet[17],"*t3 + ", thld4Vet[17],"*t4
it18 |", thld1Vet[18],"*t1 + ", thld2Vet[18],"*t2 + ", thld3Vet[18],"*t3 + ", thld4Vet[18],"*t4
it19 |", thld1Vet[19],"*t1 + ", thld2Vet[19],"*t2 + ", thld3Vet[19],"*t3 + ", thld4Vet[19],"*t4
it20 |", thld1Vet[20],"*t1 + ", thld2Vet[20],"*t2 + ", thld3Vet[20],"*t3 + ", thld4Vet[20],"*t4
it21 |", thld1Vet[21],"*t1 + ", thld2Vet[21],"*t2 + ", thld3Vet[21],"*t3 + ", thld4Vet[21],"*t4
it22 |", thld1Vet[22],"*t1 + ", thld2Vet[22],"*t2 + ", thld3Vet[22],"*t3 + ", thld4Vet[22],"*t4
it23 |", thld1Vet[23],"*t1 + ", thld2Vet[23],"*t2 + ", thld3Vet[23],"*t3 + ", thld4Vet[23],"*t4
it24 |", thld1Vet[24],"*t1 + ", thld2Vet[24],"*t2 + ", thld3Vet[24],"*t3 + ", thld4Vet[24],"*t4
it25 |", thld1Vet[25],"*t1 + ", thld2Vet[25],"*t2 + ", thld3Vet[25],"*t3 + ", thld4Vet[25],"*t4
it26 |", thld1Vet[26],"*t1 + ", thld2Vet[26],"*t2 + ", thld3Vet[26],"*t3 + ", thld4Vet[26],"*t4
it27 |", thld1Vet[27],"*t1 + ", thld2Vet[27],"*t2 + ", thld3Vet[27],"*t3 + ", thld4Vet[27],"*t4
it28 |", thld1Vet[28],"*t1 + ", thld2Vet[28],"*t2 + ", thld3Vet[28],"*t3 + ", thld4Vet[28],"*t4
it29 |", thld1Vet[29],"*t1 + ", thld2Vet[29],"*t2 + ", thld3Vet[29],"*t3 + ", thld4Vet[29],"*t4
it30 |", thld1Vet[30],"*t1 + ", thld2Vet[30],"*t2 + ", thld3Vet[30],"*t3 + ", thld4Vet[30],"*t4
it31 |", thld1Vet[1],"*t1 + ", thld2Vet[1],"*t2 + ", thld3Vet[1],"*t3 + ", thld4Vet[1],"*t4
it32 |", thld1Vet[2],"*t1 + ", thld2Vet[2],"*t2 + ", thld3Vet[2],"*t3 + ", thld4Vet[2],"*t4
it33 |", thld1Vet[3],"*t1 + ", thld2Vet[3],"*t2 + ", thld3Vet[3],"*t3 + ", thld4Vet[3],"*t4
it34 |", thld1Vet[4],"*t1 + ", thld2Vet[4],"*t2 + ", thld3Vet[4],"*t3 + ", thld4Vet[4],"*t4
it35 |", thld1Vet[5],"*t1 + ", thld2Vet[5],"*t2 + ", thld3Vet[5],"*t3 + ", thld4Vet[5],"*t4
it36 |", thld1Vet[6],"*t1 + ", thld2Vet[6],"*t2 + ", thld3Vet[6],"*t3 + ", thld4Vet[6],"*t4
it37 |", thld1Vet[7],"*t1 + ", thld2Vet[7],"*t2 + ", thld3Vet[7],"*t3 + ", thld4Vet[7],"*t4
it38 |", thld1Vet[8],"*t1 + ", thld2Vet[8],"*t2 + ", thld3Vet[8],"*t3 + ", thld4Vet[8],"*t4
it39 |", thld1Vet[9],"*t1 + ", thld2Vet[9],"*t2 + ", thld3Vet[9],"*t3 + ", thld4Vet[9],"*t4
it40 |", thld1Vet[10],"*t1 + ", thld2Vet[10],"*t2 + ", thld3Vet[10],"*t3 + ", thld4Vet[10],"*t4
it41 |", thld1Vet[11],"*t1 + ", thld2Vet[11],"*t2 + ", thld3Vet[11],"*t3 + ", thld4Vet[11],"*t4
it42 |", thld1Vet[12],"*t1 + ", thld2Vet[12],"*t2 + ", thld3Vet[12],"*t3 + ", thld4Vet[12],"*t4
it43 |", thld1Vet[13],"*t1 + ", thld2Vet[13],"*t2 + ", thld3Vet[13],"*t3 + ", thld4Vet[13],"*t4
it44 |", thld1Vet[14],"*t1 + ", thld2Vet[14],"*t2 + ", thld3Vet[14],"*t3 + ", thld4Vet[14],"*t4
it45 |", thld1Vet[15],"*t1 + ", thld2Vet[15],"*t2 + ", thld3Vet[15],"*t3 + ", thld4Vet[15],"*t4
it46 |", thld1Vet[16],"*t1 + ", thld2Vet[16],"*t2 + ", thld3Vet[16],"*t3 + ", thld4Vet[16],"*t4
it47 |", thld1Vet[17],"*t1 + ", thld2Vet[17],"*t2 + ", thld3Vet[17],"*t3 + ", thld4Vet[17],"*t4
it48 |", thld1Vet[18],"*t1 + ", thld2Vet[18],"*t2 + ", thld3Vet[18],"*t3 + ", thld4Vet[18],"*t4
it49 |", thld1Vet[19],"*t1 + ", thld2Vet[19],"*t2 + ", thld3Vet[19],"*t3 + ", thld4Vet[19],"*t4
it50 |", thld1Vet[20],"*t1 + ", thld2Vet[20],"*t2 + ", thld3Vet[20],"*t3 + ", thld4Vet[20],"*t4
it51 |", thld1Vet[21],"*t1 + ", thld2Vet[21],"*t2 + ", thld3Vet[21],"*t3 + ", thld4Vet[21],"*t4
it52 |", thld1Vet[22],"*t1 + ", thld2Vet[22],"*t2 + ", thld3Vet[22],"*t3 + ", thld4Vet[22],"*t4
it53 |", thld1Vet[23],"*t1 + ", thld2Vet[23],"*t2 + ", thld3Vet[23],"*t3 + ", thld4Vet[23],"*t4
it54 |", thld1Vet[24],"*t1 + ", thld2Vet[24],"*t2 + ", thld3Vet[24],"*t3 + ", thld4Vet[24],"*t4
it55 |", thld1Vet[25],"*t1 + ", thld2Vet[25],"*t2 + ", thld3Vet[25],"*t3 + ", thld4Vet[25],"*t4
it56 |", thld1Vet[26],"*t1 + ", thld2Vet[26],"*t2 + ", thld3Vet[26],"*t3 + ", thld4Vet[26],"*t4
it57 |", thld1Vet[27],"*t1 + ", thld2Vet[27],"*t2 + ", thld3Vet[27],"*t3 + ", thld4Vet[27],"*t4
it58 |", thld1Vet[28],"*t1 + ", thld2Vet[28],"*t2 + ", thld3Vet[28],"*t3 + ", thld4Vet[28],"*t4
it59 |", thld1Vet[29],"*t1 + ", thld2Vet[29],"*t2 + ", thld3Vet[29],"*t3 + ", thld4Vet[29],"*t4
it60 |", thld1Vet[30],"*t1 + ", thld2Vet[30],"*t2 + ", thld3Vet[30],"*t3 + ", thld4Vet[30],"*t4",
      sep="")
  

# modelos a serem testados
##modelo AFC sem controle desbalanceado
##Para essa análise foram selecionados os 5 primeiros itens positivos e o último negativo de cada dimensão
modelAFCDesbalSemCrt <- 
"f1 =~ NA*it1  + it2  + it3  + it4  + it5 + it36
 f1 ~~ 1*f1
 f2 =~ NA*it7  + it8  + it9  + it10  + it11 + it42 
 f2 ~~ 1*f2
 f3 =~ NA*it13  + it14  + it15  + it16  + it17 + it48 
 f3 ~~ 1*f3
 f4 =~ NA*it19  + it20  + it21  + it22  + it23 + it54 
 f4 ~~ 1*f4
 f5 =~ NA*it25  + it26  + it27  + it28  + it29 + it60 
 f5 ~~ 1*f5"

##modelo AFC sem controle balanceado
##foram utilizados os 3 primeiros itens positivos e os 3 últimos negativos por dimensão
modelAFCBalancSemCrt <- 
"f1 =~ NA*it1  + it2  + it3  + it34  + it35 + it36
 f1 ~~ 1*f1
 f2 =~ NA*it7  + it8  + it9  + it40  + it41 + it42 
 f2 ~~ 1*f2
 f3 =~ NA*it13  + it14  + it15  + it46  + it47 + it48 
 f3 ~~ 1*f3
 f4 =~ NA*it19  + it20  + it21  + it52  + it53 + it54 
 f4 ~~ 1*f4
 f5 =~ NA*it25  + it26  + it27  + it58  + it59 + it60 
 f5 ~~ 1*f5"

##modelo AFC desbalanceada com MIMIC
#no looping há um calculo do indicador de aquiescencia por replicacao
modelAFCDesbalAqMIMIC <- 
"f1 =~ NA*it1  + it2  + it3  + it4  + it5 + it36
 f1 ~~ 1*f1
 f2 =~ NA*it7  + it8  + it9  + it10  + it11 + it42 
 f2 ~~ 1*f2
 f3 =~ NA*it13  + it14  + it15  + it16  + it17 + it48  
 f3 ~~ 1*f3
 f4 =~ NA*it19  + it20  + it21  + it22  + it23 + it54 
 f4 ~~ 1*f4
 f5 =~ NA*it25  + it26  + it27  + it28  + it29 + it60 
 f5 ~~ 1*f5
it1  + it2  + it3  + it4  + it5 + it36 ~ AqMIMICdesb
it7  + it8  + it9  + it10  + it11 + it42  ~ AqMIMICdesb
it13  + it14  + it15  + it16  + it17 + it48 ~ AqMIMICdesb
it19  + it20  + it21  + it22  + it23 + it54 ~ AqMIMICdesb
it25  + it26  + it27  + it28  + it29 + it60 ~ AqMIMICdesb
AqMIMICdesb ~~ 0*f1 + 0*f2 + 0*f3 + 0*f4 + 0*f5"

##modelo AFC Balanceada com MIMIC
modelAFCBalAqMIMIC <- 
"f1 =~ NA*it1  + it2  + it3  + it34  + it35 + it36
 f1 ~~ 1*f1
 f2 =~ NA*it7  + it8  + it9  + it40  + it41 + it42
 f2 ~~ 1*f2
 f3 =~ NA*it13  + it14  + it15  + it46  + it47 + it48 
 f3 ~~ 1*f3
 f4 =~ NA*it19  + it20  + it21  + it52  + it53 + it54 
 f4 ~~ 1*f4
 f5 =~ NA*it25  + it26  + it27  + it58  + it59 + it60 
 f5 ~~ 1*f5
it1  + it2  + it3  + it34  + it35 + it36 ~ AqMIMICbal
it7  + it8  + it9  + it40  + it41 + it42 ~ AqMIMICbal
it13  + it14  + it15  + it46  + it47 + it48 ~ AqMIMICbal
it19  + it20  + it21  + it52  + it53 + it54 ~ AqMIMICbal
it25  + it26  + it27  + it58  + it59 + it60 ~ AqMIMICbal
AqMIMICbal ~~ 0*f1 + 0*f2 + 0*f3 + 0*f4 + 0*f5"


##modelo AFC com interceptos randomicos desbalanceada
modelAFCDesbalIR <- 
"f1 =~ NA*it1  + it2  + it3  + it4  + it5 + it36
 f1 ~~ 1*f1
 f2 =~ NA*it7  + it8  + it9  + it10  + it11 + it42 
 f2 ~~ 1*f2
 f3 =~ NA*it13  + it14  + it15  + it16  + it17 + it48  
 f3 ~~ 1*f3
 f4 =~ NA*it19  + it20  + it21  + it22  + it23 + it54 
 f4 ~~ 1*f4
 f5 =~ NA*it25  + it26  + it27  + it28  + it29 + it60 
 f5 ~~ 1*f5
aq =~ 1*it1  + 1*it2  + 1*it3  + 1*it4  + 1*it5 + 1*it36 +
      1*it7  + 1*it8  + 1*it9  + 1*it10  + 1*it11 + 1*it42 +
      1*it13  + 1*it14  + 1*it15  + 1*it16  + 1*it17 + 1*it48 +
      1*it19  + 1*it20  + 1*it21  + 1*it22  + 1*it23 + 1*it54 +
      1*it25  + 1*it26  + 1*it27  + 1*it28  + 1*it29 + 1*it60 
aq ~~ 0*f1 + 0*f2 + 0*f3 + 0*f4 + 0*f5"


##modelo AFC com interceptos randomicos balanceada
modelAFCBalIR <- 
"f1 =~ NA*it1  + it2  + it3  + it34  + it35 + it36
 f1 ~~ 1*f1
 f2 =~ NA*it7  + it8  + it9  + it40  + it41 + it42
 f2 ~~ 1*f2
 f3 =~ NA*it13  + it14  + it15  + it46  + it47 + it48 
 f3 ~~ 1*f3
 f4 =~ NA*it19  + it20  + it21  + it52  + it53 + it54 
 f4 ~~ 1*f4
 f5 =~ NA*it25  + it26  + it27  + it58  + it59 + it60 
 f5 ~~ 1*f5
aq =~ 1*it1 + 1*it2  + 1*it3  + 1*it34  + 1*it35 + 1*it36 +
      1*it7  + 1*it8  + 1*it9  + 1*it40  + 1*it41 + 1*it42 +
      1*it13  + 1*it14  + 1*it15  + 1*it46  + 1*it47 + 1*it48 +
      1*it19  + 1*it20  + 1*it21  + 1*it52  + 1*it53 + 1*it54 +
      1*it25  + 1*it26  + 1*it27  + 1*it58  + 1*it59 + 1*it60 
aq ~~ 0*f1 + 0*f2 + 0*f3 + 0*f4 + 0*f5"


#objetos para guardar parâmetros estimados
EST_ParEst_BalSemCon<-as.data.frame(matrix(NA,nrow=30, ncol=nrepl))
EST_ParEst_BalMIMIC<-as.data.frame(matrix(NA,nrow=30, ncol=nrepl))
EST_ParEst_BalIR<-as.data.frame(matrix(NA,nrow=30, ncol=nrepl))
EST_ParEst_DesSemCon<-as.data.frame(matrix(NA,nrow=30, ncol=nrepl))
EST_ParEst_DesMIMIC<-as.data.frame(matrix(NA,nrow=30, ncol=nrepl))
EST_ParEst_DesIR<-as.data.frame(matrix(NA,nrow=30, ncol=nrepl))

#objetos para recolher problemas de identificação
identBalIR=0
identBalMIMIC=0
identBalSemCon=0
identDesIR=0
identDesMIMIC=0
identDesSemCon=0

#replicacao das simulacoes e analises
for (i in 1:nrepl){
VetorOrdinal<-c("it1", "it2", "it3", "it4", "it5", "it6", "it7", "it8", "it9", "it10", 
                "it11", "it12", "it13", "it14", "it15", "it16", "it17", "it18", "it19", "it20",
                "it21", "it22", "it23", "it24", "it25", "it26", "it27", "it28", "it29", "it30",
                "it31", "it32", "it33", "it34", "it35", "it36", "it37", "it38", "it39", "it40",
                "it41", "it42", "it43", "it44", "it45", "it46", "it47", "it48", "it49", "it50",
                "it51", "it52", "it53", "it54", "it55", "it56", "it57", "it58", "it59", "it60")
bancoTemp<- simulateData(model=model_Sim,sample.nobs = sample, seed=i)
banco<-bancoTemp[, VetorOrdinal]
rm(bancoTemp)

#calcular os indicadores de aquiescencia
banco$AqMIMICdesb <- apply(banco[,c(1, 36, 7, 42, 13, 48, 19, 54, 25, 60)],1, mean)
banco$AqMIMICbal  <- apply(banco[,c(1:3, 34:36, 7:9, 40:42, 13:15, 46:48, 19:21, 52:54, 25:27, 58:60)],1, mean)


#analises 
#balanceadas
out_AFCBalSemCon<-sem(modelAFCBalancSemCrt, data=banco, estimator = "DWLS", ordered = VetorOrdinal)
out_AFCBalMIMIC<-sem(modelAFCBalAqMIMIC, data=banco, estimator="DWLS",ordered = VetorOrdinal)
out_AFCBalIR<-sem(modelAFCBalIR, data=banco, estimator="DWLS",ordered = VetorOrdinal)
#analise desbalanceada
out_AFCDesSemCon<-sem(modelAFCDesbalSemCrt, data=banco, estimator = "DWLS", ordered = VetorOrdinal)
out_AFCDesMIMIC<-sem(modelAFCDesbalAqMIMIC, data=banco, estimator="DWLS", ordered = VetorOrdinal)
out_AFCDesIR<-sem(modelAFCDesbalIR, data=banco, estimator="DWLS", ordered = VetorOrdinal)


#recuperar parâmetros
ParEst_BalSemCon<-parameterestimates(out_AFCBalSemCon) %>% filter(.,op=="=~")
ParEst_BalMIMIC<-parameterestimates(out_AFCBalMIMIC) %>% filter(.,op=="=~")
ParEst_BalIR<-parameterestimates(out_AFCBalIR) %>% filter(.,op=="=~") %>% filter(.,se>0)
ParEst_DesSemCon<-parameterestimates(out_AFCDesSemCon) %>% filter(.,op=="=~")
ParEst_DesMIMIC<-parameterestimates(out_AFCDesMIMIC) %>% filter(.,op=="=~")
ParEst_DesIR<-parameterestimates(out_AFCDesIR) %>% filter(.,op=="=~") %>% filter(.,se>0)
ParEstAqu_BalMIMIC<-parameterestimates(out_AFCBalMIMIC) %>% filter(.,op=="~~") %>% filter(.,lhs=="AqMIMICbal")
ParEstAqu_BalIR<-parameterestimates(out_AFCBalIR) %>% filter(.,op=="~~") %>% filter(.,lhs=="aq")
ParEstAqu_DesMIMIC<-parameterestimates(out_AFCDesMIMIC) %>% filter(.,op=="~~") %>% filter(.,lhs=="AqMIMICdesb")
ParEstAqu_DesIR<-parameterestimates(out_AFCDesIR) %>% filter(.,op=="~~") %>% filter(.,lhs=="aq")

EST_ParEst_BalSemCon[,i]<-ParEst_BalSemCon$est
EST_ParEst_BalMIMIC[,i]<-ParEst_BalMIMIC$est
EST_ParEst_BalIR[,i]<-ParEst_BalIR$est
EST_ParEst_DesSemCon[,i]<-ParEst_DesSemCon$est
EST_ParEst_DesMIMIC[,i]<-ParEst_DesMIMIC$est
EST_ParEst_DesIR[,i]<-ParEst_DesIR$est
SE_ParEst_BalSemCon[,i]<-ParEst_BalSemCon$se
SE_ParEst_BalMIMIC[,i]<-ParEst_BalMIMIC$se
SE_ParEst_BalIR[,i]<-ParEst_BalIR$se
SE_ParEst_DesSemCon[,i]<-ParEst_DesSemCon$se
SE_ParEst_DesMIMIC[,i]<-ParEst_DesMIMIC$se
SE_ParEst_DesIR[,i]<-ParEst_DesIR$se

EST_ParEstAqu_BalMIMIC[,i]<-ParEstAqu_BalMIMIC$est
EST_ParEstAqu_BalIR[,i]<-ParEstAqu_BalIR$est
EST_ParEstAqu_DesMIMIC[,i]<-ParEstAqu_DesMIMIC$est
EST_ParEstAqu_DesIR[,i]<-ParEstAqu_DesIR$est

# indicar problema de identificação
if (parameterestimates(out_AFCBalIR) %>% filter(.,op=="~~" & est<0 ) %>% nrow() > 0) {
  identBalIR<-identBalIR+1}
if (parameterestimates(out_AFCBalMIMIC) %>% filter(.,op=="~~" & est<0 ) %>% nrow() > 0) {
  identBalMIMIC<-identBalMIMIC+1}
if (parameterestimates(out_AFCBalSemCon) %>% filter(.,op=="~~" & est<0 ) %>% nrow() > 0) {
  identBalSemCon<-identBalSemCon+1}
if (parameterestimates(out_AFCDesIR) %>% filter(.,op=="~~" & est<0 ) %>% nrow() > 0) {
  identDesIR<-identDesIR+1}
if (parameterestimates(out_AFCDesMIMIC) %>% filter(.,op=="~~" & est<0 ) %>% nrow() > 0) {
  identDesMIMIC<-identDesMIMIC+1}
if (parameterestimates(out_AFCDesSemCon) %>% filter(.,op=="~~" & est<0 ) %>% nrow() > 0) {
  identDesSemCon<-identDesSemCon+1}

} # final do looping das replicações


### resumo das replicações na condição c

#inverter as cargas erradas
inverter = function (itens, positivo, negativo)
{
  if (NA %in% itens[positivo]  | NA %in% itens[negativo]) {
    print(rep(NA,length(itens))) 
  } else if (sum (itens[positivo] < 0) == length(positivo) & 
           sum (itens[negativo] > 0) == length(negativo)) {
        print (itens * -1)
  } else {
        print (itens)}
}
  

#invertendo balanceados
EST_ParEst_BalSemCon <- apply (EST_ParEst_BalSemCon[1:6,], 2, inverter, positivo = 1:3, negativo = 4:6) %>% 
  rbind(apply (EST_ParEst_BalSemCon[7:12,], 2, inverter, positivo = 1:3, negativo = 4:6)) %>% as.data.frame() %>%
  rbind(apply (EST_ParEst_BalSemCon[13:18,], 2, inverter, positivo = 1:3, negativo = 4:6)) %>% as.data.frame() %>%
  rbind(apply (EST_ParEst_BalSemCon[19:24,], 2, inverter, positivo = 1:3, negativo = 4:6)) %>% as.data.frame() %>%
  rbind(apply (EST_ParEst_BalSemCon[25:30,], 2, inverter, positivo = 1:3, negativo = 4:6)) %>% as.data.frame()

EST_ParEst_BalMIMIC <- apply (EST_ParEst_BalMIMIC[1:6,], 2, inverter, positivo = 1:3, negativo = 4:6) %>% 
  rbind(apply (EST_ParEst_BalMIMIC[7:12,], 2, inverter, positivo = 1:3, negativo = 4:6)) %>% as.data.frame() %>%
  rbind(apply (EST_ParEst_BalMIMIC[13:18,], 2, inverter, positivo = 1:3, negativo = 4:6)) %>% as.data.frame() %>%
  rbind(apply (EST_ParEst_BalMIMIC[19:24,], 2, inverter, positivo = 1:3, negativo = 4:6)) %>% as.data.frame() %>%
  rbind(apply (EST_ParEst_BalMIMIC[25:30,], 2, inverter, positivo = 1:3, negativo = 4:6)) %>% as.data.frame()

EST_ParEst_BalIR <- apply (EST_ParEst_BalIR[1:6,], 2, inverter, positivo = 1:3, negativo = 4:6) %>% 
  rbind(apply (EST_ParEst_BalIR[7:12,], 2, inverter, positivo = 1:3, negativo = 4:6)) %>% as.data.frame() %>%
  rbind(apply (EST_ParEst_BalIR[13:18,], 2, inverter, positivo = 1:3, negativo = 4:6)) %>% as.data.frame() %>%
  rbind(apply (EST_ParEst_BalIR[19:24,], 2, inverter, positivo = 1:3, negativo = 4:6)) %>% as.data.frame() %>%
  rbind(apply (EST_ParEst_BalIR[25:30,], 2, inverter, positivo = 1:3, negativo = 4:6)) %>% as.data.frame()

#invertendo desbalanceados
EST_ParEst_DesSemCon <- apply (EST_ParEst_DesSemCon[1:6,], 2, inverter, positivo = 1:5, negativo = 6) %>% 
  rbind(apply (EST_ParEst_DesSemCon[7:12,], 2, inverter, positivo = 1:5, negativo = 6)) %>% as.data.frame() %>%
  rbind(apply (EST_ParEst_DesSemCon[13:18,], 2, inverter, positivo = 1:5, negativo = 6)) %>% as.data.frame() %>%
  rbind(apply (EST_ParEst_DesSemCon[19:24,], 2, inverter, positivo = 1:5, negativo = 6)) %>% as.data.frame() %>%
  rbind(apply (EST_ParEst_DesSemCon[25:30,], 2, inverter, positivo = 1:5, negativo = 6)) %>% as.data.frame()

EST_ParEst_DesMIMIC <- apply (EST_ParEst_DesMIMIC[1:6,], 2, inverter, positivo = 1:5, negativo = 6) %>% 
  rbind(apply (EST_ParEst_DesMIMIC[7:12,], 2, inverter, positivo = 1:5, negativo = 6)) %>% as.data.frame() %>%
  rbind(apply (EST_ParEst_DesMIMIC[13:18,], 2, inverter, positivo = 1:5, negativo = 6)) %>% as.data.frame() %>%
  rbind(apply (EST_ParEst_DesMIMIC[19:24,], 2, inverter, positivo = 1:5, negativo = 6)) %>% as.data.frame() %>%
  rbind(apply (EST_ParEst_DesMIMIC[25:30,], 2, inverter, positivo = 1:5, negativo = 6)) %>% as.data.frame()

EST_ParEst_DesIR <- apply (EST_ParEst_DesIR[1:6,], 2, inverter, positivo = 1:5, negativo = 6) %>% 
  rbind(apply (EST_ParEst_DesIR[7:12,], 2, inverter, positivo = 1:5, negativo = 6)) %>% as.data.frame() %>%
  rbind(apply (EST_ParEst_DesIR[13:18,], 2, inverter, positivo = 1:5, negativo = 6)) %>% as.data.frame() %>%
  rbind(apply (EST_ParEst_DesIR[19:24,], 2, inverter, positivo = 1:5, negativo = 6)) %>% as.data.frame() %>%
  rbind(apply (EST_ParEst_DesIR[25:30,], 2, inverter, positivo = 1:5, negativo = 6)) %>% as.data.frame()


# frame para resumo
resumoBal_Est<-as.data.frame(matrix(NA,nrow=30, ncol=4))
resumoDes_Est<-as.data.frame(matrix(NA,nrow=30, ncol=4))
resumo_SE<-as.data.frame(matrix(NA,nrow=30, ncol=6))
resumo_Aquie<-as.data.frame(matrix(NA,nrow=1, ncol=4))

colnames(resumoBal_Est)<-c("verdadeiro","Est_BalSemCon","Est_BalMIMIC","Est_BalIR")
colnames(resumoDes_Est)<-c("verdadeiro","Est_DesSemCon","Est_DesMIMIC","Est_DesIR")
colnames(resumo_SE)<-c("SE_BalSemCon","SE_BalMIMIC","SE_BalIR",
                       "SE_DesSemCon","SE_DesMIMIC","SE_DesIR")
colnames(resumo_Aquie)<-c("Est_BalMIMIC","Est_BalIR",
                          "Est_DesMIMIC","Est_DesIR")

resumoBal_Est[,1]<-c(loadPos1[1:3],loadNeg1[4:6],loadPos1[7:9],loadNeg1[10:12],loadPos1[13:15],loadNeg1[16:18],
                     loadPos1[19:21],loadNeg1[22:24],loadPos1[25:27],loadNeg1[28:30])
resumoBal_Est[,2]<-apply(EST_ParEst_BalSemCon, 1, mean, na.rm=TRUE)
resumoBal_Est[,3]<-apply(EST_ParEst_BalMIMIC, 1, mean, na.rm=TRUE)
resumoBal_Est[,4]<-apply(EST_ParEst_BalIR, 1, mean, na.rm=TRUE)
resumoDes_Est[,1]<-c(loadPos1[1:5],loadNeg1[6],loadPos1[7:11],loadNeg1[12],loadPos1[13:17],loadNeg1[18],
                     loadPos1[19:23],loadNeg1[24],loadPos1[25:29],loadNeg1[30])
resumoDes_Est[,2]<-apply(EST_ParEst_DesSemCon, 1, mean, na.rm=TRUE)
resumoDes_Est[,3]<-apply(EST_ParEst_DesMIMIC, 1, mean, na.rm=TRUE)
resumoDes_Est[,4]<-apply(EST_ParEst_DesIR, 1, mean, na.rm=TRUE)

resumo_SE[,1]<-apply(SE_ParEst_BalSemCon, 1, mean, na.rm=TRUE) 
resumo_SE[,2]<-apply(SE_ParEst_BalMIMIC, 1, mean, na.rm=TRUE)
resumo_SE[,3]<-apply(SE_ParEst_BalIR, 1, mean, na.rm=TRUE)
resumo_SE[,4]<-apply(SE_ParEst_DesSemCon, 1, mean, na.rm=TRUE) 
resumo_SE[,5]<-apply(SE_ParEst_DesMIMIC, 1, mean, na.rm=TRUE)
resumo_SE[,6]<-apply(SE_ParEst_DesIR, 1, mean, na.rm=TRUE)

resumo_Aquie[,1]<-apply(EST_ParEstAqu_BalMIMIC, 1, mean, na.rm=TRUE) 
resumo_Aquie[,2]<-apply(EST_ParEstAqu_BalIR, 1, mean, na.rm=TRUE)
resumo_Aquie[,3]<-apply(EST_ParEstAqu_DesMIMIC, 1, mean, na.rm=TRUE)
resumo_Aquie[,4]<-apply(EST_ParEstAqu_DesIR, 1, mean, na.rm=TRUE) 

write.csv2(resumoBal_Est, file=paste('resumoBal_Est_L_',ldmin,'V_',varAqu,'S_',sample,'.csv'))
write.csv2(resumoDes_Est, file=paste('resumoDes_Est_L_',ldmin,'V_',varAqu,'S_',sample,'.csv'))
write.csv2(resumo_SE, file=paste('resumo_SE_L_',ldmin,'V_',varAqu,'S_',sample,'.csv'))
write.csv2(resumo_Aquie, file=paste('resumo_Aquie_L_',ldmin,'V_',varAqu,'S_',sample,'.csv'))

data.frame(identBalIR, identBalMIMIC, identBalSemCon, 
           identDesIR, identDesMIMIC, identDesSemCon) %>% 
  write.csv2(., file=paste('ProblemasIdentif',ldmin,'V_',varAqu,'S_',sample,'.csv'))

} 

