browseURL("http://faculty.smu.edu/tfomby/eco6352/Exercises%20and%20Exercise%20Keys/")

library(readxl)#importer des fichiers excel
library(nlme)#MCG
library(car)#durbinwatson
library(lmtest)#bptest et Lrtest
library(pscl)#pseudo R²
library(nnet)#multinom
library(MASS)#polr
library(ggplot2)#visualisation de la base de données
library(ggpubr)#comparaisond de graphiques

setwd("C:/Users/Himmels/Desktop/Projet trabelsi/mutlinomial ordonné")
getwd()

cra = read_excel("cra.xlsx")
attach(cra)
DT <- cra
DT
View(DT)


# Voir à quoi ressemble notre dataset :

# fonctions de densité

plot = function(x){
  fd1a=qplot(data=DT[rrating==x,], loa,fill=(rrating=x),geom = "density")
  fd1 = fd1a +geom_vline(aes(xintercept=median(loa)),color="red", linetype="dashed", size= 1) +geom_vline(aes(xintercept=mean(loa)), color="green", linetype="dashed", size= 1)  
return(fd1)
  }
fd5 = plot(1)
fd6 = plot(2)
fd7 = plot(3)
fd8 = plot(4)

fd1a=qplot(data=DT[rrating==1,], loa,fill=(rrating=1),geom = "density")
fd1 = fd1a +geom_vline(aes(xintercept=median(loa)),color="red", linetype="dashed", size= 1) +geom_vline(aes(xintercept=mean(loa)), color="green", linetype="dashed", size= 1)
fd2a=qplot(data=DT[rrating==2,], loa,fill=(rrating=2),geom = "density")
fd2 = fd2a +geom_vline(aes(xintercept=median(loa)),color="red", linetype="dashed", size= 1) +geom_vline(aes(xintercept=mean(loa)), color="green", linetype="dashed", size= 1)
fd3a=qplot(data=DT[rrating==3,], loa,fill=(rrating=3),geom = "density")
fd3 = fd3a +geom_vline(aes(xintercept=median(loa)),color="red", linetype="dashed", size= 1) +geom_vline(aes(xintercept=mean(loa)), color="green", linetype="dashed", size= 1)
fd4a=qplot(data=DT[rrating==4,], loa,fill=(rrating=4),geom = "density")
fd4 = fd4a +geom_vline(aes(xintercept=median(loa)),color="red", linetype="dashed", size= 1) +geom_vline(aes(xintercept=mean(loa)), color="green", linetype="dashed", size= 1)

ggarrange(fd1,fd2,fd3,fd4,nrow=2,ncol=2)
ggarrange(fd5,fd6,fd7,fd8,nrow=2,ncol=2)


fd11a=qplot(data=DT[rrating==1,], prl,fill=(rrating=1),geom = "density")
fd11 = fd11a +geom_vline(aes(xintercept=median(prl)),color="red", linetype="dashed", size= 1) +geom_vline(aes(xintercept=mean(prl)), color="green", linetype="dashed", size= 1)
fd22a=qplot(data=DT[rrating==2,], prl,fill=(rrating=2),geom = "density")
fd22 = fd22a +geom_vline(aes(xintercept=median(prl)),color="red", linetype="dashed", size= 1) +geom_vline(aes(xintercept=mean(prl)), color="green", linetype="dashed", size= 1)
fd33a=qplot(data=DT[rrating==3,], prl,fill=(rrating=3),geom = "density")
fd33 = fd33a +geom_vline(aes(xintercept=median(prl)),color="red", linetype="dashed", size= 1) +geom_vline(aes(xintercept=mean(prl)), color="green", linetype="dashed", size= 1)
fd44a=qplot(data=DT[rrating==4,], prl,fill=(rrating=4),geom = "density")
fd44 = fd44a +geom_vline(aes(xintercept=median(prl)),color="red", linetype="dashed", size= 1) +geom_vline(aes(xintercept=mean(prl)), color="green", linetype="dashed", size= 1)

ggarrange(fd11,fd22,fd33,fd44,nrow=2,ncol=2)


fd111a=qplot(data=DT[rrating==1,], ass,fill=(rrating=1),geom = "density")
fd111 = fd111a +geom_vline(aes(xintercept=median(ass)),color="red", linetype="dashed", size= 1) +geom_vline(aes(xintercept=mean(ass)), color="green", linetype="dashed", size= 1)
fd222a=qplot(data=DT[rrating==2,], ass,fill=(rrating=2),geom = "density")
fd222 = fd222a +geom_vline(aes(xintercept=median(ass)),color="red", linetype="dashed", size= 1) +geom_vline(aes(xintercept=mean(ass)), color="green", linetype="dashed", size= 1)
fd333a=qplot(data=DT[rrating==3,], ass,fill=(rrating=3),geom = "density")
fd333 = fd333a +geom_vline(aes(xintercept=median(ass)),color="red", linetype="dashed", size= 1) +geom_vline(aes(xintercept=mean(ass)), color="green", linetype="dashed", size= 1)
fd444a=qplot(data=DT[rrating==4,], ass,fill=(rrating=4),geom = "density")
fd444 = fd444a +geom_vline(aes(xintercept=median(ass)),color="red", linetype="dashed", size= 1) +geom_vline(aes(xintercept=mean(ass)), color="green", linetype="dashed", size= 1)

ggarrange(fd111,fd222,fd333,fd444,nrow=2,ncol=2)

#Nuages de points

b1=qplot(data=DT,loa,rrating,main = "Ratio pret/actif total de la banque")
b1
b2=qplot(data=DT, prl,rrating,main ="Ratio actif douteux/actif total par rapport de la categorie" )
b3=qplot(data=DT,equ,rrating,main ="Ratio capitaux propre/actif total" )
b4= qplot(data=DT, roa,rrating,main = "Ratio dividende sur actif")
ggarrange(b1,b2,b3,b4,nrow=2,ncol=2)

b5=qplot(data=DT, sec,rrating,main= "Ratio investissements de valeurs sur actifs")
b6= qplot(data=DT, ass,rrating,main= "Logarithme de l'actif de la banque")
b7= qplot(data=DT, metro,rrating,main =" MSA = 1, 0 sinon" )
#"metropolitan statistical area (MSA) is a geographical region with a relatively high
#population density at its core and close economic ties throughout the area"-Wikipédia
b8= qplot(data=DT, growth,rrating, main="Taux de croissance du PIB")
ggarrange(b5,b6,b7,b8,nrow=2,ncol=2)

M <- rbind(mean(ass[rrating==1]),mean(ass[rrating==2]),mean(ass[rrating==3]),mean(ass[rrating==4]))
M <- cbind(M,rbind(mean(prl[rrating==1]),mean(prl[rrating==2]),mean(prl[rrating==3]),mean(prl[rrating==4])))
M <- cbind(M,rbind(mean(loa[rrating==1]),mean(loa[rrating==2]),mean(loa[rrating==3]),mean(loa[rrating==4])))
M <- data.frame(M,row.names =(c("rrating=1","rrating=2","rrating=3","rrating=4")))
M

#Test économétriques :

# Modèle estimé par les MCO 

modeleMCO <- (lm(rrating~loa+prl+equ+roa+sec+ass+metro+growth))
summary(modeleMCO)

# Modèle estimé par multinom (mutninomial ordonné)

modeleML <-(multinom(rrating~loa+prl+equ+roa+sec+ass+metro+growth,DT))
summary(modeleML)

#Test du likelihood ratio

TestML <- function(A,B,C,D,E,F,G,H){
  modele1 <-(multinom(rrating~A+B+C+D+E+F+G+H,DT))
  modele2 <-(multinom(rrating~A+B+C+D+E+F+G,DT))
  return(lrtest(modele1,modele2))
}

A <- TestML(ass,equ,growth,metro,prl,roa,sec,loa)
B <- TestML(ass,equ,growth,loa,metro,roa,sec,prl)
C <- TestML(ass,growth,loa,metro,prl,roa,sec,equ)
D <- TestML(ass,equ,growth,loa,metro,prl,sec,roa)
E <- TestML(ass,equ,growth,loa,metro,prl,roa,sec)
F <- TestML(equ,growth,loa,metro,prl,roa,sec,ass)
G <- TestML(ass,equ,growth,loa,prl,roa,sec,metro)
H <- TestML(ass,equ,loa,metro,prl,roa,sec,growth)
A

d <- data.frame(loa=paste(A$`Pr(>Chisq)`,"**"),prl=paste(B$`Pr(>Chisq)`,"**"),equ=paste(C$`Pr(>Chisq)`,"**"),
                roa=paste(D$`Pr(>Chisq)`,""),sec=paste(E$`Pr(>Chisq)`,""),ass=paste(F$`Pr(>Chisq)`,"***"),
                metro=paste(G$`Pr(>Chisq)`,"***"),growth=paste(H$`Pr(>Chisq)`,"*"),row.names = c("NA","Pr(>chisq)"))
d[2,]

#Qualité de l'ajustement log-linéaire donné par le pseudo R²

pR2(modeleML)

# Modele poly-ordonne

modeleORD <- polr(as.factor(rrating)~loa+prl+equ+roa+sec+ass+metro+growth,DT)
summary(modeleORD)


#Test du likelihood ratio

TestORD <- function(A,B,C,D,E,F,G,H){
  modele1 <-(polr(as.factor(rrating)~A+B+C+D+E+F+G+H,DT))
  modele2 <-(polr(as.factor(rrating)~A+B+C+D+E+F+G,DT))
  return(lrtest(modele1,modele2))
}


A <- TestORD(ass,equ,growth,metro,prl,roa,sec,loa)
B <- TestORD(ass,equ,growth,loa,metro,roa,sec,prl)
C <- TestORD(ass,growth,loa,metro,prl,roa,sec,equ)
D <- TestORD(ass,equ,growth,loa,metro,prl,sec,roa)
E <- TestORD(ass,equ,growth,loa,metro,prl,roa,sec)
F <- TestORD(equ,growth,loa,metro,prl,roa,sec,ass)
G <- TestORD(ass,equ,growth,loa,prl,roa,sec,metro)
H <- TestORD(ass,equ,loa,metro,prl,roa,sec,growth)
A
e <- data.frame(loa=paste(A$`Pr(>Chisq)`,"**"),prl=paste(B$`Pr(>Chisq)`,"**"),equ=paste(C$`Pr(>Chisq)`,"**"),
                roa=paste(D$`Pr(>Chisq)`,""),sec=paste(E$`Pr(>Chisq)`,""),ass=paste(F$`Pr(>Chisq)`,"***"),
                metro=paste(G$`Pr(>Chisq)`,"***"),growth=paste(H$`Pr(>Chisq)`,"*"),row.names = c("NA","Pr(>chisq)"))
e[2,]


# Test du pseudo R²

pR2(modeleORD)



# Test de prédiction
# modeleML utilisé plus haut
ModeleP1 = predict(modeleML,newdata = DT )#en valeur
ModeleP1
predict(modeleML,DT, type = "prob")#en probabilités 
ModeleP1 == rrating
summary(ModeleP1==rrating)
171/(171+179)



#Nouveau test de prédiction
# Première étape : Création d'un modèle avec 80% des observations
DT

train1=DT[rrating==1,]
test1 = train1[81:100,]
train1=train1[1:80,]
train1
test1

train2=DT[rrating==2,]
test2= train2[81:100,]
train2=train2[1:80,]
train2

train3=DT[rrating==3,]
test3= train3[81:100,]
train3=train3[1:80,]
train3

train4=DT[rrating==4,]
test4= train4[41:50,]
train4=train4[1:40,]
train4

#la modelisation du modèle prédictif :

DTtrain = rbind(train1,train2,train3,train4)#80% de l'échantillon
DTtest = rbind(test1,test2,test3,test4)#20% de l'échantillon
DTtrain
modeleMLtrain <-(multinom(rrating~loa+prl+equ+roa+sec+ass+metro+growth,DTtrain))

#Deuxième étape : la prédiction du modèle DTtrain pour l'échantillon à 20% (erreur hors echantillon)

ModeleP2 = predict(modeleMLtrain,newdata = DTtest )#value
ModeleP2
predict(modeleMLtrain,DTtest, type = "prob")#proba
ModeleP2 == DTtest$rrating
summary(ModeleP2==DTtest$rrating)
31/(39+31) #probabilité de prédiction du modèle avec 80% des échantillons, avec erreur hors echantillon

171/(171+179) #probabilité de prédiction du modèle, avec erreur à l'interieur de l'echantillon





