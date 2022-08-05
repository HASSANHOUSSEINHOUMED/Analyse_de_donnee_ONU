# J'ai écrit ces codes lors de mon DUT2 STID pour pouvoir crée des belles graphiques 
# afin de rendre mes analyses plus jolies et comprehensible.
# Les codes ci-dessus sont écrit en utilisant R qui est un logiciel libre et un langage de programmation.

# Importation du base de donnée
setwd("C:\\Users\\toshiba\\Desktop\\PROJET TUTOREE")
###########################################################################
hassan=read.table(file="pays.csv",sep=",",h=T,row.names=1)
hassan
View(hassan)
cor(hassan)

# Analyse en composante principale(ACP) 
library(FactoMineR)
acp=PCA(hassan)
acp
round(acp$var$coord[,1:2],2)
round(acp$var$contrib[,1:2],2)
round(acp$var$cos[,1:2],2)

# Importation du jeu de donnée et quelques représentations des jolies graphiques en utilisant différents packages.
data=read.table(file="databasee.csv",sep=";",h=T,row.names=1)
data
View(data)
attach(data)
quali=data[,c("continent","Intervalle.des.enfants","Situation.du.pays")]
colntab=colnames(quali)
 generalc=c(1:5)
 par(mfrow=c(2,2))
 par(mar=c(4,4,6,4))
 for(i in 1:5){
 tagen = table(quali[,generalc[i]])
 tagenb = round(tagen*100/nrow(quali),2)
 bp<- barplot(tagen,xlim=c(0,max(tagen+50)),ylim=c(0,(nrow(tagen)+1))
  ,horiz=T,las=1,main=colntab[generalc[i]],col=rep(c("green3",    
  "red3","blue","purple","orange")),xlab="Nombres d'individus")
 text(tagen,bp,labels=format(paste(tagen,paste(tagenb,"%",sep=""),
 sep=" | "), 4),pos = 4, cex = 1)
 } 

plot(Eco,Fertilite,type="b")
cor(Fertilite,PIB)
plot(Purbain,Croissance)

names(data)
a=table(taux.de.fertilite,continent)
chisq.test(a)
library(FactoMineR)
b=CA(a)
x=table(Situation.du.pays,taux.de.fertilite)
chisq.test(x)
           
sqrt(26.702/(125*2))
library(FactoMineR)

library(factoextra)
z=CA(x)
fviz_screeplot(z, addlabels = TRUE, ylim = c(0, 100))
fviz_screeplot(b, addlabels = TRUE, ylim = c(0, 100))

geom_hline (yintercept = 33.33, linetype = 2, color = "red")
fviz_ca_biplot (z, repel = TRUE)
fviz_ca_biplot (b, repel = TRUE)

row <- get_ca_row(z)
col=get_ca_col(z)
fviz_ca_row(z,repel=TRUE)
fviz_ca_col(z,repel=TRUE)
fviz_ca_row (z, col.row = "steelblue", shape.row = 15)
fviz_ca_row (z, col.row = "cos2",
             gradient.cols = c ("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

library(corrplot)
corrplot(row$cos2, is.corr = FALSE)
fviz_cos2(z, choice = "row", axes = 1:2)
corrplot(row$contrib, is.corr=FALSE)    
fviz_contrib(z, choice = "row", axes = 1, top = 10)
fviz_contrib(z, choice = "row", axes = 2, top = 10)
fviz_ca_row (z, col.row = "contrib",
             gradient.cols = c ("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)


library(gplots)
y=as.table(as.matrix (x))
balloonplot(t (y), main = "housetasks", xlab = "", ylab = "",label = FALSE, show.margins = FALSE)


plot(lm(PIB~Eco))
plot(lm(Croissance~Pop+Fertilite+ Purbain))












