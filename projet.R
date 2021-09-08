#Sript

#Aller au repertoire courant

# Read data imcenfant.csv
IMC_Enfant = read.csv2(file =  "imcenfant.csv" )

#verification de l'importation
IMC_Enfant;

#dimension du dataframe
N = dim(IMC_Enfant);

#.............................................#
#       Explorer et nettoyer des données      #
#.............................................#

#Retrouver le nom des variables
names(IMC_Enfant);

#Connaître la structure d'une table de données
str(IMC_Enfant);

#Voir les données importées (les dixs prémiers)
head(IMC_Enfant,10);

#Modifier le nom d'une variable
names(IMC_Enfant)[4] = "Age";

#Extraire une partie des données
IMC_Enfant_F = subset(IMC_Enfant, SEXE == "F")  # Les filles 
IMC_Enfant_G = subset(IMC_Enfant, SEXE == "G")  # Les garcons

#Rechercher les valeurs manquantes
which(is.na(IMC_Enfant),arr.ind=TRUE);

#Corriger les valeurs des données manquantes
fix(IMC_Enfant);


#.............................................#
#    Statistiques descriptives des données    #
#.............................................#

#La médiane pour le poids
median(IMC_Enfant$poids,na.rm=T)

#La médiane pour la taille
median(IMC_Enfant$taille,na.rm=T)

#Les quantiles pour le poids
quantile(IMC_Enfant$poids,na.rm=T)

#Les quantiles pour la taille
quantile(IMC_Enfant$taille,na.rm=T)

#La moyenne
mean(IMC_Enfant$poids,na.rm=T)

mean(IMC_Enfant$taille,na.rm=T)

#Les extrêmes
min(IMC_Enfant$poids,na.rm=T)
max(IMC_Enfant$poids,na.rm=T)

min(IMC_Enfant$taille,na.rm=T)
max(IMC_Enfant$taille,na.rm=T)

#La variance
var(IMC_Enfant$poids,na.rm=T)
var(IMC_Enfant$taille,na.rm=T)

#L'écart-type
sd(IMC_Enfant$poids,na.rm=T)
sd(IMC_Enfant$taille,na.rm=T)

#L'intervalle de confiance de la moyenne
t.test(IMC_Enfant$poids,conf.level=0.95)$conf.int
t.test(IMC_Enfant$taille,conf.level=0.95)$conf.int


summary(IMC_Enfant$poids)
summary(IMC_Enfant$taille)
summary(IMC_Enfant$SEXE)

#Variable reponse
IMC_Enfant$IMC = IMC(IMC_Enfant$poids, IMC_Enfant$taille)

#.............................................#
#     Présentation graphique des données      #
#.............................................#

#Diagramme en barre représentant la Zone d'Education Prioritaire de l'école
IMC_Enfant$ZEP = factor(c(rep("O",N[1]/2),rep("N",N[1]/2)))
pdf(file="Figure 1.pdf")
plot(IMC_Enfant$ZEP,col='darkblue',
     ylab='effectifs',
     ylim=c(0,100),
     density=c(NA,50,30),
     main='Distribution en fonction de \n la Zone d\'Education Prioritaire')
dev.off()

#Diagramme en barre horizontale réprésentant la repartition du sexe
IMC_Enfant$SEXES = factor(c(rep("F",N[1]/2),rep("G",N[1]/2)))
pdf(file="Figure 2.pdf")
plot(IMC_Enfant$SEXES,col='slateblue1',
     density=c(NA,50,30),
     xlab='Effectifs',
     xlim=c(0,100),
     main='distribution \n en fonction du sexe \n des enfants',horiz=T)
dev.off()

#Camembert répresentant la distribution en fonction de l'age
IMC_Enfant$Ages = factor(c(rep("4",N[1]/2),rep("3",N[1]/2)))

Nom = levels(IMC_Enfant$Ages)
Donnee = table(IMC_Enfant$Ages)
pdf(file="Figure 3.pdf")
pie(Donnee,col='slateblue1', density=c(NA,50,30),
    main='Distribution \n en fonction de l\'age',
     labels=c(paste(Nom[1],';',
      Donnee[1]),paste(Nom[2],";",
      Donnee[2]),paste(Nom[3])))
dev.off()

#Histogramme Répartition par groupe de poids
pdf(file="Figure 4.pdf")
Titre = 'Répartition par groupe de poids'
hist(IMC_Enfant$poids,col='slateblue1',xlab='Poids en Kg',main=Titre)
dev.off()


#.............................................#
#           Analyses des données              #
#.............................................#

#Analyse en fonction du taille, le poids et le sexe
aggregate(IMC_Enfant$poids,list(IMC_Enfant$SEXE),summary)

aggregate(IMC_Enfant$taille,list(IMC_Enfant$SEXE,IMC_Enfant$poids<20),summary)

aggregate(IMC_Enfant$poids,list(IMC_Enfant$SEXE,IMC_Enfant$taille<95),summary)

tapply(IMC_Enfant$poids,list(IMC_Enfant$SEXE,IMC_Enfant$taille),median,na.rm=T)

tapply(IMC_Enfant$taille,list(IMC_Enfant$SEXE,IMC_Enfant$poids),median,na.rm=T)


#Analyse en fonction de la Zone d'education Prioritaire et le sexe
Tab1 = table(IMC_Enfant$SEXE,IMC_Enfant$ZEP)
prop.table(Tab1,1)
round(prop.table(Tab1,1)*100,1)


Var.1 = IMC_Enfant$SEXE
Var.2 = IMC_Enfant$ZEP
X1 = 'Homme'
X2 = 'Femme'
Z = '%'
Y1 = 'NoN'
Y2 = 'OUI'
a = table(Var.1,Var.2)
b = round(prop.table(a,1)*100,digit=1)
Total2 = margin.table(a,1)
Conting2 = cbind(a[,1],b[,1],a[,2],b[,2])
dimnames(Conting2) = list(c(X1,X2),c(Y1,Z,Y2,Z))
Conting2 = cbind(Conting2,Total=Total2)
Total2 = margin.table(Conting2,2)
Total2[2] = round((Total2[1]/Total2[7])*100,1)
Total2[4] = round((Total2[3]/Total2[7])*100,1)
Total2[6] = round((Total2[5]/Total2[7])*100,1)
Conting2 = rbind(Conting2,Total=Total2)
Conting2

#Graphiques de la distribution du sexe en fonction du poids
pdf(file="Figure 5.pdf") 

plot(IMC_Enfant$poids~IMC_Enfant$SEXE,col="slateblue1",
     density=c(NA,20,50),xlab="SEXE",ylab="POIDS",
     main ="Poids en fonction du sexe")
