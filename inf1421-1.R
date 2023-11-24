library(rpart)
library(readxl)
DonneesOzone <- read_excel("DonneesOzone.xls", sheet = 1)
print(nrow(DonneesOzone))
TendanceCentrale <- summary(DonneesOzone[][2:11]) # horizontale
print(TendanceCentrale)
# indicateur de dispersion
Variance <- var(DonneesOzone[][2:11])
print(Variance)


boxplot(DonneesOzone[][3:5], 'boxstyle', 'filled', 'notch', 'on', ylab =
          "Temperature observée en degrée")
title(
  "Temperatures observée à 9, 12 et 15h",
  cex.lab = 0.8,
  cex.axis = 0.8,
  cex.main = 0.8,
  cex.sub = 0.8
)
dev.new()
boxplot(DonneesOzone[][6:8], 'boxstyle', 'filled', 'notch', 'on', ylab =
          "Nébulosité")
title(
  "Nébulosité observée à 9, 12 et 15h",
  cex.lab = 0.8,
  cex.axis = 0.8,
  cex.main = 0.8,
  cex.sub = 0.8
)
boxplot.stats(DonneesOzone$Ne9)$stats #, $n, $conf et $out === fin
# library(plotrix)
# dev.new()
# lbls <- paste(names(table(DonneesOzone[][13])),"\n",
#              mytable,sep="")
# slices<-matrix(table(DonneesOzone[][13]))
# pct<-round(slices/sum(slices)*100)
# lbls<-paste(lbls,pct,"%",sep="")
# pie3D(table(DonneesOzone[][13]),labels=lbls,explode=0.1,
#      main="Diagramme circulaire pour la variable ’Vent’"
#      ,cex.lab=0.8,cex.axis=0.8,cex.main=0.8,cex.sub=0.8,col=rainbow(length(lbls)))
# dev.new()
# lbls<-paste(names(table(DonneesOzone[][14])),"\n",mytable,sep="")
# slices<-matrix(table(DonneesOzone[][14]))
# pct<-round(slices/sum(slices)*100)
# lbls<-paste(lbls,pct,"%",sep="")
# pie3D(table(DonneesOzone[][14]),labels=lbls,explode=0.1,main="Diagramme circulaire pour la variable ’Pluie’",cex.lab=0.8,cex.axis=0.8,cex.main=0.8,cex.sub=0.8,col=rainbow(length(lbls)))

library(caret)
JeuDonnees <- read_excel("JeuDonnees.xlsx", sheet = 1)
# diagonale BON, horizontale réelle
conf <- table(JeuDonnees$'Classe réelle', JeuDonnees$'Classe prédite')
f.conf <- confusionMatrix(conf)
print(f.conf)

for (i in 1:3) {
  TauxClasse <- conf[i, i] / sum(conf[i, ]) * 100
  Resultat <-
    sprintf('Le taux de classification de la Classe %d est de %.2f%%',
            i,
            TauxClasse)
  print(Resultat)
}
TauxDeClassification <-
  mean(JeuDonnees$'Classe réelle' == JeuDonnees$'Classe prédite') * 100
Resultat <-
  sprintf('Le taux de classification global est de %.2f%%',
          TauxDeClassification)
print(Resultat)
