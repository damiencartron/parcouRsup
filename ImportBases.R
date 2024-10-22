library(readr)
library(dplyr)
library(tidyverse)
library(openxlsx)
# library(xlsx)
library(ggplot2)
library(questionr)
library(forcats)
library(knitr)
library(kableExtra)
library(rmarkdown)
library(markdown)

# A FAIRE ----
# -	Si tu veux appliquer le DRY/DRO principle (don’t repeat yourself, don’t repeat others), il faut inclure dans le code la commande qui va télécharger les données sur le site. 
# download.file("https://data.enseignementsup-recherche.gouv.fr/api/explore/v2.1/catalog/datasets/fr-esr-parcoursup/exports/csv?lang=fr&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B", "chemin/nomQueTuDonnes.csv")



#Import CSV ----
##Import Parcoursup 2023 ----
fr_esr_parcoursup_2023 <- read_delim("fr-esr-parcoursup_2023.csv", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

d23<- fr_esr_parcoursup_2023

##Import Parcoursup 2022 ----
fr_esr_parcoursup_2022 <- read_delim("fr-esr-parcoursup_2022.csv", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

d22<- fr_esr_parcoursup_2022


##Import Parcoursup 2021 ----
fr_esr_parcoursup_2021 <- read_delim("fr-esr-parcoursup_2021.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

d21<- fr_esr_parcoursup_2021

## Import 2020 -------
fr_esr_parcoursup_2020 <- read_delim("fr-esr-parcoursup_2020.csv", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
d20<- fr_esr_parcoursup_2020
d20 <- d20 %>% dplyr::rename(`Filière de formation détaillée bis` = `Filière de formation détaillée...14`,
                             `Filière de formation détaillée`     = `Filière de formation détaillée...11`)
# d20$MentionTTB <- 0
d20$`% d’admis néo bacheliers avec mention Très Bien avec félicitations au bac` <- 0

## Import 2019 - Z year ---- 
fr_esr_parcoursup_2019 <- read_delim("fr-esr-parcoursup-2019.csv", 
                                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
d19 <- fr_esr_parcoursup_2019

## Import 2018---- 
fr_esr_parcoursup_2018 <- read_delim("fr-esr-parcoursup-2018.csv", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
d18 <- fr_esr_parcoursup_2018

## fusion des bases -----
d<-bind_rows(d23,d22,d21,d20)

## recodages ----
d$UAI <- d$`Code UAI de l'établissement`
d$Statut <- d$`Statut de l’établissement de la filière de formation (public, privé…)`
d$Filiere <- d$`Filière de formation très agrégée`
d$Filierebis <- d$`Filière de formation détaillée bis`
d$FiliereAgregee <- d$`Filière de formation très agrégée`
d$FiliereDetaillee <- d$`Filière de formation très détaillée`
d$Concours <- d$`Filière de formation détaillée`
d$AdmisT3 <- round(d$`% d’admis ayant reçu leur proposition d’admission avant la fin de la procédure principale`,1)
d$AdmisT2 <- round(d$`% d’admis ayant reçu leur proposition d’admission avant le baccalauréat`,1)
d$AdmisT1 <- round(d$`% d’admis ayant reçu leur proposition d’admission à l'ouverture de la procédure principale`,1)
d$MentionNo <- round(d$`% d’admis néo bacheliers sans mention au bac`,0)
d$MentionAB <- round(d$`% d’admis néo bacheliers avec mention Assez Bien au bac`,0)
d$MentionB  <- round(d$`% d’admis néo bacheliers avec mention Bien au bac`,0)
d$MentionTB <- round(d$`% d’admis néo bacheliers avec mention Très Bien au bac`,0)
d$MentionTTB <- round(d$`% d’admis néo bacheliers avec mention Très Bien avec félicitations au bac`,0)
d$MentionTBTTB <- round(d$`% d’admis néo bacheliers avec mention Très Bien au bac`+ d$`% d’admis néo bacheliers avec mention Très Bien avec félicitations au bac`,0)

d$NbMentionNo <- d$`Dont effectif des admis néo bacheliers sans mention au bac`
d$NbMentionAB <- d$`Dont effectif des admis néo bacheliers avec mention Assez Bien au bac`
d$NbMentionB <- d$`Dont effectif des admis néo bacheliers avec mention Bien au bac`
d$NbMentionTB <- d$`Dont effectif des admis néo bacheliers avec mention Très Bien au bac`
d$NbMentionTTB <- d$`Dont effectif des admis néo bacheliers avec mention Très Bien avec félicitations au bac`
d$NbMentionTBTTB <- d$NbMentionTB+d$NbMentionTTB
d$NbCandidatCalcule <- d$NbMentionNo+d$NbMentionAB+d$NbMentionB+d$NbMentionTBTTB
d$Etablissement <- d$Établissement
d$filles <- round(d$`% d’admis dont filles`,0)
d$Capacite <- d$`Capacité de l’établissement par formation`
d$NbCandidats <- d$`Effectif total des candidats pour une formation`
d$NbClasse <- d$`Effectif total des candidats classés par l’établissement en phase principale`
d$LastCalled <-d$`Effectif total des candidats ayant reçu une proposition d’admission de la part de l’établissement`
d$TailleLC <- d$LastCalled - d$Capacite
d$LCvsCapacite <- round((d$TailleLC / d$Capacite)+1, 1)
d$TxAccess <- round(d$LastCalled / d$NbCandidats,3)*100


#####################
d$EtbShort <- d$Etablissement %>% #juste des noms courts pour les graphiques et même Excel
  fct_recode(
    "INSA Toulouse" = "INSA Toulouse",  
    "INSA Lyon" = "INSA Lyon", 
    "HumTech" = "Cursus Humanités et technologie UTC Université de Technologie de Compiègne",
    "UTC" = "UTC Université de Technologie de Compiègne",
    "INSA Stbg" = "INSA Strasbourg",
    "INSA Rouen" = "INSA Rouen Normandie",
    "INSA Nord" = "INSA (Institut National des Sciences Appliquées) Hauts de France de VALENCIENNES",
    "INSA Nord" = "INSA Hauts-de-France (Valenciennes)",
    "UTT" = "UTT Université de Technologie de Troyes",
    "INSA Centre" = "INSA Centre Val de Loire",
    "INSA Rennes"="INSA Rennes",
    "Le Parc" = "Lycée du Parc",
    "LLG" = "Lycée Louis Le Grand",
    "St-Louis" = "Lycée Saint-Louis (6ème)", 
    "Hoche" = "Lycée Hoche",
    "Janson" = "Lycée Janson De Sailly",
    "P. Valéry" = "Lycée Paul Valery",
    "Faidherbe" = "Lycée Faidherbe",
    "F.Ste Marie" = "Lycée Fénelon Sainte-Marie",
    "ESILV" = "ESILV - Paris la Défense",
    "Lamartine" = "Lycée Lamartine",
    "Herriot" = "Lycée Edouard Herriot",
    "Carnot" = "Lycée Carnot",
    "Fénelon" = "Lycée Fénelon",
    "H. Boucher" = "Lycée Hélène Boucher",
    "Lakanal" = "Lycée Lakanal",
    "Berthelot" = "Lycée Marcelin Berthelot",
    "ChateauB" = "Lycée Francois René De Chateaubriand",
    "H4" = "Lycée Henri IV",
    "Ste Marie" = "Lycée Sainte-Marie de Neuilly",
    "Isen Ouest" = "ISEN Yncrea Ouest Caen",
    "Isen Med" = "ISEN Méditerranée",
    "ESEO Paris" = "ESEO Paris - Vélizy",
    "ESIEE" = "ESIEE Paris",
    "ESIAP" = "ESAIP Aix-en-Provence",
    "Montpellier" = "Polytech Montpellier", 
    "Angers" = "Polytech Angers", 
    "Lille" = "Polytech Lille", 
    "Sorbonne" = "Polytech Sorbonne", 
    "Lille" = "Polytech Lille", 
    "Clermont" = "Polytech Clermont", 
    "Nice" = "Polytech Nice Sophia", 
    "Saclay" = "Polytech Paris-Saclay", 
    "Sorbonne" = "Polytech Sorbonne", 
    "Clermont" = "Polytech Clermont", 
    "Nancy" = "Polytech Nancy", 
    "Angers" = "Polytech Angers", 
    "Annecy" = "Polytech Annecy-Chambéry", 
    "Grenoble" = "Polytech Grenoble", 
    "Marseille" = "Polytech Marseille", 
    "Montpellier" = "Polytech Montpellier", 
    "Nantes" = "Polytech Nantes", 
    "Orléans" = "Polytech Orléans", 
    "Tours" = "Polytech Tours", 
    "Lyon" = "Polytech Lyon", 
    "Stan" = "Lycée Stanislas", 
    "St J Passy" = "Lycée Saint Jean de Passy",
    "Michelet" = "Lycée Michelet",
    "Ginette" = "Lycée Sainte Geneviève",
    "Paris Cité" = "Université de Paris",
    "UMLV" = "Université Gustave Eiffel"
    )


dep <- d 
d <- subset(d,select = c(Session,UAI,Etablissement,EtbShort,Statut,Filiere,Filierebis,FiliereDetaillee, Concours,
                         Capacite,NbCandidats,NbClasse,LastCalled,TailleLC,LCvsCapacite, TxAccess,
                         AdmisT1, AdmisT2,AdmisT3,MentionNo,MentionAB,MentionB,MentionTB,MentionTTB,MentionTBTTB,filles,FiliereAgregee,FiliereDetaillee,
                         NbMentionNo,NbMentionAB,NbMentionB,NbMentionTB,NbMentionTTB,NbCandidatCalcule))


# levels(d$EtbShort)
# tri des établissements par niveau de félicitations du jury ----
# d$EtbShort <- d$EtbShort %>%
#   fct_reorder2(d$MentionTTB, d$MentionTB, .desc = TRUE)

# d$EtbShort <- d$EtbShort %>%
#   fct_reorder2(d$MentionTB, d$MentionB, .desc = TRUE)
# # pas de félicitations dans le fichier 2020 

# EXTRACTION ----
## extraction données MP2I ----
MP2I <- subset(d,Filierebis =="MP2I")
MP2I <- subset(MP2I,UAI != "9711032V")

mesUAI <- c("0590119J", "0690026D", "0750655E", "0750658H", "0750679F", "0750699C", "0753873C", "0782562L")
# les virés : "0210015C","0130040Y",
MP2Is <- subset(MP2I,UAI %in% mesUAI)

#View(MP2Is)

## extraction des Insa et UT ----
mesUAI <- c("0597131F","0690192J","0690192J","0350097R","0350097R","0350097R","0760165S","0310152X","0310152X","0310152X",
            "0601223H","0601223D","0101060Y","0922563L")
# je vire les Insa sans informatique (Val de loire, Strasbourg) "0410979S", "0670190T",
InsaUT <- subset(d,UAI %in% mesUAI)
InsaUT <- subset(InsaUT,FiliereDetaillee =="Bac Général" | FiliereDetaillee =="Série générale" | 
                   FiliereDetaillee == "Bac ES, L" | FiliereDetaillee == "Bac ES" | 
                   FiliereDetaillee == "Bac L" | FiliereDetaillee == "Bac S")
 # View(InsaUT)
write.xlsx(InsaUT,'InsaUT.xlsx',colNames = TRUE,firstActiveCol = 4)



## Extractions des Ecoles d'ingénieurs autres ----
mesUAI <- c("0932019P")
EInge <- subset(d,UAI %in% mesUAI)
EInge <- subset(EInge, UAI == "0932019P" & FiliereAgregee == "Ecole d'Ingénieur" & FiliereDetaillee == "Bacs généraux")

##Extraction des écoles Puissance Alpha ---- 
PAlpha <- subset(d, Concours == "Concours Puissance Alpha - Formation d'ingénieur Bac + 5 - Bacs généraux" | 
                   Concours == "Concours Puissance Alpha - Formation d'ingénieur Bac + 5 - Bacs généraux - 2 Sciences" &
                   (Filiere == "Ecole d'Ingénieur" &
                   (FiliereDetaillee == "Bacs généraux" | FiliereDetaillee =="Bacs généraux - 2 Sciences") &
                   FiliereAgregee == "Ecole d'Ingénieur"))

# Concours Puissance Alpha - Formation d'ingénieur Bac + 5 - Bacs généraux - 2 Sciences

## Extraction des Concours Avenir -----
mesUAI <- c("0922563L","0762378X")

Avenir <- subset(d,UAI %in% mesUAI)
Avenir <- subset(Avenir,FiliereDetaillee =="Bac Général" | FiliereDetaillee =="Série générale" | 
                   FiliereDetaillee == "Bac ES, L" | FiliereDetaillee == "Bac ES" | 
                   FiliereDetaillee == "Bac L" | FiliereDetaillee == "Bac S" |
                   FiliereDetaillee == "Bac Série générale")
# View(Avenir)


## Extraction des Polytech -----
# Les Polytech 
mesUAI <- c("0341143H","0492226D","0596610P","0754400A","0596610P","0631383L","0061661Y","0911986P","0754400A","0631383L","0540130Y","0492226D","0741510P","0382881A","0133682G","0341143H",
            "0442409E","0451638L","0371610Z","0693550J")
Polytech <- subset(d,UAI %in% mesUAI & Capacite >39)
# en fait les doublons sont l'option bio ; il suffit de supprimer les moins de 50 places si je veux aller vite
# j'ai vérifié pour 2022 ça fonctionne mais pas vérifié pour les autres années 



## Extraction des MPSI -----
mesUAI <- c("0940120V","0783053V","0690026D","0750655E","0750658H","0590119J","0753840S","0750654D","0782562L","0750699C","0783053V")
MPSI <- subset(d,UAI %in% mesUAI & Filierebis =="MPSI")

## Extraction des Prépa littéraires AL -----
mesUAI <- c("0690027E","0210015C","0920145H","0940120V","0350710G","0590119J","0690026D","0750654D","0750670W","0920875B","0750655E","0750660K",
            "0750699C","0750714U")
AL <- subset(d,UAI %in% mesUAI & Filierebis =="Lettres")
# View(AL)

## Extraction des HEC  -----
d2<-d
d<-d2
d$UAI[d$UAI =="0750654D" & d$Filierebis =="ECG - Mathématiques appliquées + HGG"] <- "0750654Dapplik"

d$EtbShort[d$UAI =="0750654Dapplik"] <- factor(d,"H4Applik")

d$Filierebis[d$UAI =="0750654Dapplik" & d$Filierebis =="ECG - Mathématiques appliquées + HGG"] <- "ECG - Mathématiques approfondies + HGG"
mesUAI <- c("0750654D","0750654Dapplik","0753947H","0920145H","0920149M","0753840S","0783053V")
#d$EtbShort[d$EtbShort == "H4" & d$UAI=="0750654Dapplik"] <- "H4 Appliqué"
HEC <- subset(d,UAI %in% mesUAI & Filierebis =="ECG - Mathématiques approfondies + HGG")

HEC$EtbShort <- fct_explicit_na(HEC$EtbShort, na_level = "H4 Appliqué")
# bon c'est dégueu mais jamais réussi à renommer le facteur directement 
# bon faudrait faire comme pour universités 

## Extractions des Universités -----
Univ <- subset(d, Concours == "Université de Paris - Licence - Informatique - Parcours Informatique Générale - Campus Grands Moulins" |
                  Concours == "Université de Paris - Double licence - Mathématiques / Informatique - Double licence Mathématiques Informatique - Campus Grands Moulins" |
                  Concours == "Université Gustave Eiffel - Licence - Portail INFORMATIQUE,  MATHEMATIQUES, INGENIERIE MATHEMATIQUE ET INFORMATIQUE")

#j'avais comme poru HEC plein de pb poru recoder EtbShort ; même Marie n'y arrivait pas ; elle a fini par avoir l'idée de repasser en caractère EtbShor puis en factor pour mes grpahqiues
Univ$EtbShort <- as.character(Univ$EtbShort)
Univ$EtbShort <- ifelse(Univ$Filierebis=="Mathématiques",
       "Paris-Cité DL",
       Univ$EtbShort) 
Univ$EtbShort <- as.factor(Univ$EtbShort)


# Export dans des onglets excel -----
Tout <- bind_rows(InsaUT, MP2Is,EInge,PAlpha,Avenir,MPSI,AL)
Selec <- bind_rows(InsaUT,MP2Is,EInge,Univ)
l <- list("Tout" = Tout, "InsaUT" = InsaUT, "MP2Is" = MP2Is, "EInge" = EInge, "PAlpha" = PAlpha, "MPSI" = MPSI, "AL" = AL, "Selection" = Selec) 
write.xlsx(l,"ExportTot.xlsx",firstRow = TRUE, firstActiveCol  = 1)


# HISTOGRAMMES Mentions----

# Fonction pour les graphiques  -----
graphfacet <- function(ficIn, ficName, annee) {

## Ecriture des fichiers excel de sortie ----
write.xlsx(ficIn,paste0(ficName, ".xlsx"),colNames = TRUE)
write.xlsx(subset(ficIn,Session==2023),paste0(ficName, "_2023.xlsx"),colNames = TRUE)
write.xlsx(subset(ficIn,Session==2022),paste0(ficName, "_2022.xlsx"),colNames = TRUE)
write.xlsx(subset(ficIn,Session==2021),paste0(ficName, "_2021.xlsx"),colNames = TRUE)
write.xlsx(subset(ficIn,Session==2020),paste0(ficName, "_2020.xlsx"),colNames = TRUE)

## Graphique Ratio liste d'attente 

histo <- subset(ficIn, select = c(EtbShort,Session,LCvsCapacite))
histo <- pivot_longer(histo, LCvsCapacite)
histo <- histo %>% dplyr::rename(LCvsCapacite = name,  Repartition =value)

monratio <- ggplot(data=histo, aes(x=floor(Session), y=Repartition, label = Repartition)) +
  geom_line(stat = "identity", position = "identity")+
  geom_point()+
  geom_text(nudge_y = 2)+
  labs(x="Année", y="NbPlaces/Dernier entré", title = "Ratio dernier pris sur taille promo", subtitle = paste0(ficName, " - session ", annee))+
  xlim(2020,2022)+
  facet_wrap(~EtbShort)
# bon à savoir :   facet_wrap(~EtbShort, scales="free") permet d'avoir des échelles différentes 

monratio 
ggsave(paste0(ficName, "_RatioLC.png"),device = "png",
       height =  8.25, width = 11.75)

## Graphique taux d'accès ---- 
histo <- subset(ficIn, select = c(EtbShort,Session,TxAccess))
histo <- pivot_longer(histo, TxAccess)
histo <- histo %>% dplyr::rename(TxAccess = name,  Repartition =value)

montxaccess <- ggplot(data=histo, aes(x=floor(Session), y=Repartition, label = Repartition)) +
  geom_line(stat = "identity", position = "identity")+
  geom_point()+
  geom_text(nudge_y = 2)+
  labs(x="Année", y="Taux d'accès", title = "Evolution du taux d'accès", subtitle = paste0(ficName, " - session ", annee))+
  facet_wrap(~EtbShort)

montxaccess 
ggsave(paste0(ficName, "_TxAccess.png"),device = "png",
       height =  8.25, width = 11.75)
## Subset année et tri de la base ----
ficInOrign <- ficIn
ficIn <- subset(ficIn, Session == annee)
  
ficIn$EtbShort <- ficIn$EtbShort %>%
    fct_reorder2(ficIn$MentionTTB, ficIn$MentionTB, .desc = TRUE)
  
ficIn$EtbShort <- ficIn$EtbShort %>%
    fct_reorder(ficIn$MentionTTB, .desc = TRUE)
  levels(ficIn$EtbShort)

###  subset last year
  ficInLY <- subset(ficInOrign, Session == annee-1)
  
  ficInLY$EtbShort <- ficInLY$EtbShort %>%
    fct_reorder2(ficInLY$MentionTTB, ficInLY$MentionTB, .desc = TRUE)
  
  ficInLY$EtbShort <- ficInLY$EtbShort %>%
    fct_reorder(ficInLY$MentionTTB, .desc = TRUE)
  levels(ficInLY$EtbShort)
  
  
  # l <- list( "Tout" = ficName, paste0(ficName, "_2021") = subset(ficIn,Session==2021), paste0(ficName, "_2020") = subset(ficIn,Session==2020)) 
  # write.xlsx(l,paste0(ficName, ".xlsx"),firstRow = TRUE, firstActiveCol  = 1)
  

  
  histo <- subset(ficIn, select = c(EtbShort,Session,MentionNo,MentionAB,MentionB,MentionTB, MentionTTB))
  histo <- pivot_longer(histo, c(MentionNo,MentionAB,MentionB,MentionTB,MentionTTB))
  histo <- histo %>% dplyr::rename(Mention = name,  Repartition =value)
  
  histo$Mention <- fct_relevel(histo$Mention, "MentionNo", "MentionAB", "MentionB", "MentionTB", "MentionTTB")
  histo$Mentionlab <- histo$Mention %>%
    fct_recode("Sans" = "MentionNo",
               "AB" = "MentionAB",
               "B" = "MentionB",
               "TB" = "MentionTB",
               "Félic" = "MentionTTB")
  
  mongraph <- ggplot(histo, aes(x = Mentionlab, y=Repartition, fill = Mention))+
    geom_bar(stat = 'identity', width = 1)+
    facet_wrap(~EtbShort)+
    geom_col()+
    theme(legend.position = "bottom")+
    labs(x="",y="Distribution", fill = "", title="Répartition des admis par mention au bac", subtitle = paste0(ficName, " - session ", annee))
  

  ggsave(paste0(ficName, "_Facet_",annee,".png"),device = "png",
         height =  8.25, width = 11.75)
  
  f <- function(x) {
    res <- scales::percent(x, accuracy = 1)
    res[x < .01] <- ""
    res
  }
  
  ##graphique Barre empilées ----
  mesbarres <- ggplot(histo)+
    aes(x = EtbShort, y = Repartition, fill = Mention, label = f(Repartition/100)) +
    geom_bar(stat = "identity", position = "fill") +
    theme(legend.position = "bottom") + 
    geom_text(stat = "identity", position = position_fill(.5),
              colour = "white", fontface = "bold", size = 3.5) +
    labs(x="",y="Distribution", fill = "",title="Répartition des admis par mention au bac", subtitle = paste0(ficName, " - session ", annee)) +
           theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  ggsave(paste0(ficName, "_bars",annee,".png"),device = "png",
         height =  8.25, width = 11.75)
  
  ##graphique Barre empilées n-1  ----
  histo <- subset(ficInLY, select = c(EtbShort,Session,MentionNo,MentionAB,MentionB,MentionTB, MentionTTB))
  histo <- pivot_longer(histo, c(MentionNo,MentionAB,MentionB,MentionTB,MentionTTB))
  histo <- histo %>% dplyr::rename(Mention = name,  Repartition =value)
  
  histo$Mention <- fct_relevel(histo$Mention, "MentionNo", "MentionAB", "MentionB", "MentionTB", "MentionTTB")
  histo$Mentionlab <- histo$Mention %>%
    fct_recode("Sans" = "MentionNo",
               "AB" = "MentionAB",
               "B" = "MentionB",
               "TB" = "MentionTB",
               "Félic" = "MentionTTB")  
  
  mesbarresLY <- ggplot(histo)+
    aes(x = EtbShort, y = Repartition, fill = Mention, label = f(Repartition/100)) +
    geom_bar(stat = "identity", position = "fill") +
    theme(legend.position = "bottom") + 
    geom_text(stat = "identity", position = position_fill(.5),
              colour = "white", fontface = "bold", size = 3.5) +
    labs(x="",y="Distribution", fill = "",title="Répartition des admis par mention au bac", subtitle = paste0(ficName, " - session ", annee-1)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  ggsave(paste0(ficName, "_bars",annee-1,".png"),device = "png",
         height =  8.25, width = 11.75)
  
  maliste <- list(monratio,montxaccess,mongraph,mesbarres,mesbarresLY)
  return(maliste)
}



