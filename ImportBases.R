library(readr)
library(dplyr)
library(tidyverse)
library(openxlsx)
# library(xlsx)
library(ggplot2)
library(questionr)
library(forcats)

#Import CSV ----
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

## fusiion des bases -----
d<-bind_rows(d21,d20)

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
d$Etablissement <- d$Établissement
d$filles <- round(d$`% d’admis dont filles`,0)
d$Capacite <- d$`Capacité de l’établissement par formation`
d$NbCandidats <- d$`Effectif total des candidats pour une formation`
d$NbClasse <- d$`Effectif total des candidats classés par l’établissement en phase principale`
d$LastCalled <-d$`Effectif total des candidats ayant reçu une proposition d’admission de la part de l’établissement`
d$TailleLC <- d$LastCalled - d$Capacite
d$LCvsCapacite <- round(d$TailleLC / d$Capacite, 1)
d$TxAccess <- round(d$LastCalled / d$NbCandidats,3)*100

d <- subset(d,select = c(Session,UAI,Etablissement,Statut,Filiere,Filierebis,FiliereDetaillee, Concours,
                         Capacite,NbCandidats,NbClasse,LastCalled,TailleLC,LCvsCapacite, TxAccess,
                         AdmisT1, AdmisT2,AdmisT3,MentionNo,MentionAB,MentionB,MentionTB,MentionTTB,MentionTBTTB,filles,FiliereAgregee,FiliereDetaillee))







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
    "Ste Marie" = "Lycée Fénelon Sainte-Marie"
  )


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
mesUAI <- c("0597131F","0410979S","0690192J","0690192J","0350097R","0350097R","0350097R","0760165S","0670190T","0310152X","0310152X","0310152X",
            "0601223H","0601223D","0101060Y")

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
PAlpha <- subset(d, Concours == "Concours Puissance Alpha - Formation d'ingénieur Bac + 5 - Bacs généraux" &
                   Filiere == "Ecole d'Ingénieur" &
                   FiliereDetaillee == "Bacs généraux" &
                   FiliereAgregee == "Ecole d'Ingénieur")

## Extraction des MPSI -----
mesUAI <- c("0783053V","0690026D","0750655E","0750658H","0590119J","0753840S","0750654D","0782562L","0750699C")
MPSI <- subset(d,UAI %in% mesUAI & Filierebis =="MPSI")




# Export dans des onglets excel -----
Tout <- bind_rows(InsaUT, MP2Is,EInge,PAlpha,MPSI)
Selec <- bind_rows(InsaUT,MP2Is,EInge)
l <- list("Tout" = Tout, "InsaUT" = InsaUT, "MP2Is" = MP2Is, "EInge" = EInge, "PAlpha" = PAlpha, "MPSI" = MPSI, "Selection" = Selec) 
write.xlsx(l,"ExportTot.xlsx",firstRow = TRUE, firstActiveCol  = 1)


# HISTOGRAMMES Mentions----

# Fonction pour les graphiques  -----
graphfacet <- function(ficIn, ficName, annee) {
#   ficIn$EtbShort <- ficIn$EtbShort %>%
#     fct_reorder2(ficIn$MentionTTB, ficIn$MentionTB, .desc = TRUE)
#     levels(ficIn$EtbShort)

#Inspiration graphiques https://larmarange.github.io/analyse-R/exemples-graphiques-avances.html 
  
  # tri des établissements par niveau de félicitations du jury ----

  # if (annee == 2021) {
  #   ficIn$EtbShort <- ficIn$EtbShort %>%
  #     fct_reorder2(ficIn$MentionTTB, ficIn$MentionTB, .desc = TRUE)
  #   levels(ficIn$EtbShort)
  # } else {
  #   ficIn$EtbShort <- ficIn$EtbShort %>%
  #     fct_reorder2(ficIn$MentionTB, ficIn$MentionB, .desc = TRUE)
  #   levels(ficIn$EtbShort)
  # }
  
  ficIn$EtbShort <- ficIn$EtbShort %>%
    fct_reorder2(ficIn$MentionTTB, ficIn$MentionTB, .desc = TRUE)
  
  ficIn$EtbShort <- ficIn$EtbShort %>%
    fct_reorder(ficIn$MentionTTB, .desc = TRUE)
  levels(ficIn$EtbShort)

  


  
  write.xlsx(ficIn,paste0(ficName, ".xlsx"),colNames = TRUE)
  write.xlsx(subset(ficIn,Session==2021),paste0(ficName, "_2021.xlsx"),colNames = TRUE)
  write.xlsx(subset(ficIn,Session==2020),paste0(ficName, "_2020.xlsx"),colNames = TRUE)
  
  # l <- list( "Tout" = ficName, paste0(ficName, "_2021") = subset(ficIn,Session==2021), paste0(ficName, "_2020") = subset(ficIn,Session==2020)) 
  # write.xlsx(l,paste0(ficName, ".xlsx"),firstRow = TRUE, firstActiveCol  = 1)
  
  ficIn <- subset(ficIn, Session == annee)
  
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
    labs(x="",y="Distribution", fill = "", title="Répartition des admis par mention au bac", subtitle = ficName)
  

  ggsave(paste0(ficName, "_Facet.png"),device = "png",
         height =  8.25, width = 11.75)
  
  f <- function(x) {
    res <- scales::percent(x, accuracy = 1)
    res[x < .01] <- ""
    res
  }
  
  #graphique Barre empilées 
  mesbarres <- ggplot(histo)+
    aes(x = EtbShort, y = Repartition, fill = Mention, label = f(Repartition/100)) +
    geom_bar(stat = "identity", position = "fill") +
    theme(legend.position = "bottom") + 
    geom_text(stat = "identity", position = position_fill(.5),
              colour = "white", fontface = "bold", size = 3.5) +
    labs(x="",y="Distribution", fill = "")
  
  
  # ggsave(paste0(ficIn, "_bar.png"),device = "png",
  # height =  8.25, width = 11.75)
  
  # ggsave(paste0(deparse(substitute(ficIn)), "_bars.png"),device = "png")
  ggsave(paste0(ficName, "_bars.png"),device = "png",
         height =  8.25, width = 11.75)
  
  
  maliste <- list(mongraph,mesbarres)
  return(maliste)
}

## graphiques pour les Insa & UT -----
graphfacet(InsaUT, "Insa & UT", 2020)
graphfacet(InsaUT, "Insa & UT", 2021)


## graphiques pour les prépas MP2I -----
graphfacet(MP2Is, "MP2I", 2020)
graphfacet(MP2Is, "MP2I", 2021)

## graphiques pour les prépas MPSI -----
graphfacet(MPSI, "MPSI")
# ggsave("MPSI.png")







## Graphiques pour Puissance Alpha ----
graphfacet(PAlpha,"PuissanceAlpha")
# ggsave("PAlpha.png")

## graphiques pour  les formations pré-sélectionnées ----
graphfacet(Selec, "Selection")
# ggsave("Selec.png")

## graphiques pour toutes les formations ----
graphfacet(Tout, "Toutes")
# ggsave("PAlpha.png")




# graphique qui fait un sous thème avec la moyenne mais en effectif 
# ggplot(histo, aes(x = Mention, y=Repartition))+geom_bar(stat = 'identity', width = 1)+facet_wrap(~Etablissement)+
#   labs(x = "Mentions",y="Pourcentage", title="Répartition des admis par mention")+
#   scale_color_ipsum()+theme_ipsum_rc()+
#   gghighlight()