
## graphiques pour les Insa & UT -----
graphfacet(InsaUT, "Insa & UT", 2020)
graphfacet(InsaUT, "Insa & UT", 2021)
graphfacet(InsaUT, "Insa & UT", 2022)


## graphiques pour les prépas MP2I -----
graphfacet(MP2Is, "MP2I", 2020)
graphfacet(MP2Is, "MP2I", 2021)
graphfacet(MP2Is, "MP2I", 2022)

## graphiques pour les prépas MPSI -----
graphfacet(MPSI, "MPSI",2021)
graphfacet(MPSI, "MPSI",2022)


## graphiques pour le concours Avenir  -----
graphfacet(Avenir, "Concours Avenir", 2020)
graphfacet(Avenir, "Concours Avenir", 2021)
graphfacet(Avenir, "Concours Avenir", 2022)

## graphiques Prépas AL ---
graphfacet(AL, "Prépas AL", 2020)
graphfacet(AL, "Prépas AL", 2021)
graphfacet(AL, "Prépas AL", 2022)


## Graphiques pour Puissance Alpha ----
graphfacet(PAlpha,"PuissanceAlpha",2020)
graphfacet(PAlpha,"PuissanceAlpha",2021)
graphfacet(PAlpha,"PuissanceAlpha",2022)

## graphiques pour  les formations pré-sélectionnées ----
graphfacet(Selec, "Selection",2020)
graphfacet(Selec, "Selection",2021)
graphfacet(Selec, "Selection",2022)
# ggsave("Selec.png")

## graphiques pour toutes les formations ----
graphfacet(Tout, "Toutes")
# ggsave("PAlpha.png")


# TESTS -----------
# PA <- subset(PAlpha,Session == "2022")

PA <- PAlpha
mesUAI <- c("0932019P","0870997L","0693623N","0951820M","0941934S","0132246A","0783720V","0941890U","0530939H","0831458J","0292125C","0442837V","0753559L","0142440B")
PA <- subset(PA,UAI %in% mesUAI)

print(subset(PA,select = c(UAI,Etablissement), Session =="2022"),n=59)
#PAe <- subset(PA,select = c(UAI,Etablissement), Session =="2022")
PAe <- subset(PA,Session =="2022")
write.xlsx(PAe,"PAe_2020.xlsx",colNames = TRUE)


# ESAIP <- subset(d22, UAI =="0132246A")
d22$UAI <- d22$`Code UAI de l'établissement`

write.xlsx(subset(d22, UAI =="0132246A" & Session == "2022"),"ESIAP.xlsx",colNames = TRUE)
write.xlsx(subset(d22, UAI =="0142440B" & Session == "2022"),"ISEN.xlsx",colNames = TRUE)
write.xlsx(subset(d22, UAI =="0941934S" & Session == "2022"),"EFREI_o.xlsx",colNames = TRUE)



subset(PA,select = c(UAI,Etablissement))
print(subset(PA,select = c(UAI,Etablissement)),n=59)

PA <- PA %>%arrange(desc(Etablissement))

## test d'une moyenne pondérée pour l'EFREI
PAt <- subset(dep, Concours == "Concours Puissance Alpha - Formation d'ingénieur Bac + 5 - Bacs généraux" | 
                   Concours == "Concours Puissance Alpha - Formation d'ingénieur Bac + 5 - Bacs généraux - 2 Sciences" &
                   (Filiere == "Ecole d'Ingénieur" &
                      (FiliereDetaillee == "Bacs généraux" | FiliereDetaillee =="Bacs généraux - 2 Sciences") &
                      FiliereAgregee == "Ecole d'Ingénieur"))
PAt <- subset(PAt, Session == "2022" & (UAI == "0941934S" | UAI =="0132246A"), select = c(EtbShort,NbMentionNo,NbMentionAB,NbMentionB,NbMentionTB,NbMentionTTB,NbCandidatCalcule))
write.xlsx(PAt,"EFREI.xlsx",colNames = TRUE)
PAt
# PAt$tot <- sum(PAt$NbCandidatCalcule)
PAt2 <- PAt
PAt2$NbMentionNo <- sum(PAt$NbMentionNo)
PAt2$NbMentionAB <- sum(PAt$NbMentionAB)
PAt2$NbMentionB <- sum(PAt$NbMentionB)
PAt2$NbMentionTB <- sum(PAt$NbMentionTB)
PAt2$NbMentionTTB <- sum(PAt$NbMentionTTB)
PAt2$NbCandidatCalcule <- sum(PAt$NbCandidatCalcule)
PAt2
# ça marche mais il faut 1/ supprimer les doublons et 2/ trouver un moyen de le faire par école automatiquement

# PApivot <- pivot_longer(PAt,cols = c(NbMentionNo,NbMentionAB,NbMentionB,NbMentionTB,NbMentionTTB,NbCandidatCalcule))
# PApivot
###### LA CA MARCHE PRESQUE PARFAITEMENT ! c'est en cours ! 
#group_by
Pat3 <- PAt
Pat3 <- Pat3 %>%
  group_by(EtbShort) %>%
  dplyr::summarize(sum(NbMentionNo),sum(NbMentionAB),sum(NbMentionB),sum(NbMentionTB),sum(NbMentionTTB),sum(NbCandidatCalcule))
  
Pat3
Pat3 <- Pat3 %>%
  dplyr::rename(NbMentionNo = `sum(NbMentionNo)`,
                NbMentionAB = `sum(NbMentionAB)`,
                NbMentionB = `sum(NbMentionB)`,
                NbMentionTB = `sum(NbMentionTB)`,
                NbMentionTTB = `sum(NbMentionTTB)`,
                NbCandidatCalcule = `sum(NbCandidatCalcule)`)
Pat3
Pat3$MentionNo <- round(Pat3$NbMentionNo/Pat3$NbCandidatCalcule*100,0)
Pat3$MentionAB <- round(Pat3$NbMentionAB/Pat3$NbCandidatCalcule*100,0)
Pat3$MentionB <- round(Pat3$NbMentionB/Pat3$NbCandidatCalcule*100,0)
Pat3$MentionTB <- round(Pat3$NbMentionTB/Pat3$NbCandidatCalcule*100,0)
Pat3$MentionTTB <- round(Pat3$NbMentionTTB/Pat3$NbCandidatCalcule*100,0)
Pat3$Session <- 2022 


histo <- Pat3
#graphique Barre empilées 


histo <- subset(histo, select = c(EtbShort,Session,MentionNo,MentionAB,MentionB,MentionTB, MentionTTB))
histo <- pivot_longer(histo, c(MentionNo,MentionAB,MentionB,MentionTB,MentionTTB))
histo <- histo %>% dplyr::rename(Mention = name,  Repartition =value)

histo$Mention <- fct_relevel(histo$Mention, "MentionNo", "MentionAB", "MentionB", "MentionTB", "MentionTTB")
histo$Mentionlab <- histo$Mention %>%
  fct_recode("Sans" = "MentionNo",
             "AB" = "MentionAB",
             "B" = "MentionB",
             "TB" = "MentionTB",
             "Félic" = "MentionTTB")



f <- function(x) {
  res <- scales::percent(x, accuracy = 1)
  res[x < .01] <- ""
  res
}
mesbarres <- ggplot(histo)+
  aes(x = EtbShort, y = Repartition, fill = Mention, label = f(Repartition/100)) +
  geom_bar(stat = "identity", position = "fill") +
  theme(legend.position = "bottom") + 
  geom_text(stat = "identity", position = position_fill(.5),
            colour = "white", fontface = "bold", size = 3.5) +
  labs(x="",y="Distribution", fill = "",title="Répartition des admis par mention au bac", subtitle = "session 2022")
mesbarres

#### c'est fini

######
PApivot %>%
  group_by(EtbShort)%>%
  summarise(NbCandidat = sum(name == "NbCandidatCalcule" ))

PA

PApivot %>%
  group_by(EtbShort)%>%
  summarise(NbCandidat = sum(value) %>%
              (name=="NbCandidatCalcule" ))
