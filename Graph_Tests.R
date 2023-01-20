
# ici je fais un petit graphique d'évolution de la pression via la liste d'attente 
# il faudrait faire un autre graphique avec l'évolution du taux de pression (le nombre donné par parcoursup) : C FAIT ! 

histo <- subset(InsaUT, select = c(EtbShort,Session,LCvsCapacite))
# histo <- pivot_longer(histo, c(MentionNo,MentionAB,MentionB,MentionTB,MentionTTB))
histo <- pivot_longer(histo, LCvsCapacite)
histo <- histo %>% dplyr::rename(LCvsCapacite = name,  Repartition =value)

# histo$Mention <- fct_relevel(histo$Mention, "MentionNo", "MentionAB", "MentionB", "MentionTB", "MentionTTB")
# histo$Mentionlab <- histo$Mention %>%
#   fct_recode("Sans" = "MentionNo",
#              "AB" = "MentionAB",
#              "B" = "MentionB",
#              "TB" = "MentionTB",
#              "Félic" = "MentionTTB")

mongraph <- ggplot(histo, aes(x = LCvsCapacite, y=Repartition, fill = Repartition))+
  geom_bar(stat = 'identity', width = 1)+
  facet_wrap(~EtbShort)+
  geom_col()+
  theme(legend.position = "bottom")+
  labs(x="", y="Distribution", fill = "", title="Répartition des admis par mention au bac", subtitle = "test")



qplot(x=Session,y=Repartition, data = histo, geom = "line") +
  facet_wrap(~EtbShort) + scale_x_discrete(name = "Année")


# qplot(x=Session,y=Repartition, data = histo, geom = "line") +
#   facet_wrap(~EtbShort) + scale_x_discrete(name = "Année", 
#                                            break = c(2020,2021),
#                                            labels=c(2020="2020",2021="2021"))

# http://www.sthda.com/french/wiki/ggplot2-graduation-des-axes-guide-pour-personnaliser-les-etiquettes-des-graduations-logiciel-r-et-visualisation-de-donnees 
#   geom_density(adjust = 2) + 

# qplot(x=Session,y=Repartition, data = histo, geom = "line")

histo <- subset(InsaUT, select = c(EtbShort,Session,TxAccess))
histo <- pivot_longer(histo, TxAccess)
histo <- histo %>% dplyr::rename(TxAccess = name,  Repartition =value)

qplot(x=Session,y=Repartition, data = histo, geom = "line") +
  facet_wrap(~EtbShort) + scale_x_discrete(name = "Année")
