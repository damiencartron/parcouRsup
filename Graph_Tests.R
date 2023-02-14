
# ici je fais un petit graphique d'évolution de la pression via la liste d'attente 
# il faudrait faire un autre graphique avec l'évolution du taux de pression (le nombre donné par parcoursup) : C FAIT ! 

histo <- subset(InsaUT, select = c(EtbShort,Session,LCvsCapacite))
# histo <- pivot_longer(histo, c(MentionNo,MentionAB,MentionB,MentionTB,MentionTTB))
histo <- pivot_longer(histo, LCvsCapacite)
histo <- histo %>% dplyr::rename(LCvsCapacite = name,  Repartition =value)
#######################ATTTENTION  ########################
# histo <- subset(histo,EtbShort == "UTC")
#######################ATTTENTION  ########################

# histo$Mention <- fct_relevel(histo$Mention, "MentionNo", "MentionAB", "MentionB", "MentionTB", "MentionTTB")
# histo$Mentionlab <- histo$Mention %>%
#   fct_recode("Sans" = "MentionNo",
#              "AB" = "MentionAB",
#              "B" = "MentionB",
#              "TB" = "MentionTB",
#              "Félic" = "MentionTTB")


## Graphique Utilisation LC ---- 
mongraph <- ggplot(histo, aes(x = LCvsCapacite, y=Repartition, fill = Repartition))+
  geom_bar(stat = 'identity', width = 1)+
  facet_wrap(~EtbShort)+
  geom_col()+
  theme(legend.position = "bottom")+
  labs(x="", y="Distribution", fill = "", title="Répartition des admis par mention au bac", subtitle = "test")
mongraph

ggplot(data=df, aes(x=dose, y=len, group=1)) +
  geom_line()+
  geom_point()

ggplot(data=df, aes(x=dose, y=len)) +
  geom_line()+
  geom_point()

############################ TEST ki marche le mieux  ######################
ggplot(data=histo, aes(x=Session, y=Repartition, label = Repartition)) +
  geom_line(stat = "identity", position = "identity")+
  geom_point()+
  geom_text(nudge_y = 2)+
  ggtitle(" Ratio Dernier pris sur taille promo")+
  xlim(2020,2022)+
  facet_wrap(~EtbShort, scales="free")
# bon à savoir :   facet_wrap(~EtbShort, scales="free") permet d'avoir des échelles différentes 


ggplot(data=histo, aes(x=Session, y=Repartition, label = Repartition)) +
  geom_line(stat = "identity", position = "identity")+
  geom_point()+
  geom_text(nudge_y = 2)+
  ggtitle(" Ratio Dernier pris sur taille promo")+
  xlim(2020,2022)+
  facet_wrap(~EtbShort, scales="free")



############################ FIN TEST ######################
ggplot(data=histo, aes(x=Session, y=LCvsCapacite)) +
  geom_line()+
  facet_wrap(~EtbShort)




qplot(x=Session,y=Repartition, data = histo, geom = "line") +
  facet_wrap(~EtbShort) + scale_x_discrete(name = "Année")


# qplot(x=Session,y=Repartition, data = histo, geom = "line") +
#   facet_wrap(~EtbShort) + scale_x_discrete(name = "Année", 
#                                            break = c(2020,2021),
#                                            labels=c(2020="2020",2021="2021"))

# http://www.sthda.com/french/wiki/ggplot2-graduation-des-axes-guide-pour-personnaliser-les-etiquettes-des-graduations-logiciel-r-et-visualisation-de-donnees 
#   geom_density(adjust = 2) + 

# qplot(x=Session,y=Repartition, data = histo, geom = "line")

## Graphique taux d'accès ---- 
histo <- subset(InsaUT, select = c(EtbShort,Session,TxAccess))
histo <- pivot_longer(histo, TxAccess)
histo <- histo %>% dplyr::rename(TxAccess = name,  Repartition =value)

ggplot(data=histo, aes(x=Session, y=TxAccess, group = supp)) +
  geom_line()+
  facet_wrap(~EtbShort)

qplot(x=Session,y=Repartition, data = histo, geom = "line") +
  facet_wrap(~EtbShort) + scale_x_discrete(name = "Année")

