
# En fait ici j'ai refait la fonction graphique car j'avais des problèmes dans l'introduction de l'année ; là je tente l'année en manuel 

# Fonction pour les graphiques  -----
graphfacet <- function(ficIn, ficName) {
  
  #Inspiration graphiques https://larmarange.github.io/analyse-R/exemples-graphiques-avances.html 
  
  # tri des établissements par niveau de félicitations du jury ----
  ficIn$EtbShort <- ficIn$EtbShort %>%
    fct_reorder2(ficIn$MentionTTB, ficIn$MentionTB, .desc = TRUE)
  
  ficIn$EtbShort <- ficIn$EtbShort %>%
    fct_reorder(ficIn$MentionTTB, .desc = TRUE)
  levels(ficIn$EtbShort)

  write.xlsx(ficIn,paste0(ficName, ".xlsx"),colNames = TRUE)
  # write.xlsx(subset(ficIn,Session==2021),paste0(ficName, "_2021.xlsx"),colNames = TRUE)
  # write.xlsx(subset(ficIn,Session==2020),paste0(ficName, "_2020.xlsx"),colNames = TRUE)
  # 
  # l <- list( "Tout" = ficName, paste0(ficName, "_2021") = subset(ficIn,Session==2021), paste0(ficName, "_2020") = subset(ficIn,Session==2020)) 
  # write.xlsx(l,paste0(ficName, ".xlsx"),firstRow = TRUE, firstActiveCol  = 1)
  
  # ficIn <- subset(ficIn, Session == annee)
  
  histo <- subset(ficIn, select = c(EtbShort,MentionNo,MentionAB,MentionB,MentionTB, MentionTTB))
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
graphfacet(InsaUT21, "Insa & UT")


## graphiques pour les Insa & UT -----
InsaUT20 <- subset(InsaUT, Session ==2020)
InsaUT21 <- subset(InsaUT, Session ==2021)
graphfacet(InsaUT20, "Insa & UT")
