

library(dplyr)
dat <- data.table::fread("~/Dropbox/Tensor_myeloid 10.27.2018/SUPPLEMENT/SUPPLEMENTARY_TABLE_29.txt")
dat$Component <- as.factor(dat$Component)
n_components <- dat$Component %>% unique() %>% length()
n_TFs <- dat$TF %>% unique() %>% length()

# dat %>% group_by(Component) %>% tally()

dat.cast <- data.table::dcast.data.table(dat,formula = TF ~  Component, value.var = "Z-score") %>% 
  `colnames<-`(paste("C",colnames(.),sep="_")) %>% data.frame()
row.names(dat.cast) <- dat.cast$C_TF
dat.cast <- dat.cast[,!(names(dat.cast)=="C_TF")]

dat.cast[is.na(dat.cast)] <- 0
dat.cast[abs(dat.cast)==Inf] <- 0
library(heatmaply)
hm <- heatmaply(dat.cast,  
                xlab = paste0("Component (n = ",n_components,")"), 
                ylab=paste0("Transcription Factor (n = ",n_TFs,")"), 
                main = "Transcription Factor Enrichment",
                key.title = "Z-score",
                col = c("purple","black","yellow"),
                height = 20, width=10, 
                cexRow = .7,
                cexCol = 1,
                branches_lwd = .7,
                subplot_heights = c(.05,.95),
                subplot_widths = c(.95,.05),
                # file = "TF_enrichment_plot.png",
                return_ppxpy = T) 
hm
# plotly::orca(hm,"TF_enrichment_plot.png", height = 20, width=10)

ggsave("~/Desktop/Tensor_myeloid/TF_enrichment_plot.png", 
       hm$p + labs(title="Transcription Factor Enrichment",
                   x= paste0("Component (n = ",n_components,")"),
                   y=paste0("Transcription Factor (n = ",n_TFs,")")) + 
         theme(legend.title.align=0.5),
       dpi=400, height = 15, width=15)

 
# Top TFs
top50 <- sort(rowMeans(abs(dat.cast)), decreasing = T)[1:50] %>% names()

hm2 <- heatmaply(dat.cast[top50,],  
                xlab = paste0("Component"), 
                ylab=paste0("Transcription Factor"), 
                main = "Transcription Factor Enrichment (Top 50)",
                key.title = "Z-score",
                col = c("purple","black","yellow"),
                height = 20, width=10, 
                cexRow = 1,
                cexCol = 1,
                branches_lwd = .7,
                subplot_heights = c(.05,.95),
                subplot_widths = c(.95,.05),
                # file = "TF_enrichment_plot.png",
                return_ppxpy = T) 
hm2 
ggsave("~/Desktop/Tensor_myeloid/top50TF_enrichment_plot.png", 
       hm2$p + labs(title="Transcription Factor Enrichment (Top 50)",
                   x= paste0("Component"),
                   y=paste0("Transcription Factor")) + 
         theme(legend.title.align=0.5),
       dpi=400, height = 10, width=15)

heatmap(as.matrix(dat.cast), 
        main = "Transcription Factor Enrichment", 
        scale="none", 
        col=gplots::redgreen(75))  


library(ggplot2) 
selected.TFs <- c("IRF1","AP1","STAT1","NFKB1","NF-kappaB","USF1","SPI1","IRF2","CTCF","MEF2A","CEBPA") %>% rev()
dat.select <- subset(dat, TF %in% selected.TFs & isSig==1)  
dat.select[is.na(dat.select$`Z-score`),"Z-score"] <-0
dat.select[abs(dat.select$`Z-score`)==Inf,"Z-score"] <-0
 
# Get the number of data points per 
(dat.select %>% group_by(TF, Component) %>% tally())$n %>% max()

# Order rows
hc.rows <- hclust(dist(dat.cast))
dat.select$TF <- factor(dat.select$TF, levels = selected.TFs, labels = selected.TFs, ordered = T)

# Order cols
# hc.cols <- hclust(dist(t(dat.cast))) 
ordered.components <- gsub("C_","",hc.cols$labels[hc.cols$order])
dat.select$Component <- factor(dat.select$Component, levels = ordered.components, labels = ordered.components, ordered = T)

rp <- ggplot(data = dat.select, aes(y=TF, x=Component, level=`Z-score`)) +
  geom_raster(aes(fill=`Z-score`), interpolate = T) +
  # geom_tile(aes(fill=`Z-score`)) +
  # stat_density(aes(fill = `Z-score`), geom = "raster", position = "identity")
  theme_classic() + 
  scale_fill_gradientn(colours = c("purple","black","yellow")) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "black", colour = NA)) + 
  labs(title = "Transcription Factor Enrichment", y="Transcription Factor")
ggsave("./TF_raster_plot.png",rp,dpi = 400, width = 10)

# Using raster
# library(tidyverse)
# 
# ggplot(dat.cast, aes(x=Component, y=TF, fill=`Z-score`) ) +
#   stat_density_2d() +
#   theme(axis.text.x = element_text(angle = 90))
#  