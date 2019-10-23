

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


# Using raster
# library(tidyverse)
# 
# ggplot(dat.cast, aes(x=Component, y=TF, fill=`Z-score`) ) +
#   stat_density_2d() +
#   theme(axis.text.x = element_text(angle = 90))
#  