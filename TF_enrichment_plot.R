


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
                cexCol = .7,
                branches_lwd = .7,
                subplot_heights = c(.05,.95),
                subplot_widths = c(.95,.05),
                file = "~/Desktop/Tensor_myeloid/TF_enrichment_plot.png" ) 
hm
ggsave("~/Desktop/Tensor_myeloid/TF_enrichment_plot.png", hm$p,
       dpi=400, height = 20, width=10)
  


# Using raster
library(tidyverse)

ggplot(dat.cast, aes(x=Component, y=TF, fill=`Z-score`) ) +
  stat_density_2d() +
  theme(axis.text.x = element_text(angle = 90))

a <- data.frame( x=rnorm(20000, 10, 1.9), y=rnorm(20000, 10, 1.2) )
b <- data.frame( x=rnorm(20000, 14.5, 1.9), y=rnorm(20000, 14.5, 1.9) )
c <- data.frame( x=rnorm(20000, 9.5, 1.9), y=rnorm(20000, 15.5, 1.9) )
data <- rbind(a,b,c)
