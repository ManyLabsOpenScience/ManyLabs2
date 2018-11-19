## MANYLABS 2 FIGURE (https://osf.io/8cd4r/)
#
# Just load the data and plot the Figure!
#
# corresponding coder: Fred Hasselman (https://osf.io/ujgs6/)

# SETUP ----
library(rio)
library(ggplot2)

pdat <- rio::import("~/OSFdata/!!RawData/Data_Figure_NOweird_long.csv")
means <- rio::import("~/OSFdata/!!RawData/Data_Figure_NOweird_means.csv")
sums <- rio::import("~/OSFdata/!!RawData/Data_Figure_NOweird_sums.csv")
oriEffectsNW <- rio::import("~/OSFdata/!!RawData/Data_Figure_NOweird_oriEffects.csv")

xx <- c(-.9999,-.999,-.99, -.9, -.5,-.1,-.01,-.001,0,.001,.01,.1,.5,.9,.99,.999,.9999)
reg <- lm(atanh(xx)~xx +xx^2 + xx^3+xx^4)

#predict(reg)

g2 <- ggplot(pdat, aes(offset_dens, loc, group = study.labels)) +
  geom_hline(yintercept = 0, color = "grey70") +
  geom_vline(xintercept = xbreaks[-c(27,30)], color = "grey70") +
  geom_vline(xintercept = 26, color = "grey70", linetype=2) +
  geom_path(size=1, alpha = .5, color = "grey70")+
  geom_path() +
  geom_segment(data=sums, aes(x = offset,
                              y = loc,
                              xend = offset_fix,
                              yend = loc),
               inherit.aes=FALSE, alpha=.7, size= .2) +
  geom_segment(data=means, aes(x = offset, y = loc, xend = offset_dens, yend = loc), inherit.aes=FALSE, size=.8, alpha=.8) +
  geom_point(data=oriEffectsNW, aes(x = offset_fix, y = usethisES, shape=oriWEIRD.n, fill=oriWEIRD), colour = "black", inherit.aes = FALSE, size=1.2, alpha=.8) +
  scale_colour_manual('Sample', values = cols) +
  scale_x_continuous(name = '', breaks = xbreaks, labels = xlabels, limits = c(-0.5,max(xbreaks)+0.5), expand = c(0,0)) +
  scale_y_continuous("Effect Size r", expand = c(0,0),limits = c(-1,1), sec.axis = sec_axis(trans = ~reg$coefficients[1]+.*reg$coefficients[2], name = "Cohen's q")) +
  scale_fill_manual('',values = cols, guide=FALSE) +
  scale_shape_manual('Original Effect Size',values=OriShapes, breaks = Sbreaks,labels = c("","","","")) +
  guides(colour = guide_legend(order = 1, ncol=1,title.vjust=.81, title.theme = element_text(size = 9,angle=0,face="bold")), shape = guide_legend(ncol =1,title.position = "right",title.vjust=.81, title.theme = element_text(size = 9,angle=0,face="bold"))) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.margin = margin(0,0,0,0),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(20,20,20,10)) +
  coord_flip()
g2

# SAVE ----

figname <- paste0("ML2_SplitViolin_MEANsort_QonTop_2ndAxis_with_line",as.Date(now()))
ggsave(plot = g2, filename = paste0(outdir,"/",figname,".eps"),width=10, units = "in",dpi=600)
ggsave(plot = g2, filename = paste0(outdir,"/",figname,".pdf"),width=10, units = "in",dpi=600)
ggsave(plot = g2, filename = paste0(outdir,"/",figname,".png"),width=10, units = "in",dpi=600)
ggsave(plot = g2, filename = paste0(outdir,"/",figname,".tiff"),width=10, units = "in",dpi=600)
