
#-------------- reading the correlation files -------------#

setwd("T:\\LEPS\\NES\\newStaff\\OSORIO\\UKB_May_collaboration\\May_project_20230216\\update 20230403\\Data from May")


library(corrplot) # make correlation heatmap #

#import data
ICVF_cor  = read.csv("ICVF_correlations", row.names=1)
ISVOF_cor = read.csv("ISVOF_correlations", row.names=1)
OD_cor    = read.csv("OD_correlations", row.names=1)

#making heat maps
setwd("T:/LEPS/NES/newStaff/YHHu/")

jpeg(height=1500, width=1500, file="ICVF_cor_heatmap.jpeg", res = 150)
corrplot(as.matrix(ICVF_cor), method = 'ellipse', order = 'AOE', type = 'upper', tl.col = 'black')
dev.off()

jpeg(height=1500, width=1500, file="ISVOF_cor_heatmap.jpeg", res = 150)
ISVOF_cor.heatmap <- corrplot(as.matrix(ISVOF_cor), method = 'ellipse', order = 'AOE', type = 'upper', tl.col = 'black')
dev.off()

jpeg(height=1500, width=1500, file="OD_cor_heatmap.jpeg", res = 150)
OD_cor.heatmap <- corrplot(as.matrix(OD_cor), method = 'ellipse', order = 'AOE', type = 'upper', tl.col = 'black')
dev.off()



