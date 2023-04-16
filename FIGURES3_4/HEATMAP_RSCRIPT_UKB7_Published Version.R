#######################################################################
#######  Heat maps for total and hospital-treated infections and ISOVF, ICVF and OD  across LE8 tertile or AD-PGS tertile
#######  Mission: Plot the betas   
#######  Programmer: Yi-Han Hu
#######  Date: Mar. 01 2023
#######  Final Update: Mar. 15 2023
#######################################################################

op <- options(nwarnings = 10000)
# --------------------------------------
# Specify working directory where the script and data files are
# --------------------------------------
WorkingDirectory = "file location"

# --------------------------------------
# Set working directory
# --------------------------------------
setwd(WorkingDirectory)

# --------------------------------------
# Turn off scientific notation
# --------------------------------------
options(scipen=999)

# --------------------------------------
# Install/load the packages
# --------------------------------------
library(readxl) #read excel file.
library(haven) # read SAS, DTA file.
library(dplyr)
library(stringr)
library(ggplot2)
library(RColorBrewer)

# ---------------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------------#
# ---------------------------------- Part 1 Data preprocess -----------------------------------#
# ---------------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------------#
# --------------------------------------
# Load data:
# --------------------------------------
Dprime_overall <- read_dta("Data/Outputdata_overall_Dprime.dta")
dim(Dprime_overall)
head(Dprime_overall)


Cprime_overall <- read_dta("Data/Outputdata_overall_Cprime.dta")
dim(Cprime_overall)
head(Cprime_overall)

Dprime_overall <- Dprime_overall %>% 
  mutate(outcome = gsub(".*?z([^ ]*) i.*", "\\1",command),
         modality = gsub("([^_]*)_.*", "\\1", outcome),
         ROI = gsub("^[^_]*_", "", outcome))
colnames(Dprime_overall)

Cprime_overall <- Cprime_overall %>% 
  mutate(outcome = gsub(".*?z([^ ]*) i.*", "\\1",command),
         modality = gsub("([^_]*)_.*", "\\1", outcome),
         ROI = gsub("^[^_]*_", "", outcome))

colnames(Cprime_overall)

# ---------------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------------#
# ------------------------------------- Part 2 Heat maps --------------------------------------#
# ---------------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------------#
# --------------------------------------
# Heatmap for ISOVF, ICVF and OD
# --------------------------------------
heatmap <- function(data, stratified_by = "AD_PGStert", modality.cat = "ICVF", sig.basedon.p = FALSE, hide.unsig = TRUE){
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

  if (stratified_by == "AD_PGStert"){
    data <- data %>% mutate(tert = AD_PGStert) %>% mutate(predictor = ifelse(parm=="infectionburdenhosptert", "IB(hosp)", "IB(total)"),
                                                                 term_new = paste("PGS_tert", tert, "_", predictor, sep = ""))
    tert_name <- "AD PGS"
  } else if(stratified_by == "LE8_TOTALSCOREtert"){
    data <- data %>% mutate(tert = LE8_TOTALSCOREtert) %>% mutate(predictor = ifelse(parm=="infectionburdenhosptert", "IB(hosp)", "IB(total)"),
                                                                         term_new = paste("LE8_tert", tert, "_", predictor, sep = ""))
    tert_name <- "LE8"
  }
  
  if (sig.basedon.p == TRUE){
    data <- data %>% mutate(sig.value = p)
    size.title <- "p-value" 
    fill.title <- (expression(paste(beta," coefficients")))
  } else if(sig.basedon.p == FALSE){
    data <- data %>% mutate(sig.value = myqvallargervolumes)    
    size.title <- "q-value" 
    fill.title <- (expression(paste(beta," coefficients")))
  }
  
  data.long <- data %>% 
    mutate(ap=ifelse(sig.value < 0.01, 1,
                     ifelse(sig.value >= 0.01 & sig.value < 0.05, 2, 3)),
           aq=ifelse(round(sig.value, digits=3) < 0.05 , "Pass", "insig"),
           bg.line=ifelse(tert==1, "Dark Grey",
                          ifelse(tert==3, "Dark Grey", "White")),
           bg.color=ifelse(tert==1, "White",
                           ifelse(tert==3, "White", "Dark Grey")),
           asterisk=ifelse(signif == 1, "*", ""))%>% 
    arrange(desc(ROI), tert, predictor) %>% 
    filter(modality == modality.cat)

  if (hide.unsig == TRUE){
    p.plot <- ggplot(data = data.long, aes(x = factor(term_new), y = factor(ROI)))+
      geom_tile(color = data.long$bg.line, fill = data.long$bg.color)+
      geom_point(aes(shape=factor(aq),
                     size=factor(ap), 
                     fill=estimate))+
      geom_text(aes(label = asterisk), size = 6) +
      scale_shape_manual(values=c(1, 21), guide = "none")+
      scale_fill_gradientn(colours = myPalette(100), aesthetics = c("colour","fill"))+
      scale_size_manual(values=c(6, 4, 2), labels = c("< .01", "< .05", "\u2265 .05"))+
      labs(title=paste("Heatmap (", "Infection burden", " and ", modality.cat," metrics)", sep = ""),
           subtitle=paste("Stratified by ", tert_name, " tertiles", sep = ""),
           x=paste("Infection burden"," by ", tert_name, " strata", sep = ""),
           y="Brain ROI (47 tracts + global)",
           size = size.title,
           fill = fill.title,
           caption="solid circle: q < .05; *: q < .002")+
      theme(plot.title = element_text(color="Dark blue", size=15, face="bold.italic", hjust = 0.5),
            plot.subtitle=element_text(size=14, hjust=0.5, face="italic", color="Dark blue"),
            plot.caption=element_text(size=10, hjust=0.5, color="Dark grey"),
            axis.title.x = element_text(color="deepskyblue", size=13, face="bold"),
            axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
            aspect.ratio=7/4)+
      coord_fixed()
  } else if (hide.unsig == FALSE){
    p.plot <- ggplot(data = data.long, aes(x = factor(term_new), y = factor(ROI)))+
      geom_tile(color = data.long$bg.line, fill = data.long$bg.color)+
      geom_point(aes(size=factor(ap), 
                     fill=estimate),shape=21)+
      geom_text(aes(label = asterisk), size = 6) +
      scale_fill_gradientn(colours = myPalette(100), aesthetics = c("colour","fill"))+
      scale_size_manual(values=c(6, 4, 2), labels = c("< .01", "< .05", "\u2265 .05"))+
      labs(title=paste("Heatmap (", "Infection burden", " and ", modality.cat," metrics)", sep = ""),
           subtitle=paste("Stratified by ", tert_name, " tertiles", sep = ""),
           x=paste("Infection burden"," by ", tert_name, " strata", sep = ""),
           y="Brain ROI (47 tracts + global)",
           size = size.title,
           fill = fill.title,
           caption="solid circle: q < .05; *: q < .002")+
      theme(plot.title = element_text(color="Dark blue", size=15, face="bold.italic", hjust = 0.5),
            plot.subtitle=element_text(size=14, hjust=0.5, face="italic", color="Dark blue"),
            plot.caption=element_text(size=10, hjust=0.5, color="Dark grey"),
            axis.title.x = element_text(color="deepskyblue", size=13, face="bold"),
            axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
            aspect.ratio=7/4)+
      coord_fixed()
  }
  return(p.plot)
}


modality.list <- c("ISOVF", "ICVF", "OD")
for (modality in modality.list){
  # AD plots
  AD.hide.unsig <- heatmap(data = Dprime_overall, stratified_by = "AD_PGStert", modality.cat = modality, sig.basedon.p = FALSE, hide.unsig = TRUE)
  ggsave(paste(WorkingDirectory,"Output//Plot//", modality, "_ADtert_hide_unsig.jpeg",sep=""), AD.hide.unsig, width = 8.5, height = 11, units = "in", dpi = 300)

  # LE8 plots
  LE8.hide.unsig <- heatmap(data = Cprime_overall, stratified_by = "LE8_TOTALSCOREtert", modality.cat = modality, sig.basedon.p = FALSE, hide.unsig = TRUE)
  ggsave(paste(WorkingDirectory,"Output//Plot//", modality, "_LE8tert_hide_unsig.jpeg",sep=""), LE8.hide.unsig, width = 8.5, height = 11, units = "in", dpi = 300)
  
}
