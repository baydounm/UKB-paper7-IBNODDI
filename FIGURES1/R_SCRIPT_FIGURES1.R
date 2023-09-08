# Basically, we need to plot the estimate against the -Log10 p-value and
# add the parm label for p<type I error after multiple testing correction
# (0.05/76), and change colors based on how strong the effects (these are
# Ln odds ratios). The equivalent of an OR of 2 is 0.69 and for an odds
# ratio of 0.5 is -0.69, at which point the color should be changed. I am
# attaching the paper as it stands and the excel datasheet showing the
# prevalence proportion of each infection within each grouped source.

library(ggplot2)
library(ggrepel)
library(readstata13)

datObj = read.dta13('/zdsk/Manuscripts/Baydoun/UK-BioBank/2023-01-06--04--BrainVol-CVD-Inf/2023-01-06/Outputdata_overall_F_INFECTIONPARMS.dta')

datObj$EffSize	= factor((datObj$estimate > -0.69) + (datObj$estimate > 0.69), 0:2, c('LnOR < -0.69', '0.69 ≤ LnOR ≤ 0.69', 'LnOR > 0.69'))

datObj$label	= sub('_0_0_yn', '', datObj$parm)
datObj$label	= sub('ts_',     '', datObj$label)

Volcano = function() ggplot(data=datObj, aes(x=estimate, y=-log10(p), label=label, col=EffSize)) +
	geom_point(shape=20) +
	geom_text_repel(size=3) +
	labs(x='Ln(Odds ratio)') +
	labs(col='Effect size') +
	theme_minimal() +
	theme(legend.position = c(0.5, 0.8)) +
	scale_color_manual(values=c("darkgreen", "black", "darkorange")) +
        geom_hline(yintercept=-log10(0.05/76), col="gray", linetype=2)

pdf('/zdsk/Manuscripts/Baydoun/UK-BioBank/2023-01-06--04--BrainVol-CVD-Inf/2023-01-06/2023-02-12.pdf')
Volcano()
jnk = dev.off()

png('/zdsk/Manuscripts/Baydoun/UK-BioBank/2023-01-06--04--BrainVol-CVD-Inf/2023-01-06/2023-02-12.png')
Volcano()
jnk = dev.off()
