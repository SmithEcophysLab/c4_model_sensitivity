# c4_model_sensitivity.R
## code to explore sensitiviety of the von Caemmerer model of C4 photosynthesis to parameter choices

## load libraries
library(ggplot2)
library(plantecophys)

## create parameter distributions to sample from
vcmax_plantecophys <- 60
vpmax_plantecophys <- 120
jmax_plantecophys <- 400

vcmax_vals <- rnorm(1000, vcmax_plantecophys, vcmax_plantecophys*0.5)
vpmax_vals <- rnorm(1000, vpmax_plantecophys, vpmax_plantecophys*0.5)
jmax_vals <- rnorm(1000, jmax_plantecophys, jmax_plantecophys*0.5)

hist(vcmax_vals)
hist(vpmax_vals)
hist(jmax_vals)

## run model
AciC4(Ci=100) # test

LLLC_df <- data.frame()
for(i in 1:length(vcmax_vals)){
  
  photosynthesis_traits <- AciC4(Ci = 0.4*150, PPFD = 400,
                                 Vcmax = vcmax_vals[i],
                                 JMAX25 = jmax_vals[i],
                                 VPMAX25 = vpmax_vals[i])
  
  LLLC_df <- rbind(LLLC_df, photosynthesis_traits)
  
}

HLLC_df <- data.frame()
for(i in 1:length(vcmax_vals)){
  
  photosynthesis_traits <- AciC4(Ci = 0.4*150, PPFD = 1500,
                                 Vcmax = vcmax_vals[i],
                                 JMAX25 = jmax_vals[i],
                                 VPMAX25 = vpmax_vals[i])
  
  HLLC_df <- rbind(HLLC_df, photosynthesis_traits)
  
}

LLHC_df <- data.frame()
for(i in 1:length(vcmax_vals)){
  
  photosynthesis_traits <- AciC4(Ci = 0.4*1000, PPFD = 400,
                                 Vcmax = vcmax_vals[i],
                                 JMAX25 = jmax_vals[i],
                                 VPMAX25 = vpmax_vals[i])
  
  LLHC_df <- rbind(LLHC_df, photosynthesis_traits)
  
}

HLHC_df <- data.frame()
for(i in 1:length(vcmax_vals)){
  
  photosynthesis_traits <- AciC4(Ci = 0.4*1000, PPFD = 1500,
                                 Vcmax = vcmax_vals[i],
                                 JMAX25 = jmax_vals[i],
                                 VPMAX25 = vpmax_vals[i])
  
  HLHC_df <- rbind(HLHC_df, photosynthesis_traits)
  
}

LLLC_df$type <- 'LLLC'
HLLC_df$type <- 'HLLC'
LLHC_df$type <- 'LLHC'
HLHC_df$type <- 'HLHC'

vc_sensitivity_df <- rbind(LLLC_df, HLLC_df, LLHC_df, HLHC_df)

#### make figure
vc_sensitivity_plot <- ggplot(aes(y = An, x = type, fill = type), data = subset(vc_sensitivity_df, An > 0)) +
  theme_minimal(base_size = 14) +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        axis.line = element_line(color = "black", linewidth = 0.6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_violin() +
  geom_boxplot(width = 0.05, fill = 'white', outlier.colour = NA) +
  stat_summary(fun.y=median, geom="point", size=2, color="black") +
  ylab(expression('A'[net] * ' (µmol m' ^ '-2' * ' s' ^ '-1' * ')')) +
  xlab('') +
  scale_fill_manual(values = c('darkgreen', 'yellow', 'blue', 'grey')) +
  ylim(c(0, 80)) +
  guides(fill = FALSE)
vc_sensitivity_plot

## write out plot
jpeg(filename = "plots/vc_sensitivity_plot.jpeg", width = 8, height = 6, units = 'in', res = 600)
plot(vc_sensitivity_plot)
dev.off()
