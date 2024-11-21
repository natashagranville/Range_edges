#### How does the tree-cover threshold used to define forest affect the proportion of forest cover


library(ggplot2)
library(tidyr)


fp10_600m <- read.csv('data/processed/Forest_cover_proportions_Hansen/Forest10_proportions_600m.csv')
fp20_600m <- read.csv('data/processed/Forest_cover_proportions_Hansen/Forest20_proportions_600m.csv')
fp30_600m <- read.csv('data/processed/Forest_cover_proportions_Hansen/Forest30_proportions_600m.csv')
fp40_600m <- read.csv('data/processed/Forest_cover_proportions_Hansen/Forest40_proportions_600m.csv')
fp50_600m <- read.csv('data/processed/Forest_cover_proportions_Hansen/Forest50_proportions_600m.csv')
fp60_600m <- read.csv('data/processed/Forest_cover_proportions_Hansen/Forest60_proportions_600m.csv')
fp70_600m <- read.csv('data/processed/Forest_cover_proportions_Hansen/Forest70_proportions_600m.csv')
fp80_600m <- read.csv('data/processed/Forest_cover_proportions_Hansen/Forest80_proportions_600m.csv')
fp90_600m <- read.csv('data/processed/Forest_cover_proportions_Hansen/Forest90_proportions_600m.csv')

fps <- as.data.frame(cbind(fp10_600m$fp10, fp20_600m$fp20, fp30_600m$fp30,
                           fp40_600m$fp40, fp50_600m$fp50, fp60_600m$fp60,
                           fp70_600m$fp70, fp80_600m$fp80, fp90_600m$fp90))

colnames(fps) <- c('fp10', 'fp20', 'fp30', 'fp40', 'fp50', 'fp60', 'fp70', 'fp80', 'fp90')


pivot <- pivot_longer(fps, cols = 1:9, values_to = 'fp')


forest_cover_violin_plot <- ggplot(pivot, aes(x = name, y = fp, fill = name, colour = name)) +
  geom_violin() +
  geom_boxplot(width=0.1, color="black") +
  ggtitle('') +
  scale_colour_manual(values = c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFF2AE", "#E5D8BD", "#FDDAEC","#CCCCCC")) +
  scale_fill_manual(values = c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFF2AE", "#E5D8BD", "#FDDAEC","#CCCCCC")) +
  theme_classic() +
  labs(x = 'Tree cover threshold', y = 'Forest cover proportion') +
  scale_x_discrete(labels = c('10%', '20%', '30%', '40%', '50%', '60%', '70%', '80%', '90%')) +
  theme(legend.position = 'none',
        axis.text = element_text(size = 17, colour = 'black'),
        axis.title = element_text(size = 17),
        plot.title = element_text(size = 20))

ggsave("figures/forest_cover_violin_plot.jpg")

