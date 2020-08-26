# set global environment ####
pkgs = c('mgcv', 'moments', 'ggplot2')
lapply(pkgs, library, character.only = TRUE)

# relation area versus eui ####
df = read.csv('~/git/cbcs/source/real_data.csv')
df$consumo_area = df$consumo/df$area

gam = gam(consumo_area ~ area, data = df)
rsquared = 1 - sum(gam$residuals^2)/sum((gam$y - mean(gam$y))^2)
rsquared = round(rsquared, 4)

png('./plot_table/area_eui.png', width = 33.8, height = 19, units = 'cm', res = 500)
plot = ggplot(df, aes(area, consumo_area)) +
  geom_point(size = 0.2) +
  geom_smooth(method = 'gam', se = FALSE,
              aes(colour = 'Generalized\nAdditive\nModel')) +
  geom_text(x = 2500, y = 750, label = paste('R² =', rsquared), size = 7) +
  labs(x = 'Area (m²)', y = 'EUI (kWh/m².year)\n') +
  scale_color_manual(name = 'Legend:', values = c('Generalized\nAdditive\nModel' = 'red')) +
  theme(legend.title = element_text(size = 21, hjust = 0.5),
        legend.text = element_text(size = 19),
        legend.position = 'right',
        axis.title.x = element_text(size = 21),
        axis.title.y = element_text(size = 21),
        axis.text.x = element_text(size = 19),
        axis.text.y = element_text(size = 19))
plot(plot)
dev.off()

# eui distribution ####
sample = read.csv('./result/sample.csv')

kurt = round(kurtosis(sample$targ), 3)
skew = round(skewness(sample$targ), 3)

png('./plot_table/targ_dist.png', width = 33.8, height = 19, units = 'cm', res = 500)
plot = ggplot(sample) +
  geom_histogram(aes(x = targ), colour = 'black', fill = 'white') +
  geom_text(aes(x = 250, y = 820), label = paste('Skewness =', skew), size = 7) +
  geom_text(aes(x = 250, y = 720), label = paste('Kurtosis =', kurt), size = 7) +
  labs(x = 'EUI (kWh/m².year)', y = 'Frequency Count') +
  theme(legend.title = element_text(size = 21, hjust = 0.5),
        legend.text = element_text(size = 19),
        legend.position = 'right',
        axis.title.x = element_text(size = 21),
        axis.title.y = element_text(size = 21),
        axis.text.x = element_text(size = 19),
        axis.text.y = element_text(size = 19))
plot(plot)
dev.off()
