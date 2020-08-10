library(ggplot2)

data = read.csv('~/git/cbcs/source/real_data.csv')
data$consumo_area = data$consumo/data$area

plot = ggplot(data, aes(area, consumo_area)) +
  geom_point(size = 0.2) +
  geom_smooth(colour = 'red', se = FALSE,  show.legend = TRUE) +
  labs(x = 'Área (m²)',
       y = 'Consumo por área (kWh/m².ano)',
       colour = 'Regressão\nlinear') +
  theme(legend.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        legend.position = 'right',
        axis.title.x = element_text(size = 21),
        axis.title.y = element_text(size = 21),
        axis.text.x = element_text(size = 19),
        axis.text.y = element_text(size = 19))
plot
