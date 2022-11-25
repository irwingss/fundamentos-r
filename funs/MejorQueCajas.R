library(tidyverse)

promedio <- iris %>%
  summarise_at(.vars=vars(Petal.Length),
               .funs=lst(mean)) %>%
  pull(mean)

segmentos <- iris %>%
  group_by(Species) %>%
  summarise_at(.vars=vars(Petal.Length),
               .funs=lst(mean))

p <- iris %>%
  ggplot(aes(y=Petal.Length, x=Species, color=Species))+
  geom_jitter(width = 0.3, height = 0, size=3, alpha=0.1)+
  coord_flip()+
  ggstatsplot::theme_ggstatsplot()+
  stat_summary(fun=mean, geom="point", size=5)+
  geom_hline(yintercept = promedio, size=1.1)+
  geom_segment(data=segmentos,
               aes(x=Species, y=promedio,
                   xend=Species, yend=mean),
               lwd=1)+
  annotate(geom="text", x = 3.1, y = 3,
           family = "Poppins", size = 2.8,
           color = "gray20", lineheight = .9,
           label = glue::glue("Promedio \ngeneral: {round(promedio, 1)} cm")
  )+
  annotate(geom="text", x = 2.3, y = 6.1,
           family = "Poppins", size = 2.8,
           color = "gray20", lineheight = .9,
           label = glue::glue("El valor más alto \nes para la especie")
  ) +
  annotate(geom="text", x = 2.1, y = 6.1,
           family = "Poppins", size = 2.8,
           color = "gray20", lineheight = .9,
           label = substitute(paste(italic("virginica")))
  )+
  geom_curve(aes(y = 3.1, x = 2.95, yend = promedio-0.1, xend = 2.5),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
             color = "gray20", curvature = 0.3
  )+
  geom_curve(aes(y = 6.7, x = 2.4, yend = 6.9, xend = 2.8),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
             color = "gray20", curvature = 0.3
  )+
  scale_color_manual(values = c("#e31e39","#049abf","#9639a1"))+
  scale_y_continuous(breaks = 0:7)+
  labs(x="Especies", y="Longitud de pétalo (cm)")+
  theme(legend.position = "none",
        axis.text.y = element_text(face=3),
        axis.title = element_text(face=2, hjust = 0.5))
print(p)
# ggsave(filename = "mejorqueboxplot.png",
#       plot = p, width = 15,height = 10,dpi = 600,units = "cm")

