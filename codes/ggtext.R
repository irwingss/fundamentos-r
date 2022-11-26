library(ggtext) #remotes::install_github("wilkelab/ggtext")

ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point(size = 3) +
  scale_color_manual(
    name = NULL,
    values = c(setosa = "#0072B2", virginica = "#009E73", versicolor = "#D55E00"),
    labels = c(
      setosa = "<i style='color:#0072B2'>I. setosa</i>",
      virginica = "<i style='color:#009E73'>I. virginica</i>",
      versicolor = "<i style='color:#D55E00'>I. versicolor</i>")
  ) +
  labs(
    title = "**Fisher's *Iris* dataset**  
    <span style='font-size:11pt'>Sepal width vs. sepal length for 
    <span style='color:#0072B2;'>setosa</span>, 
    <span style='color:#D55E00;'>versicolor</span>, and
    <span style='color:#009E73;'>virginica</span>
    </span>",
    x = "Sepal length (cm)", y = "Sepal width (cm)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_markdown(lineheight = 1.1),
    legend.text = element_markdown(size = 11)
  )
