library(tidyverse)
library(ggthemes)
View(canva_pal)

scale_fill_masterx<- function (..., palette = "pairs14"){
  discrete_scale("fill", "masterx", masterx_pal(palette), ...)
}

scale_color_masterx<- function (..., palette = "pairs14"){
  discrete_scale("color", "masterx", masterx_pal(palette), ...)
}

masterx_pal<- function (palette = "pairs14") {
  if (!palette %in% names(masterx_palettes)) {
    stop("Palette ", sQuote(palette), " not a valid name.",
         call. = FALSE)
  }
  scales::manual_pal(unname(masterx_palettes[[palette]]))
}

masterx_palettes <- lst(
  pairs14 = c("#c82135","#ff8a8a",
                "#1384b8", "#64cae9",
                "#08b742", "#94df6f",
                "#db6709", "#fdb626",
                "#400ec0", "#c0a9fb",
                "#a8466f", "#ed7cc6",
                "#352c2d","#526665"),
  pal22 = c("#e31e39","#049abf","#9639a1","#fdb626",
            "#0dc56d","#afda4d","#9ad5bf","#e56244",
            "#a04c41","#04c9c2","#4ebcdd","#3b506f",
            "#ff69bc","#a8466f","#352c2d","#8b7675","#ed8f49",
            "#c82135","#d92339","#f73444","#394b4b","#526665"),
  pal22_ordered = c("#3b506f","#049abf", "#4ebcdd","#04c9c2",
                    "#0dc56d","#afda4d","#9ad5bf","#9639a1",
                    "#a8466f","#ff69bc","#c82135","#d92339",
                    "#e31e39","#f73444","#e56244","#ed8f49",
                    "#fdb626","#a04c41","#8b7675","#352c2d",
                    "#394b4b","#526665")
  )
