library(knitr)
library(pander)


opts_chunk$set(tidy.opts = list(keep.blank.lines = TRUE),
               results='asis',
               warning=FALSE,
               message=FALSE,
               strip.white=FALSE,
               echo=FALSE,
               dpi=600)

panderOptions('table.split.table', Inf)
panderOptions('table.caption.prefix', "Tabla: ")
panderOptions('missing', "")

ggplot2::theme_set(ggplot2::theme_minimal(base_size = 8))