---
title: "Dynamic Report"
author: "Korede Adegboye"
output: pdf_document
params:
  plots: !r
---


To learn more, see [Interactive Documents](http://rmarkdown.rstudio.com/authoring_shiny.html).

## Plots

```r
# The `params` object is available in the document.

plots <- function(i) {
  grid.draw(params$plots[[i]])
}
```

## Call plotting function


```

