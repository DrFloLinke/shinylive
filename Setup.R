library(shinylive)
library(httpuv)

setwd("~/Dropbox/shinylive")

export("~/Dropbox/shinylive/shiny_effect size", "docs/effect-size")

httpuv::runStaticServer("docs")
