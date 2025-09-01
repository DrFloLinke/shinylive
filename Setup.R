library(shinylive)
library(httpuv)

setwd("~/Dropbox/shinylive")

export("~/Dropbox/shinylive/shiny_effect size", "docs/effect-size")

httpuv::runStaticServer("docs")





# App will be at:
# https://drfloreiche.github.io/shinylive/effect-size/

# Add more apps later
# Repeat the export to new subfolders:
#  shinylive::export("~/Dropbox/AQDR/shiny/another_app", "docs/another-app")