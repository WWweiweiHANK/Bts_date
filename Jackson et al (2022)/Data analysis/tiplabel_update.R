# Tiplabels function with and aligntips option
tiplabels <- function (text, tip, adj = c(0.5, 0.5), frame = "rect", pch = NULL, 
          thermo = NULL, pie = NULL, piecol = NULL, col = "black", 
          bg = "yellow", horiz = FALSE, width = NULL, height = NULL, 
          offset = 0, aligntips = F, ...) 
{
  lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  if (missing(tip)) 
    tip <- 1:lastPP$Ntip
  XX <- lastPP$xx[tip]
  if (aligntips)
    XX[tip] <- max(lastPP$xx[tip])
  YY <- lastPP$yy[tip]
  if (offset != 0) {
    if (lastPP$type %in% c("phylogram", "cladogram")) {
      switch(lastPP$direction, rightwards = {
        XX <- XX + offset
      }, leftwards = {
        XX <- XX - offset
      }, upwards = {
        YY <- YY + offset
      }, downwards = {
        YY <- YY - offset
      })
    }
    else {
      if (lastPP$type %in% c("fan", "radial")) {
        tmp <- rect2polar(XX, YY)
        if (lastPP$align.tip.label) 
          tmp$r[] <- max(tmp$r)
        tmp <- polar2rect(tmp$r + offset, tmp$angle)
        XX <- tmp$x
        YY <- tmp$y
      }
      else {
        if (lastPP$type == "unrooted") 
          warning("argument 'offset' ignored with unrooted trees")
      }
    }
  }
  BOTHlabels(text, tip, XX, YY, adj, frame, pch, thermo, pie, 
             piecol, col, bg, horiz, width, height, ...)
}
