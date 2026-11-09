#' Plot clone.well size profile and split by a variable
#'
#' @import plotly
#' @export

plot_clone.wells.split <- function(db = db, split.by = "TISSUE", barmode = "overlay"){

  cells.wells <- table(db[["PLATE_ID"]], db[[split.by]])
  cells.wells <- as.data.frame.matrix(cells.wells)

  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )

  x <- list(
    title = "Sequence number",
    titlefont = f
  )
  y <- list(
    title = "Well number",
    titlefont = f
  )



  histplot <- plot_ly(alpha = 0.6) %>% layout(xaxis = x, yaxis = y)

  for(i in colnames(cells.wells)){
    histplot <- histplot %>% add_trace(type='histogram', x = cells.wells[[i]][cells.wells[[i]] > 0], name = i)
  }

  histplot <- histplot %>% layout(barmode = barmode, title = "Number of unique sequences per well (Clone.Wells)")

  return(histplot)
}
