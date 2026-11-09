#' Plot clone.well size profile
#'
#' @import plotly
#' @export

plot_clone.wells <- function(db = db){

cells.wells <- table(db[["PLATE_ID"]])

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

p <- plot_ly(x = cells.wells, type = "histogram") %>% layout(xaxis = x, yaxis = y, title = "\nNumber of unique sequences per well (Clone.Wells)")

return(p)
}
