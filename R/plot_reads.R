#' Plot read profile per assemble sequence
#'
#' @import plotly
#' @export

plot_reads <- function(db = db){
  num <- log10(sort(db[["READ.COUNTS.SUM"]]))

f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)

x <- list(
  title = "Sequence number sorted by ascending",
  titlefont = f
)

y <- list(
  title = "log10 (READ.COUNTS.SUM)",
  titlefont = f
)

p <- plot_ly(x = ~1:nrow(db))
p <- p %>% add_lines(y = num, name = "vh")
p <- p %>% layout(xaxis = x, yaxis = y, title = '\nRead number per assembled sequence')

return(p)
}
