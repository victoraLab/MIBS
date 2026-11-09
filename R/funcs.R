#'
#' @importFrom dplyr left_join
#' @export


mf <- function(x){
  y <- x[!duplicated(x[["CLONE_ID"]]), ]
  read.c <- aggregate(x[["READ.COUNTS"]], by=list(CLONE_ID=x[["CLONE_ID"]]), FUN=sum)
  y <- left_join(y, read.c, by = c("CLONE_ID")) %>% rename(READ.COUNTS.SUM = x)
  #idx <- match(y[["CLONE_ID"]], read.c[["CLONE_ID"]])
  #y[["READ.COUNTS.SUM"]] <- read.c[idx,][["x"]]
  return(y)
}

#'
#' @export

wellcol <- function(databases=databases){
  clone.temp <- split(databases, f = databases[["PLATE_ID"]])
  clone.temp2 <- lapply(clone.temp, FUN = mf)
  db <- bind_rows(clone.temp2)
  return(db)
}

#'
#' @export
#'

group.dist <- function(x, summarise.by = "count", split.by = NULL, pattern = input$pattern){
  dist <- stringdist(x[["JUNCTION_AA"]], pattern, method = "lv")
  if (split.by == "All" && summarise.by == "count") {
    dist <- data.frame(lv = dist)
    hist <- dist %>% dplyr::group_by(lv) %>% summarise(n = dplyr::n())
    colnames(hist) <- c("lv", "counts")
    hist$ratio <- hist$counts/sum(hist$counts)
    hist <- xdist(hist)
    return(list(All = hist))
  }

  if (length(split.by) != 0 && summarise.by == "count") {
    dist <- data.frame(lv = dist)
    split.dist <- split(dist, split.by)
    if (summarise.by == "count") {
      summa <- function(x) {
        x <- x %>% dplyr::group_by(lv) %>% summarise(dplyr::n())
        colnames(x) <- c("lv", "counts")
        x[["ratio"]] <- x[["counts"]]/sum(x[["counts"]])
        x <- xdist(x)
      }
      hist <- lapply(split.dist, FUN = summa)
      return(hist)
    }
  }
}

#'
#' @export


xdist <- function (x, s = seq(0, 20)){
  idx <- s %in% x[["lv"]]
  f <- data.frame(lv = s[!idx], ratio = 0, counts = 0)
  x <- rbind(x, f)
  return(x[order(x[["lv"]]), ])
}

#' @export
#'
#'

js <- c(
  "function(el, x){",
  "  var d3 = Plotly.d3;",
  "  el.on('plotly_restyle', function(evtData) {",
  "    var out = {};",
  "    d3.select('g.legend').selectAll('.traces').each(function(){",
  "      var trace = d3.select(this)[0][0].__data__[0].trace;",
  "      out[trace.name] = trace.visible;",
  "    });",
  "    Shiny.setInputValue('traces', out);",
  "  });",
  "}")


#' @export
#'

lv.up <- function(x = x, min = min, max = max){
  length(unique(x)) > min && length(unique(x)) < max

  }

#'
#' @importFrom  Biostrings toString
#' @export


alignment2Fasta <- function(alignment, filename) {
  sink(filename)

  n <- length(rownames(alignment))
  for(i in seq(1, n)) {
    cat(paste0('>', rownames(alignment)[i]))
    cat('\n')
    the.sequence <- toString(unmasked(alignment)[[i]])
    cat(the.sequence)
    cat('\n')
  }

  sink(NULL)
}



#' This function returns a DNA match by using an
#'
#' This function extracts a DNA match from the AA sequence
#'
#'@importFrom  stringr str_extract
#'
#' @export

matchDNAwithAA <- function(dnaQuery, aaSubject){

  M = "ATG"
  W = "TGG"
  G = "GG."
  A = "GC."
  P = "CC."
  V = "GT."
  T = "AC."
  L = "[TC]T."
  R = "[CA]G."
  F = "TT[TC]"
  K = "AA[AG]"
  Q = "CA[AG]"
  E = "GA[AG]"
  C = "TG[TC]"
  Y = "TA[TC]"
  H = "CA[TC]"
  N = "AA[TC]"
  D = "GA[TC]"
  I = "AT[ATC]"
  S = "(TC.|AG[TC])"


  x <- strsplit(x = aaSubject, split = "")
  c <- c()
  for (i in x[[1]]){
    t <- get(i)
    c <- c(c,t)
  }

 dna <- str_extract(string = dnaQuery, pattern = paste(c, sep="",collapse=""))


  return(dna)

}



