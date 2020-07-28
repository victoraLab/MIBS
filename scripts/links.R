links <- function (bands = "bands", CDR3 = "X3", 
          Group = "X2", coord1 = "X5", coord2 = "X6") 
{
  chr <- split(bands, f = bands[[Group]])
  combos <- t(combn(unique(bands[[Group]]), 2))
  apply(combos, 1, FUN = function(x) {
    for (i in chr[[x[1]]][[CDR3]]) {
      idx.chr2 <- match(i, chr[[x[2]]][[CDR3]])
      if (!is.na(idx.chr2)) {
        t <- sprintf("^%s$", i)
        idx.chr1 <- grep(t, chr[[x[1]]][[CDR3]])
        print(sprintf("%s %s %s %s %s %s",
                      chr[[x[1]]][idx.chr1,][[Group]],
                      chr[[x[1]]][idx.chr1, ][[coord1]],
                      chr[[x[1]]][idx.chr1,][[coord2]],
                      chr[[x[2]]][idx.chr2,][[Group]],
                      chr[[x[2]]][idx.chr2,][[coord1]], 
                      chr[[x[2]]][idx.chr2,][[coord2]]))
      }
    }
  })
}
