circos.karyotype <- function(karyotype = x, CDR3 = "CDR3", Group = "Group", save = F, file.name = "karyotype.txt") 
{
  kariotype <- list()
  for (i in names(karyotype)) {
    s <- sum(karyotype[[i]]$Size)
    o <- grep(i, names(karyotype))
    kariotype[[o]] <- sprintf("chr - %s %s 0 %s %s", i, 
                              o, s, i)
  }
  bands <- list()
  bands <- lapply(karyotype, function(x, alt = 1, bs = c("stalk", 
                                                         "gpos50")) {
    s0 <- 0
    s1 <- s0
    for (f in 1:nrow(x)) {
      a <- as.character(x[f, ][[CDR3]])
      s1 <- s0 + x[f, ]$Size
      i <- unique(x[[Group]])
      bands[[f]] <- sprintf("band %s %s %s %s %s %s", 
                            i, a, a, s0, s1, bs[alt])
      if (alt == 1) {
        alt <- 2
      }
      else {
        alt <- 1
      }
      s0 <- s1
    }
    return(unlist(bands))
  })
  bands <- unlist(bands)
  names(bands) <- NULL
  kariotype <- unlist(kariotype)
  karyotype <- c(kariotype, bands)
  print(karyotype)
  if (save == TRUE) {
    zz <- file(file.name, "wb")
    writeBin(paste(karyotype, collapse = "\n"), zz)
    close(zz)
  }
}
