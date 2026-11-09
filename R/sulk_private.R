#' Sulk pipeline for private clonotypes
#'
#'
#' @param path Path to changeO results for each sample.
#' @param ouput Output folder path.
#' @param mrn An integer. Indicates the minimum read number per sequence.
#'s
#'
#' @importFrom data.table fread
#' @import dplyr
#' @import openxlsx
#' @import plotly
#' @export
#'


sulk_private <- function(path = NULL, output = NULL, mrn = 6,  sample.ids = NULL,
                         breaks = c(1, 2 , 3, 4, 7, 13, 33),
                         bar.leg.dist = 10,
                         tags = c("[1-2)", "[2-3)", "[3-4)","[4-7)","[7-13)", "[13-33)"),
                         bar.order = c("SPF", "GF", "MM12", "naive")){

  dir.create(output)
  files <- list.files(path)
  #all files are loaded here in a single list
  databases <- lapply(sprintf("%s%s", path , files), fread, header = T, sep = "\t")

  #adds sample IDs to each data.frame
  if(is.null(sample.ids)){
    names(databases) <- gsub("_clone.*", "" , files)
  } else{
    if( ncol(databases) != sample.ids){ stop(sprintf('sample.ids must be a character vector of length %s', ncol(databases)))}
    names(databases) <- sample.ids
  }


  #merge all tables into the same df
  databases <- bind_rows(databases)
  colnames(databases) <- toupper(colnames(databases))
  #create unique IDs for each condition-mouse-tissue sample
  databases <- databases %>% mutate(CLONE_ID = paste0(databases$BIO.ID, "_", databases$CLONE_ID))
  #Number of detected sequences before single-well collapsing
  numS0 <- nrow(databases)

  #Each of the 100-cell pool consists of a tiny bulk Rep-Seq. Here we collapse all
  #identical assemblies contained within a single well since we can't say much of
  #cell number using the number of reads. We also sum up the number of reads in each
  #sequence to obtain an unique sequence representative per well and remove all sequences
  #with 5 reads or less.

  db <- wellcol(databases = databases)
  db <- db %>% filter(READ.COUNTS.SUM >= mrn)

  #Number of detected sequences after single-well collapsing
  numS1 <- nrow(db)

  p1 <- plot_reads(db)

  orca(p1, paste0(output, "/", "p1.pdf"))

  bar_clone.wells(db = db, output = output, weight = F, tags = tags, breaks = breaks, bar.order = bar.order, leg.dist = bar.leg.dist)
  bar_clone.wells(db = db, output = output, weight = T, tags = tags, breaks = breaks, bar.order = bar.order, leg.dist = bar.leg.dist)

  p4 <- plot_clone.wells(db)
  orca(p4, paste0(output, "/", "p4.pdf"))

  p5 <- plot_clone.wells.split(db, split.by = "TISSUE")
  orca(p5, paste0(output, "/", "p5.pdf"))

  db[["VGENE"]] <- stringr::str_extract(string = db[["V_CALL"]], pattern = "(IGHV)(.*?)(?=\\*)")
  db[["JGENE"]] <- stringr::str_extract(string = db[["J_CALL"]], pattern = "(IGHJ)(.*?)(?=\\*)")
  db[["All"]] <- "All"

  write.xlsx(x = db, file = paste0(output, "/", "db.xlsx"))


  }







