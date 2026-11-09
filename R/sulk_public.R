#' Sulk pipeline for public clonotypes
#'
#'
#' @param path Path to changeO results for each sample.
#' @param ouput Output folder path.
#' @param mrn An integer. Indicates the minimum read number per sequence.
#'
#' @importFrom readr read_delim
#' @import dplyr
#' @import openxlsx
#' @import plotly
#' @export
#'

sulk_public <- function(path = NULL, output = NULL, mrn = 6,  sample.ids = NULL){


db <- read_delim(path, delim = "\t")

#Sorting clones and groupping based on CDR3_V_J_well
#each unique sequence can only exist once within each well:

sulk.db <- db %>% group_by(JUNCTION_AA, VGENE, JGENE, PLATE_ID) %>% summarise(number = n())  %>% arrange(desc(number))

if(any(sulk.db[["number"]] > 1)){stop("Each clone can only exist once in each well.")}

#Create the public clonotype database (themes), with identical VJ
#and 4nt separation in JUNCTION.

db[["aa_clone"]] <- paste0(db[["JUNCTION_AA"]], "_", db[["CLONE"]])

db.themes <- db %>% group_by(MOUSE, CLONE) %>% summarise(Size = n(), JUNCTIONS = toString(JUNCTION_AA), VCALL = toString(V_CALL), VGENE = toString(VGENE), JGENE = toString(JGENE)) %>% arrange(desc(Size))


#Remove themes that are present in a single well
db.themes <- db.themes %>% filter (Size > 1)

db.themes[["CLONE_MOUSE"]] <- paste0(db.themes[["CLONE"]],"_", db.themes[["MOUSE"]])

db.themes.split <- split(db.themes, f = db.themes$MOUSE)

#The circos.karyotype function takes a dataframe list as input to
#build a fake karyotype-like file, following the circos format rules,
#to be used in the circos software. Each data frame, here all 19 mice
#used in this experiment, will become a chromossome-like entity.
#Morever, here we take advantage of the use of bands by circos,
#to describe chromossal bands, and use that to represent
#clonotypes (CDR3 = CLONE), of any size. A Size named column,
#with the size of each clonal entity, must be present.
#The circos.karyotype function should save a ready to use file for
#circos with an assembled karyotype-like design of your clonal data.
#The group parameter just points to the data frame split column.


write.xlsx(x = db.themes, file = paste0(output, "/", "db.themes.xlsx"))

}
