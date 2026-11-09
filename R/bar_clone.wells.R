#' Plot the clone well profile
#'
#' @param db Database containing clonal unique IDs per sample (CLONE_ID)
#' @param breaks Clone.wells categories to split
#' @param tags Clone.wells category names
#' @param bar.order A character vector with CLONE.ID strings to order barplots
#' @param output output folder
#' @import dplyr
#' @importFrom scales hue_pal
#' @export

bar_clone.wells <- function(db = db,
                            breaks = c(1, 2 , 3, 4, 7, 13, 33),
                            tags = c("[1-2)", "[2-3)", "[3-4)","[4-7)","[7-13)", "[13-33)"),
                            bar.order = c("SPF", "GF", "MM12", "naive"),
                            leg.dist = 10,
                            output = output,
                            weight = F) {

  #Clone.Wells = cw
  #Count the number of clones spanning n wells (maximum is 32 since in our experimental
  #design no sample had more than 32 wells)
  cw <- table(db[["CLONE_ID"]])
  # put values into bins
  group_tags <- cut(cw,
                    breaks=breaks,
                    include.lowest=TRUE,
                    right=FALSE,
                    labels=tags)
  # inspect bins
  #summary(group_tags)

  #Extract group names for barplot
  names(group_tags) <- sub("^([^_]*_[^_]*_[^_]*).*", "\\1", names(cw))

  #Table each clone present in n number of wells
  bar.count <- table(names(group_tags), group_tags)
  bar.count <- as.matrix(as.data.frame.matrix(bar.count))

  #Normalize each value it to 1
  bar.count <- bar.count/rowSums(bar.count)

  #Specify bargraph order
  order <- c()
  for(i in bar.order){
    x <- c(grep(i, rownames(bar.count)))
    order <- c(order, x)
  }

  ordered <- rownames(bar.count)[order]
  #Create colors for bars

  colors <- hue_pal()(length(colnames(bar.count)))

  #Change order
  y <- t(bar.count[ordered,])
  # Increase margin size
  #Plot bar

  if(weight == F){
  pdf(file = paste0(output, "/", "p2.pdf"), width = 8.72, height = 6.65)
  par(mar=c(5,5,1,5)); barplot(y*100,
                               ylab = 'Percentage of sequences in n wells',
                               main = "unweighted barplot",
                               cex.names=.4,
                               col = colors,
                               horiz = F,
                               las=2,
                               legend.text=TRUE,
                               args.legend=list(
                                 x=ncol(y) + leg.dist,
                                 y=100,
                                 bty = "n"
                               ))
  dev.off()
  }

  if(weight == T){

    weights <- data.frame(cw=as.numeric(cw), group_tags=group_tags,names = names(group_tags)) %>% group_by(names, group_tags) %>% summarise(mean = mean(cw))

    weights <- xtabs(mean ~ names + group_tags, weights)
    #Table each clone present in n number of wells
    bar.count <- table(names(group_tags), group_tags)
    bar.count <- as.matrix(as.data.frame.matrix(bar.count))

    bar.count.w <- bar.count * weights
    bar.count.w <- as.matrix(as.data.frame.matrix(bar.count.w))

    #Normalize each value it to 1
    bar.count.w <- bar.count.w/rowSums(bar.count.w)

    #Specify bargraph order
    order <- c()
    for(i in bar.order){
      x <- c(grep(i, rownames(bar.count.w)))
      order <- c(order, x)
    }

    ordered <- rownames(bar.count.w)[order]
    #Create colors for bars

    colors <- hue_pal()(length(colnames(bar.count.w)))

    #Change order
    y <- t(bar.count.w[ordered,])
    # Increase margin size
    #Plot bar

    pdf(file = paste0(output, "/", "p3.pdf"), width = 8.72, height = 6.65)
    par(mar=c(5,5,1,5)); barplot(y*100,
                                 ylab = 'Percentage of sequences in n wells',
                                 main = "weighted barplot",
                                 cex.names=.4,
                                 col = colors,
                                 horiz = F,
                                 las=2,
                                 legend.text=TRUE,
                                 args.legend=list(
                                   x=ncol(y) + leg.dist,
                                   y=100,
                                   bty = "n"
                                 ))
    dev.off()
  }

  }
