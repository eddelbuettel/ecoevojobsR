wikipedia <- readLines("colloquial_names_wiki.txt")

wikipedia <- wikipedia[grep('â€“', wikipedia)]

wikipedia2 <- data.frame(do.call(rbind, strsplit(wikipedia, split = ' â€“ ')))

wikipedia3 <- data.frame(as.character(wikipedia2$X1),
                         X2 = NA)
for (i in 1:nrow(wikipedia2)) {
  

wikipedia3 <- cbind(as.character(wikipedia2$X1),
                    do.call(rbind, strsplit(as.character(wikipedia2$X2), split = ",")))
