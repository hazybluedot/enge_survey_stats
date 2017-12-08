# this function reshapes the data frame into a form that 
sankeyData <- function(d) {
  # 'cast' from the reshape library performs what Excel would call a pivot table.
  # I start with multiple rows for each study ID, one row for each 'term' (BOS, EOS, EOY) 
  # they took the survey, and cast with the formula StudyID ~ term
  # to get a data frame of one row per study ID where the 'terms' are made into columns.
  sd <- cast(d, StudyID ~ term, function(x) x[1], value = "major")
  for(t in levels(d$term)) {
    sd[[t]] <- factor(sd[[t]], labels = majors)
  }
  return(sd)
}

sankeyLinks <- function(sd) {
  # create links from BOS to EOS. The values should correspond to indexes in the nodes data frame
  # networkD3 uses 0 indexed arrays since all the work is done in JavaScript, which is 0-indexed,
  # so we subtract 1 from the factor level.
  sl1 <- data.frame(source = as.integer(sd[,2]) - 1,
                    target = as.integer(sd[,3]) + length(majors) - 1)
  # create links from EOS to EOY
  sl2 <- data.frame(source = as.integer(sd[,3]) + length(majors) - 1,
                    target = as.integer(sd[,4]) - 1 + length(majors)*2)
  sl <- rbind(sl1,sl2)
  sl <- aggregate(sl[c("source", "target")],
                  by = list(sl$source, sl$target), length)[c(1,2,3)]
  
  #sl1 <- aggregate(StudyID ~ BOS + EOS, data = sd, length)
  #sl1$BOS <- as.integer(sl1$BOS) - 1
  #sl1$EOS <- as.integer(sl1$EOS) + length(majors) - 1
  #names(sl1) <- c("source", "target", "value")
  #sl2 <- aggregate(StudyID ~ EOS + EOY, data = sd, length)
  #sl2$EOS <- as.integer(sl2$EOS) + length(majors) - 1
  #sl2$EOY <- as.integer(sl2$EOY) + 2*length(majors) -1
  #names(sl2) <- c("source", "target", "value")
  #sl <- rbind(sl1, sl2)
  names(sl) <- c("source", "target", "value")
  return(sl)
}

chordMat <- function(sd) {
  # take the reshaped data frame and generate an adjaceny matrix
  # which can be past to the chord diagram function
  nMajors <- length(majors)
  #chordLinks <- aggregate(sd, by = list(sd[,2], sd[,3]), length)[c(1,2,3)]
  chordLinks <- aggregate(StudyID ~ BOS + EOY, data = sd, length)
  chordLinks[,1] <- as.integer(chordLinks[,1])
  chordLinks[,2] <- as.integer(chordLinks[,2])
  el <- as.matrix(chordLinks)
  mat <- matrix(0, nMajors, nMajors)
  rownames(mat) <- majors
  colnames(mat) <- majors
  for(i in 1:nrow(chordLinks)) mat[ el[i,1], el[i,2] ] <- el[i,3]
  return(mat)
}

plotDiagrams <- function(d) {
  nodes <- do.call(rbind, lapply(terms, function(term) data.frame(name = majors, group = term)))
  sd <- sankeyData(d)
  sankey <- sankeyNetwork(Links = sankeyLinks(na.omit(sd)), Nodes = nodes, Source = "source",
                          Target = "target", Value = "value", NodeID = "name", fontSize = 12, nodeWidth = 30)
  saveNetwork(sankey, "sankey.html", selfcontained = TRUE)
  chord <- chordNetwork(Data = chordMat(sd), fontSize=8, labels = majors)
  saveNetwork(chord, "chord.html", selfcontained = TRUE)
  
  rownames(mat) <- majors.abbv
  colnames(mat) <- majors.abbv
  chordLinks <- aggregate(StudyID ~ BOS + EOY, data = sd, length)
  chordLinks$BOS <- factor(chordLinks$BOS, labels = majors.abbv)
  chordLinks$EOY <- factor(chordLinks$EOY, labels = majors.abbv)
  chordDiagram(chordLinks, directional = TRUE)
}
