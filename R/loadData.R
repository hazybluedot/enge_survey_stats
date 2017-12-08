majors <- c("Aerospace Engineering",
            "Biological Systems Engineering",
            "Chemical Engineering",
            "Civil and Environmental Engineering",
            "Computer Engineering",
            "Computer Science",
            "Construction and Engineering Management",
            "Electrical Engineering",
            "Engineering Science and Mechanics",
            "Industrial and Systems Engineering",
            "Materials Science and Engineering",
            "Mechanical Engineering",
            "Mining and Minerals Engineering",
            "Ocean Engineering",
            "Undecided")

# Survey questions H8 and H9 are likert scale related to MATLAB

majors.abbv <- c("AOE", "BSE", "ChE", "CEE", "CpE", "CS", "CEM", "EE", "ESM", "ISE", "MSE", "ME", "MME", "OE", "Und")

terms <- list("BOS", "EOS", "EOY") # Beginning of Year, Middle of Year, End of Year 

# unfortunately neither item numbers (e.g. A1,A2,I1,I2) or qualtrics numbers (e.g. Q4, Q1_12) 
# are guaranteed to refer to the same question from term to term and year to year

loadFile <- function(fname, q4labels = majors, char.items = c('A1', 'F4', 'I1', 'I2')) {
  raw <- read_excel(fname)
  names(raw)[1:2] <- c("StudyID", "Consent")
  pattern <- "([A-Z][1-9][0-9]?[a-z]?)\\."
  n <- str_match(raw[1,], pattern)[,2] # first row contains question text. here we extract just the question number
  df <- within(as.data.frame(raw[-1,]), {
    Consent <- as.factor(mapvalues(Consent, from = 1:0, to=c("Yes", "No"), warn_missing = FALSE))
    Q4 <- as.factor(mapvalues(as.numeric(Q4), from = seq(length(q4labels)), to = q4labels ))
  })
  
  # set names to the printed question numbers (contained in text of first row) rather than qualtrics numbers
  #names(df)[c(-(1:2), -(43:length(n)))] <- n[c(-(1:2), -(43:length(n)))]
  
  # this is a brittle hack, if question order changes from year to year we will have problems
  qnames <- names(df) # Qualtrics names
  post.fix <- str_match(raw[1,], '[A-Z][1-9][0-9]?[a-z]?\\.[^-\\.]+\\.{3}-(.*)$')[,2]
  n <- apply(cbind(n, post.fix), 1, function(li) {
    if (all(!is.na(li))) {
      return(paste(li, collapse='_'))
    } else {
      return(li[1])
    }
  })
  
  #print(name_map)
  text.names <- which(!is.na(str_match(qnames, '_TEXT$')))
  int_idx <- unique(c(-1, -2, -1*text.names, -1*which(n %in% char.items)))
  print(paste('preserving indexes', paste(int_idx, collapse=", "), 'as characters', sep= " "))
  df[int_idx] <- data.frame(apply(df[int_idx], 2, as.integer))
  
  n[text.names] <- sapply(n[text.names], function(name) paste(name, '_TEXT', sep=''))
  name_map <- data.frame(qname=names(df), iname=n)

  names(df)[names(df) == 'Q4'] <- 'major'
  n[3] <- NA
  #names(df)[!is.na(n)] <- na.omit(n)
  df[1] <- sapply(df[1], as.character)
  #names(df)[1] <- "StudyID"
  #print(paste("names of df[1]", paste(names(df)[1:3], collapse = " "), sep =": "))
  return(list(df=df, name_map=name_map))
}

loadYear_ <- function(year, base.dir) {
  dirName <- paste('20', year, '-', year + 1, sep = "")
  postfix <- '_Consented_Hashed.xlsx'
  files <- list(paste('BOS_1215_F', year, postfix, sep=""),
                paste('EOS_1215_F', year, postfix, sep=""),
                paste('EOS_1216_S', year + 1, postfix, sep=""))
  fnames <- lapply(files, function(f) paste(base.dir, dirName, f, sep = "/"))
  d <- lapply(fnames, loadFile)
  names(d) <- terms #c("BOS", "EOS", "EOY")
  for(n in names(d)) {
    d[[n]]$df$term <- n
    #with(d[[n]], print(paste("StudyID:", typeof(df$StudyID), paste(head(df$StudyID), collapse =" "), sep = " ")))
  }
  
  df <- bind_rows(lapply(d, function(li) li$df))
  df$term <- as.factor(df$term)
  df$AY <- year
  return(df)
}

#' load ENGE Survey data for the specified 2 digit years
#' @param years integer vector
#' @param base.dir string, defaults to '.'
#' @export
#' @examples 
#' df <- loadYears(14:16)
#' df <- loadYears(14:16, base.dir = '~/ENGE/data')
loadYears <- function(years, base.dir = '.') {
  years <- lapply(years, function(year) loadYear_(year, base.dir = base.dir))
  df <- bind_rows(years)
  df$AY <- as.factor(df$AY)
  return(df)
}

