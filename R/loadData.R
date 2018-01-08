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

# unfortunately neither item numbers (e.g. A1,A2,I1,I2) or qualtrics numbers (e.g. Q4, Q1_12) 
# are guaranteed to refer to the same question from term to term and year to year

loadFile <- function(fname) {
    require('stringr')
    require('readxl')
    require('dplyr')
  raw <- read_excel(fname, col_types = "text")
  names(raw)[1:2] <- c("StudyID", "Consent")
  df <- within(as.data.frame(raw[-1,]), {
      StudyID <- as.character(StudyID)
      consent.levels <- list("Yes", "No")
      names(consent.levels) <- c(1,0)
      Consent <- do.call(function(...) recode_factor(Consent, ...), consent.levels)
      q4.levels <- as.list(majors)
      names(q4.levels) <- seq(length(majors))
      Q4 <- do.call(function(...) recode_factor(as.numeric(Q4), ...), q4.levels)
      rm(list=c('q4.levels', 'consent.levels'))
  })
  
  pattern <- "([A-Z][1-9][0-9]?[a-z]?)\\.(.*)$"
  names <- str_match(raw[1,], pattern) # first row contains question text. here we extract just the item number and text
  items <- data.frame(qualtrics.id = names(df), id = names[,2], text = str_trim(names[,3]), stringsAsFactors = FALSE)
  items$hash <- itemHash(items$text)
  # set names to the printed question numbers (contained in text of first row) rather than qualtrics numbers
  #names(df)[c(-(1:2), -(43:length(n)))] <- n[c(-(1:2), -(43:length(n)))]
  
  # find question text containing '...-' as these all have the same
  # item id and need to have the sub-squestion postfixed to make
  # unique ids
  post.fix <- str_match(raw[1,], '[A-Z][1-9][0-9]?[a-z]?\\.[^-\\.]+\\.{3}-(.*)$')[,2]
  items$id <- apply(cbind(items$id, post.fix), 1, function(li) {
    if (all(!is.na(li))) {
      return(paste(li, collapse='_'))
    } else {
      return(li[1])
    }
  })
  
  names(df)[!is.na(items$hash)] <- items$hash[!is.na(items$hash)]
  return(list(df=df, items=items))
}

loadYear_ <- function(year, base.dir) {
    require('stringr')
    dirName <- paste('20', year, '-', year + 1, sep = "")
    # TODO consider using list.files(dirname, '(BOS|EOS)_(1215|1216)_([FfSs]\\d{2})_.*\\.xlsx$')
    pattern <- '(BOS|EOS)_(1215|1216)_([FfSs]\\d{2})_.*\\.xlsx$'
    file.path <- paste(base.dir, dirName,sep="/")
    files <- list.files(file.path, pattern)
    if (length(files) == 0) {
        stop(file.path, ': No files matching /', pattern, '/ found. Is this the correct folder?')
    }
    parts <- as.data.frame(str_match(files, pattern))
    names(parts) <- c('full.name', 'term', 'course', 'semester')
    #postfix <- '_Consented_Hashed.xlsx'
    #files <- list(paste('BOS_1215_F', year, postfix, sep=""),
    #              paste('EOS_1215_F', year, postfix, sep=""),
    #              paste('EOS_1216_S', year + 1, postfix, sep=""))
    fnames <- lapply(files, function(f) paste(base.dir, dirName, f, sep = "/"))
    terms <- apply(parts, 1, function(row) paste0(str_to_upper(substr(row[2], 1, 2)), str_to_upper(substr(row[4], 1, 1))))
    d <- lapply(fnames, loadFile)
    message('parsed terms: [', paste(terms, collapse=','), ']')
    names(d) <- terms #c("BOS", "EOS", "EOY")
    for(n in terms) {
        message('setting term to ', n)
        d[[n]]$df$term <- n
        d[[n]]$items$term <- n
    }
                              
    df <- bind_rows(lapply(d, name_getter('df')))
    df$term <- as.factor(df$term)
    df$AY <- year

    items <- bind_rows(lapply(d, name_getter('items')))
    items$AY <- year
    message('name of items: ', paste(names(items), collapse = ', '))
    return(list(df=df, items=items))
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
  df <- bind_rows(lapply(years, name_getter('df')))
  items <- unique(bind_rows(lapply(years, name_getter('items'))))
  df$AY <- as.factor(df$AY)
  return(list(df=df, items=items))
}

