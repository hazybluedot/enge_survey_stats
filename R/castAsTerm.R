#' Return a subset of the data using item names for a particular term and year
#' items for all terms/years will be included but will adopt the name for the specificied term and year
#'
#' @param d the data list from loadYears
#' @param term string, the term, one of 'BOF', 'EOF', 'EOS'
#' @param AY the two digit academic year
#' @export
castAsTerm <- function(d, uterm, uAY, reversed = c(), text.fields = c()) {
    library(data.table)
    items <- subset(d$items, term == uterm & AY == uAY)[c("hash", "id", "qualtrics.id")]
    items <- unique(na.omit(items))
    df <- d$df[c("StudyID", "term", "AY", items$hash)]
    setnames(df, old=items$hash, new=items$id)

    # coerce data to correct type
    text.names <- which(!is.na(str_match(items$qualtrics.id, '_TEXT$')))
    
    int_idx <- unique(c(-1*text.names, -1*which(names(df) %in% c('StudyID', 'term', 'AY', 'A1', text.fields))))
    message('preserving indexes ', paste(int_idx, collapse=", "), '.')

    df[int_idx] <- data.frame(apply(df[,int_idx], 2, as.integer))
    
    if (length(reversed) > 0) {
       # min.score <- min(df[reversed], na.rm = TRUE)
       # max.score <- max(df[reversed], na.rm = TRUE)
       # message('Reverse coding using (', min.score + max.score, ' - score) items', paste(reversed, collapse = ', '))
       # df[reversed] <- min.score + max.score - df[reversed]
                                        # TODO: if we find min/max automatically we need to use all numeric items for a section
        # since it is likely that for reverse scored items no student will select the "strongly agree"
        df[reversed] <- 7 - df[reversed]
    }
    
    return(df)
}
