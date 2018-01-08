itemHash <- function(string) {
    str_replace_all(str_to_lower(string), '\\W+', '')
}

name_getter <- function(name) {
    return(function(thing) {
        thing[[name]]
    })
}
