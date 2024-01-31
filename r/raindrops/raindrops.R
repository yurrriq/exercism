sounds <- c(Pling = 3, Plang = 5, Plong = 7)

raindrops <- function(number) {
    output <- paste0(names(sounds[number%%sounds == 0]), collapse = "")
    if (nchar(output) != 0) {
        output
    } else {
        as.character(number)
    }
}
