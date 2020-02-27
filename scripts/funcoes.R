# Automatizando a conversão de números para letras.
num2let <- function(x) {
    stopifnot(is.character(x))
    u <- strsplit(x = trimws(x), split = "")
    k <- max(as.integer(unlist(u)))
    sapply(u,
           function(v) {
               paste(letters[k:1][as.integer(rev(v))],
                     collapse = "")
           })
}
# num2let(c("1", "12", "123", "23", "3"))
