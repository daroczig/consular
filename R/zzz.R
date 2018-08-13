consular <- new.env()

.onLoad <- function(libname, pkgname) {
    consular$agent_url <- 'http://localhost:8500'
    consular$version <- 'v1'
}
