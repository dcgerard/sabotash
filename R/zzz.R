.onLoad <- function(libname, pkgname) {
    ## The evil part ------------------------------------------------------------
    suppressWarnings({
        utils::assignInNamespace("ash", value = bad_ash, ns = "ashr",
                                 envir = as.environment("package:ashr"))
        utils::assignInNamespace("ash.workhorse", value = bad_ash.workhorse, ns = "ashr",
                                 envir = as.environment("package:ashr"))
    })
}
