.onLoad <- function(libname, pkgname) {
    ## The evil part ------------------------------------------------------------
    suppressWarnings({
        require(ashr)
        unlockBinding("ash", as.environment("package:ashr"))
        utils::assignInNamespace("ash", value = bad_ash, ns = "ashr",
                                 envir = as.environment("package:ashr"))
        assign("ash", bad_ash, as.environment("package:ashr"))
        lockBinding("ash", as.environment("package:ashr"))

        unlockBinding("ash.workhorse", as.environment("package:ashr"))
        utils::assignInNamespace("ash.workhorse", value = bad_ash.workhorse, ns = "ashr",
                                 envir = as.environment("package:ashr"))
        assign("ash.workhorse", bad_ash.workhorse, as.environment("package:ashr"))
        lockBinding("ash.workhorse", as.environment("package:ashr"))
    })
}
