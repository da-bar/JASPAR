#' JASPAR object class
#'
#' @description The JASPAR object class is a thin class for storing the
#' path of JASPAR-style SQLite file. The file is retrieved through
#' \code{AnnotationHub} and kept in its cache, where it is reused until the
#' provider publishes a newer file.
#' @details The AnnotationHub record is chosen by comparing \code{version} with
#' the titles of the JASPAR databases in the hub metadata (see
#' \code{\link{getAvailableJASPARVersions}}). The package contains no
#' accession numbers: a release that is registered in AnnotationHub, or whose
#' record has been corrected, can be retrieved without updating the package.
#'
#' If the file cannot be retrieved, the error names the version and the
#' AnnotationHub accession. With an online hub, a copy in the cache that is not
#' an SQLite file, or that is shorter than its header says, for example after an
#' interrupted transfer, is downloaded once more.
#'
#' Open the file read-only, for example with
#' \code{RSQLite::dbConnect(RSQLite::SQLite(), db(x), flags =
#' RSQLite::SQLITE_RO)}, so that the cached copy cannot be changed.
#' @aliases JASPAR
#' @slot db Object of class \code{"character"} a character string of the path
#' of SQLite file.
#' @slot version The version of JASPAR database which is loaded
#' @param version The version of JASPAR database which should be loaded, for
#' example \code{"JASPAR2024"} or \code{"JASPAR2022"}. See
#' \code{\link{getAvailableJASPARVersions}} for the versions in AnnotationHub.
#' Default is "JASPAR2024"
#' @param hub An \code{AnnotationHub} object. Default is
#' \code{AnnotationHub()}, which needs an internet connection. Its first call
#' downloads the hub database (about 140 MB) and later calls take a few seconds,
#' so create the object once if you retrieve several versions. To work
#' offline, use \code{AnnotationHub::AnnotationHub(localHub = TRUE)}: the hub
#' then contains only the databases that were downloaded before, and it cannot
#' download a new copy of a damaged file.
#' @param object JASPAR class object
#' @returns A \code{JASPAR} object. \code{db} gives the path of the SQLite file
#' and \code{version} the version of the database.
#' @author Damir Baranasic
#' @keywords classes
#' @examples
#'
#' library(JASPAR)
#' library(RSQLite)
#'
#' jaspar <- JASPAR(version = 'JASPAR2024')
#' jaspar
#' JASPARConnect <- RSQLite::dbConnect(RSQLite::SQLite(), db(jaspar),
#'                                     flags = RSQLite::SQLITE_RO)
#' RSQLite::dbGetQuery(JASPARConnect, 'SELECT * FROM MATRIX LIMIT 5')
#' RSQLite::dbDisconnect(JASPARConnect)
#'
#' @rdname JASPAR
#' @import methods
#' @importFrom AnnotationHub AnnotationHub cache isLocalHub
#' @exportClass JASPAR

setClass("JASPAR", slots = c(db = "character", version = "character")
         )

setMethod("initialize", "JASPAR",
          function(.Object, version = "JASPAR2024", hub = AnnotationHub()) {

            if (!is.character(version) || length(version) != 1L ||
                is.na(version)) {
              stop("'version' must be a single character string, ",
                   "for example \"JASPAR2024\".", call. = FALSE)
            }
            hub <- .getHub(hub)

            # choose the record by version from the hub metadata
            local <- isTRUE(isLocalHub(hub))
            id <- .selectJASPARRecord(.jasparRecords(hub), version, local)

            .Object@db <- .retrieveJASPAR(hub, id, version, local)
            .Object@version <- version
            return(.Object)
          })

#' @rdname JASPAR
#' @export

JASPAR <- function(version = "JASPAR2024", hub = AnnotationHub()) {
  new("JASPAR", version = version, hub = hub)
}

#' @rdname JASPAR
#' @export

setMethod("show", "JASPAR",
          function(object) {
            cat("class: JASPAR\n")
            cat("version: ", object@version, "\n", sep = "")
            cat("db: ", object@db, "\n", sep = "")
          })

#' @name db
#'
#' @title Access database from JASPAR object
#' @description The accessor function for retrieving the location of the
#' database location slot from the JASPAR object
#' @author Damir Baranasic
#' @param object JASPAR class object
#' @returns Returns the path of the SQLite file in the AnnotationHub cache
#' @keywords function
#' @examples
#'
#' library(JASPAR)
#' jaspar <- JASPAR(version = 'JASPAR2024')
#' db(jaspar)
#'
#' @import methods
#' @export

setGeneric("db", function(object)
  standardGeneric("db")
)

#' @rdname db

setMethod("db", "JASPAR",
          function(object){
            object@db
          })

#' @name version
#'
#' @title Access the database version from JASPAR object
#' @description The accessor function for retrieving the version of the
#' JASPAR database from the JASPAR object
#' @author Damir Baranasic
#' @param object JASPAR class object
#' @returns Returns the version of the JASPAR database in the JASPAR object
#' @keywords function
#' @examples
#'
#' library(JASPAR)
#' jaspar <- JASPAR(version = 'JASPAR2024')
#' version(jaspar)
#'
#' @import methods
#' @export

setGeneric("version", function(object)
  standardGeneric("version")
)

#' @rdname db

setMethod("version", "JASPAR",
          function(object){
            object@version
          })

#' Available JASPAR releases in AnnotationHub
#'
#' Queries AnnotationHub for the JASPAR databases that \code{\link{JASPAR}}
#' can retrieve and returns their versions, newest first. The versions come
#' from the hub metadata, so a newly registered release is listed without a
#' package update. A listed version can still fail to download if the location
#' registered in the hub cannot be reached.
#'
#' @param hub An \code{AnnotationHub} object. Default is
#' \code{AnnotationHub()}, which needs an internet connection. With
#' \code{AnnotationHub::AnnotationHub(localHub = TRUE)} only the databases that
#' were downloaded before are listed.
#'
#' @return A character vector of available versions, newest first
#' (e.g., \code{c("JASPAR2026", "JASPAR2024", "JASPAR2022")}).
#'
#' @examples
#' # List the JASPAR releases that are registered in AnnotationHub
#' vers <- getAvailableJASPARVersions()
#' vers
#'
#' @export

getAvailableJASPARVersions <- function(hub = AnnotationHub()) {
  hub <- .getHub(hub)
  .sortVersions(.jasparRecords(hub)$title)
}
