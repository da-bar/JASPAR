#' JASPAR object class
#'
#' @description The JASPAR object class is a thin class for storing the
#' path of JASPAR-style SQLite file.
#' @aliases JASPAR
#' @slot db Object of class \code{"character"} a character string of the path
#' of SQLite file.
#' @slot version The version of JASPAR database which is loaded
#' @param version The version of JASPAR database which should be loaded. 
#' Default is "JASPAR2026"
#' @returns JASPAR-class
#' @author Damir Baranasic
#' @keywords classes
#' @examples
#'
#' library(JASPAR)
#' library(RSQLite)
#'
#' JASPAR <- JASPAR(version = 'JASPAR2026')
#' JASPARConnect <- RSQLite::dbConnect(RSQLite::SQLite(), db(JASPAR))
#' RSQLite::dbGetQuery(JASPARConnect, 'SELECT * FROM MATRIX LIMIT 5')
#' dbDisconnect(JASPARConnect)
#'
#' @rdname JASPAR
#' @import methods
#' @importFrom utils read.csv
#' @import BiocFileCache
#' @exportClass JASPAR

setClass("JASPAR", slots = c(db = "character", version = "character")
         )

setMethod("initialize", "JASPAR",
          function(.Object, package = "JASPAR", version = "JASPAR2024") {

            metaData <- system.file("extdata", "metadata.csv", package=package)
            if (!file.exists(metaData)) {
              stop("metadata.csv not found in the specified package.")
            }

            metaDataDF <- read.csv(metaData, stringsAsFactors = FALSE)

            # sanity checks
            reqCols <- c("Title", "SourceUrl")
            missingCols <- setdiff(reqCols, colnames(metaDataDF))
            if (length(missingCols)) {
               stop("Required column(s) missing in metadata.csv: ",
                    paste(missingCols, collapse = ", "))
            }

            all_versions <- unique(metaDataDF$Title)
            
            # validate requested version against the one copy of metadata we 
            # already read
            if (!version %in% all_versions) {
              stop(
                "Requested JASPAR version '", version, "' is ", 
                "not available in this package. ",
                "Call getAvailableJASPARVersions() to see available versions."
              )
            }

            # filter metadata to the chosen version (one or more rows)
            sub <- metaDataDF[metaDataDF$Title == version, , drop = FALSE]
            if (!nrow(sub)) {
              stop("No metadata rows found for version '", version, "'.")
            } else if (nrow(sub) != 1) {
              stop("Please define only one version of JASPAR.")
            }

            if (!"SourceUrl" %in% colnames(sub)) {
              stop("SourceUrl column not found in metadata.csv.")
            }

            url <- sub$SourceUrl
            files <- bfcrpath(BiocFileCache(), url)

            if (length(files) == 0) {
              stop("No files found based on SourceUrl from metadata.csv.")
            }

            # This package should have only one file
            .Object@db <- files[1]
            .Object@version <- version
            return(.Object)
          })

#' @rdname JASPAR
#' @export

JASPAR <- function(version = "JASPAR2026") {
  new("JASPAR", version = version)
}

#' @name db
#'
#' @title Access database from JASPAR object
#' @description The accessor function for retrieving the location of the
#' database location slot from the JASPAR object
#' @author Damir Baranasic
#' @param object JASPAR class object
#' @returns Returns the location of the JASPAR.sqlite file
#' @keywords function
#' @examples
#'
#' library(JASPAR)
#' JASPAR <- JASPAR()
#' db(JASPAR)
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
#' @returns Returns the version of the JASPAR satabase in the JASPAR object
#' @keywords function
#' @examples
#'
#' library(JASPAR)
#' JASPAR <- JASPAR()
#' version(JASPAR)
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

#' Available JASPAR releases in this package
#'
#' Reads the package's \code{extdata/metadata.csv} and returns the unique
#' release titles (e.g., "2020", "2022", "2024"), sorted so the newest is first.
#'
#' @param package Character scalar, package name that 
#' contains \code{extdata/metadata.csv}. Defaults to \code{"JASPAR"}.
#'
#' @return A character vector of available releases 
#' (e.g., \code{c("2024","2022","2020")}).
#' 
#' #' @examples
#' # List available JASPAR releases bundled with this package
#' vers <- getAvailableJASPARVersions()
#' vers
#' 
#' @export
#' @importFrom utils read.csv

getAvailableJASPARVersions <- function(package = "JASPAR") {
  metaPath <- system.file("extdata", "metadata.csv", package = package)
  if (!file.exists(metaPath)) 
    stop("metadata.csv not found in package '", package, "'.")
  df <- utils::read.csv(metaPath, stringsAsFactors = FALSE)
  if (!"Title" %in% colnames(df)) 
    stop("Title column not foundin metadata.csv.")

  vers <- unique(df$Title)
  # try to sort by numeric year (desc). 
  # If non-numeric, fallback to desc lexicographic
  yrs <- suppressWarnings(as.integer(gsub("\\D", "", vers)))
  if (all(!is.na(yrs))) {
    vers[order(yrs, decreasing = TRUE)]
  } else {
    sort(vers, decreasing = TRUE)
  }
}