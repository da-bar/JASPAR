# Internal helpers that talk to AnnotationHub. They are kept apart from the
# class definition so that the record selection, the download and the checks on
# the downloaded file can be tested without a network connection. Nothing in
# this file names an AnnotationHub accession: records are found through their
# metadata only.

# Metadata of the JASPAR SQLite databases registered in AnnotationHub, one row
# per record
.jasparRecords <- function(hub) {
  .recordTable(names(hub), hub$title, hub$rdataclass, hub$rdatadateadded)
}

# The records of the JASPAR databases: the R data class is "JASPAR", or the
# title is a release such as "JASPAR2024". Either marks a database. Other
# resources that mention JASPAR in their title, such as genomic tracks, have
# neither.
.recordTable <- function(id, title, rdataclass, added) {
  id <- as.character(id)
  title <- as.character(title)
  added <- as.character(added)
  if (length(added) != length(id))
    added <- rep(NA_character_, length(id))
  keep <- as.character(rdataclass) %in% "JASPAR" |
    grepl("^JASPAR[0-9]{4}$", title)
  data.frame(id = id[keep], title = title[keep], added = added[keep],
             stringsAsFactors = FALSE)
}

# Release titles, newest first. The year is read from the digits of the title;
# titles without digits fall back to descending alphabetical order.
.sortVersions <- function(titles) {
  titles <- unique(titles)
  years <- as.numeric(gsub("\\D", "", titles))
  if (anyNA(years))
    return(sort(titles, decreasing = TRUE))
  titles[order(years, decreasing = TRUE)]
}

# Accession of the record that holds the requested version. If a version is
# registered more than once, the most recently added record wins. A local hub
# (AnnotationHub(localHub = TRUE)) contains only the databases that were
# downloaded before, which the error says.
.selectJASPARRecord <- function(records, version, local = FALSE) {
  hits <- records[records$title %in% version, , drop = FALSE]
  if (!nrow(hits)) {
    available <- .sortVersions(records$title)
    if (local) {
      stop("JASPAR version '", version, "' has not been downloaded. ",
           "A hub created with localHub = TRUE contains only the databases ",
           "that were downloaded before",
           if (length(available))
             paste0(": ", paste(available, collapse = ", ")),
           ". Use an online hub to retrieve it.", call. = FALSE)
    }
    listed <- if (length(available))
      paste0("Available versions: ", paste(available, collapse = ", "), ".")
    else
      "AnnotationHub lists no JASPAR databases."
    stop("JASPAR version '", version, "' is not available in AnnotationHub. ",
         listed, call. = FALSE)
  }
  # accession numbers are compared as numbers: AH10 is above AH9
  digits <- sub("^AH", "", hits$id)
  number <- as.numeric(ifelse(grepl("^[0-9]+$", digits), digits, NA_character_))
  hits$id[order(hits$added, number, decreasing = TRUE)[1L]]
}

# Reason why a file cannot be an intact SQLite database, or NULL if it looks
# intact. The 100-byte header holds a magic string, the page size and, if the
# file was written by a recent SQLite, the number of pages; together they give
# the size the file must have at least. A magic string alone would not notice a
# file that was cut short.
.sqliteProblem <- function(path) {
  header <- readBin(path, "raw", 100L)
  if (!length(header))
    return("the file is empty")
  if (!identical(header[seq_len(16L)],
                 c(charToRaw("SQLite format 3"), as.raw(0L))))
    return("the file is not an SQLite database")
  if (length(header) < 100L)
    return("the file is too short to be an SQLite database")
  number <- function(bytes)
    sum(as.numeric(bytes) * 256^(rev(seq_along(bytes)) - 1))
  pageSize <- number(header[17:18])
  if (pageSize == 1)
    pageSize <- 65536
  pages <- number(header[29:32])
  # the page count counts only if the change counter equals the
  # version-valid-for number
  if (pages > 0 && number(header[25:28]) == number(header[93:96]) &&
      file.size(path) < pages * pageSize) {
    bytes <- function(x) format(x, scientific = FALSE, trim = TRUE)
    return(paste0("the file is truncated: it has ", bytes(file.size(path)),
                  " bytes, but ", bytes(pages * pageSize), " are expected"))
  }
  NULL
}

# Reason why what AnnotationHub returned cannot be used, or NULL
.fileProblem <- function(path) {
  if (length(path) != 1L || is.na(path) || !file.exists(path))
    return("AnnotationHub did not return the path of a file")
  got <- .capture(.sqliteProblem(path))
  if (inherits(got$value, "error")) {
    return(paste0("the file cannot be read (", conditionMessage(got$value),
                  ")"))
  }
  got$value
}

# Evaluate an expression and hold back its warnings. AnnotationHub reports why a
# download or a connection failed (HTTP status, connection problem) only in
# warnings, so they are collected here instead of being shown. A failure is
# returned as the error condition in $value.
.capture <- function(expr) {
  held <- new.env(parent = emptyenv())
  held$warnings <- character()
  keep <- function(w) {
    held$warnings <- c(held$warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
  value <- tryCatch(withCallingHandlers(expr, warning = keep),
                    error = function(e) e)
  list(value = value, warnings = held$warnings)
}

# The hub to use. The default, AnnotationHub(), is evaluated here so that a
# failed connection is reported with a hint on how to work offline.
.getHub <- function(hub) {
  got <- .capture(hub)
  if (inherits(got$value, "error")) {
    stop("Could not connect to AnnotationHub.\n  reason: ",
         .failureReason(got$warnings, got$value), "\n  ",
         "To use the databases that were downloaded before, create the hub ",
         "with AnnotationHub::AnnotationHub(localHub = TRUE) and pass it as ",
         "the 'hub' argument.", call. = FALSE)
  }
  if (!is(got$value, "AnnotationHub"))
    stop("'hub' must be an AnnotationHub object.", call. = FALSE)
  # pass on warnings that were held back, for example about a database that
  # could not be checked for updates
  for (w in got$warnings)
    warning(w, call. = FALSE)
  got$value
}

# One call of AnnotationHub::cache() for a single record. A failure is returned
# as the error condition in $path.
.fetchJASPAR <- function(hub, id, force = FALSE) {
  got <- .capture(cache(hub[id], force = force))
  list(path = got$value, warnings = got$warnings)
}

# The most useful reason for a failure, for display only: the wording comes from
# AnnotationHub, httr2 and curl and changes between versions. It is the text
# after "reason:" of the first warning that has one, without the lines that
# only name the call that failed.
.failureReason <- function(warnings, error) {
  # text from a localized system may not be valid in the current encoding
  warnings <- iconv(warnings, "UTF-8", "UTF-8", sub = "byte")
  reasons <- regmatches(warnings,
                        regexpr("(?s)(?<=reason: ).*", warnings, perl = TRUE))
  if (length(reasons)) {
    reason <- gsub("Caused by error[^\r\n]*[\r\n]+", "", reasons[1L])
    reason <- gsub("(^|[[:space:]])![[:space:]]", " ", reason)
    return(trimws(gsub("[[:space:]]+", " ", reason)))
  }
  if (length(warnings))
    return(sub("\n.*$", "", warnings[1L]))
  conditionMessage(error)
}

# Local path of the SQLite file of one record. AnnotationHub downloads the
# file on first use and serves it from its cache afterwards. Any failure stops
# with an error that names the version and the accession. A local hub cannot
# download, so it cannot repair a damaged file either.
.retrieveJASPAR <- function(hub, id, version, local = FALSE) {
  fail <- function(...) {
    stop("Could not retrieve ", version, " (AnnotationHub record ", id,
         ").\n  ", ..., call. = FALSE)
  }
  failedDownload <- function(got) {
    fail("reason: ", .failureReason(got$warnings, got$path), "\n  ",
         "A failed download is not cached, so you can try again later.")
  }

  got <- .fetchJASPAR(hub, id)
  if (inherits(got$path, "error"))
    failedDownload(got)

  problem <- .fileProblem(got$path)
  if (!is.null(problem) && local) {
    fail("reason: ", problem, ".\n  ",
         "A hub created with localHub = TRUE cannot download a new copy. ",
         "Retrieve the version with an online hub to repair the cache.")
  }
  if (!is.null(problem)) {
    # AnnotationHub does not look into the files it caches, so a copy that was
    # damaged (an interrupted transfer, a web page served instead of the file)
    # would be returned for ever. Download it once more.
    got <- .fetchJASPAR(hub, id, force = TRUE)
    if (inherits(got$path, "error"))
      failedDownload(got)
    problem <- .fileProblem(got$path)
    if (!is.null(problem)) {
      fail("reason: ", problem, ", also after downloading it again.\n  ",
           "The server may be sending a damaged file. Try again later.")
    }
  }

  # pass on warnings that were held back, for example about a corrupt cache
  for (w in got$warnings)
    warning(w, call. = FALSE)
  unname(got$path)
}
