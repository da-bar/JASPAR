# These tests need no network. They work on hand-made metadata, on synthetic
# SQLite headers, and on a replacement for AnnotationHub::cache(). The
# accessions are made up on purpose: the package must not depend on specific
# ones.

dir <- tempfile("jaspar-tests-")
dir.create(dir)

# A file that carries the 100-byte header of an SQLite database with `pages`
# pages of `pageSize` bytes, but is `bytes` long.
sqlite_file <- function(name, pages = 3L, pageSize = 4096L,
                        bytes = pages * pageSize, counters = TRUE) {
  path <- file.path(dir, name)
  header <- raw(100L)
  header[1:16] <- c(charToRaw("SQLite format 3"), as.raw(0L))
  header[17:18] <- writeBin(as.integer(pageSize), raw(), size = 2L,
                            endian = "big")
  header[25:28] <- writeBin(1L, raw(), size = 4L, endian = "big")
  header[29:32] <- writeBin(as.integer(pages), raw(), size = 4L,
                            endian = "big")
  header[93:96] <- writeBin(if (counters) 1L else 2L, raw(), size = 4L,
                            endian = "big")
  writeBin(c(header, raw(max(bytes - 100L, 0L))), path)
  path
}

html_file <- function(name) {
  path <- file.path(dir, name)
  writeLines(rep("<html><body><h1>404 Not Found</h1></body></html>", 5L), path)
  path
}

records <- data.frame(
  id = c("AH5", "AH7", "AH9"),
  title = c("JASPAR2026", "JASPAR2024", "JASPAR2022"),
  added = "2025-10-10",
  stringsAsFactors = FALSE
)

test_that("only the JASPAR databases are taken from the hub metadata", {
  # 3 databases, 13 genomic tracks and 2 resources with duplicated titles
  id <- c("AH1", "AH2", paste0("AH", 100:112), "AH3", "AH4")
  title <- c("JASPAR2026", "JASPAR2024",
             paste0("hg38.JASPAR2022_CORE_", 1:13, ".RData"),
             "jaspar2020_tf_bindsites.rds", "jaspar2020_tf_bindsites.rds")
  rdataclass <- c("JASPAR", "JASPAR", rep("GRanges", 13), "list", "list")
  added <- rep(c("2025-10-10", "2022-04-01", "2026-09-04"), c(2L, 13L, 2L))
  found <- .recordTable(id, title, rdataclass, added)
  expect_identical(found$id, c("AH1", "AH2"))
  expect_identical(found$title, c("JASPAR2026", "JASPAR2024"))
  expect_identical(found$added, c("2025-10-10", "2025-10-10"))
  expect_identical(colnames(found), c("id", "title", "added"))
})

test_that("a database is recognised by its data class or by its title", {
  found <- .recordTable(
    id = c("AH1", "AH2", "AH3", "AH4"),
    title = c("JASPAR2026", "legacy", "mm9.JASPAR2022.RData",
              "jaspar2020_tf_bindsites.rds"),
    rdataclass = c("SomeOtherClass", "JASPAR", "GRanges", "list"),
    added = "2025-10-10")
  expect_identical(found$id, c("AH1", "AH2"))
})

test_that("an empty hub gives an empty table", {
  found <- .recordTable(NULL, NULL, NULL, numeric())
  expect_identical(nrow(found), 0L)
  expect_identical(colnames(found), c("id", "title", "added"))
  expect_error(.selectJASPARRecord(found, "JASPAR2024"),
               "lists no JASPAR databases")
})

test_that("a record is selected by version", {
  expect_identical(.selectJASPARRecord(records, "JASPAR2024"), "AH7")
  expect_identical(.selectJASPARRecord(records, "JASPAR2022"), "AH9")
})

test_that("every registered version can be selected, JASPAR2026 included", {
  expect_identical(.selectJASPARRecord(records, "JASPAR2026"), "AH5")
  # a record that is registered again under another accession needs no
  # change in the package
  moved <- records
  moved$id[moved$title == "JASPAR2026"] <- "AH123456"
  expect_identical(.selectJASPARRecord(moved, "JASPAR2026"), "AH123456")
})

test_that("the most recently added record wins if a version is listed twice", {
  twice <- rbind(records,
                 data.frame(id = "AH3", title = "JASPAR2024",
                            added = "2026-01-15", stringsAsFactors = FALSE))
  # the date decides, although AH3 has the lower accession number
  expect_identical(.selectJASPARRecord(twice, "JASPAR2024"), "AH3")
  # without dates, the higher accession number decides
  twice$added <- NA_character_
  expect_identical(.selectJASPARRecord(twice, "JASPAR2024"), "AH7")
  # accession numbers are compared as numbers: AH10 is above AH7
  twice$id[twice$id == "AH3"] <- "AH10"
  expect_identical(.selectJASPARRecord(twice, "JASPAR2024"), "AH10")
})

test_that("an unknown version lists the available ones", {
  expect_error(.selectJASPARRecord(records, "JASPAR2020"),
               paste0("'JASPAR2020' is not available.*",
                      "JASPAR2026, JASPAR2024, JASPAR2022"))
  # newest first and each version once, whatever the order of the records
  shuffled <- rbind(records[c(3, 1, 2), ], records[2, ])
  expect_error(.selectJASPARRecord(shuffled, "JASPAR2020"),
               "Available versions: JASPAR2026, JASPAR2024, JASPAR2022\\.$")
})

test_that("a local hub says that a version has not been downloaded", {
  expect_error(.selectJASPARRecord(records[2:3, ], "JASPAR2026", local = TRUE),
               paste0("'JASPAR2026' has not been downloaded.*localHub = TRUE.*",
                      "JASPAR2024, JASPAR2022.*online hub"))
  expect_error(.selectJASPARRecord(records[0, ], "JASPAR2026", local = TRUE),
               "has not been downloaded")
  # a version that is there is found in a local hub as well
  expect_identical(.selectJASPARRecord(records, "JASPAR2024", local = TRUE),
                   "AH7")
})

test_that("versions are sorted with the newest first", {
  expect_identical(.sortVersions(c("JASPAR2022", "JASPAR2026", "JASPAR2024")),
                   c("JASPAR2026", "JASPAR2024", "JASPAR2022"))
  expect_identical(.sortVersions(c("JASPAR2022", "JASPAR2022")), "JASPAR2022")
  expect_identical(.sortVersions(character()), character())
  # titles without a year: alphabetical order, descending
  expect_identical(.sortVersions(c("a", "b")), c("b", "a"))
})

test_that("intact SQLite files are accepted", {
  expect_null(.sqliteProblem(sqlite_file("ok.db")))
  # a page size of 1 stands for 65536 bytes
  expect_null(.sqliteProblem(sqlite_file("big.db", pages = 1L, pageSize = 1L,
                                         bytes = 65536L)))
  # the page count is not valid if the two counters differ, so it is not used
  expect_null(.sqliteProblem(sqlite_file("old.db", bytes = 4096L,
                                         counters = FALSE)))
})

test_that("files that are not intact SQLite databases are rejected", {
  expect_match(.sqliteProblem(html_file("page.html")),
               "not an SQLite database")
  expect_match(.sqliteProblem(sqlite_file("cut.db", bytes = 2L * 4096L)),
               "truncated: it has 8192 bytes, but 12288 are expected")
  expect_match(.sqliteProblem(sqlite_file("head.db", bytes = 100L)),
               "truncated")
  # sizes are not written in scientific notation
  expect_match(.sqliteProblem(sqlite_file("round.db", pages = 489L,
                                          bytes = 1000000L)),
               "it has 1000000 bytes, but 2002944 are expected")
  empty <- file.path(dir, "empty.db")
  file.create(empty)
  expect_match(.sqliteProblem(empty), "empty")
  short <- file.path(dir, "short.db")
  writeBin(c(charToRaw("SQLite format 3"), as.raw(0L), as.raw(1:10)), short)
  expect_match(.sqliteProblem(short), "too short")
})

test_that("what AnnotationHub returns must be the path of a file", {
  expect_match(.fileProblem(character()), "did not return the path")
  expect_match(.fileProblem(c(a = "x", b = "y")), "did not return the path")
  expect_match(.fileProblem(NA_character_), "did not return the path")
  expect_match(.fileProblem(file.path(dir, "missing.db")),
               "did not return the path")
  expect_null(.fileProblem(c(AH7 = sqlite_file("ok2.db"))))
})

test_that("the reason of a failed download is taken from the warnings", {
  warnings <- c(paste0("download failed\n  web resource path: ",
                       "https://x.org/fetch/1\n  reason: HTTP 404 Not Found."),
                "bfcadd() failed; resource removed\n  reason: download failed")
  err <- simpleError("1 resources failed to download")
  expect_identical(.failureReason(warnings, err), "HTTP 404 Not Found.")
  expect_identical(.failureReason("something odd\nmore text", err),
                   "something odd")
  expect_identical(.failureReason(character(), err),
                   "1 resources failed to download")
})

test_that("the cause of a dropped transfer is kept in the reason", {
  warnings <- paste0("download failed\n  web resource path: https://x.org/1\n",
                     "  reason: Failed to perform HTTP request.\n",
                     "Caused by error in `curl::curl_fetch_disk()`:\n",
                     "! Transferred a partial file [127.0.0.1]:\n",
                     "end of response with 4688192 bytes missing")
  expect_identical(
    .failureReason(warnings, simpleError("1 resources failed to download")),
    paste0("Failed to perform HTTP request. Transferred a partial file ",
           "[127.0.0.1]: end of response with 4688192 bytes missing"))
})

test_that("warnings are held back and an error is returned as a value", {
  got <- .capture({
    warning("first")
    warning("second")
    "result"
  })
  expect_identical(got$value, "result")
  expect_identical(got$warnings, c("first", "second"))
  got <- .capture({
    warning("held")
    stop("boom")
  })
  expect_s3_class(got$value, "error")
  expect_identical(conditionMessage(got$value), "boom")
  expect_identical(got$warnings, "held")
})

test_that("a failed connection gives the reason and how to work offline", {
  leaked <- character()
  err <- tryCatch(
    withCallingHandlers(
      .getHub({
        warning(paste0("download failed\n  reason: Could not connect to ",
                       "server [bioconductor.org]"))
        stop("failed to connect")
      }),
      warning = function(w) {
        leaked <<- c(leaked, conditionMessage(w))
        invokeRestart("muffleWarning")
      }),
    error = function(e) e)
  expect_match(conditionMessage(err), "Could not connect to AnnotationHub")
  expect_match(conditionMessage(err),
               "reason: Could not connect to server [bioconductor.org]",
               fixed = TRUE)
  expect_match(conditionMessage(err),
               "AnnotationHub::AnnotationHub(localHub = TRUE)", fixed = TRUE)
  expect_length(leaked, 0L)
})

test_that("something that is not a hub is rejected", {
  expect_error(.getHub("not a hub"), "must be an AnnotationHub object")
})

test_that("a retrieved file is returned as an unnamed path", {
  path <- sqlite_file("retrieved.db")
  local_mocked_bindings(cache = function(x, ..., force = FALSE) c(AH7 = path))
  expect_identical(.retrieveJASPAR(c(AH7 = "x"), "AH7", "JASPAR2024"), path)
})

test_that("a failed download names the version, the accession and the reason", {
  local_mocked_bindings(cache = function(x, ..., force = FALSE) {
    warning("download failed\n  web resource path: https://x.org/fetch/1\n",
            "  reason: HTTP 404 Not Found.")
    warning("bfcadd() failed; resource removed\n  reason: download failed")
    stop("1 resources failed to download")
  })
  leaked <- character()
  err <- tryCatch(
    withCallingHandlers(
      .retrieveJASPAR(c(AH5 = "x"), "AH5", "JASPAR2026"),
      warning = function(w) {
        leaked <<- c(leaked, conditionMessage(w))
        invokeRestart("muffleWarning")
      }),
    error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err),
               "Could not retrieve JASPAR2026 \\(AnnotationHub record AH5\\)")
  expect_match(conditionMessage(err), "reason: HTTP 404 Not Found.",
               fixed = TRUE)
  expect_match(conditionMessage(err), "try again later")
  # the warnings of AnnotationHub are not shown on top of the error
  expect_length(leaked, 0L)
})

test_that("a failed download without warnings gives the error message", {
  local_mocked_bindings(cache = function(x, ..., force = FALSE) {
    stop("1 resources failed to download")
  })
  expect_error(.retrieveJASPAR(c(AH5 = "x"), "AH5", "JASPAR2026"),
               "JASPAR2026 \\(AnnotationHub record AH5\\).*1 resources failed")
})

test_that("a hub that returns no file is reported", {
  local_mocked_bindings(cache = function(x, ..., force = FALSE) {
    structure(character(), names = character())
  })
  expect_error(.retrieveJASPAR(c(AH7 = "x"), "AH7", "JASPAR2024"),
               paste0("JASPAR2024 \\(AnnotationHub record AH7\\).*",
                      "did not return the path"))
})

test_that("a damaged copy in the cache is downloaded once more", {
  good <- sqlite_file("good.db")
  bad <- html_file("bad.html")
  forced <- logical()
  local_mocked_bindings(cache = function(x, ..., force = FALSE) {
    forced <<- c(forced, force)
    c(AH9 = if (force) good else bad)
  })
  expect_identical(.retrieveJASPAR(c(AH9 = "x"), "AH9", "JASPAR2022"), good)
  expect_identical(forced, c(FALSE, TRUE))
})

test_that("a local hub does not try to download a damaged file again", {
  forced <- logical()
  local_mocked_bindings(cache = function(x, ..., force = FALSE) {
    forced <<- c(forced, force)
    c(AH9 = html_file("bad3.html"))
  })
  err <- tryCatch(.retrieveJASPAR(c(AH9 = "x"), "AH9", "JASPAR2022",
                                  local = TRUE),
                  error = function(e) e)
  expect_match(conditionMessage(err),
               "JASPAR2022 \\(AnnotationHub record AH9\\)")
  expect_match(conditionMessage(err), "not an SQLite database")
  expect_match(conditionMessage(err), "localHub = TRUE cannot download")
  expect_false(grepl("downloading it again", conditionMessage(err)))
  # the file was asked for once, without force
  expect_identical(forced, FALSE)
})

test_that("a file that stays damaged is reported with the reason", {
  forced <- logical()
  local_mocked_bindings(cache = function(x, ..., force = FALSE) {
    forced <<- c(forced, force)
    c(AH9 = sqlite_file("cut2.db", bytes = 4096L))
  })
  expect_error(.retrieveJASPAR(c(AH9 = "x"), "AH9", "JASPAR2022"),
               paste0("JASPAR2022 \\(AnnotationHub record AH9\\).*truncated.*",
                      "also after downloading it again"))
  # one download and one more, not more
  expect_identical(forced, c(FALSE, TRUE))
})

test_that("a failure of the second download is reported as a failed download", {
  bad <- html_file("bad2.html")
  local_mocked_bindings(cache = function(x, ..., force = FALSE) {
    if (force) {
      warning("download failed\n  reason: Timeout was reached")
      stop("1 resources failed to download")
    }
    c(AH9 = bad)
  })
  expect_error(.retrieveJASPAR(c(AH9 = "x"), "AH9", "JASPAR2022"),
               "JASPAR2022 \\(AnnotationHub record AH9\\).*Timeout was reached")
})

test_that("warnings that were held back are passed on after a success", {
  path <- sqlite_file("warned.db")
  local_mocked_bindings(cache = function(x, ..., force = FALSE) {
    warning("Corrupt Cache: duplicate files")
    c(AH7 = path)
  })
  expect_warning(
    found <- .retrieveJASPAR(c(AH7 = "x"), "AH7", "JASPAR2024"),
    "Corrupt Cache")
  expect_identical(found, path)
})

test_that("arguments are checked before the hub is contacted", {
  expect_error(JASPAR(version = 2024), "single character string")
  expect_error(JASPAR(version = c("JASPAR2024", "JASPAR2022")),
               "single character string")
  expect_error(JASPAR(version = NA_character_), "single character string")
  expect_error(JASPAR(version = "JASPAR2024", hub = "not a hub"),
               "AnnotationHub object")
  expect_error(getAvailableJASPARVersions(hub = "not a hub"),
               "AnnotationHub object")
})

test_that("the hub is not contacted if the version is invalid", {
  local_mocked_bindings(
    AnnotationHub = function(...) stop("the hub was contacted"))
  expect_error(JASPAR(version = 2024), "single character string")
  expect_error(JASPAR(version = c("JASPAR2024", "JASPAR2022")),
               "single character string")
  # control: a valid version does contact the hub, here the mock
  expect_error(JASPAR(version = "JASPAR2024"), "the hub was contacted")
})

test_that("a JASPAR object is shown with its version and path", {
  # an object made without a hub: new() would call AnnotationHub()
  x <- getClass("JASPAR")@prototype
  class(x) <- getClass("JASPAR")@className
  x@version <- "JASPAR2024"
  x@db <- "/path/to/JASPAR2024"
  expect_output(show(x), "class: JASPAR")
  expect_output(show(x), "version: JASPAR2024")
  expect_output(print(x), "db: /path/to/JASPAR2024")
  expect_identical(db(x), "/path/to/JASPAR2024")
  expect_identical(version(x), "JASPAR2024")
})

test_that("truncation is detected inside the last page and with page size 1", {
  # one byte short of the last full page
  expect_match(.sqliteProblem(sqlite_file("cut1.db", bytes = 3L * 4096L - 1L)),
               "truncated")
  # the page size 1 stands for 65536 bytes
  expect_match(.sqliteProblem(sqlite_file("big-cut.db", pages = 2L,
                                          pageSize = 1L, bytes = 65536L)),
               "truncated")
})

test_that("a version must equal a title; abbreviations and patterns do not", {
  longer <- rbind(records,
                  data.frame(id = "AH11", title = "JASPAR2030_BETA",
                             added = "2025-10-10", stringsAsFactors = FALSE))
  for (v in c("JASPAR", "JASPAR202", "JASPAR2024x", "JASPAR2024 ",
              "JASPAR20.4", "JASPAR2030"))
    expect_error(.selectJASPARRecord(longer, v), "not available", info = v)
})

test_that("a failed download is attempted only once", {
  calls <- 0L
  local_mocked_bindings(cache = function(x, ..., force = FALSE) {
    calls <<- calls + 1L
    stop("1 resources failed to download")
  })
  expect_error(.retrieveJASPAR(c(AH5 = "x"), "AH5", "JASPAR2026"),
               "Could not retrieve")
  expect_identical(calls, 1L)
})

test_that("cache() is called with exactly the selected record", {
  seen <- NULL
  path <- sqlite_file("selected.db")
  local_mocked_bindings(cache = function(x, ..., force = FALSE) {
    seen <<- x
    c(AH7 = path)
  })
  .retrieveJASPAR(c(AH5 = "a", AH7 = "b", AH9 = "c"), "AH7", "JASPAR2024")
  expect_identical(seen, c(AH7 = "b"))
})

test_that("JASPAR() works for any registered version, JASPAR2026 included", {
  path <- sqlite_file("end-to-end.db")
  hub <- methods::new("AnnotationHub")
  local_mocked_bindings(
    .jasparRecords = function(hub) records,
    cache = function(x, ..., force = FALSE) c(AH5 = path))
  jaspar <- JASPAR(version = "JASPAR2026", hub = hub)
  expect_identical(version(jaspar), "JASPAR2026")
  expect_identical(db(jaspar), path)
  # initialize() has the same default version as JASPAR()
  expect_identical(version(new("JASPAR", hub = hub)), "JASPAR2024")
})

test_that("getAvailableJASPARVersions() lists every record, newest first", {
  local_mocked_bindings(.jasparRecords = function(hub) records[c(3, 1, 2), ])
  expect_identical(
    getAvailableJASPARVersions(hub = methods::new("AnnotationHub")),
    c("JASPAR2026", "JASPAR2024", "JASPAR2022"))
})

test_that("both functions default to the full hub, not a local one", {
  expect_identical(formals(JASPAR)$hub, quote(AnnotationHub()))
  expect_identical(formals(getAvailableJASPARVersions)$hub,
                   quote(AnnotationHub()))
})

test_that("the package code names no AnnotationHub accession", {
  ns <- asNamespace("JASPAR")
  code <- unlist(lapply(ls(ns, all.names = TRUE), function(name) {
    object <- get(name, envir = ns)
    if (is.function(object))
      deparse(object)
  }))
  expect_false(any(grepl("AH[0-9]+", code)))
})

test_that("a file that cannot be read is treated like a damaged one", {
  # a directory exists but cannot be read as a file
  expect_match(.fileProblem(dir), "cannot be read")
  good <- sqlite_file("after-unreadable.db")
  forced <- logical()
  local_mocked_bindings(cache = function(x, ..., force = FALSE) {
    forced <<- c(forced, force)
    c(AH9 = if (force) good else dir)
  })
  expect_identical(.retrieveJASPAR(c(AH9 = "x"), "AH9", "JASPAR2022"), good)
  expect_identical(forced, c(FALSE, TRUE))
})

test_that("text in another encoding does not break the reason", {
  warnings <- paste0("download failed\n  reason: caf", "\xe9")
  expect_identical(.failureReason(warnings, simpleError("failed")),
                   "caf<e9>")
})

unlink(dir, recursive = TRUE)
