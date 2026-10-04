# These tests talk to AnnotationHub and to the servers behind it. They use
# JASPAR2024 and JASPAR2022 only, so that they do not depend on the download
# location of any other release.

skip_if_not_installed("RSQLite")

# One hub for all tests, because creating it takes a few seconds. The tests are
# skipped if AnnotationHub cannot be reached, for example when working offline.
# (skip_if_offline() is not used because it also skips unless NOT_CRAN is set,
# which would switch these tests off on the builders.)
hub <- tryCatch(AnnotationHub::AnnotationHub(), error = function(e) NULL)
if (is.null(hub))
  skip("AnnotationHub cannot be reached")

# number of matrices in the SQLite file of a JASPAR object
count_matrices <- function(jaspar) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db(jaspar),
                            flags = RSQLite::SQLITE_RO)
  on.exit(RSQLite::dbDisconnect(con))
  RSQLite::dbGetQuery(con, "SELECT COUNT(*) AS n FROM MATRIX")$n
}

test_that("the registered versions are listed, newest first", {
  versions <- getAvailableJASPARVersions(hub = hub)
  expect_type(versions, "character")
  expect_true(all(c("JASPAR2024", "JASPAR2022") %in% versions))
  # every database record of the hub is listed, whatever its title
  expect_true(all(unique(hub$title[hub$rdataclass == "JASPAR"]) %in% versions))
  expect_false(anyDuplicated(versions) > 0)
  expect_identical(versions, .sortVersions(versions))
})

test_that("the date of each record comes from the hub metadata", {
  records <- .jasparRecords(hub)
  expect_false(anyNA(records$added))
  expect_identical(records$added,
                   as.character(hub[records$id]$rdatadateadded))
})

test_that("a version that is not registered gives an informative error", {
  expect_error(JASPAR(version = "JASPAR1999", hub = hub),
               "'JASPAR1999' is not available.*JASPAR2024")
})

test_that("JASPAR2024 is retrieved as an SQLite database", {
  jaspar <- JASPAR(version = "JASPAR2024", hub = hub)
  expect_s4_class(jaspar, "JASPAR")
  expect_identical(version(jaspar), "JASPAR2024")
  expect_true(file.exists(db(jaspar)))
  expect_null(.sqliteProblem(db(jaspar)))
  expect_gt(count_matrices(jaspar), 5000L)
})

test_that("JASPAR2022 is retrieved as an SQLite database", {
  jaspar <- JASPAR(version = "JASPAR2022", hub = hub)
  expect_s4_class(jaspar, "JASPAR")
  expect_identical(version(jaspar), "JASPAR2022")
  expect_true(file.exists(db(jaspar)))
  expect_null(.sqliteProblem(db(jaspar)))
  expect_gt(count_matrices(jaspar), 2900L)
})

test_that("a local hub retrieves the versions that were downloaded", {
  # JASPAR2024 and JASPAR2022 are in the cache from the tests above
  local <- AnnotationHub::AnnotationHub(localHub = TRUE)
  expect_true(all(c("JASPAR2024", "JASPAR2022") %in%
                    getAvailableJASPARVersions(hub = local)))
  expect_identical(db(JASPAR("JASPAR2024", hub = local)),
                   db(JASPAR("JASPAR2024", hub = hub)))
  expect_error(JASPAR(version = "JASPAR1999", hub = local),
               "'JASPAR1999' has not been downloaded")
})

test_that("the default version is JASPAR2024", {
  expect_identical(version(JASPAR(hub = hub)), "JASPAR2024")
  expect_identical(formals(JASPAR)$version, "JASPAR2024")
})

test_that("a repeated call uses the file in the cache", {
  first <- db(JASPAR("JASPAR2024", hub = hub))
  modified <- file.mtime(first)
  second <- db(JASPAR("JASPAR2024", hub = hub))
  expect_identical(second, first)
  # a file that was downloaded again would have a new modification time
  expect_identical(file.mtime(second), modified)
})

test_that("a JASPAR object is shown with its version and path", {
  jaspar <- JASPAR(version = "JASPAR2022", hub = hub)
  expect_output(show(jaspar), "version: JASPAR2022")
  expect_output(show(jaspar), db(jaspar), fixed = TRUE)
})
