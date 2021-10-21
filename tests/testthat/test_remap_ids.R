ids <- c("A", "B", "C", "D", "E", "F", "G")
filename <- "files/remap_ids/linker.tsv"

test_that("remap.ids checks input types", {
  expect_error(remap.ids(ids, c("two", "files")))
  expect_error(remap.ids(ids, 123))
  expect_error(remap.ids(ids, data.frame(A = "A", B = "B")))
  expect_error(remap.ids(ids, paste(filename, "2", sep = "")))
})

test_that("remap.ids permits NA linker file", {
  expect_identical(
    remap.ids(ids, NA),
    ids
  )
})

test_that("remap.ids successfully links IDs", {
  expect_identical(
    remap.ids(ids[1:5], filename),
    c("a", "b", "c", "d", "f")
  )
})

test_that("remap.ids links absent IDs to NA", {
  expect_identical(
    remap.ids(ids, filename),
    c("a", "b", "c", "d", "f", NA, NA)
  )
})
