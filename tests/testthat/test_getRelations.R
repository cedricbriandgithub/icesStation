test_that("test example code in getRelations.R",
          {
            skip_on_cran()
            tt <- icesVocab::getCodeDetail("Station", 1000)
            expect_type(getRelations(st. = tt), "character")
            expect_gt(length(getRelations(st. = tt)), 6)
            expect_type(getAttribute(st. = tt), "list")
          }
)

test_that("test example code in getListRelations",
          {
            skip_on_cran()
            tt <- icesVocab::getCodeDetail("Station", 1000)
            # this one should return "FI"
            expect_equal(getListRelations(st. = tt, relation_key. = "ISO_3166"), "FI")
            # when there are several arguments, this should be collated with "~"
            expect_true(grepl("~",getListRelations(st. = tt, relation_key. = "Station_DTYPE")))
          }
)
