test_that("test example code in getListStation",
          {
            skip_on_cran()
           # only get past month data
             station <- getListStation(date = Sys.Date() - as.difftime(4, units="weeks"))
             expect_true(class(station) == "data.frame")
          }
)



test_that("test  getListStation with deprecated",
          {
            skip_on_cran()
            # stations 286 and 287 are deprecated
            station <- getListStation(code = c(1,286,287), deprecated = FALSE)
            expect_true(nrow(station) == 1)
          }
)



test_that("test  getListStation with vector of code",
          {
            skip_on_cran()
            expect_equal(nrow(getListStation(code = 1:10)),10)
          }
)
