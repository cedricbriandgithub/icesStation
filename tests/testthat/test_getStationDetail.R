test_that("test example code in getStationDetail",
          {
            stdetail <- getStationDetail(.code = 1)
            #station_notes shoud be returned from the attribute field (from icesVocab > 1.3.3)
            expect_equal(stdetail$Station_Notes, 'Center of the area sampled as the Belgian Continental Plat.')
            #station_activeFrom should be returned (from icesVocab > 1.3.3)
            expect_equal(stdetail$Station_ActiveFromDate, "1997")
            # station should have a modified field
            expect_type(stdetail$modified, "character")
            # this station should return a active until date (from icesVocab > 1.3.3)
            expect_equal(getStationDetail(.code = 4873)$Station_ActiveUntilDate, "1994")
            # this station shoud return the deprecated field (from icesVocab > 1.3.3)
            expect_false(getStationDetail(.code = 4873)$Station_Deprecated)
          }
)
