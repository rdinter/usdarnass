skip_on_cran()
skip_on_travis()
# ---- returns the correct type of value ----------------------------------

test_that("nass_count returns how many", {
  x <- nass_count()
  
  expect_is(x, "numeric")
})

test_that("nass_param returns what of", {
  x <- nass_param("source_desc")
  
  expect_is(x, "character")
})

test_that("nass_data returns what", {
  x <- nass_data(year = 2012,
                 short_desc = "AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $",
                 county_name = "WAKE",
                 state_name = "NORTH CAROLINA")
  
  expect_is(x, "data.frame")
})


# ---- parameter issues ---------------------------------------------------
# 
# test_that("year doesn't accept logicals", {
#   expect_error(nass_count(year = >2012))
# })
# 
