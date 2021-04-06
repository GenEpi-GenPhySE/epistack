context("mean_color")

result <- "#479810FF"

result2 <- "#E82E1E00"

test_that("test the RGB mean color ", {
    expect_equal(
        mean_color(c("#000000FF", "#FFF000FF", "#0FF000FF", "#0F8040FF")),
        result
    )
})
#> Test passed



test_that("test the RGB mean color", {
    expect_equal(
        mean_color(c("#FF866A00", "#FF000000", "#A9110100", "#F7230C00")),
        result1
    )
})

