# inputs and outputs --------
result1 <- "#479810FF"
result2 <- "#E72E1D00"

test_that("test the RGB mean color ", {
    expect_equal(
        meanColor(c("#000000FF", "#FFF000FF", "#0FF000FF", "#0F8040FF")),
        result1
    )
})

# tests --------
test_that("test the RGB mean color", {
    expect_equal(
        meanColor(c("#FF866A00", "#FF000000", "#A9110100", "#F7230C00")),
        result2
    )
})

