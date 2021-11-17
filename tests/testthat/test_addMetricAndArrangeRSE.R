data("stackepi")

result <- addMetricAndArrangeRSE(
    stackepi,
    data.frame(
        gene_id = SummarizedExperiment::rowRanges(stackepi)$gene_id,
        value = seq(0, 1, length.out = length(stackepi))
    ),
    rse_key = "gene_id",
    order_key = "gene_id", order_value = "value", shuffle_tie = FALSE
)

SummarizedExperiment::rowRanges(result)$value <- NULL

test_that("test on RSE", {
    expect_equal(
        result,
        stackepi[seq(length(stackepi), 1, by = -1L),]
    )
})
