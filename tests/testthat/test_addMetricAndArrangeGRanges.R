context("addMetricAndArrangeGRanges")




gr <- GenomicRanges::GRanges(
    seqnames = S4Vectors::Rle(c("chr1", "chr2", "chr1", "chr3"), c(10, 10, 15, 15)),
    ranges = IRanges::IRanges(101:150, end = 111:160, names = head(letters, 50)),
    strand = S4Vectors::Rle(BiocGenerics::strand(c("-", "+", "*", "+", "-")), c(10, 15, 5, 10, 10)),
    gene_id = base::sample(gRanges_test$gene_id, size = 50),
    score = base::sample(1:50),
    GC = base::sample(seq(1, 0, length=50)))



this_order_key <- data.frame(gene_id = gr$gene_id, exp = rnorm(length(gr$GC)))

result1 <- new("GRanges",
               seqnames = new("Rle",
                              values = structure(integer(0),
                                                 .Label = c("chr1", "chr2", "chr3"),
                                                 class = "factor"),
                            lengths = integer(0),
                            elementMetadata = NULL,
                            metadata = list()),
               ranges = new("IRanges",
                            start = integer(0),
                            width = integer(0),
                            NAMES = character(0),
                            elementType = "ANY",
                            elementMetadata = NULL,
                            metadata = list()),
               strand = new("Rle",
                            values = structure(integer(0),
                                               .Label = c("+", "-", "*"),
                                               class = "factor"),
                            lengths = integer(0),
                            elementMetadata = NULL,
                            metadata = list()),
               seqinfo = new("Seqinfo",
                             seqnames = c("chr1", "chr2", "chr3"),
                             seqlengths = c(NA_integer_, NA_integer_, NA_integer_),
                             is_circular = c(NA, NA, NA),
                             genome = c(NA_character_,NA_character_, NA_character_)),
               elementMetadata = new("DFrame",
                                     rownames = NULL,
                                     nrows = 0L,
                                     listData = list(gene_id = character(0),
                                                     exp = numeric(0),
                                                     score = integer(0),
                                                     GC = numeric(0)),
                                     elementType = "ANY",
                                     elementMetadata = NULL,
                                     metadata = list()),
               elementType = "ANY",
               metadata = list())





ramdomOrder <- data.frame(gene_id = gRanges_test$gene_id, value = rnorm(length(gRanges_test)))

result2 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)





test_that("test the sort with value value", {
    expect_equal(
        head(addMetricAndArrangeGRanges(gRanges_test,
                                   ramdomOrder, gr_key = "gene_id",
                                   order_key = "gene_id",
                                   order_value = "exp" )$exp, n = 20),
        result2
    )
})
#> Test passed



test_that("test the sort with GC", {
    expect_equal(
        addMetricAndArrangeGRanges(gr, this_order_key, gr_key = "gene_id",
                                   order_key = "gene_id", order_value = "exp" ),
        result1
     )
})


