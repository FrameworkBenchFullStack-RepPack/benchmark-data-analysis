# Data Analysis Scripts
The repository contains the R scripts needed to run the data analysis.

## File Description
The project contains three files, each containing a step in the data analysis.

[The mann-segment file](https://github.com/FrameworkBenchFullStack-RepPack/benchmark-data-analysis/blob/main/mann-segment.R) runs Mann-Whitney U with segmentation on the data to determine the number of warmup rounds needed for the data to be statistically stable. 

[The cochrans-split file](https://github.com/FrameworkBenchFullStack-RepPack/benchmark-data-analysis/blob/main/cochrans-split.R) runs Cochran's formula on the data. This can be done with a smaller subset to determine the number of repetitions.

[The ks-test file](https://github.com/FrameworkBenchFullStack-RepPack/benchmark-data-analysis/blob/main/ks-test.R) runs the One-sample Kolmogorov-Smirnov test. This is just to verify that the data is not normally distributed.

[The mann-whitney-u-one-sided file](https://github.com/FrameworkBenchFullStack-RepPack/benchmark-data-analysis/blob/main/mann-whitney-u-one-sided.R) runs the one-sided Mann-Whitney U on the raw data. It runs a Kruskal-Wallis test as part of the Mann-Whitney U. It uses Holm to correct the P-values.
