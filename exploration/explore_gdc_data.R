
library(TCGAbiolinks)
library(dplyr)
library(maftools)
library(stringr)

# ---- project settings ----

tumor <- 'KIRC'
project <- stringr::str_c('TCGA', tumor, sep = '-')


# ---- query results ----

# DNA methylation
query.met <- GDCquery(project = project,
                      data.category = 'DNA Methylation',
                      legacy = FALSE,
                      platform = c('Illumina Human Methylation 450'))

# RNA seq
query.exp.FPKM <- GDCquery(project = project,
                      data.category = 'Transcriptome Profiling',
                      data.type = 'Gene Expression Quantification', 
                      workflow.type = 'HTSeq - FPKM-UQ')
query.exp.count <- GDCquery(project = project,
                            data.category = 'Transcriptome Profiling',
                            data.type = 'Gene Expression Quantification', 
                            workflow.type = 'HTSeq - Counts')

# miRNA
query.mirna <- GDCquery(project = project,
                        data.category = 'Transcriptome Profiling',
                        data.type = 'miRNA Expression Quantification')

# snvs
query.snv <- GDCquery(project = project,
                      data.category = 'Simple Nucleotide Variation')

# mafs
query.maf <- GDCquery_Maf(tumor, pipelines = c("muse", 'mutect2'))

# note: to read mafs
# maf <- query.maf %>% read.maf(removeSilent = TRUE, save.csv = TRUE, useAll = FALSE)

# get subtype information
query.subtype <- TCGAquery_subtype(tumor = tumor)

# get indexed clinical data
query.clin <- GDCquery_clinic(project = project, "Clinical")

# ---- follow mRNA seq example ----

# Following mRNA seq example from:
# https://bioconductor.org/packages/release/bioc/vignettes/TCGAbiolinks/inst/doc/tcgaBiolinks.html#case_study_n_2:_pan_cancer_downstream_analysis_lgg

library(TCGAbiolinks)
library(SummarizedExperiment)

query.exp <- GDCquery(project = project, 
                      legacy = FALSE,
                      data.category = 'Transcriptome Profiling',
                      data.type = 'Gene Expression Quantification', 
                      workflow.type = 'HTSeq - Counts',
                      sample.type = "Primary solid Tumor")
GDCdownload(query.exp)
data.exp <- GDCprepare(query = query.exp, save = TRUE, save.filename = str_c(tumor, '.rda'))

# get subtype information
dataSubt <- TCGAquery_subtype(tumor = tumor)

# get indexed clinical data
dataClin <- GDCquery_clinic(project = project, "Clinical")

