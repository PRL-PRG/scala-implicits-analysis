#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly=T)

if (file.exists(args[1])) {
    cat(read.csv(args[1])$exit_code[1])
} else {
    cat(args[2])
}
