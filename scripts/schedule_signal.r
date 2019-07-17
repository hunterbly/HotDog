#!/usr/bin/env Rscript

library(HotDog)

args = commandArgs(trailingOnly=TRUE)

input.date = args[1]

# Call
df = get_hit_signal(ref.date = date, format = 'long')
#return(df)
print(nrow(df))
