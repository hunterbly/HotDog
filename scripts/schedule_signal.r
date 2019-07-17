#!/usr/bin/env Rscript

library(HotDog)

args = commandArgs(trailingOnly=TRUE)

input.date = args[1]

# Call
df = get_hit_signal(ref.date, format = 'long')
return(df)
