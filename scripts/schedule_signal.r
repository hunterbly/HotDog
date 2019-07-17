#!/usr/bin/env Rscript

library(HotDog)

args = commandArgs(trailingOnly=TRUE)

input.date = args[1]
str(input.date)
# Call
# df = get_hit_signal(ref.date = date, format = 'long', local = TRUE)
# return(df)
df = get_hit_signal(ref.date = input.date, format = 'long', local = TRUE)
print(nrow(df))
