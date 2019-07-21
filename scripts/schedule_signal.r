#!/usr/bin/env Rscript

suppressWarnings(library(HotDog))
suppressWarnings(library(dplyr))

args = commandArgs(trailingOnly=TRUE)

input.date = args[1]
str(input.date)
# Call
# df = get_hit_signal(ref.date = date, format = 'long')
# return(df)
df = get_hit_signal(ref.date = input.date,
                    format = 'long')
df.nz = df %>% filter(hit != 0)

# Save to db
save_hit_signal(df.signal = df.nz)
