#!/usr/bin/env Rscript

suppressWarnings(library(HotDog))
suppressWarnings(library(dplyr))

args = commandArgs(trailingOnly=TRUE)

input.date = args[1]

if(is.na(input.date)){
    input.date = as.character(Sys.Date())
}else {
    # pass
}


df = get_hit_signal(ref.date = input.date,
                    format = 'long',
                    local = TRUE)
df.nz = df %>% filter(hit != 0)

# Save to db
save_hit_signal(df.signal = df.nz, local = TRUE)
