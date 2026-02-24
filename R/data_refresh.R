

library(tidyverse)
library(RSocrata)

df <- RSocrata::read.socrata("https://data.winnipeg.ca/resource/qd6b-q49i.json")
print(head(df))
str(df)

## check if dir exists; if not, create one.
if (!dir.exists("data")) dir.create("data")
readr::write_csv(x = df, file = "data/narcan.csv", append = FALSE)