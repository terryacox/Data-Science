setwd("~/Birds/Official")
library(tidyverse)
infile <- "World List 20MAR2024.txt"
clementsfile <- "~/Birds/Clements-v2023-October-2023.csv"
outfile <- "World List 20MAR2024.txt"
outxlsx <- "World List 20MAR2024.xlsx"

# Current Clements List
c.tbl <- read_csv(clementsfile)
extinct <- c("Cryptic Treehunter") # Recently extinct species
species.tbl <-
  c.tbl |>
  filter(category == "species" & is.na(extinct)) |>
  select(c(SortOrder = "sort v2023", CommonName = "English name",
           ScientificName = "scientific name", Family = family)) |>
  filter(!(CommonName %in% extinct)) |>
  print()

# My current world list
world.tbl <- read_csv(infile)

###### Re-write using anti_join ################

# w.not.c <- 
#   world.tbl |>
#   anti_join(species.tbl, by = "CommonName") |>
#   print(n=3)
# 
# c.not.w <-
#   species.tbl |>
#   anti_join(world.tbl, by = "CommonName") |>
#   print(n=3)

###### Match on common names ################

w.not.c <- 
  world.tbl |>
  left_join(species.tbl, by = "CommonName") |>
  filter(is.na(Family.y)) 

if(nrow(w.not.c)) stop("Revise infile") 

c.not.w <-
  species.tbl |>
  left_join(world.tbl, by = "CommonName") |>
  filter(is.na(Family.y))

if(nrow(c.not.w)) stop("Revise infile") 

###### Create updated bird list ################

udf <-
  world.tbl |>
  left_join(species.tbl, by = "CommonName") |>
  arrange(SortOrder) |>
  select(CommonName, ScientificName = ScientificName.y, Date, 
         Location, Family = Family.y) |>
  print(n=3)

###### Error check ################

if (anyDuplicated(udf$CommonName)) stop("Duplicate CommonName values")

if (anyDuplicated(udf$ScientificName)) stop("Duplicate ScientificName values")

if (!identical(world.tbl$CommonName, species.tbl$CommonName)){ 
  stop("Common names don't match")
}

###### Save txt file ################

write_csv(udf, file=outfile, na="")

###### Generate xlsx file ################

seen.tbl <- 
  udf |>
  filter(!is.na(Location)) 

hummers.tbl <-
  udf |>
  filter(grepl("Hummingbirds", Family)) |>
  select(-Family) 

seenHummers.tbl <-
  hummers.tbl |>
  filter(!is.na(Location)) 

library(openxlsx2)

dimsHeader <- wb_dims(rows=1, cols=seq_len(ncol(udf)))
dimsHummerHeader <- wb_dims(rows=1, cols=seq_len(ncol(hummer.tbl)))
wb <- wb_workbook() |>
  wb_set_base_font(font_size=16) |>
  wb_add_worksheet("World List") |>
  wb_add_data(x=udf, na.strings = "") |>
  wb_add_fill(dims=dimsHeader, color=wb_color(name="lightgray")) |>
  wb_set_col_widths(cols=1:ncol(udf), widths="auto") |>
  wb_add_filter(row=1, cols=1:ncol(udf)) |> 
  wb_add_worksheet("World Seen") |>
  wb_add_data(x=seen.tbl) |>
  wb_add_fill(dims=dimsHeader, color=wb_color(name="lightgray")) |>
  wb_set_col_widths(cols=1:ncol(seen.tbl), widths="auto") |>
  wb_add_filter(row=1, cols=1:ncol(seen.tbl)) |>
  wb_add_worksheet("Hummingbird List") |>
  wb_add_data(x=hummers.tbl, na.strings = "") |>
  wb_add_fill(dims=dimsHummerHeader, color=wb_color(name="lightgray")) |>
  wb_set_col_widths(cols=1:ncol(hummers.tbl), widths="auto") |>
  wb_add_filter(row=1, cols=1:ncol(hummers.tbl)) |>
  wb_add_worksheet("Hummingbirds Seen") |>
  wb_add_data(x=seenHummers.tbl) |>
  wb_add_fill(dims=dimsHummerHeader, color=wb_color(name="lightgray")) |>
  wb_set_col_widths(cols=1:ncol(seenHummers.tbl), widths="auto") |>
  wb_add_filter(row=1, cols=1:ncol(seenHummers.tbl))
wb_save(wb,file=outxlsx)


                  
