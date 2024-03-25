###### Initialize #####################

setwd("~/Birds/Official")
library(tidyverse)
library(openxlsx2)
library(stringr)

infile <- "World List 20MAR2024.txt"
outxlsx <- "World List 20MAR2024.xlsx"
outgenxlsx <- "Hummingbird Genera To See.xlsx"
ingenxlsx <- "Hummingbird Genera To See.xlsx"

###### Check input txt file #####################

tct <- count.fields(infile, sep=",", quote="\"")
tctn <- which(tct != 5)
if (length(tctn)){
  outerr <- paste0("Error in line ", tctn)
  print(outerr)
  stop("Revise infile")
}

###### Set up data #####################

world.tbl <- read_csv(infile)

seen.tbl <-
  world.tbl |>
  subset(!is.na(Location)) |>
  print()

hummers.tbl <-
  world.tbl |>
  filter(grepl("Hummingbirds", Family)) |>
  select(-Family) |>
  print()

seenHummers.tbl <-
  hummers.tbl |>
  filter(!is.na(Location)) |>
  print()

###### Create tables #####################

seen.tbl |>
  select(Date) |>
  mutate(Date = year(Date)) |>
  table()

seen.tbl |>
  select(Location) |>
  table()

seen.tbl |>
  select(Date) |>
  table() |>
  as_tibble() |>
  arrange(desc(n)) |>
  slice_head(n=10) |>
  print()

seenHummers.tbl |>
  select(Date) |>
  mutate(Date = year(Date)) |>
  table()

seenHummers.tbl |>
  select(Location) |>
  table()

seenHummers.tbl |>
  select(Date) |>
  table() |>
  as_tibble() |>
  arrange(desc(n)) |>
  slice_head(n=10) |>
  print()

###### Create xlsx workbook #####################

dimsHeader <- wb_dims(rows=1, cols=seq_len(ncol(world.tbl)))
dimsHummerHeader <- wb_dims(rows=1, cols=seq_len(ncol(hummer.tbl)))
wb <- wb_workbook() |>
  wb_set_base_font(font_size=16) |>
  wb_add_worksheet("World List") |>
  wb_add_data(x=world.tbl) |>
  wb_add_fill(dims=dimsHeader, color=wb_color(name="lightgray")) |>
  wb_set_col_widths(cols=1:ncol(world.tbl), widths="auto") |>
  wb_add_filter(row=1, cols=1:ncol(world.tbl)) |> 
  wb_add_worksheet("World Seen") |>
  wb_add_data(x=seen.tbl) |>
  wb_add_fill(dims=dimsHeader, color=wb_color(name="lightgray")) |>
  wb_set_col_widths(cols=1:ncol(seen.tbl), widths="auto") |>
  wb_add_filter(row=1, cols=1:ncol(seen.tbl)) |>
  wb_add_worksheet("Hummingbird List") |>
  wb_add_data(x=hummers.tbl) |>
  wb_add_fill(dims=dimsHummerHeader, color=wb_color(name="lightgray")) |>
  wb_set_col_widths(cols=1:ncol(hummers.tbl), widths="auto") |>
  wb_add_filter(row=1, cols=1:ncol(hummers.tbl)) |>
  wb_add_worksheet("Hummingbirds Seen") |>
  wb_add_data(x=seenHummers.tbl) |>
  wb_add_fill(dims=dimsHummerHeader, color=wb_color(name="lightgray")) |>
  wb_set_col_widths(cols=1:ncol(seenHummers.tbl), widths="auto") |>
  wb_add_filter(row=1, cols=1:ncol(seenHummers.tbl))
wb_save(wb,file=outxlsx) 

###### Miscellaneous searches #####################

seen.tbl |>
  filter(Location == "Mexico" & year(Date) == 1987)

seen.tbl |>
  filter(year(Date) == 2000)

seen.tbl |>
  filter(Location == "Thailand")

seen.tbl |>
  filter(grepl("hemispingus", CommonName,ignore.case=TRUE))

###### Do dates make sense? #####################

seen.tbl |>
  mutate(Year=year(Date)) |>
  count(Location, Year) |>
  group_by(Year)|>
  print(n=Inf) 

seen.tbl |>
  mutate(Month=month(Date)) |>
  count(Location, Month) |>
  group_by(Month) |>
  print(n=Inf)

seen.tbl |>
  mutate(Year=year(Date)) |>
  count(Year, Location) |>
  group_by(Location)|>
  print(n=Inf) 

###### Birds seen by location and date #####################

seen.tbl |>
  filter(year(Date) == 1987) |>
  mutate(Month=month(Date)) |>
  count(Location, Month) |>
  group_by(Month) |>
  print(n=Inf)

seen.tbl |>
  filter(year(Date) == 1987 & month(Date) == 8) |>
  mutate(Day = day(Date)) |>
  count(Location, Day) |>
  arrange(Day) |>
  print(n=Inf)
  
###### Familes not seen #####################

seenFam.tbl <-
  seen.tbl |>
  select(Family) |>
  unique() 

fam.tbl<-
  world.tbl |>
  select(Family) |>
  unique() 

notseenFam.tbl <-
  fam.tbl |>
  rbind(seenFam.tbl) |>
  table() |>
  as_tibble() |>
  filter(n == 1) |>
  select(Family) |>
  print(n=Inf)

###### Hummingbird genera seen and not seen #####################

hummersPlus.tbl <-
  hummers.tbl |>
  separate_wider_delim(ScientificName, delim=" ", names=c("Genus", NA),
                       cols_remove=FALSE) |>
  print()

seenGen.tbl <-
  hummersPlus.tbl |>
  filter(!is.na(Location)) |>
  select(Genus) |>
  unique() |>
  print()

notseenGen.tbl <-
  hummersPlus.tbl |>
  select(Genus) |>
  unique() |>
  rbind(seenGen.tbl) |>
  count(Genus) |>
  filter(n == 1) |>
  select(Genus) |>
  print(n=Inf)

genus.tbl <-
  hummersPlus.tbl |>
  right_join(notseenGen.tbl, by="Genus") |>
  select(c("CommonName", "ScientificName")) |>
  print()

###### Revise genus xlsx #####################

genus.xl <- read_excel(ingenxlsx)

out.tbl <- 
  genus.tbl |>
  left_join(genus.xl, by="CommonName") |>
  select(-ScientificName.y) |>
  rename(ScientificName = ScientificName.x) |>
  print()

tbldate <- date()
dimsHeader <- wb_dims(rows=1, cols=seq_len(ncol(out.tbl)))
wb <- wb_workbook() |>
  wb_set_base_font(font_size=16) |>
  wb_add_worksheet(sheet="Genera To See List",
                   header=c(NA,"Hummingbird Genera To See",NA),
                   footer=c(NA,tbldate,NA)) |>
  wb_add_data(x=out.tbl, na.strings = "") |>
  wb_add_fill(dims=dimsHeader, color=wb_color(name="lightgray")) |>
  wb_set_col_widths(cols=1:ncol(out.tbl), widths="auto") |>
  wb_add_filter(row=1, cols=1:ncol(out.tbl)) 
wb_save(wb,file=outgenxlsx) 






