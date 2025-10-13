# workbench_jpg_sdo.r
#
# Author: Thomas K. Friedli
#
# Last version: 2025-10-12 / Frt
# Created:      2019-09-09 / Frt
# ------------------------------------------------------------------------------

devtools::load_all()

# Load packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(jpeg)

# Set parameters ---------------------------------------------------------------

# local path names

inp_data_path = "F:/work/sunxplrr/work/in/"
out_data_path = "F:/work/sunxplrr/work/out/"

# online path names

# inp_root_path = "https://jsoc1.stanford.edu/data/hmi/images/"
# inp_year_path = "2024/"
# inp_month_path = "08/"
# inp_day_path = "04/"
# 
# inp_data_path = paste0(inp_root_path,inp_year_path,inp_month_path,inp_day_path)
# out_data_path = "F:/work/sunxplrr/work/out/"

# file names

inp_file_name = "20240804_084500_Ic_flat_4k.jpg" # SDO jsoc yellow flat

# file names and parameters ----------------------------------------------------

sdo.image      <- "TRUE"
cut.image      <- "TRUE" 
flip.image     <- "FALSE"
flop.image     <- "FALSE"
mean.method    <- "mode"
parse.filename <- "TRUE"
parse.method   <- "SDO/HMI"
zero.pos.angle <- "TRUE"
delta.p        <- 0
gamma          <- 1.5

## -----------------------------------------------------------------------------
# Import JPG image: details ----------------------------------------------------

# Imports image as matrix
imDat <- jpeg::readJPEG(paste0(inp_data_path,inp_file_name))[,,2]

# Converts image matrix to tibble
fitsim <- fun_mat2tibbl(imDat)

# ggplot(fitsim, aes(i, j)) +
#   geom_raster(aes(fill = x)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)

# Corrects gamma
fitsim <- fun_correct_gamma(fitsim, gamma = gamma, correct.gamma = "TRUE")

# ggplot(fitsim, aes(i, j)) +
#   geom_raster(aes(fill = x)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)

# Rotates image about -90 degrees
fitsim <- fun_rotate_jpg(fitsim, rotate.jpg = "TRUE")

# ggplot(fitsim, aes(i, j)) +
#   geom_raster(aes(fill = x)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)

# Import JPG image -------------------------------------------------------------

im <- fun_read_jpg_image(filename = paste0(inp_data_path,inp_file_name))

# ggplot(fun_mat2tibbl(im$imDat), aes(i, j)) +
#   geom_raster(aes(fill = x)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)

# FITS header und tibble hdrlst ------------------------------------------------

header <- im$header
hdrlst <- fun_hdr2list(im$hdr) 

# Auslesen des fehlenden DATE-OBS Keywords aus dem SDO Dateinamen --------------

parsed.keywords <- fun_parse_keywords(inp_file_name, header, hdrlst, 
                                      sdo.image, parse.method)

header <- parsed.keywords$header
hdrlst <- parsed.keywords$hdrlst

# Berechnet die Ephemeriden für die physischen Sonnenkoordinaten ---------------

sun.ephem <- fun_sun_ephem(header = header,
                           hdrlst = hdrlst,
                           zero.pos.angle = zero.pos.angle,
                           delta.p = delta.p)

header <- sun.ephem$header
hdrlst <- sun.ephem$hdrlst

# Konvertierung von FITS imDat in eine tibble ----------------------------------

fitsim <- fun_mat2tibbl(im$imDat)

# Plottet Bilddatei ------------------------------------------------------------

# ggplot(fitsim, aes(i, j)) +
#   geom_raster(aes(fill = x)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)

# Konvertierung von einer tibble in ein FITS imDat -----------------------------

im$imDat <- fun_tibbl2mat(x = fitsim)

# Speichern FITS imDat als FITS Datei ------------------------------------------

out_file_name = "test_fitsim_save.fit"
fun_write_image(im$imDat, paste0(out_data_path, out_file_name), hdrlst, header)

## -----------------------------------------------------------------------------
## Modul Importieren Bilddatei -------------------------------------------------

mod.frame.import <- mod_frame_import(inp_data_path = inp_data_path,
                                     inp_file_name = inp_file_name,
                                     parse.filename = parse.filename,
                                     parse.method = parse.method,
                                     zero.pos.angle = zero.pos.angle,
                                     delta.p = delta.p,
                                     flip.image = flip.image,
                                     flop.image = flop.image)

fitsim     <- mod.frame.import$fitsim
hdrlst     <- mod.frame.import$hdrlst
header     <- mod.frame.import$header

# ggplot(fitsim, aes(i, j)) +
#   geom_raster(aes(fill = x)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)

fitsim.save <- fitsim %>% 
  select(i, j, x = x)

imDat <- fun_tibbl2mat(fitsim.save)

out_file_name = "test_mod_frame.import.fit"
fun_write_image(imDat, paste0(out_data_path, out_file_name), hdrlst, header)

## -----------------------------------------------------------------------------
# Auslesen einer Bildzeile -----------------------------------------------------

row <- 1273

fitsim.row <- fun_extract_row(fitsim, row = row)

# ggplot(fitsim.row, aes(i, x)) +
#   geom_line()

# Auslesen einer Bildspalte ----------------------------------------------------

col <- 1255

fitsim.col <- fun_extract_col(fitsim, col = col)

# ggplot(fitsim.col, aes(j, x)) +
#   geom_line()

# Wegschneiden von Text an den Bildrändern -------------------------------------

cut.threshold = 20
cut.method = "relative"
cut.border.pix = 100

fitsim.cut <- fun_cut_image(fitsim, hdrlst = hdrlst, header = header,
                            cut.image = cut.image,
                            cut.threshold = cut.threshold, 
                            cut.method = cut.method,
                            cut.border.pix = cut.border.pix)

# ggplot(fitsim.cut, aes(i, j)) +
#   geom_raster(aes(fill = x)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)

fitsim.cut.save <- fitsim.cut %>% 
  select(i, j, x=x)

imDat <- fun_tibbl2mat(fitsim.cut.save)

out_file_name = "test_fitsim.cut.fit"
fun_write_image(imDat, paste0(out_data_path, out_file_name), hdrlst, header)

# Generieren einer Maske -------------------------------------------------------

threshold <- 10
method = "relative"

fitsim.mask <- fun_mask_create(fitsim, hdrlst = hdrlst, 
                               threshold = threshold, method = method)

fitsim.mask.threshold <- fitsim.mask$mask.threshold
fitsim.mask           <- fitsim.mask$mask

# ggplot(fitsim.mask, aes(i, j)) +
#   geom_raster(aes(fill = th)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)

fitsim.mask.save <- fitsim.mask %>% 
  select(i, j, x=th)

imDat <- fun_tibbl2mat(fitsim.mask.save)

out_file_name = "test_fitsim.mask.fit"
fun_write_image(imDat, paste0(out_data_path, out_file_name), hdrlst, header)

# Konturieren einer generierten Maske ------------------------------------------

fitsim.border <- fun_mask_border(x=fitsim.mask, mask = "th")

# ggplot(fitsim.border, aes(i, j)) +
#   geom_raster(aes(fill = border)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)

fitsim.border.save <- fitsim.border %>% 
  select(i, j, x=border)

imDat <- fun_tibbl2mat(fitsim.border.save)

out_file_name = "test_fitsim.border.fit"
fun_write_image(imDat, paste0(out_data_path, out_file_name), hdrlst, header)

# Ausfüllen einer konturierten Maske -------------------------------------------

fitsim.disc <- fun_mask_fill(fitsim.border, mask = "border")

# ggplot(fitsim.disc, aes(i, j)) +
#   geom_raster(aes(fill = fill)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)

fitsim.disc.save <- fitsim.disc %>% 
  select(i, j, x=fill)

imDat <- fun_tibbl2mat(fitsim.disc.save)

out_file_name = "test_fitsim.disc.fit"
fun_write_image(imDat, paste0(out_data_path, out_file_name), hdrlst, header)

# Berechnen Scheibenzentrum und Scheibenradius ---------------------------------

disc.center <- fun_disc_center(fitsim.disc, disc = "fill")

hdrlst$CENTER_X <- disc.center$x_i
hdrlst$CENTER_Y <- disc.center$y_j
hdrlst$RADIUS   <- disc.center$r

# Zeichnen eines Kreises mit bekanntem Zentrum und Radius ----------------------

disc.circle <- fun_mask_circle(fitsim, disc.center, border.pix = 0)

# ggplot(disc.circle, aes(i, j)) +
#   geom_raster(aes(fill = circle)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)

disc.circle.save <- disc.circle %>% 
  select(i, j, x=circle)

imDat <- fun_tibbl2mat(disc.circle.save)

out_file_name = "test_disc.circle.fit"
fun_write_image(imDat, paste0(out_data_path, out_file_name), hdrlst, header)

# Ausfüllen eines berechneten Kreises ------------------------------------------

disc.mask <- fun_mask_fill(disc.circle, mask = "circle")

# ggplot(disc.mask, aes(i, j)) +
#   geom_raster(aes(fill = fill)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)

disc.mask.save <- disc.mask %>% 
  select(i, j, x = fill)

imDat <- fun_tibbl2mat(disc.mask.save)

out_file_name = "test_disc.mask.fit"
fun_write_image(imDat, paste0(out_data_path, out_file_name), hdrlst, header)

# Sonnenscheibe ausschneiden ---------------------------------------------------

disc.info <- fitsim.border %>% 
  left_join(disc.circle, by=c("i","j")) %>% 
  select(-th)

disc.image <- fun_disc_math(im1 = disc.info, values_1 = "x", 
                            im2 = disc.mask, values_2 = "fill",
                            method = "mult", values.name = "image")

# ggplot(disc.image, aes(i, j)) +
#   geom_raster(aes(fill = image)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)

disc.image.save <- disc.image %>% 
  select(i, j, x = image)

imDat <- fun_tibbl2mat(disc.image.save)

out_file_name = "test_disc.image.fit"
fun_write_image(imDat, paste0(out_data_path, out_file_name), hdrlst, header)

# Bildqualität: Unruhe des Sonnenrandes ----------------------------------------  

disc.quality <- fun_disc_quality(disc.image, hdrlst = hdrlst,
                                 disc.center = disc.center, 
                                 border = "border")

disc.quality.data <- disc.quality$data
hdrlst$IMQTYPIX   <- disc.quality$sigma.border$sd.pix
hdrlst$IMQTYARC   <- disc.quality$sigma.border$sd.arcsec

# hist(disc.quality.data$res)

# Differenz ursprüngliches Bild zu ausgeschnittenem Bild -----------------------

image.diff <- fun_disc_math(im1 = fitsim, values_1 = "x", 
                            im2 = disc.image, values_2 = "image", 
                            method = "diff", values.name = "delta")

# ggplot(image.diff, aes(i, j)) +
#   geom_raster(aes(fill = delta)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)

image.diff.save <- image.diff %>% 
  select(i, j, x = delta)

imDat <- fun_tibbl2mat(image.diff.save)

out_file_name = "test_im.diff.fit"
fun_write_image(imDat, paste0(out_data_path, out_file_name), hdrlst, header)

# Berechnen der heliografischen Positionsangaben jedes Pixels ------------------

disc.coordinates <- fun_disc_coordinates(disc.image, hdrlst)

# Berechnen eines Gradnetzes ---------------------------------------------------

disc.grid <- fun_disc_grid(disc.coordinates, hdrlst,
                           grid_each_deg = 10, res_each_deg_on_grid = 0.01)

# ggplot(disc.grid, aes(i, j)) +
#   geom_raster(aes(fill = grid)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)

disc.grid.save <- disc.grid %>% 
  select(i, j, x = grid)

imDat <- fun_tibbl2mat(disc.grid.save)

out_file_name = "test_disc.grid.fit"
fun_write_image(imDat, paste0(out_data_path, out_file_name), hdrlst, header)

# Darstellen eines Gradnetzes --------------------------------------------------

image.grid <- fun_grid_plt(disc.grid, hdrlst)

# ggplot(image.grid, aes(i, j)) +
#   geom_raster(aes(fill = image.grid)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)

image.grid.save <- image.grid %>% 
  select(i, j, x = image.grid)

imDat <- fun_tibbl2mat(image.grid.save)

out_file_name = "test_image.grid.fit"
fun_write_image(imDat, paste0(out_data_path, out_file_name), hdrlst, header)

# Zentriert das Bild -----------------------------------------------------------

im_to_cent <- disc.image %>% 
  select(i, j, x = image)

im_center <- fun_center_image(image = im_to_cent, hdrlst = hdrlst,
                              sdo.image = sdo.image)

# ggplot(im_center, aes(i, j)) +
#   geom_raster(aes(fill = x)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)

imCenter <- fun_tibbl2mat(im_center)

out_file_name = "test_center_image.fit"
fun_write_image(imCenter, paste0(out_data_path, out_file_name), hdrlst, header)

## -----------------------------------------------------------------------------
## Modul Identifizieren der Sonnenscheibe --------------------------------------

mod.disc.image <- mod_disc_image(fitsim, 
                                 hdrlst = hdrlst, 
                                 header = header,
                                 cut.image = cut.image,
                                 threshold = 10, 
                                 method = "relative", 
                                 cut.threshold = 20, 
                                 cut.method = "relative", 
                                 cut.border.pix = 100, 
                                 add.border.pix = 0,
                                 image.values.name = "image", 
                                 grid_each_deg = 10, 
                                 res_each_deg_on_grid = 0.01)

discim     <- mod.disc.image$disc.image
hdrlst     <- mod.disc.image$hdrlst
header     <- mod.disc.image$header

# ggplot(discim, aes(i, j)) +
#   geom_raster(aes(fill = image)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)

# ggplot(discim, aes(i, j)) +
#   geom_raster(aes(fill = image.grid)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)

discim.save <- discim %>% 
  select(i, j, x = image)

imDat <- fun_tibbl2mat(discim.save)

out_file_name = "test_mod_disc.image.fit"
fun_write_image(imDat, paste0(out_data_path, out_file_name), hdrlst, header)

## -----------------------------------------------------------------------------
## Modul Flatkorrektur ---------------------------------------------------------

# mean.method = "mean"
# mean.method = "median"
# mean.method = "mode"

mod.disc.flat <- mod_disc_flat(discim,
                               hdrlst = hdrlst,
                               header = header,
                               mean.method = mean.method,
                               sdo.image = sdo.image)

disc.flat     <- mod.disc.flat$disc.flat
hdrlst        <- mod.disc.flat$hdrlst
header        <- mod.disc.flat$header

# ggplot(disc.flat, aes(i, j)) +
#   geom_raster(aes(fill = flat)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)
# 
# ggplot(fun_extract_row(disc.flat, row = 1100), aes(i, flat)) +
#   geom_line()

disc.flat.save <- disc.flat %>% 
  mutate(flat = 10000*flat) %>%
  select(i, j, x = flat)

imDat <- fun_tibbl2mat(disc.flat.save)

out_file_name = "test_mod_disc.flat.fit"
fun_write_image(imDat, paste0(out_data_path, out_file_name), hdrlst, header)

## -----------------------------------------------------------------------------
# Histogramm der Intensity Contrasts -------------------------------------------

disc.flat.in <- disc.flat %>%
  filter(fill > 0) %>% 
  select(i,j,x = flat)

hist(disc.flat.in$x, breaks=500)

## -----------------------------------------------------------------------------
# Maskieren der Flecken --------------------------------------------------------

# spot.threshold  <- 0.85 # spots e.g. penumbrae & umbrae
# umbra.threshold <- 0.55 # umbra

spot.threshold  <- 0.85
umbra.threshold <- 0.55
method = "absolute"

disc.flat.in <- disc.flat %>% 
  filter(fill > 0) %>%
  select(i,j,x=flat)

disc.spot.mask <- fun_mask_create(disc.flat.in, hdrlst = hdrlst, 
                                  threshold = spot.threshold,
                                  min.mask.value = umbra.threshold,
                                  method = method)

disc.spot.threshold <- disc.spot.mask$mask.threshold

disc.spot.in <- disc.spot.mask$mask |> 
  select(i, j, spot = th)

disc.spot <- disc.flat |> 
  left_join(disc.spot.in, by=c("i","j")) |> 
  mutate(spot = if_else(is.na(spot),0,spot))

# ggplot(disc.spot, aes(i, j)) +
#   geom_raster(aes(fill = spot)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)

disc.spot.save <- disc.spot %>% 
  mutate(x = 55000 * spot) %>%
  select(i, j, x)

imDat <- fun_tibbl2mat(disc.spot.save)

out_file_name = "test_disc.spot.fit"
fun_write_image(imDat, paste0(out_data_path, out_file_name), hdrlst, header)

# Maskieren der Umbren ---------------------------------------------------------

# spot.threshold  <- 0.85 # spots e.g. penumbrae & umbrae
# umbra.threshold <- 0.55 # umbra

spot.threshold  <- 0.85
umbra.threshold <- 0.55
method = "absolute"

disc.flat.in <- disc.flat %>% 
  filter(fill > 0) %>%
  select(i,j,x=flat)

disc.umbra.mask <- fun_mask_create(disc.flat.in, hdrlst = hdrlst, 
                                   threshold = umbra.threshold,
                                   min.mask.value = 0,
                                   method = method)

disc.umbra.threshold <- disc.umbra.mask$mask.threshold

disc.umbra.in <- disc.umbra.mask$mask |> 
  select(i, j, umbra = th)

disc.umbra <- disc.spot |> 
  left_join(disc.umbra.in, by=c("i","j")) |> 
  mutate(umbra = if_else(is.na(umbra),0,umbra))

# ggplot(disc.umbra, aes(i, j)) +
#   geom_raster(aes(fill = umbra)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)

disc.umbra.save <- disc.umbra %>% 
  mutate(x = 55000 * umbra) %>%
  select(i, j, x)

imDat <- fun_tibbl2mat(disc.umbra.save)

out_file_name = "test_disc.umbra.fit"
fun_write_image(imDat, paste0(out_data_path, out_file_name), hdrlst, header)

# Maskieren der Penumbren und der Umbren ---------------------------------------

disc.umbra.red <- disc.umbra %>% 
  select(-umbra)

disc.penandum <- fun_disc_math(im1 = disc.umbra.red, values_1 = "spot", 
                               im2 = disc.umbra, values_2 = "umbra",
                               method = "mult", values.name = "penandum")

# ggplot(disc.penandum, aes(i, j)) +
#   geom_raster(aes(fill = penandum)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)

disc.penandum.save <- disc.penandum %>% 
  mutate(x = 55000 * penandum) %>%
  select(i, j, x)

imDat <- fun_tibbl2mat(disc.penandum.save)

out_file_name = "test_disc.penandum.fit"
fun_write_image(imDat, paste0(out_data_path, out_file_name), hdrlst, header)

# Markieren der Flecken --------------------------------------------------------

# spot.threshold  <- 0.85 # spots e.g. penumbrae & umbrae
# umbra.threshold <- 0.66 # umbra

spot.threshold  <- 0.85
umbra.threshold <- 0.66
method = "absolute"

disc.flat.in <- disc.flat %>%
  select(i,j,x=flat)

disc.sparea <- fun_mask_diff(disc.flat.in, hdrlst = hdrlst, 
                             lower.threshold = 0, 
                             upper.threshold = spot.threshold, 
                             method = method)

disc.sparea.umbra.threshold  <- disc.sparea$mask.lower.threshold
disc.sparea.spot.threshold   <- disc.sparea$mask.upper.threshold
disc.sparea.mask            <- disc.sparea$mask

# ggplot(disc.sparea.mask, aes(i, j)) +
#   geom_raster(aes(fill = th)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)

disc.sparea.save <- disc.sparea.mask %>% 
  mutate(x = 55000 * th) %>%
  select(i, j, x)

imDat <- fun_tibbl2mat(disc.sparea.save)

out_file_name = "test_disc.sparea.fit"
fun_write_image(imDat, paste0(out_data_path, out_file_name), hdrlst, header)

# Markieren der Umbren ---------------------------------------------------------

# spot.threshold  <- 0.85 # spots e.g. penumbrae & umbrae
# umbra.threshold <- 0.66 # umbra

spot.threshold  <- 0.85
umbra.threshold <- 0.66
method = "absolute"

disc.flat.in <- disc.flat %>%
  select(i,j,x=flat)

disc.umarea <- fun_mask_diff(disc.flat.in, hdrlst = hdrlst, 
                             lower.threshold = 0, 
                             upper.threshold = umbra.threshold, 
                             method = method)

disc.umarea.umbra.threshold  <- disc.umarea$mask.lower.threshold
disc.umarea.spot.threshold   <- disc.umarea$mask.upper.threshold
disc.umarea.mask             <- disc.umarea$mask

# ggplot(disc.umarea.mask, aes(i, j)) +
#   geom_raster(aes(fill = th)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)

disc.umarea.save <- disc.umarea.mask %>% 
  mutate(x = 55000 * th) %>%
  select(i, j, x)

imDat <- fun_tibbl2mat(disc.umarea.save)

out_file_name = "test_disc.umarea.fit"
fun_write_image(imDat, paste0(out_data_path, out_file_name), hdrlst, header)

# Markieren der Penumbra -------------------------------------------------------

# spot.threshold  <- 0.85 # spots e.g. penumbrae & umbrae
# umbra.threshold <- 0.66 # umbra

spot.threshold  <- 0.85
umbra.threshold <- 0.66
method = "absolute"

disc.flat.in <- disc.flat %>%
  select(i,j,x=flat)

disc.penarea <- fun_mask_diff(disc.flat.in, hdrlst = hdrlst, 
                              lower.threshold = umbra.threshold, 
                              upper.threshold = spot.threshold, 
                              method = method)

disc.penarea.umbra.threshold  <- disc.penarea$mask.lower.threshold
disc.penarea.spot.threshold   <- disc.penarea$mask.upper.threshold
disc.penarea.mask             <- disc.penarea$mask

# ggplot(disc.penarea.mask, aes(i, j)) +
#   geom_raster(aes(fill = th)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)

disc.penarea.save <- disc.penarea.mask %>% 
  mutate(x = 55000 * th) %>%
  select(i, j, x)

imDat <- fun_tibbl2mat(disc.penarea.save)

out_file_name = "test_disc.penarea.fit"
fun_write_image(imDat, paste0(out_data_path, out_file_name), hdrlst, header)

## -----------------------------------------------------------------------------
## Modul Spot Extraktion -------------------------------------------------------

# spot.threshold  <- 0.85 # spots e.g. penumbrae & umbrae
# umbra.threshold <- 0.55 # umbra

spot.threshold  <- 0.85
umbra.threshold <- 0.55

mod.spot.extraction <- mod_spot_extraction(disc.flat, 
                                           hdrlst, 
                                           header, 
                                           spot.threshold = spot.threshold, 
                                           umbra.threshold = umbra.threshold)

disc.features <- mod.spot.extraction$disc.features
hdrlst        <- mod.spot.extraction$hdrlst
header        <- mod.spot.extraction$header

# ggplot(disc.features, aes(i, j)) +
#   geom_raster(aes(fill = penandum)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)

disc.features.save <- disc.features %>%
  mutate(penandum = 55000*penandum) %>%
  select(i, j, x = penandum)

imDat <- fun_tibbl2mat(disc.features.save)

out_file_name = "test_mod_spot.extraction.fit"
fun_write_image(imDat, paste0(out_data_path, out_file_name), hdrlst, header)

## -----------------------------------------------------------------------------
## Modul Spot indices and charts calculation -----------------------------------

xmin       <- -90
xmax       <- 90
ymin       <- -90
ymax       <- 90
resolution <- 0.05

mod.spot.indices <- mod_spot_indices(disc.features, hdrlst, header, 
                                     xmin = xmin, xmax = xmax, 
                                     ymin = ymin, ymax = ymax, 
                                     resolution = resolution)

disc.indices       <- mod.spot.indices$images
disc.charts        <- mod.spot.indices$charts
total.indices      <- mod.spot.indices$total.indices 
hemisphere.indices <- mod.spot.indices$hemisphere.indices
latitude.indices   <- mod.spot.indices$latitude.indices
hdrlst             <- mod.spot.indices$hdrlst
header             <- mod.spot.indices$header


# ggplot(disc.indices, aes(i, j)) +
#   geom_raster(aes(fill = 55000*flat)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)
# 
# ggplot(disc.indices, aes(i, j)) +
#   geom_raster(aes(fill = 55000*penandum)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)
# 
# 
# ggplot(disc.charts, aes(longitude, latitude)) +
#   geom_raster(aes(fill = 55000*flat)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)
# 
# ggplot(disc.charts, aes(longitude, latitude)) +
#   geom_raster(aes(fill = 55000*penandum)) +
#   scale_fill_gradient(low="black", high="white") +
#   coord_fixed(ratio = 1)


# disc flat

disc.indices.save <- disc.indices %>%
  mutate(flat = 55000*flat) %>%
  select(i, j, x = flat)

imDat <- fun_tibbl2mat(disc.indices.save)

out_file_name = "test_mod_spot.indices_flat.fit"
fun_write_image(imDat, paste0(out_data_path, out_file_name), hdrlst, header)

# disc penandum

disc.indices.save <- disc.indices %>%
  mutate(penandum = 55000*penandum) %>%
  select(i, j, x = penandum)

imDat <- fun_tibbl2mat(disc.indices.save)

out_file_name = "test_mod_spot.indices_penandum.fit"
fun_write_image(imDat, paste0(out_data_path, out_file_name), hdrlst, header)

# chart flat

disc.charts.save <- disc.charts %>%
  mutate(flat = 55000*flat) %>%
  select(i = longitude, j = latitude, x = flat)

imDat.charts <- fun_tibbl2mat(disc.charts.save)

out_file_name = "test_mod_spot_charts_flat.fit"
fun_write_image(imDat.charts, paste0(out_data_path, out_file_name), hdrlst, header)

# chart penandum

disc.charts.save <- disc.charts %>%
  mutate(penandum = 55000*penandum) %>%
  select(i = longitude, j = latitude, x = penandum)

imDat.charts <- fun_tibbl2mat(disc.charts.save)

out_file_name = "test_mod_spot_charts_penandum.fit"
fun_write_image(imDat.charts, paste0(out_data_path, out_file_name), hdrlst, header)

## -----------------------------------------------------------------------------
## Modul Output ----------------------------------------------------------------

mod.output <- mod_output(disc.indices,
                         hdrlst, 
                         header,
                         inp_file_name,
                         out_data_path,
                         total.indices, 
                         hemisphere.indices,
                         latitude.indices, 
                         charts = disc.charts, 
                         rds.output = "FALSE",
                         full.output = "FALSE",
                         light.save = "FALSE",
                         fits.save = "FALSE",
                         jpg.save = "TRUE")

images             <- mod.output$images
hdrlst             <- mod.output$hdrlst
header             <- mod.output$header
total.indices      <- mod.output$total.indices 
hemisphere.indices <- mod.output$hemisphere.indices
latitude.indices   <- mod.output$latitude.indices
charts             <- mod.output$charts

## -----------------------------------------------------------------------------


