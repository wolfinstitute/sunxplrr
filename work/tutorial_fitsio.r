# tutorial_fitsio.r
#
# Author: Thomas K. Friedli
#
# Last version: 2020-01-20 / Frt
# Created:      2019-09-09 / Frt
# ------------------------------------------------------------------------------

# Load packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

devtools::load_all(".")

# Set path and file names ------------------------------------------------------

inp_data_path = "./data/"
out_data_path = "./work/out/"

sdo.image <- "FALSE"
cut.image <- "FALSE"

# inp_file_name = "20191118k233.fit"          # Plage from new cycle
# inp_file_name = "20191026k227.fit"          # No activity
# inp_file_name = "20191019k228.fit"          # No activity, Jeannine
# inp_file_name = "20191013k225.fit"          # No activity
# inp_file_name = "20190628k147.fit"          # Very low activity, Jeannine
# inp_file_name = "20180228k026.fit"          # With clouds
 inp_file_name = "20141024K814.fit"          # Very high activity
# inp_file_name = "20141024K814_8bit.fit"     # Very high activity
# inp_file_name = "20140610K331.fit"          # High activity
# inp_file_name = "20120422K350.fit"          # Template for w, h und k
#
# sdo.image <- "TRUE"
# cut.image <- "TRUE"
# 
# inp_file_name = "20141024_080255_2048_1700.fits"
# inp_file_name = "20191116_080341_2048_1700.fits"

## -----------------------------------------------------------------------------
# Import FITS frame ------------------------------------------------------------

im <- fun_read_image(filename = paste0(inp_data_path,inp_file_name))

# FITS header und tibble hdrlst ------------------------------------------------

header <- im$header
hdrlst <- fun_hdr2list(im$hdr) 

# Auslesen des fehlenden DATE-OBS Keywords aus dem SDO Dateinamen --------------

sdo.keywords <- fun_sdo_keywords(inp_file_name, header, hdrlst, sdo.image)

header <- sdo.keywords$header
hdrlst <- sdo.keywords$hdrlst

# Berechnen der Ephemeriden für die physischen Sonnenkoordinaten ---------------

sun.ephem <- fun_sun_ephem(hdrlst = hdrlst, sdo.image = sdo.image) 

# Konvertierung von FITS imDat in eine tibble ----------------------------------

fitsim <- fun_mat2tibbl(im$imDat)

# Plotten der einer Bilddatei --------------------------------------------------

ggplot(fitsim, aes(i, j)) +
  geom_raster(aes(fill = x)) +
  scale_fill_continuous(type = "gradient") +
  coord_fixed(ratio = 1)

# Konvertierung von einer tibble in ein FITS imDat -----------------------------

im$imDat <- fun_tibbl2mat(fitsim)

# Speichern FITS imDat als FITS Datei ------------------------------------------

out_file_name = "test_fitsim_save.fit"
fun_write_image(im$imDat, paste0(out_data_path,out_file_name), hdrlst, header)

# Flipt das Bild ---------------------------------------------------------------

im_flip <- fun_flip_image(image = fitsim, hdrlst = hdrlst, header = header,
                          sdo.image = sdo.image)

hdrlst  <- im_flip$hdrlst
header  <- im_flip$header
im_flip <- im_flip$flipped_image

# ggplot(im_flip, aes(i, j)) +
#   geom_raster(aes(fill = x)) +
#   coord_fixed(ratio = 1)

imFlip <- fun_tibbl2mat(im_flip)

out_file_name = "test_flip_image.fit"
fun_write_image(imFlip, paste0(out_data_path,out_file_name), hdrlst, header)

# Flopt das Bild ---------------------------------------------------------------

im_flop <- fun_flop_image(image = fitsim, hdrlst = hdrlst, header = header,
                          sdo.image = sdo.image)

hdrlst  <- im_flop$hdrlst
header  <- im_flop$header
im_flop <- im_flop$flopped_image

# ggplot(im_flop, aes(i, j)) +
#   geom_raster(aes(fill = x)) +
#   coord_fixed(ratio = 1)

imFlop <- fun_tibbl2mat(im_flop)

out_file_name = "test_flop_image.fit"
fun_write_image(imFlop, paste0(out_data_path,out_file_name), hdrlst, header)

## -----------------------------------------------------------------------------
## Modul Importieren FITS Datei

mod.fits.import <- mod_fits_import(inp_data_path = inp_data_path, 
                                   inp_file_name = inp_file_name,
                                   sdo.image = sdo.image, 
                                   cut.image = cut.image)

fitsim     <- mod.fits.import$fitsim
hdrlst     <- mod.fits.import$hdrlst
header     <- mod.fits.import$header

# ggplot(fitsim, aes(i, j)) +
#   geom_raster(aes(fill = x)) +
#   coord_fixed(ratio = 1)

fitsim.save <- fitsim %>% 
  select(i, j, x = x)

imDat <- fun_tibbl2mat(fitsim.save)

out_file_name = "test_mod_fits.import.fit"
fun_write_image(imDat, paste0(out_data_path,out_file_name), hdrlst, header)

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

# Generieren einer Maske -------------------------------------------------------

# threshold <- 8000
# method = "absolute"

threshold <- 10
method = "relative"

fitsim.mask <- fun_mask_create(fitsim, hdrlst = hdrlst, threshold = threshold,
                               method = method)

fitsim.mask.threshold <- fitsim.mask$mask.threshold
fitsim.mask           <- fitsim.mask$mask

# ggplot(fitsim.mask, aes(i, j)) +
#   geom_raster(aes(fill = th)) +
#   coord_fixed(ratio = 1)

fitsim.mask.save <- fitsim.mask %>% 
  select(i, j, x=th)

imDat <- fun_tibbl2mat(fitsim.mask.save)

out_file_name = "test_fitsim.mask.fit"
fun_write_image(imDat, paste0(out_data_path,out_file_name), hdrlst, header)

# Konturieren einer generierten Maske ------------------------------------------

fitsim.border <- fun_mask_border(fitsim.mask, mask = "th")

# ggplot(fitsim.border, aes(i, j)) +
#   geom_raster(aes(fill = border)) +
#   coord_fixed(ratio = 1)

fitsim.border.save <- fitsim.border %>% 
  select(i, j, x=border)

imDat <- fun_tibbl2mat(fitsim.border.save)

out_file_name = "test_fitsim.border.fit"
fun_write_image(imDat, paste0(out_data_path,out_file_name), hdrlst, header)

# Ausfüllen einer konturierten Maske -------------------------------------------

fitsim.disc <- fun_mask_fill(fitsim.border, mask = "th")

# ggplot(fitsim.disc, aes(i, j)) +
#   geom_raster(aes(fill = fill)) +
#   coord_fixed(ratio = 1)

fitsim.disc.save <- fitsim.disc %>% 
  select(i, j, x=fill)

imDat <- fun_tibbl2mat(fitsim.disc.save)

out_file_name = "test_fitsim.disc.fit"
fun_write_image(imDat, paste0(out_data_path,out_file_name), hdrlst, header)

# Berechnen Scheibenzentrum und Scheibenradius ---------------------------------

disc.center <- fun_disc_center(fitsim.disc, disc = "fill")

hdrlst$CENTER_X <- disc.center$x_i
hdrlst$CENTER_Y <- disc.center$y_j
hdrlst$RADIUS   <- disc.center$r

# Zeichnen eines Kreises mit bekanntem Zentrum und Radius ----------------------

disc.circle <- fun_mask_circle(fitsim, disc.center, border.pix = 0)

# ggplot(disc.circle, aes(i, j)) +
#   geom_raster(aes(fill = circle)) +
#   coord_fixed(ratio = 1)

disc.circle.save <- disc.circle %>% 
  select(i, j, x=circle)

imDat <- fun_tibbl2mat(disc.circle.save)

out_file_name = "test_disc.circle.fit"
fun_write_image(imDat, paste0(out_data_path,out_file_name), hdrlst, header)

# Ausfüllen eines berechneten Kreises ------------------------------------------

disc.mask <- fun_mask_fill(disc.circle, mask = "circle")

# ggplot(disc.mask, aes(i, j)) +
#   geom_raster(aes(fill = fill)) +
#   coord_fixed(ratio = 1)

disc.mask.save <- disc.mask %>% 
  select(i, j, x = fill)

imDat <- fun_tibbl2mat(disc.mask.save)

out_file_name = "test_disc.mask.fit"
fun_write_image(imDat, paste0(out_data_path,out_file_name), hdrlst, header)

# Sonnenscheibe ausschneiden ---------------------------------------------------

disc.info <- fitsim.border %>% 
  left_join(disc.circle, by=c("i","j")) %>% 
  select(-th)

disc.image <- fun_disc_math(im1 = disc.info, values_1 = "x", im2 = disc.mask, 
                            values_2 = "fill", method = "mult", 
                            values.name = "image")

# ggplot(disc.image, aes(i, j)) +
#   geom_raster(aes(fill = image)) +
#   coord_fixed(ratio = 1)

disc.image.save <- disc.image %>% 
  select(i, j, x = image)

imDat <- fun_tibbl2mat(disc.image.save)

out_file_name = "test_disc.image.fit"
fun_write_image(imDat, paste0(out_data_path,out_file_name), hdrlst, header)

# Differenz ursprüngliches Bild zu ausgeschnittenem Bild -----------------------

image.diff <- fun_disc_math(im1 = fitsim, values_1 = "x", im2 = disc.image, 
                            values_2 = "image", method = "diff", 
                            values.name = "delta")

# ggplot(image.diff, aes(i, j)) +
#   geom_raster(aes(fill = delta)) +
#   coord_fixed(ratio = 1)

image.diff.save <- image.diff %>% 
  select(i, j, x = delta)

imDat <- fun_tibbl2mat(image.diff.save)

out_file_name = "test_im.diff.fit"
fun_write_image(imDat, paste0(out_data_path,out_file_name), hdrlst, header)

# Berechnen der heliografischen Positionsangaben jedes Pixels ------------------

disc.coordinates <- fun_disc_coordinates(disc.image, hdrlst)

# Berechnen eines Gradnetzes ---------------------------------------------------

disc.grid <- fun_disc_grid(disc.coordinates, hdrlst,
                           grid_each_deg = 10, res_each_deg_on_grid = 0.01)

# ggplot(disc.grid, aes(i, j)) +
#   geom_raster(aes(fill = grid)) +
#   coord_fixed(ratio = 1)

disc.grid.save <- disc.grid %>% 
  select(i, j, x = grid)

imDat <- fun_tibbl2mat(disc.grid.save)

out_file_name = "test_disc.grid.fit"
fun_write_image(imDat, paste0(out_data_path,out_file_name), hdrlst, header)

# Darstellen eines Gradnetzes ---------------------------------------------------

image.grid <- fun_grid_plt(disc.grid, hdrlst)

image.grid <- fun_grid_plt(discim, hdrlst)

im.row <- fun_extract_row(discim, row = as.integer(hdrlst$CENTER_X))

ggplot(im.row) +
  geom_line(aes(i, x))


# ggplot(image.grid, aes(i, j)) +
#   geom_raster(aes(fill = image.grid)) +
#   coord_fixed(ratio = 1)

image.grid.save <- image.grid %>% 
  select(i, j, x = image.grid)

imDat <- fun_tibbl2mat(image.grid.save)

out_file_name = "test_image.grid.fit"
fun_write_image(imDat, paste0(out_data_path,out_file_name), hdrlst, header)

# Zentriert das Bild ----------------------------------------------------------

im_to_cent <- discim %>% 
  select(i,j,x=image)

im_center <- fun_center_image(image = im_to_cent, hdrlst = hdrlst,
                              sdo.image = sdo.image)

# ggplot(im_center, aes(i, j)) +
#   geom_raster(aes(fill = x)) +
#   coord_fixed(ratio = 1)

imCenter <- fun_tibbl2mat(im_center)

out_file_name = "test_center_image.fit"
fun_write_image(imCenter, paste0(out_data_path,out_file_name), hdrlst, header)

## -----------------------------------------------------------------------------
## Modul Identifizieren der Sonnenscheibe

mod.disc.image <- mod_disc_image(fitsim, hdrlst = hdrlst, header = header, 
                                 threshold = 10, method = "relative", 
                                 cut.threshold = 20, cut.method = "relative", 
                                 cut.border.pix = 100, add.border.pix = 0,
                                 image.values.name = "image", 
                                 grid_each_deg = 10, 
                                 res_each_deg_on_grid = 0.01)

discim     <- mod.disc.image$disc.image
hdrlst     <- mod.disc.image$hdrlst
header     <- mod.disc.image$header

# ggplot(discim, aes(i, j)) +
#   geom_raster(aes(fill = image)) +
#   coord_fixed(ratio = 1)

discim.save <- discim %>% 
  select(i, j, x = image)

imDat <- fun_tibbl2mat(discim.save)

out_file_name = "test_mod_disc.image.fit"
fun_write_image(imDat, paste0(out_data_path,out_file_name), hdrlst, header)

## -----------------------------------------------------------------------------
# Iteratives Schätzen eines Polynoms 5. Grades ---------------------------------

clv.fit <- fun_clv_fitting(discim, hdrlst, 
                           model = "poly_with_plane", 
                           run = 3,
                           clip.resid.out = "TRUE")

disc.clv  <- clv.fit$z
clv.coeff <- clv.fit$clv.coeff
fit.coeff <- clv.fit$fit.coeff
fit.sigma <- clv.fit$fit.sigma
fit.clip.at <- clv.fit$fit.clip.at

sum.clv.i0  <- sum(clv.coeff)

# ggplot(disc.clv, aes(i, j)) +
#   geom_raster(aes(fill = clv)) +
#   coord_fixed(ratio = 1)
# 
# ggplot(disc.clv, aes(i, j)) +
#   geom_raster(aes(fill = resid3)) +
#   coord_fixed(ratio = 1)

disc.clv.resid <- disc.clv %>% 
  mutate(resid = 2^(as.integer(hdrlst$BITPIX) - 1) + resid) %>% 
  mutate(resid = if_else(resid < 0, 0, resid)) %>% 
  select(i, j, x=resid)

# ggplot(disc.clv.resid, aes(i, j)) +
#   geom_raster(aes(fill = x)) +
#   coord_fixed(ratio = 1)
 
# im.row <- fun_extract_row(disc.clv.resid, row = as.integer(hdrlst$CENTER_X))
# 
# ggplot(im.row) +
#   geom_line(aes(i, x))
# 
# im.col <- fun_extract_col(disc.clv.resid, col = as.integer(hdrlst$CENTER_Y))
# 
# ggplot(im.col) +
#   geom_line(aes(j, x))

imDat <- fun_tibbl2mat(disc.clv.resid)

out_file_name = "test_disc.clv.resid.fit"
fun_write_image(imDat, paste0(out_data_path,out_file_name), hdrlst, header)

# Subtrahieren der geschätzten Flächenkomponente (deprecated) ------------------

disc.flatfield <- fun_clv_rmplane(discim, hdrlst, coeff = disc.clv.coeff)

# ggplot(disc.flatfield, aes(i, j)) +
#   geom_raster(aes(fill = flat)) +
#   coord_fixed(ratio = 1)

disc.flatfield.save <- disc.flatfield %>% 
  select(i, j, x=flat)

im.row <- fun_extract_row(disc.flatfield.save, 
                          row = as.integer(hdrlst$CENTER_X))

# ggplot(im.row) +
#   geom_line(aes(i, x))

im.col <- fun_extract_col(disc.flatfield.save, 
                          col = as.integer(hdrlst$CENTER_Y))

# ggplot(im.col) +
#   geom_line(aes(j, x))

imDat <- fun_tibbl2mat(disc.flatfield.save)

out_file_name = "test_disc.clv.flatfield.fit"
fun_write_image(imDat, paste0(out_data_path,out_file_name), hdrlst, header)

# Berechnen einer Standardrandabschattungsfunktion -----------------------------

sclv.fun <- fun_clv_function(disc.clv, clv.i0 = sum.clv.i0, sclv.method = "NL",
                             sdo.image = "FALSE", clv.coeff = NULL)

disc.sclv <- sclv.fun$z
disc.sclv.coeff <- sclv.fun$cf

# ggplot(disc.sclv, aes(i, j)) +
#   geom_raster(aes(fill = sclv)) +
#   coord_fixed(ratio = 1)

im.row <- fun_extract_row(disc.sclv, row = as.integer(hdrlst$CENTER_X))

# ggplot(im.row) +
#   geom_line(aes(i, sclv))

im.col <- fun_extract_col(disc.sclv, col = as.integer(hdrlst$CENTER_Y))

# ggplot(im.col) +
#   geom_line(aes(j, sclv))

disc.sclv.save <- disc.sclv %>% 
  select(i, j, x=sclv)

imDat <- fun_tibbl2mat(disc.sclv.save)

out_file_name = "test_disc.sclv.fit"
fun_write_image(imDat, paste0(out_data_path,out_file_name), hdrlst, header)

# Kalibrieren des Bildes -------------------------------------------------------

disc.calib <- fun_clv_calibrate(x = disc.sclv)

# ggplot(disc.calib, aes(i, j)) +
#   geom_raster(aes(fill = calib)) +
#   coord_fixed(ratio = 1)

im.row <- fun_extract_row(disc.calib, row = as.integer(hdrlst$CENTER_X))

ggplot(im.row) +
  geom_line(aes(i, calib))

disc.calib.save <- disc.calib %>% 
  select(i, j, x=calib)

imDat <- fun_tibbl2mat(disc.calib.save)

out_file_name = "test_disc.calib.fit"
fun_write_image(imDat, paste0(out_data_path,out_file_name), hdrlst, header)

# Hinzufügen flächengleicher Ringe (deprecated) --------------------------------

disc.rings <- fun_disc_rings(discim, num.rings = 50)

ggplot(disc.rings, aes(i, j)) +
  geom_raster(aes(fill = rings)) +
  coord_fixed(ratio = 1)

disc.rings.save <- disc.rings %>% 
  select(i, j, x = rings)

imDat <- fun_tibbl2mat(disc.rings.save)

out_file_name = "test_disc.rings.fit"
fun_write_image(imDat, paste0(out_data_path,out_file_name), hdrlst, header)

# Hinzufügen der Ringränder zum Bild (deprecated) ------------------------------

ring.plt <- fun_ring_plt(discim, hdrlst, theta = 35, num.rings = 50)

# ggplot(ring.plt, aes(i, j)) +
#   geom_raster(aes(fill = ring)) +
#   coord_fixed(ratio = 1)

ring.plt.save <- ring.plt %>% 
  select(i, j, x = ring)

imDat <- fun_tibbl2mat(ring.plt.save)

out_file_name = "test_fun_ring_plt.fit"
fun_write_image(imDat, paste0(out_data_path,out_file_name), hdrlst, header)

# Randabschattungskorrektur ----------------------------------------------------

disc.flat <- fun_clv_correct(x = disc.calib, name.image = "calib", 
                             name.clv = "sclv")

# ggplot(disc.flat, aes(i, j)) +
#   geom_raster(aes(fill = flat)) +
#   coord_fixed(ratio = 1)

im.row <- fun_extract_row(disc.flat, row = as.integer(hdrlst$CENTER_X))

ggplot(im.row) +
  geom_line(aes(i, flat))

im.col <- fun_extract_col(disc.flat, col = as.integer(hdrlst$CENTER_Y))

# ggplot(im.col) +
#   geom_line(aes(j, flat))

disc.flat.save <- disc.flat %>% 
  mutate(flat = 100*flat) %>%
  select(i, j, x=flat)

imDat <- fun_tibbl2mat(disc.flat.save)

out_file_name = "test_disc.flat.fit"
fun_write_image(imDat, paste0(out_data_path,out_file_name), hdrlst, header)

## -----------------------------------------------------------------------------
## Modul Randabschattungskorrektur

mod.clv.correction <- mod_clv_correction(discim,
                                         hdrlst = hdrlst,
                                         header = header,
                                         model = "poly_with_plane",
                                         run = 3, 
                                         clip.resid.out = "FALSE")

disc.flat     <- mod.clv.correction$disc.flat
hdrlst        <- mod.clv.correction$hdrlst
header        <- mod.clv.correction$header

# ggplot(disc.flat, aes(i, j)) +
#   geom_raster(aes(fill = flat)) +
#   coord_fixed(ratio = 1)

# ggplot(image, aes(i, j)) +
#   geom_raster(aes(fill = flat)) +
#   coord_fixed(ratio = 1)

disc.flat.save <- disc.flat %>% 
  mutate(flat = 100*flat) %>%
  select(i, j, x = flat)

# disc.flat.save <- image %>% 
#   mutate(flat = 100*flat) %>%
#   select(i, j, x = flat)

imDat <- fun_tibbl2mat(disc.flat.save)

out_file_name = "test_mod_clv.correction.fit"
fun_write_image(imDat, paste0(out_data_path,out_file_name), hdrlst, header)

## -----------------------------------------------------------------------------
# Histogramm der Intensity Contrasts (to be improved) --------------------------

disc.flat.in <- disc.flat %>%
  filter(fill > 0) %>% 
  select(i,j,x=flat)

hist(disc.flat.in$x, breaks=500)

# Markieren der Plages ---------------------------------------------------------

# threshold <- 1.00 # All without background
# threshold <- 1.10 # quiet network, enhanced network and plages
# threshold <- 1.15 # quiet network, enhanced network and plages
# threshold <- 1.28 # enhanced network and plages
# threshold <- 1.25 # enhanced network and plages

threshold <- 1.35 # plages: adopted
method = "absolute"

# disc.flat.in <- disc.flat %>% 
#   select(i,j,x=flat)

disc.flat.in <- image %>% 
  select(i,j,x=flat)

disc.plage <- fun_mask_create(disc.flat.in, hdrlst = hdrlst, 
                              threshold = threshold,
                              method = method)

disc.plage.threshold      <- disc.plage$mask.threshold
disc.plage.mask           <- disc.plage$mask

disc.plage.save <- disc.plage.mask %>% 
  mutate(x = 100 * th) %>%
  select(i, j, x)

imDat <- fun_tibbl2mat(disc.plage.save)

out_file_name = "test_disc.plage.fit"
fun_write_image(imDat, paste0(out_data_path,out_file_name), hdrlst, header)

# Markieren des Networks -------------------------------------------------------

# lower.threshold <- 1.00 # background
# upper.threshold <- 1.10 # quiet network
# 
# lower.threshold <- 1.10 # quiet network
# upper.threshold <- 1.25 # enhanced network

lower.threshold <- 1.25 # enhanced network
upper.threshold <- 1.35 # plages

method = "absolute"

# disc.flat.in <- disc.flat %>% 
#   select(i,j,x=flat)

disc.flat.in <- image %>% 
  select(i,j,x=flat)

disc.network <- fun_mask_diff(disc.flat.in, hdrlst = hdrlst, 
                              lower.threshold = lower.threshold, 
                              upper.threshold = upper.threshold, 
                              method = method)

disc.network.lower.threshold  <- disc.network$mask.lower.threshold
disc.network.upper.threshold  <- disc.network$mask.upper.threshold
disc.network.mask             <- disc.network$mask

# ggplot(disc.network.mask, aes(i, j)) +
#   geom_raster(aes(fill = th)) +
#   coord_fixed(ratio = 1)

disc.network.save <- disc.network.mask %>% 
  mutate(x = 100 * th) %>%
  select(i, j, x)

imDat <- fun_tibbl2mat(disc.network.save)

out_file_name = "test_disc.network.fit"
fun_write_image(imDat, paste0(out_data_path,out_file_name), hdrlst, header)

## -----------------------------------------------------------------------------
## Modul Feature Extraktion

plage.contrast <- 1.35 
en.contrast <- 1.25 
qn.contrast <- 1.10

mod_feature_extraction <- mod_feature_extraction(disc.flat, hdrlst, header, 
                                           plage.contrast = plage.contrast, 
                                           en.contrast = en.contrast, 
                                           qn.contrast = qn.contrast)

disc.features <- mod_feature_extraction$disc.features
hdrlst        <- mod_feature_extraction$hdrlst
header        <- mod_feature_extraction$header

ggplot(disc.features, aes(i, j)) +
  geom_raster(aes(fill = ehnetwork)) +
  coord_fixed(ratio = 1)

disc.features.save <- disc.features %>% 
  mutate(qtnetwork = 100*qtnetwork) %>%
  select(i, j, x = qtnetwork)

disc.features.save <- disc.features %>%
  mutate(ehnetwork = 100*ehnetwork) %>%
  select(i, j, x = ehnetwork)

disc.features.save <- disc.features %>%
  mutate(plage = 100*plage) %>%
  select(i, j, x = plage)

# disc.features.save <- disc.features %>% 
#   mutate(plage_hem = 100*plage_hem) %>%
#   select(i, j, x = plage_hem)

disc.features.save <- disc.features %>%
  mutate(ttarea = 100*ttarea) %>%
  select(i, j, x = ttarea)


imDat <- fun_tibbl2mat(disc.features.save)

out_file_name = "test_mod_plage.extraction.fit"
fun_write_image(imDat, paste0(out_data_path,out_file_name), hdrlst, header)

## -----------------------------------------------------------------------------
## Modul Index calculation

mod_index_calculation <- mod_index_calculation(disc.features, hdrlst, header)

disc.indices <- mod_index_calculation$images
hdrlst       <- mod_index_calculation$hdrlst
header       <- mod_index_calculation$header

# ggplot(disc.indices, aes(i, j)) +
#   geom_raster(aes(fill = ttarea_hem)) +
#   coord_fixed(ratio = 1)

disc.indices.save <- disc.indices %>% 
  mutate(ttarea_hem = 100*ttarea_hem) %>%
  select(i, j, x = ttarea_hem)

disc.indices.save <- images %>% 
  mutate(ttarea_hem = 100*ttarea_hem) %>%
  select(i, j, x = ttarea_hem)

disc.indices.save <- images %>% 
  mutate(ttarea_hem = 100*atnetwork_hem) %>%
  select(i, j, x = atnetwork_hem)

disc.indices.save <- images %>% 
  mutate(ttarea_hem = 100*ehnetwork_hem) %>%
  select(i, j, x = ehnetwork_hem)

disc.indices.save <- images %>% 
  mutate(ttarea_hem = 100*plage_hem) %>%
  select(i, j, x = plage_hem)

imDat <- fun_tibbl2mat(disc.indices.save)

out_file_name = "test_mod_index_calculation.fit"
fun_write_image(imDat, paste0(out_data_path,out_file_name), hdrlst, header)

## -----------------------------------------------------------------------------


