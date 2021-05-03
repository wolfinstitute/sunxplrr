# tutorial_fitsio.r
#
# Author: Thomas K. Friedli
#
# Last version: 2019-11-24 / Frt
# Created:      2019-09-09 / Frt
# ------------------------------------------------------------------------------

# Load packages ----------------------------------------------------------------


library(FITSio)

library(dplyr)
library(tidyr)

library(ggplot2)
library(plotrix)

devtools::load_all(".")

# Set path and file names ------------------------------------------------------

inp_data_path = "./datatest/"
out_data_path = "./work/out/"

inp_file_name = "20190512k102.fit"

# Import FITS frame ------------------------------------------------------------

im <- readFITS(paste0(inp_data_path,inp_file_name))

# Header und Achsenvektoren ----------------------------------------------------

hdr <- im$hdr
header <- im$header

hdrlst = fun_hdr2list(im$hdr) 

ax1 = axVec(1,im$axDat)
ax2 = axVec(2,im$axDat)

# Konvertierung von FITS imDat in eine tibble ----------------------------------

fitsim <- fun_mat2tibbl(im$imDat)

ggplot(fitsim, aes(i, j)) +
  geom_raster(aes(fill = x)) +
  coord_fixed(ratio = 1)

# Konvertierung von einer tibble in ein FITS imDat -----------------------------

im$imDat <- fun_tibbl2mat(fitsim)

# Speichern FITS imDat als FITS Datei ------------------------------------------

out_file_name = "test_image.fit"
writeFITSim16i(im$imDat, paste0(out_data_path,out_file_name), header=im$header)

# Auslesen einer Bildzeile -----------------------------------------------------

row <- 875

im.row <- fun_extract_row(fitsim, row = row)

ggplot(im.row, aes(i, x)) +
  geom_line() 

# Auslesen einer Bildspalte ----------------------------------------------------

col <- 1020

im.col <- fun_extract_col(fitsim, col = col)

ggplot(im.col, aes(j, x)) +
  geom_line() 

# Generieren einer Maske -------------------------------------------------------

threshold <- 8000

im.mask <- fun_mask_create(fitsim, threshold = threshold, method = 'absolute')

ggplot(im.mask, aes(i, j)) +
  geom_raster(aes(fill = th)) +
  coord_fixed(ratio = 1)

imDat.mask <- im.mask %>% 
  select(i, j, x=th)

im$imDat <- fun_tibbl2mat(imDat.mask)

out_file_name = "test_mask.fit"
writeFITSim16i(im$imDat, paste0(out_data_path,out_file_name), header=im$header)

# Ausfüllen einer generierten Maske --------------------------------------------

im.disc <- fun_mask_fill(im.mask)

ggplot(im.disc, aes(i, j)) +
  geom_raster(aes(fill = fill)) +
  coord_fixed(ratio = 1)

# Berechnen und Speichern der Objektkonturen -----------------------------------

im.border <- fun_mask_border(im.mask, mask = "th")

ggplot(im.border, aes(i, j)) +
  geom_raster(aes(fill = border)) +
  coord_fixed(ratio = 1)


imDat.border <- im.border %>% 
  select(i, j, x=border)

im$imDat <- fun_tibbl2mat(imDat.border)

out_file_name = "test_border.fit"
writeFITSim16i(im$imDat, paste0(out_data_path,out_file_name), header=im$header)

# Berechnen Scheibenzentrum und Scheibenradius ---------------------------------

disc.center <- fun_disc_center(im.disc, disc = "fill")

# Zeichnen eines Kreises mit bekannten Zentrum und Radius ----------------------

disc.border <- fun_mask_circle(fitsim, disc.center, border.pix = 0)

ggplot(disc.border, aes(i, j)) +
  geom_raster(aes(fill = x)) +
  coord_fixed(ratio = 1)

im$imDat <- fun_tibbl2mat(disc.border)


out_file_name = "test_disc.border.fit"
writeFITSim16i(im$imDat, paste0(out_data_path,out_file_name), header=im$header)

# Zeichnen einer Scheibe mit bekanntem Zentrum und Radius ----------------------

disc.mask <- fun_mask_fill(disc.border, mask = "x")

ggplot(disc.mask, aes(i, j)) +
  geom_raster(aes(fill = fill)) +
  coord_fixed(ratio = 1)

disc.mask <- disc.mask %>% 
  select(i, j, x=fill)

im$imDat <- fun_tibbl2mat(disc.mask)

out_file_name = "test_disc.mask.fit"
writeFITSim16i(im$imDat, paste0(out_data_path,out_file_name), header=im$header)

# Sonnenscheibe ausschneiden ---------------------------------------------------

disc.image <- fun_disc_math(fitsim, values_1 = "x", disc.mask, 
                            values_2 = "x", method = "mult")

ggplot(disc.image, aes(i, j)) +
  geom_raster(aes(fill = x)) +
  coord_fixed(ratio = 1)

im$imDat <- fun_tibbl2mat(disc.image)

out_file_name = "test_disc.image.fit"
writeFITSim16i(im$imDat, paste0(out_data_path,out_file_name), header=im$header)

# Differenz ursprüngliches Bild zu ausgeschnittenem Bild -----------------------

im.diff <- fun_disc_math(fitsim, values_1 = "x", disc.image, 
                         values_2 = "x", method = "diff")

ggplot(im.diff, aes(i, j)) +
  geom_raster(aes(fill = x)) +
  coord_fixed(ratio = 1)

im$imDat <- fun_tibbl2mat(im.diff)

out_file_name = "test_im.diff.fit"
writeFITSim16i(im$imDat, paste0(out_data_path,out_file_name), header=im$header)

# Berechnen des Zentrumsabstandes jedes Pixels ---------------------------------

disc.theta <- fun_clv_theta(disc.image, disc.center, border.pix = 0)

hist(disc.theta$theta, plot = TRUE)

# Berechnen der Randabschattungsfunktion ---------------------------------------

disc.clv <- fun_clv_function(disc.theta, disc.center, border.pix = 0,
                             method = "Allen")

ggplot(disc.clv, aes(i, j)) +
  geom_raster(aes(fill = clv)) +
  coord_fixed(ratio = 1)

disc.clv.save <- disc.clv %>% 
  select(i, j, x=clv)

im$imDat <- fun_tibbl2mat(disc.clv.save)

out_file_name = "test_disc.clv.fit"
writeFITSim16i(im$imDat, paste0(out_data_path,out_file_name), header=im$header)

im.row <- fun_extract_row(disc.clv, row = as.integer(disc.center$x_i))

ggplot(im.row) +
  geom_line(aes(i, clv))

# Randabschattungskorrektur ----------------------------------------------------

disc.flat <- fun_clv_correct(disc.clv)

ggplot(disc.flat, aes(i, j)) +
  geom_raster(aes(fill = flat)) +
  coord_fixed(ratio = 1)

disc.flat.save <- disc.flat %>% 
  select(i, j, x=flat)

im$imDat <- fun_tibbl2mat(disc.flat.save)

out_file_name = "test_disc.flat.fit"
writeFITSim16i(im$imDat, paste0(out_data_path,out_file_name), header=im$header)

im.row <- fun_extract_row(disc.flat, row = as.integer(disc.center$x_i))

ggplot(im.row) +
  geom_line(aes(i, flat))


# z <- x %>% filter(x==1)


# Werte i und j  des Diskzenters, sowie den Radius r in Header überführen

header <- addKwv('CENTER_i', disc.center$x_i, header=header)
header <- addKwv('CENTER_j', disc.center$y_j, header=header)
header <- addKwv('CENTER_r', disc.center$r, header=header)
closeHdr(hdr)

im$header <- header
