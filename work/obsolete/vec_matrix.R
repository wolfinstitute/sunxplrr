X <- diag(6)

# Variante mit Loops

out <- NULL
for (j in 1:nrow(X)) {
  for (i in 1:ncol(X)) {
    out <- rbind(out, c(i, j, X[i,j]))
  }
}

out <- as.data.frame(out)
colnames(out) <- c("i", "j", "x")

# Variante mit tidyr

library(dplyr)
library(tidyr)

Y <- as.data.frame(X)

colnames(Y) <- as.character(1:ncol(X))

Y <- Y %>% 
  mutate(i = 1:nrow(Y)) %>% 
  gather(key = j, value = x, -i) %>% 
  mutate(j = as.integer(as.numeric(j)))
  

Y <- as_tibble(Y)

# Variante mit fun_mat2tibbl

Y <- fun_mat2tibbl(X)

ggplot(Y, aes(i, j)) +
  geom_raster(aes(fill = x)) +
  coord_fixed(ratio = 1)

# Inverse von tibble to matrix

Y <- Y %>% 
     spread(key = "j", value = "x") %>% 
     select(-i)

Y <- as.matrix(Y)
colnames(Y) <- NULL

# Variante mit fun_tibbl2mat

X <- fun_tibbl2mat(Y)



