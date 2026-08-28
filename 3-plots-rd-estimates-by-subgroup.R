
#################################################################################
### RD robust Plots by Subgroup
################################################################################

library(rdrobust)

### cooperation plots


df <- pas  |> filter(gender == 1) |> filter(ethnicity <= 2)

a <- rdplot(
  y = df$coop_lat,
  x = df$runner, 
  c = 0, #p = 2,
  binselect = "esmv", scale = 1, nbins = 30,
  col.lines = "black", col.dots = "grey30", 
  masspoints = 3, 
  y.lim = c(-0.2,0.2),
  title = "White Men", x.label = "Days Relative to Event", y.label = "Cooperation"
)

a <- a$rdplot


df <- pas  |> filter(gender == 1) |> filter(ethnicity > 2)

b <- rdplot(
  y = df$coop_lat,
  x = df$runner, 
  c = 0, #p = 2,
  binselect = "esmv", scale = 1, nbins = 30,
  col.lines = "black", col.dots = "grey30", 
  masspoints = 3, 
  y.lim = c(-0.2,0.2),
  title = "Non-White Men", x.label = "Days Relative to Event", y.label = "Cooperation"
)

b <- b$rdplot

df <- pas  |> filter(gender == 2) |> filter(ethnicity <= 2)

c <- rdplot(
  y = df$coop_lat,
  x = df$runner, 
  c = 0, #p = 2,
  binselect = "esmv", scale = 1, nbins = 30,
  col.lines = "black", col.dots = "grey30", 
  masspoints = 3, 
  y.lim = c(-0.2,0.2),
  title = "White Women", x.label = "Days Relative to Event", y.label = "Cooperation"
)

c <- c$rdplot


df <- pas  |> filter(gender == 2) |> filter(ethnicity > 2)

d <- rdplot(
  y = df$coop_lat,
  x = df$runner, 
  c = 0, #p = 2,
  binselect = "esmv", scale = 1, nbins = 30,
  col.lines = "black", col.dots = "grey30", 
  masspoints = 3, 
  y.lim = c(-0.2,0.2),
  title = "Non-White Women", x.label = "Days Relative to Event", y.label = "Cooperation"
)

d <- d$rdplot




p <- cowplot::plot_grid(
  a,
  c,
  b,       
  d,
  labels = c("A", "B", "C", "D"), 
  ncol = 2)
p
p_coop <- p


###



df <- pas  |> filter(gender == 1) |> filter(ethnicity <= 2)

a <- rdplot(
  y = df$trust_lat,
  x = df$runner, 
  c = 0, #p = 2,
  binselect = "esmv", scale = 1, nbins = 20,
  col.lines = "black", col.dots = "grey30", 
  masspoints = 3, 
  y.lim = c(-0.3,0.3),
  title = "White Men", x.label = "Days Relative to Event", y.label = "Trust"
)

a <- a$rdplot


df <- pas  |> filter(gender == 1) |> filter(ethnicity > 2)

b <- rdplot(
  y = df$trust_lat,
  x = df$runner, 
  c = 0, #p = 2,
  binselect = "esmv", scale = 1, nbins =20,
  col.lines = "black", col.dots = "grey30", 
  masspoints = 3, 
  y.lim = c(-0.4,0.2),
  title = "Non-White Men", x.label = "Days Relative to Event", y.label = "Trust"
)

b <- b$rdplot

df <- pas  |> filter(gender == 2) |> filter(ethnicity <= 2)

c <- rdplot(
  y = df$trust_lat,
  x = df$runner, 
  c = 0, #p = 2,
  binselect = "esmv", scale = 1, nbins = 20,
  col.lines = "black", col.dots = "grey30", 
  masspoints = 3, 
  y.lim = c(-0.3,0.3),
  title = "White Women", x.label = "Days Relative to Event", y.label = "Trust"
)

c <- c$rdplot


df <- pas  |> filter(gender == 2) |> filter(ethnicity > 2)

d <- rdplot(
  y = df$trust_lat,
  x = df$runner, 
  c = 0, #p = 2,
  binselect = "esmv", scale = 1, nbins = 20,
  col.lines = "black", col.dots = "grey30", 
  masspoints = 3,
  y.lim = c(-0.4,0.2),
  title = "Non-White Women", x.label = "Days Relative to Event", y.label = "Trust"
)

d <- d$rdplot 

p <- cowplot::plot_grid(
  a,
  c,
  b,
  d, 
  labels = c("A", "B", "C", "D"), 
  ncol = 2)
p
p_trust <- p


p_coop
p_trust

