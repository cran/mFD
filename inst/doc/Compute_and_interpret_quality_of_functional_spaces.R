## -----------------------------------------------------------------------------
data("fruits_traits", package = "mFD")

# remove non-fuzzy traits:
fruits_traits <- fruits_traits[1:5]

# plot the table:
knitr::kable(head(fruits_traits), 
             caption = "Species x traits dataframe based on *fruits* dataset")

## ---- echo = FALSE------------------------------------------------------------
summary(fruits_traits)

## -----------------------------------------------------------------------------
fruits_traits_cat <- data.frame(names(fruits_traits), c("O","N","O","O","Q"))
colnames(fruits_traits_cat) <- c("trait_name", "trait_type")
fruits_traits_cat

## -----------------------------------------------------------------------------
# compute trait-based distances:
dist_fruits <- mFD::funct.dist(
  sp_tr         = fruits_traits,
  tr_cat        = fruits_traits_cat,
  metric        = "gower",
  scale_euclid  = "noscale",
  ordinal_var   = "classic",
  weight_type   = "equal",
  stop_if_NA    = TRUE)

# sum up the distance matrix:
summary(as.matrix(dist_fruits))

## -----------------------------------------------------------------------------
# retrieve fruits names:
ex_blackberry <- c("blackberry","currant","cherry","banana")

# get the distance matrix only for these species:
round(as.matrix(dist_fruits)[ex_blackberry, ex_blackberry], 2)

## -----------------------------------------------------------------------------
fruits_traits[ex_blackberry, ]

## ---- warning = FALSE---------------------------------------------------------
# use quality.fpscaes function to compute quality metrics:
quality_fspaces_fruits <- mFD::quality.fspaces(
  sp_dist             = dist_fruits,
  fdendro             = "average",
  maxdim_pcoa         = 9,
  deviation_weighting = c("absolute", "squared"),
  fdist_scaling       = c(TRUE, FALSE))

# display the table gathering quality metrics:
quality_fspaces_fruits$"quality_fspaces"

# retrieve the functional space associated with minimal quality metric: 
apply(quality_fspaces_fruits$quality_fspaces, 2, which.min)

## ---- fig.height = 7, fig.width = 12, fig.align = "center", warning = FALSE----
library("magrittr")

quality_fspaces_fruits$"quality_fspaces" %>%
  tibble::as_tibble(rownames = "Funct.space") %>%
  tidyr::pivot_longer(cols =! Funct.space, names_to = "quality_metric", values_to = "Quality") %>%
  ggplot2::ggplot(ggplot2::aes(x = Funct.space, y = Quality, 
                               color = quality_metric, shape = quality_metric)) +
  ggplot2::geom_point() 

## ---- fig.height = 7, fig.width = 12, fig.align = "center", warning = FALSE----
mFD::quality.fspaces.plot(
  fspaces_quality = quality_fspaces_fruits, 
  quality_metric  = "mad",
  fspaces_plot    = c("tree_average", "pcoa_2d", "pcoa_3d", "pcoa_4d", 'pcoa_5d'))

## -----------------------------------------------------------------------------
# get fruits traits:
fruits_traits[c("cherry", "lime", "lemon"), ]

## ---- warning = FALSE---------------------------------------------------------
quality_fspaces_fruits$"details_fspaces"$"pairsp_fspaces_dist" %>%
  dplyr::filter(sp.x %in% c("cherry", "lime", "lemon") & 
                sp.y %in% c("cherry", "lime", "lemon")) %>%
  dplyr::select(sp.x, sp.y, tr, pcoa_4d, tree_average) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), round, 2))

## ---- warning = FALSE, fig.height = 7, fig.width = 12, fig.align = "center"----
quality_fspaces_fruits$"details_fspaces"$"pairsp_fspaces_dist" %>%
  dplyr::filter(sp.x %in% c("pineapple") | sp.y %in% c("pineapple")) %>%
  dplyr::mutate(fruit = stringr::str_replace_all(string = paste0(sp.x, "", sp.y),
                                                 pattern = "pineapple", replacement = "")) %>%
  dplyr::select(fruit, Gower_distance = tr, Cophenetic_distance = tree_average) %>%
  ggplot2::ggplot(ggplot2::aes(x = Gower_distance, y = Cophenetic_distance, label = fruit)) +
  ggplot2::geom_point(size = 1) +
  ggplot2::geom_text(size = 2, nudge_y = 0.08, check_overlap = TRUE) +
  ggplot2::geom_abline(slope = 1, intercept = 0) +
  ggplot2::scale_x_continuous(limits = c(0, 1)) +
  ggplot2::scale_y_continuous(limits = c(0, 1))

## ---- warning = FALSE, fig.height = 7, fig.width = 12, fig.align = "center"----
quality_fspaces_fruits$"details_fspaces"$"dendro" %>%
  as.dendrogram() %>%
  dendextend::plot_horiz.dendrogram(side = TRUE)

## -----------------------------------------------------------------------------
# check if distance matrix checks Euclidean properties:
quality_fspaces_fruits$"details_trdist"$"trdist_euclidean"

## -----------------------------------------------------------------------------
# retrieve eigen values: 
quality_fspaces_fruits$"details_fspaces"$"pc_eigenvalues"

## -----------------------------------------------------------------------------
quality_fspaces_fruits$"details_fspaces"$"pairsp_fspaces_dist" %>%
  dplyr::select(sp.x, sp.y, Gower = tr) %>%
  dplyr::mutate(sqrt_Gower = sqrt(Gower)) %>%
  dplyr::filter(sp.x %in% ex_blackberry & sp.y %in% ex_blackberry) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), round, 2))

## -----------------------------------------------------------------------------
# compute quality metrics with square-root transformed distances:
quality_fspaces_fruits_sqrtgower <- mFD::quality.fspaces(
  sp_dist             = sqrt(dist_fruits),
  fdendro             = NULL,
  maxdim_pcoa         = 24,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE)

# check if distance matrix checks Euclidean properties:
quality_fspaces_fruits_sqrtgower$"details_trdist"$"trdist_euclidean"
# input distance is now Euclidean

# get mean Absolute Deviation:
quality_fspaces_fruits_sqrtgower$"quality_fspaces"

## ---- fig.height = 7, fig.width = 12, fig.align = "center"--------------------
quality_fspaces_fruits$"details_fspaces"$"pairsp_fspaces_dist" %>%
  dplyr::select(sp.x, sp.y, Gower_distance = tr) %>%
  dplyr::mutate(Eucli_dist_24D_sqrt = quality_fspaces_fruits_sqrtgower$"details_fspaces"$"pairsp_fspaces_dist"$"pcoa_24d") %>%
  ggplot2::ggplot(ggplot2::aes(x = Gower_distance, y = Eucli_dist_24D_sqrt)) +
  ggplot2::geom_point(size = 1) +
  ggplot2::geom_abline(slope = 1, intercept = 0) +
  ggplot2::scale_x_continuous(limits = c(0, 1)) +
  ggplot2::scale_y_continuous(limits = c(0, 1))

## ---- echo = FALSE------------------------------------------------------------
mean(abs(quality_fspaces_fruits$"details_fspaces"$"pairsp_fspaces_dist"$"tr" -
 quality_fspaces_fruits_sqrtgower$"details_fspaces"$"pairsp_fspaces_dist"$"pcoa_24d"))

## -----------------------------------------------------------------------------
# create a new dataset:
sp_tr <- data.frame(
  tra = factor(c(LETTERS[1:2], LETTERS[1:2], LETTERS[1:2], LETTERS[1:2])),
  trb = factor(c(rep("M", 4), rep("N", 4))) ,
  trc = factor(c(rep("X", 2), rep("Y", 4), rep("X", 2)))
)
row.names(sp_tr) <- paste0("sp", 1:8)
sp_tr

# compute Gower distance between all pairs of species:
dist_gower <- cluster::daisy(sp_tr, metric = "gower")
round(dist_gower, 2)

## ---- echo = FALSE------------------------------------------------------------
# square-root transformation of Gower distance
gower_sqrt <- sqrt(dist_gower)
round(gower_sqrt, 2)

