## -----------------------------------------------------------------------------
# Load data:
data("fruits_traits", package = "mFD")

# Remove fuzzy traits in this tutorial:
fruits_traits <- fruits_traits[ , -c(6:8)]

# Display the table:
knitr::kable(head(fruits_traits),
             caption = "Species x traits data frame")

## -----------------------------------------------------------------------------
# Load data:
data("baskets_fruits_weights", package = "mFD")

# Display the table:
knitr::kable(as.data.frame(baskets_fruits_weights[1:6, 1:6]), 
      centering = TRUE,
      caption = "Species x assemblages matrix based on the **fruits** dataset")


## ----echo = FALSE, out.width = "600px", fig.cap = "Basic framework of the mFD package", fig.align = 'center'----
knitr::include_graphics("basic_framework.PNG")

## -----------------------------------------------------------------------------
# Load data:
data("fruits_traits_cat", package = "mFD")

# Remove fuzzy traits in this tutorial:
fruits_traits_cat <- fruits_traits_cat[-c(6:8), ]
# Thus remove the "fuzzy_name" column:
fruits_traits_cat <- fruits_traits_cat[ , -3]

# Display the table:
knitr::kable(head(fruits_traits_cat), 
             caption = "Traits types based on **fruits & baskets** dataset")

## -----------------------------------------------------------------------------
# Species traits summary:
fruits_traits_summ <- mFD::sp.tr.summary(
  tr_cat     = fruits_traits_cat,   
  sp_tr      = fruits_traits, 
  stop_if_NA = TRUE)

## -----------------------------------------------------------------------------
fruits_traits_summ$"tr_types"                     # Traits types 

## -----------------------------------------------------------------------------
fruits_traits_summ$"mod_list"                     # Traits types for non-continuous and non-fuzzy traits

## -----------------------------------------------------------------------------
# Summary of the assemblages * species dataframe:
asb_sp_fruits_summ <- mFD::asb.sp.summary(asb_sp_w = baskets_fruits_weights)

## -----------------------------------------------------------------------------
head(asb_sp_fruits_summ$"asb_sp_occ", 3)        # Species occurrences for the first 3 assemblages
asb_sp_fruits_occ <- asb_sp_fruits_summ$"asb_sp_occ"

## -----------------------------------------------------------------------------
asb_sp_fruits_summ$"sp_tot_w"              # Species total biomass in all assemblages

## -----------------------------------------------------------------------------
asb_sp_fruits_summ$"asb_tot_w"             # Total biomass per assemblage

## -----------------------------------------------------------------------------
asb_sp_fruits_summ$"asb_sp_richn"           # Species richness per assemblage

## -----------------------------------------------------------------------------
asb_sp_fruits_summ$"asb_sp_nm"[[1]]             # Names of species present in the first assemblage

## ----echo=FALSE---------------------------------------------------------------
asb_sp_fruits_occ <- asb_sp_fruits_summ$"asb_sp_occ"

## ----results = "hide"---------------------------------------------------------
sp_dist_fruits <- mFD::funct.dist(
  sp_tr         = fruits_traits,
  tr_cat        = fruits_traits_cat,
  metric        = "gower",
  scale_euclid  = "scale_center",
  ordinal_var   = "classic",
  weight_type   = "equal",
  stop_if_NA    = TRUE)

## -----------------------------------------------------------------------------
round(sp_dist_fruits, 3)                 # Output of the function mFD::funct.dist()

## ----results = "hide"---------------------------------------------------------
fspaces_quality_fruits <- mFD::quality.fspaces(
  sp_dist             = sp_dist_fruits,
  maxdim_pcoa         = 10,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")

## -----------------------------------------------------------------------------
round(fspaces_quality_fruits$"quality_fspaces", 3)            # Quality metrics of spaces

## ----fig.show = 'hide', results = "hide"--------------------------------------
mFD::quality.fspaces.plot(
  fspaces_quality            = fspaces_quality_fruits,
  quality_metric             = "mad",
  fspaces_plot               = c("tree_average", "pcoa_2d", "pcoa_3d", 
                                 "pcoa_4d", "pcoa_5d", "pcoa_6d"),
  name_file                  = NULL,
  range_dist                 = NULL,
  range_dev                  = NULL,
  range_qdev                 = NULL,
  gradient_deviation         = c(neg = "darkblue", nul = "grey80", pos = "darkred"),
  gradient_deviation_quality = c(low = "yellow", high = "red"),
  x_lab                      = "Trait-based distance")

## ----fig.height = 7, fig.width = 12, fig.align = "center"---------------------
mFD::quality.fspaces.plot(
  fspaces_quality            = fspaces_quality_fruits,
  quality_metric             = "mad",
  fspaces_plot               = c("tree_average", "pcoa_2d", "pcoa_3d",
                                 "pcoa_4d", "pcoa_5d", "pcoa_6d"),
  name_file                  = NULL,
  range_dist                 = NULL,
  range_dev                  = NULL,
  range_qdev                 = NULL,
  gradient_deviation         = c(neg = "darkblue", nul = "grey80", pos = "darkred"),
  gradient_deviation_quality = c(low = "yellow", high = "red"),
  x_lab                      = "Trait-based distance")

## ----results = "hide"---------------------------------------------------------
sp_faxes_coord_fruits <- fspaces_quality_fruits$"details_fspaces"$"sp_pc_coord"

## ----results = "hide"---------------------------------------------------------
fruits_tr_faxes <- mFD::traits.faxes.cor(
  sp_tr          = fruits_traits, 
  sp_faxes_coord = sp_faxes_coord_fruits[ , c("PC1", "PC2", "PC3", "PC4")], 
  plot           = TRUE)

## ----fig.height = 7, fig.width = 12, fig.align = "center"---------------------
# Print traits with significant effect:
fruits_tr_faxes$"tr_faxes_stat"[which(fruits_tr_faxes$"tr_faxes_stat"$"p.value" < 0.05), ]

# Return plots:
fruits_tr_faxes$"tr_faxes_plot"

## ----results = "hide"---------------------------------------------------------
sp_faxes_coord_fruits <- fspaces_quality_fruits$"details_fspaces"$"sp_pc_coord"

## ----results = "hide"---------------------------------------------------------
big_plot <- mFD::funct.space.plot(
  sp_faxes_coord  = sp_faxes_coord_fruits[ , c("PC1", "PC2", "PC3", "PC4")],
  faxes           = c("PC1", "PC2", "PC3", "PC4"),
  name_file       = NULL,
  faxes_nm        = NULL,
  range_faxes     = c(NA, NA),
  color_bg        = "grey95",
  color_pool      = "darkgreen",
  fill_pool       = "white",
  shape_pool      = 21,
  size_pool       = 1,
  plot_ch         = TRUE,
  color_ch        = "black",
  fill_ch         = "white",
  alpha_ch        = 0.5,
  plot_vertices   = TRUE,
  color_vert      = "blueviolet",
  fill_vert       = "blueviolet",
  shape_vert      = 23,
  size_vert       = 1,
  plot_sp_nm      = NULL,
  nm_size         = 3,
  nm_color        = "black",
  nm_fontface     = "plain",
  check_input     = TRUE)

## ----fig.height = 15, fig.width = 20, fig.align = "center"--------------------
big_plot <- mFD::funct.space.plot(
  sp_faxes_coord  = sp_faxes_coord_fruits,
  faxes           = NULL,
  name_file       = NULL,
  faxes_nm        = NULL,
  range_faxes     = c(NA, NA),
  color_bg        = "grey95",
  color_pool      = "darkgreen",
  fill_pool       = "white",
  shape_pool      = 21,
  size_pool       = 1,
  plot_ch         = TRUE,
  color_ch        = "black",
  fill_ch         = "white",
  alpha_ch        = 0.5,
  plot_vertices   = TRUE,
  color_vert      = "blueviolet",
  fill_vert       = "blueviolet",
  shape_vert      = 23,
  size_vert       = 1,
  plot_sp_nm      = NULL,
  nm_size         = 3,
  nm_color        = "black",
  nm_fontface     = "plain",
  check_input     = TRUE)

# Plot the graph with all pairs of axes:
big_plot$patchwork

## ----results = "hide"---------------------------------------------------------
alpha_fd_indices_fruits <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_fruits[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = baskets_fruits_weights,
  ind_vect         = c("fdis", "fmpd", "fnnd", "feve", "fric", "fdiv", "fori", 
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

## -----------------------------------------------------------------------------
fd_ind_values_fruits <- alpha_fd_indices_fruits$"functional_diversity_indices"
fd_ind_values_fruits

## ----results = "hide"---------------------------------------------------------
details_list_fruits <- alpha_fd_indices_fruits$"details"

## ----results = "hide",  fig.show = 'hide', message = FALSE--------------------
plots_alpha <- mFD::alpha.multidim.plot(
  output_alpha_fd_multidim = alpha_fd_indices_fruits,
  plot_asb_nm              = c("basket_1", "basket_5"),
  ind_nm                   = c("fdis", "fide", "fnnd", "feve", "fric", 
                               "fdiv", "fori", "fspe"),
  faxes                    = NULL,
  faxes_nm                 = NULL,
  range_faxes              = c(NA, NA),
  color_bg                 = "grey95",
  shape_sp                 = c(pool = 3, asb1 = 21, asb2 = 21),
  size_sp                  = c(pool = 0.7, asb1 = 1, asb2 = 1),
  color_sp                 = c(pool = "grey50", asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  color_vert               = c(pool = "grey50", asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  fill_sp                  = c(pool = NA, asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  fill_vert                = c(pool = NA, asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  color_ch                 = c(pool = NA, asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  fill_ch                  = c(pool = "white", asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  alpha_ch                 = c(pool = 1, asb1 = 0.3, asb2 = 0.3),
  shape_centroid_fdis      = c(asb1 = 22,  asb2 = 24),
  shape_centroid_fdiv      = c(asb1 = 22,  asb2 = 24),
  shape_centroid_fspe      = 23,
  color_centroid_fspe      = "black",
  size_sp_nm               = 3, 
  color_sp_nm              = "black",
  plot_sp_nm               = NULL,
  fontface_sp_nm           = "plain",
  save_file                = FALSE,
  check_input              = TRUE) 

## ----fig.height = 15, fig.width = 20, fig.align = "center", warning = FALSE----
plots_alpha$"fric"$"patchwork"

## ----fig.height = 15, fig.width = 20, fig.align = "center", warning = FALSE----
plots_alpha$"fdiv"$"patchwork"

## ----fig.height = 15, fig.width = 20, fig.align = "center", warning = FALSE----
plots_alpha$"fspe"$"patchwork"

## ----fig.height = 15, fig.width = 20, fig.align = "center", warning = FALSE----
plots_alpha$"fdis"$"patchwork"

## ----fig.height = 15, fig.width = 20, fig.align = "center", warning = FALSE----
plots_alpha$"fide"$"patchwork"

## ----fig.height = 15, fig.width = 20, fig.align = "center", warning = FALSE----
plots_alpha$"feve"$"patchwork"

## ----fig.height = 15, fig.width = 20, fig.align = "center", warning = FALSE----
plots_alpha$"fori"$"patchwork"

## ----fig.height = 15, fig.width = 20, fig.align = "center", warning = FALSE----
plots_alpha$"fnnd"$"patchwork"

## ----results = "hide", message=FALSE, eval = FALSE----------------------------
#  beta_fd_indices_fruits <- mFD::beta.fd.multidim(
#        sp_faxes_coord   = sp_faxes_coord_fruits[ , c("PC1", "PC2", "PC3", "PC4")],
#        asb_sp_occ       = asb_sp_fruits_occ,
#        check_input      = TRUE,
#        beta_family      = c("Jaccard"),
#        details_returned = TRUE)

## ----results = "hide", warning = FALSE, eval = FALSE--------------------------
#  beta_plot_fruits <- mFD::beta.multidim.plot(
#    output_beta_fd_multidim = beta_fd_indices_fruits,
#    plot_asb_nm             = c("basket_1", "basket_4"),
#    beta_family             = c("Jaccard"),
#    plot_sp_nm              = c("apple", "lemon", "pear"),
#    faxes                   = paste0("PC", 1:4),
#    name_file               = NULL,
#    faxes_nm                = NULL,
#    range_faxes             = c(NA, NA),
#    color_bg                = "grey95",
#    shape_sp                = c("pool" = 3.0, asb1 = 22, asb2 = 21),
#    size_sp                 = c("pool" = 0.8, asb1 =  1, asb2 =  1),
#    color_sp                = c("pool" = "grey50", asb1 = "blue", asb2 = "red"),
#    fill_sp                 = c("pool" = NA, asb1 = "white", asb2 = "white"),
#    fill_vert               = c("pool" = NA, asb1 = "blue", asb2 = "red"),
#    color_ch                = c("pool" = NA, asb1 = "blue", asb2 = "red"),
#    fill_ch                 = c("pool" = "white", asb1 = "blue", asb2 = "red"),
#    alpha_ch                = c("pool" = 1, asb1 = 0.3, asb2 = 0.3),
#    nm_size                 = 3,
#    nm_color                = "black",
#    nm_fontface             = "plain",
#    check_input             = TRUE)

