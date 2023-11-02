## -----------------------------------------------------------------------------
# Load data:
data("baskets_fruits_weights", package = "mFD")
# Display the table:
knitr::kable(as.data.frame(baskets_fruits_weights[1:6, 1:6]), 
      centering = TRUE,
      caption = "Species x assemblages matrix based on the **fruits** dataset")

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
data("fruits_traits_cat", package = "mFD")
# Remove fuzzy traits in this tutorial:
fruits_traits_cat <- fruits_traits_cat[-c(6:8), ]
# Thus remove the "fuzzy_name" column:
fruits_traits_cat <- fruits_traits_cat[ , -3]
# Display the table:
knitr::kable(head(fruits_traits_cat), 
             caption = "Traits types based on **fruits & baskets** dataset")

## ----results = "hide"---------------------------------------------------------
sp_dist_fruits <- mFD::funct.dist(
  sp_tr         = fruits_traits,
  tr_cat        = fruits_traits_cat,
  metric        = "gower",
  scale_euclid  = "scale_center",
  ordinal_var   = "classic",
  weight_type   = "equal",
  stop_if_NA    = TRUE)

## ----results = "hide", message = FALSE----------------------------------------
# Quality of functional spaces:
fspaces_quality_fruits <- mFD::quality.fspaces(
  sp_dist             = sp_dist_fruits,
  maxdim_pcoa         = 10,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")

# retrieve species (fruits) coordinates in the 4D space (see General tutorial):
sp_faxes_coord_fruits <- fspaces_quality_fruits$"details_fspaces"$"sp_pc_coord"

## ----results = "hide", message = FALSE----------------------------------------
alpha_fd_indices_fruits <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_fruits[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = baskets_fruits_weights,
  ind_vect         = c("fric"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

## -----------------------------------------------------------------------------
# Compute the range of functional axes:
range_sp_coord  <- range(sp_faxes_coord_fruits)

# Based on the range of species coordinates values, compute a nice range ...
# ... for functional axes:
range_faxes <- range_sp_coord +
    c(-1, 1) * (range_sp_coord[2] - range_sp_coord[1]) * 0.05
range_faxes

## ----fig.height = 3, fig.width = 5, fig.align = "center"----------------------
# get species coordinates along the two studied axes:
sp_faxes_coord_xy <- sp_faxes_coord_fruits[, c("PC1", "PC2")]

# Plot background with grey backrgound:
plot_k <- mFD::background.plot(range_faxes = range_faxes, 
                               faxes_nm = c("PC1", "PC2"),
                               color_bg = "grey95")
plot_k

## -----------------------------------------------------------------------------
# Retrieve vertices coordinates along the two studied functional axes:
vert <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_xy,  
                      order_2D = FALSE, 
                      check_input = TRUE)


## ----fig.height = 3, fig.width = 5, fig.align = "center"----------------------
plot_sp_vert <- mFD::pool.plot(ggplot_bg = plot_k,
                             sp_coord2D = sp_faxes_coord_xy,
                             vertices_nD = vert,
                             plot_pool = TRUE,
                             color_pool = "black",
                             fill_pool = NA,
                             alpha_ch =  0.8,
                             color_ch = NA,
                             fill_ch = "white",
                             shape_pool = 3,
                             size_pool = 0.8,
                             shape_vert = 16,
                             size_vert = 1,
                             color_vert = "green",
                             fill_vert = "green")
plot_sp_vert

## ----fig.height = 3, fig.width = 5, fig.align = "center"----------------------
plot_k <- mFD::pool.plot(ggplot_bg = plot_k,
                             sp_coord2D = sp_faxes_coord_xy,
                             vertices_nD = vert,
                             plot_pool = FALSE,
                             color_pool = NA,
                             fill_pool = NA,
                             alpha_ch =  0.8,
                             color_ch = "white",
                             fill_ch = "white",
                             shape_pool = NA,
                             size_pool = NA,
                             shape_vert = NA,
                             size_vert = NA,
                             color_vert = NA,
                             fill_vert = NA)
plot_k

## -----------------------------------------------------------------------------
# basket_1:
## filter species from basket_1:
sp_filter_basket1 <- mFD::sp.filter(asb_nm = c("basket_1"),
                            sp_faxes_coord = sp_faxes_coord_xy,
                                  asb_sp_w = baskets_fruits_weights)
## get species coordinates (basket_1):
sp_faxes_coord_basket1 <- sp_filter_basket1$`species coordinates`

  
# basket_6:
## filter species from basket_6:
sp_filter_basket6 <- mFD::sp.filter(asb_nm = c("basket_6"),
                            sp_faxes_coord = sp_faxes_coord_xy,
                                  asb_sp_w = baskets_fruits_weights)
## get species coordinates (basket_6):
sp_faxes_coord_basket6 <- sp_filter_basket6$`species coordinates`


# basket_10:
## filter species from basket_10:
sp_filter_basket10 <- mFD::sp.filter(asb_nm = c("basket_10"),
                            sp_faxes_coord = sp_faxes_coord_xy,
                                  asb_sp_w = baskets_fruits_weights)
## get species coordinates (basket_10):
sp_faxes_coord_basket10 <- sp_filter_basket10$`species coordinates`

## -----------------------------------------------------------------------------
sp_faxes_coord_basket1

## -----------------------------------------------------------------------------
# basket_1:
vert_nm_basket1 <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_basket1,
                                       order_2D = TRUE, 
                                    check_input = TRUE)

# basket_6:
vert_nm_basket6 <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_basket6,
                                       order_2D = TRUE, 
                                    check_input = TRUE)

# basket_10:
vert_nm_basket10 <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_basket10,
                                       order_2D = TRUE, 
                                    check_input = TRUE)

## ----fig.height = 3, fig.width = 5, fig.align = "center"----------------------
plot_try <- mFD::fric.plot(ggplot_bg = plot_k,
                    asb_sp_coord2D = list("basket_1" = sp_faxes_coord_basket1,
                                          "basket_6" = sp_faxes_coord_basket6,
                                          "basket_10" = sp_faxes_coord_basket10),
                   asb_vertices_nD = list("basket_1" = vert_nm_basket1,
                                          "basket_6" = vert_nm_basket6,
                                          "basket_10" = vert_nm_basket10),
                   
                           plot_sp = FALSE,
                   
                          color_ch = NA,
                           fill_ch = c("basket_1" = "#1c9099",
                                       "basket_6" = "#67a9cf",
                                       "basket_10" = "#d0d1e6"),
                             alpha_ch = c("basket_1" = 0.4,
                                       "basket_6" = 0.4,
                                       "basket_10" = 0.4),
                   
                             shape_sp = NA,
                             size_sp = NA,
                             color_sp = NA,
                             fill_sp = NA,
                   
                             shape_vert = NA,
                             size_vert = NA,
                             color_vert = NA,
                             fill_vert = NA)
plot_try

## ----fig.height = 3, fig.width = 5, fig.align = "center"----------------------
plot_try <- mFD::fric.plot(ggplot_bg = plot_k,
                    asb_sp_coord2D = list("basket_1" = sp_faxes_coord_basket1,
                                          "basket_6" = sp_faxes_coord_basket6,
                                          "basket_10" = sp_faxes_coord_basket10),
                   asb_vertices_nD = list("basket_1" = vert_nm_basket1,
                                          "basket_6" = vert_nm_basket6,
                                          "basket_10" = vert_nm_basket10),
                   
                           plot_sp = FALSE,
                   
                          color_ch = c("basket_1" = "#7a0177",
                                       "basket_6" = "#c51b8a",
                                       "basket_10" = "#fa9fb5"),
                           fill_ch = c("basket_1" = "#1c9099",
                                       "basket_6" = "#67a9cf",
                                       "basket_10" = "#d0d1e6"),
                             alpha_ch = c("basket_1" = 0.4,
                                       "basket_6" = 0.4,
                                       "basket_10" = 0.4),
                   
                             shape_sp = NA,
                             size_sp = NA,
                             color_sp = NA,
                             fill_sp = NA,
                   
                             shape_vert = NA,
                             size_vert = NA,
                             color_vert = NA,
                             fill_vert = NA)
plot_try

## ----fig.height = 3, fig.width = 5, fig.align = "center"----------------------
plot_try <- mFD::fric.plot(ggplot_bg = plot_k,
                    asb_sp_coord2D = list("basket_1" = sp_faxes_coord_basket1,
                                          "basket_6" = sp_faxes_coord_basket6,
                                          "basket_10" = sp_faxes_coord_basket10),
                   asb_vertices_nD = list("basket_1" = vert_nm_basket1,
                                          "basket_6" = vert_nm_basket6,
                                          "basket_10" = vert_nm_basket10),
                   
                           plot_sp = TRUE,
                   
                          color_ch = NA,
                           fill_ch = c("basket_1" = "#1c9099",
                                       "basket_6" = "#67a9cf",
                                       "basket_10" = "#d0d1e6"),
                             alpha_ch = c("basket_1" = 0.4,
                                       "basket_6" = 0.4,
                                       "basket_10" = 0.4),
                   
                             shape_sp = c("basket_1" = 21,
                                       "basket_6" = 22,
                                       "basket_10" = 24),
                             size_sp = c("basket_1" = 2,
                                       "basket_6" = 2,
                                       "basket_10" = 2),
                             color_sp = c("basket_1" = "#1c9099",
                                       "basket_6" = "#67a9cf",
                                       "basket_10" = "#d0d1e6"),
                             fill_sp = c("basket_1" = "#1c9099",
                                       "basket_6" = "#67a9cf",
                                       "basket_10" = "#d0d1e6"),
                   
                             shape_vert = c("basket_1" = 21,
                                            "basket_6" = 22,
                                           "basket_10" = 24),
                             size_vert = c("basket_1" = 2,
                                           "basket_6" = 2,
                                          "basket_10" = 2),
                             color_vert = c("basket_1" = "#1c9099",
                                            "basket_6" = "#67a9cf",
                                           "basket_10" = "#d0d1e6"),
                             fill_vert = c("basket_1" = "#1c9099",
                                           "basket_6" = "#67a9cf",
                                          "basket_10" = "#d0d1e6"))
plot_try

## ----fig.height = 3, fig.width = 5, fig.align = "center"----------------------
plot_try <- mFD::fric.plot(ggplot_bg = plot_k,
                    asb_sp_coord2D = list("basket_1" = sp_faxes_coord_basket1,
                                          "basket_6" = sp_faxes_coord_basket6,
                                          "basket_10" = sp_faxes_coord_basket10),
                   asb_vertices_nD = list("basket_1" = vert_nm_basket1,
                                          "basket_6" = vert_nm_basket6,
                                          "basket_10" = vert_nm_basket10),
                   
                           plot_sp = TRUE,
                   
                          color_ch = NA,
                           fill_ch = c("basket_1" = "#1c9099",
                                       "basket_6" = "#67a9cf",
                                       "basket_10" = "#d0d1e6"),
                             alpha_ch = c("basket_1" = 0.4,
                                       "basket_6" = 0.4,
                                       "basket_10" = 0.4),
                   
                             shape_sp = c("basket_1" = 21,
                                       "basket_6" = 22,
                                       "basket_10" = 24),
                             size_sp = c("basket_1" = 2,
                                       "basket_6" = 2,
                                       "basket_10" = 2),
                             color_sp = c("basket_1" = "#1c9099",
                                       "basket_6" = "#67a9cf",
                                       "basket_10" = "#d0d1e6"),
                             fill_sp = c("basket_1" = "#1c9099",
                                       "basket_6" = "#67a9cf",
                                       "basket_10" = "#d0d1e6"),
                   
                             shape_vert = c("basket_1" = 21,
                                            "basket_6" = 22,
                                           "basket_10" = 24),
                             size_vert = c("basket_1" = 2,
                                           "basket_6" = 2,
                                          "basket_10" = 2),
                             color_vert = c("basket_1" = "#7a0177",
                                            "basket_6" = "#c51b8a",
                                           "basket_10" = "#fa9fb5"),
                             fill_vert = c("basket_1" = "#7a0177",
                                            "basket_6" = "#c51b8a",
                                           "basket_10" = "#fa9fb5"))
plot_try

## -----------------------------------------------------------------------------
####### Preliminary steps:

## Compute the range of functional axes:
range_sp_coord  <- range(sp_faxes_coord_fruits)

## Based on the range of species coordinates values, compute a nice range ...
## ... for functional axes:
range_faxes <- range_sp_coord +
    c(-1, 1) * (range_sp_coord[2] - range_sp_coord[1]) * 0.05


####### Create a list that will contains plots for each combination of axis:
plot_FRic <- list()

####### Compute all the combiantion we can get and the number of plots
axes_plot <- utils::combn(c("PC1", "PC2", "PC3", "PC4"), 2)
plot_nb   <- ncol(axes_plot)


######## Loop on all pairs of axes:
# for each combinaison of two axis:
for (k in (1:plot_nb)) {

    # get names of axes to plot:
    xy_k <- axes_plot[1:2, k]
    
    
    ####### Steps previously showed
    
    # a - Background:
    # get species coordinates along the two studied axes:
    sp_faxes_coord_xy <- sp_faxes_coord_fruits[, xy_k]

    # Plot background with grey backrgound:
    plot_k <- mFD::background.plot(range_faxes = range_faxes, 
                               faxes_nm = c(xy_k[1], xy_k[2]),
                               color_bg = "grey95")
    
    
    # b - Global convex-hull:
    # Retrieve vertices coordinates along the two studied functional axes:
    vert <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_xy,  
                      order_2D = FALSE, 
                      check_input = TRUE)
    
    plot_k <- mFD::pool.plot(ggplot_bg = plot_k,
                             sp_coord2D = sp_faxes_coord_xy,
                             vertices_nD = vert,
                             plot_pool = FALSE,
                             color_pool = NA,
                             fill_pool = NA,
                             alpha_ch =  0.8,
                             color_ch = "white",
                             fill_ch = "white",
                             shape_pool = NA,
                             size_pool = NA,
                             shape_vert = NA,
                             size_vert = NA,
                             color_vert = NA,
                             fill_vert = NA)
    
    
    # c - Assemblages convex-hulls and species:
    
    # Step 1: Species coordinates:
    # basket_1:
    ## filter species from basket_1:
    sp_filter_basket1 <- mFD::sp.filter(asb_nm = c("basket_1"),
                                sp_faxes_coord = sp_faxes_coord_xy,
                                      asb_sp_w = baskets_fruits_weights)
    ## get species coordinates (basket_1):
    sp_faxes_coord_basket1 <- sp_filter_basket1$`species coordinates`
    
      
    # basket_6:
    ## filter species from basket_6:
    sp_filter_basket6 <- mFD::sp.filter(asb_nm = c("basket_6"),
                                sp_faxes_coord = sp_faxes_coord_xy,
                                      asb_sp_w = baskets_fruits_weights)
    ## get species coordinates (basket_6):
    sp_faxes_coord_basket6 <- sp_filter_basket6$`species coordinates`
    
    
    # basket_10:
    ## filter species from basket_10:
    sp_filter_basket10 <- mFD::sp.filter(asb_nm = c("basket_10"),
                                sp_faxes_coord = sp_faxes_coord_xy,
                                      asb_sp_w = baskets_fruits_weights)
    ## get species coordinates (basket_10):
    sp_faxes_coord_basket10 <- sp_filter_basket10$`species coordinates`
    
    
    # Step 1 follow-up Vertices names:
    # basket_1:
    vert_nm_basket1 <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_basket1,
                                     order_2D = TRUE, 
                                     check_input = TRUE)
    
    # basket_6:
    vert_nm_basket6 <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_basket6,
                                     order_2D = TRUE, 
                                     check_input = TRUE)
    
    # basket_10:
    vert_nm_basket10 <- mFD::vertices(sp_faxes_coord = sp_faxes_coord_basket10,
                                      order_2D = TRUE, 
                                      check_input = TRUE)
    
    
    # Step 2: plot convex-hulls and species of studied assemblages:
    plot_k <- mFD::fric.plot(ggplot_bg = plot_k,
                    asb_sp_coord2D = list("basket_1" = sp_faxes_coord_basket1,
                                          "basket_6" = sp_faxes_coord_basket6,
                                          "basket_10" = sp_faxes_coord_basket10),
                   asb_vertices_nD = list("basket_1" = vert_nm_basket1,
                                          "basket_6" = vert_nm_basket6,
                                          "basket_10" = vert_nm_basket10),
                   
                           plot_sp = TRUE,
                   
                          color_ch = NA,
                           fill_ch = c("basket_1" = "#1c9099",
                                       "basket_6" = "#67a9cf",
                                       "basket_10" = "#d0d1e6"),
                             alpha_ch = c("basket_1" = 0.4,
                                       "basket_6" = 0.4,
                                       "basket_10" = 0.4),
                   
                             shape_sp = c("basket_1" = 21,
                                       "basket_6" = 22,
                                       "basket_10" = 24),
                             size_sp = c("basket_1" = 2,
                                       "basket_6" = 2,
                                       "basket_10" = 2),
                             color_sp = c("basket_1" = "#1c9099",
                                       "basket_6" = "#67a9cf",
                                       "basket_10" = "#d0d1e6"),
                             fill_sp = c("basket_1" = "#1c9099",
                                       "basket_6" = "#67a9cf",
                                       "basket_10" = "#d0d1e6"),
                   
                             shape_vert = c("basket_1" = 21,
                                            "basket_6" = 22,
                                           "basket_10" = 24),
                             size_vert = c("basket_1" = 2,
                                           "basket_6" = 2,
                                          "basket_10" = 2),
                             color_vert = c("basket_1" = "#1c9099",
                                            "basket_6" = "#67a9cf",
                                           "basket_10" = "#d0d1e6"),
                             fill_vert = c("basket_1" = "#1c9099",
                                           "basket_6" = "#67a9cf",
                                          "basket_10" = "#d0d1e6"))
    
    ####### Save the plot on the plot list:
    plot_FRic[[k]] <- plot_k
    
}





## ----fig.height = 3, fig.width = 5, fig.align = "center"----------------------
plot_FRic

## ----fig.height = 6, fig.width = 8, fig.align = "center"----------------------
patchwork_FRic <- (plot_FRic[[1]] + patchwork::plot_spacer() + patchwork::plot_spacer() +
                  plot_FRic[[2]] + plot_FRic[[4]] + patchwork::plot_spacer() +
                              plot_FRic[[3]] + plot_FRic[[5]] + plot_FRic[[6]]) +
      patchwork::plot_layout(byrow = TRUE, heights = rep(1, 3),
                             widths = rep(1, 3), ncol = 3, nrow = 3,
                             guides = "collect")
patchwork_FRic

