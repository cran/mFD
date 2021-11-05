## -----------------------------------------------------------------------------
data("fruits_traits", package = "mFD")

fruits_traits <- fruits_traits[ , 1:4]      # only keep the first 4 traits to illustrate FEs

# Decrease the number of modalities per trait for convenience ...
# ... (to have less unique combinations of trait values):

# Size grouped into only 3 categories:
fruits_traits[ , "Size"] <- as.character(fruits_traits[ , "Size"])

fruits_traits[which(fruits_traits[ , "Size"] %in% c("0-1cm", "1-3cm", "3-5cm")), "Size"] <- "small"
fruits_traits[which(fruits_traits[ , "Size"] == "5-10cm"), "Size"]  <- "medium"
fruits_traits[which(fruits_traits[ , "Size"] == "10-20cm"), "Size"] <- "large"

fruits_traits[ , "Size"] <- factor(fruits_traits[, "Size"], levels = c("small", "medium", "large"), ordered = TRUE)

# Plant type grouped into only 2 categories:
fruits_traits[ , "Plant"] <- as.character(fruits_traits[, "Plant"])

fruits_traits[which(fruits_traits[ , "Plant"] != "tree"), "Plant"] <- "Not_tree"
fruits_traits[ , "Plant"] <- factor(fruits_traits[ , "Plant"], levels = c("Not_tree", "tree"), ordered = TRUE)

# Plant Origin grouped into only 2 categories:
fruits_traits[ , "Climate"] <- as.character(fruits_traits[ , "Climate"])

fruits_traits[which(fruits_traits[ , "Climate"] != "temperate"), "Climate"] <- "tropical"
fruits_traits[ , "Climate"] <- factor(fruits_traits[, "Climate"], levels = c("temperate", "tropical"), ordered = TRUE)

# Display the table:
knitr::kable(head(fruits_traits), caption = "Species x traits dataframe based on *fruits* dataset")

## -----------------------------------------------------------------------------
data("baskets_fruits_weights", package = "mFD")

knitr::kable(as.data.frame(baskets_fruits_weights[1:6, 1:6]), 
             caption = "Species x assemblages dataframe based on *fruits* dataset")

## ---- echo = FALSE------------------------------------------------------------
data("fruits_traits_cat", package = "mFD")

# only keep traits 1 - 4:
fruits_traits_cat <- fruits_traits_cat[1:4, ]

knitr::kable(head(fruits_traits_cat), 
             caption = "Traits types based on *fruits & baskets* dataset")

## -----------------------------------------------------------------------------
# summarize species assemblages: 
asb_sp_fruits_summ <- mFD::asb.sp.summary(baskets_fruits_weights)

# retrieve species occurrences for the first 3 assemblages (fruits baskets):
head(asb_sp_fruits_summ$asb_sp_occ, 3)

asb_sp_fruits_occ <- asb_sp_fruits_summ$"asb_sp_occ"

## ---- results = "hide", eval = FALSE------------------------------------------
#  mFD::sp.to.fe(
#    sp_tr       = fruits_traits,
#    tr_cat      = fruits_traits_cat,
#    fe_nm_type  = "fe_rank",
#    check_input = TRUE)

## -----------------------------------------------------------------------------
sp_to_fe_fruits <- mFD::sp.to.fe(
  sp_tr       = fruits_traits, 
  tr_cat      = fruits_traits_cat, 
  fe_nm_type  = "fe_rank", 
  check_input = TRUE) 

## -----------------------------------------------------------------------------
sp_to_fe_fruits$"fe_nm"

## -----------------------------------------------------------------------------
sp_fe <- sp_to_fe_fruits$"sp_fe"
sp_fe

## -----------------------------------------------------------------------------
fe_tr <- sp_to_fe_fruits$"fe_tr"
fe_tr

## -----------------------------------------------------------------------------
fe_nb_sp <- sp_to_fe_fruits$"fe_nb_sp"
fe_nb_sp

## -----------------------------------------------------------------------------
sp_to_fe_fruits$"details_fe"

## ---- results = "hide", eval = FALSE------------------------------------------
#  mFD::alpha.fd.fe(
#    asb_sp_occ       = asb_sp_fruits_occ,
#    sp_to_fe         = sp_to_fe_fruits,
#    ind_nm           = c("fred", "fored", "fvuln"),
#    check_input      = TRUE,
#    details_returned = TRUE)

## -----------------------------------------------------------------------------
alpha_fd_fe_fruits <- mFD::alpha.fd.fe(
  asb_sp_occ       = asb_sp_fruits_occ, 
  sp_to_fe         = sp_to_fe_fruits,
  ind_nm           = c("fred", "fored", "fvuln"),
  check_input      = TRUE,
  details_returned = TRUE) 

## -----------------------------------------------------------------------------
# dataframe with indices values for each assemblage:
alpha_fd_fe_fruits$"asb_fdfe"

# a matrix gathering the number of species per FE in each assemblage
alpha_fd_fe_fruits$"details_fdfe"

## ---- results = FALSE, eval = FALSE-------------------------------------------
#  mFD::alpha.fd.fe.plot(
#    alpha_fd_fe       = alpha_fd_fe_fruits,
#    plot_asb_nm       = c("basket_1"),
#    plot_ind_nm       = c("fred", "fored", "fvuln"),
#    name_file         = NULL,
#    color_fill_fored  = "darkolivegreen2",
#    color_line_fred   = "darkolivegreen4",
#    color_fill_bar    = "grey80",
#    color_fill_fvuln  = "lightcoral",
#    color_arrow_fvuln = "indianred4",
#    size_line_fred    = 1.5,
#    size_arrow_fvuln  = 1,
#    check_input       = TRUE)

## ---- fig.height = 7, fig.width = 12, fig.align = "center"--------------------
mFD::alpha.fd.fe.plot(
  alpha_fd_fe       = alpha_fd_fe_fruits,
  plot_asb_nm       = c("basket_1"),
  plot_ind_nm       = c("fred", "fored", "fvuln"),
  name_file         = NULL,
  color_fill_fored  = "darkolivegreen2",
  color_line_fred   = "darkolivegreen4",
  color_fill_bar    = "grey80",
  color_fill_fvuln  = "lightcoral",
  color_arrow_fvuln = "indianred4",
  size_line_fred    = 1.5,
  size_arrow_fvuln  = 1,
  check_input       = TRUE)

