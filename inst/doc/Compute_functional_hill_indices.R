## -----------------------------------------------------------------------------
data("fruits_traits", package = "mFD")

knitr::kable(head(fruits_traits),
      caption = "Species x traits data frame based on the **fruits** dataset")

## -----------------------------------------------------------------------------
data("baskets_fruits_weights", package = "mFD")

knitr::kable(as.data.frame(baskets_fruits_weights[1:6, 1:6]), 
             caption = "Species x assemblages matrix based on the **fruits** dataset")


## -----------------------------------------------------------------------------
data("fruits_traits_cat", package = "mFD")
knitr::kable(head(fruits_traits_cat), 
             caption = "Traits types based on **fruits & baskets** dataset")

## ----echo = FALSE-------------------------------------------------------------
fruits_gower <- mFD::funct.dist(
  sp_tr         = fruits_traits,
  tr_cat        = fruits_traits_cat,
  metric        = "gower",
  scale_euclid  = "noscale",
  ordinal_var   = "classic",
  weight_type   = "equal",
  stop_if_NA    = TRUE)

## -----------------------------------------------------------------------------
baskets_FD2max <- mFD::alpha.fd.hill(
  asb_sp_w = baskets_fruits_weights, 
  sp_dist  = fruits_gower, 
  tau      = "max", 
  q        = 2)

## -----------------------------------------------------------------------------
baskets_FD2mean <- mFD::alpha.fd.hill(
  asb_sp_w = baskets_fruits_weights, 
  sp_dist  = fruits_gower, 
  tau      = "mean", 
  q        = 2)

## -----------------------------------------------------------------------------
round(cbind(FD2max  = baskets_FD2max$"asb_FD_Hill"[ , 1], 
            FD2mean = baskets_FD2mean$"asb_FD_Hill"[ , 1]), 2)

## -----------------------------------------------------------------------------
# Retrieve species occurrences data:
baskets_summary    <- mFD::asb.sp.summary(baskets_fruits_weights)
baskets_fruits_occ <- baskets_summary$"asb_sp_occ"

head(baskets_fruits_occ)

# Compute alpha FD Hill with q = 0:
baskets_FD0mean <- mFD::alpha.fd.hill(
  asb_sp_w = baskets_fruits_occ, 
  sp_dist  = fruits_gower, 
  tau      = "mean", 
  q        = 0)

round(baskets_FD0mean$"asb_FD_Hill", 2)

## -----------------------------------------------------------------------------
# retrieve total weight per basket:
baskets_summary$"asb_tot_w"

# Here baskets all contain 2000g of fruits, we illustrate how to compute...
# relative weights using the output of asb.sp.summary:

baskets_fruits_relw <- baskets_fruits_weights / baskets_summary$"asb_tot_w"
apply(baskets_fruits_relw, 1, sum)

## -----------------------------------------------------------------------------
# Compute index:
baskets_betaq2 <- mFD::beta.fd.hill(
  asb_sp_w  = baskets_fruits_relw, 
  sp_dist   = fruits_gower, 
  q         = 2,
  tau       = "mean", 
  beta_type = "Jaccard")

# Then use the mFD::dist.to.df function to ease visualizing result
mFD::dist.to.df(list_dist = list("FDq2" = baskets_betaq2$"beta_fd_q"$"q2"))

