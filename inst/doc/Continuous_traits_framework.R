## ---- warning=FALSE-----------------------------------------------------------
# load dataset:
sp_tr <- read.csv(system.file("extdata", "data_cestes_sp_tr.csv", 
                              package = "mFD"), dec = ",", sep = ":")

rownames(sp_tr) <- sp_tr$"Sp"
sp_tr <- sp_tr[ , -1]

# display the table:
knitr::kable(head(sp_tr), 
             caption = "Species x Traits data frame based on *CESTES* dataset")

## ---- warning = FALSE---------------------------------------------------------
# load dataset:
asb_sp_w <- read.csv(system.file("extdata", "data_cestes_asb_sp_w.csv", 
                                 package = "mFD"), dec = ",", sep = ":")

rownames(asb_sp_w) <- paste0("site", sep = "_", asb_sp_w$Sites)
asb_sp_w <- asb_sp_w[ , -1]

asb_sp_w$Urobatis_jamaicensis <- as.numeric(asb_sp_w$Urobatis_jamaicensis)

# remove sites 12, 23, 35 because FRic can not be computed on it...
# ... (for a clean example):
asb_sp_w <- asb_sp_w[-c(11, 22, 33), ]

# display the table:
knitr::kable(asb_sp_w[1:7, 1:6], 
             caption = "Species x Assemblages data frame based on *CESTES* dataset for the first six species and first seven sites")

## ---- results = "hide"--------------------------------------------------------
mFD::tr.cont.fspace(
  sp_tr        = sp_tr, 
  pca          = TRUE, 
  nb_dim       = 7, 
  scaling      = "scale_center",
  compute_corr = "pearson")

## ---- results = "hide"--------------------------------------------------------
fspace <- mFD::tr.cont.fspace(
  sp_tr        = sp_tr, 
  pca          = TRUE, 
  nb_dim       = 10, 
  scaling      = "scale_center",
  compute_corr = "pearson")

## -----------------------------------------------------------------------------
fspace$"quality_metrics"

## -----------------------------------------------------------------------------
fspace$"eigenvalues_percentage_var"

## -----------------------------------------------------------------------------
head(fspace$"sp_faxes_coord")

## -----------------------------------------------------------------------------
dist_mat <- as.matrix(fspace$sp_dist_multidim$"6D")
dist_mat[1:5, 1:5]

## -----------------------------------------------------------------------------
dist_mat <- as.matrix(fspace$sp_dist_init)
dist_mat[1:5, 1:5]

## -----------------------------------------------------------------------------
fspace$"tr_correl"

