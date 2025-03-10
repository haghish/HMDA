
#' @export

#infogram,targetencoder,deeplearning,glm,glrm,kmeans,naivebayes,pca,svd,,,,extendedisolationforest,aggregator,word2vec,stackedensemble,coxph,generic,gam,anovaglm,psvm,rulefit,upliftdrf,modelselection,isotonicregression,dt,adaboost
# , "XGBoost"
hmda.grid <- function(algorithm = c("randomForest", "drf",  "gbm", "xrf", "isolationforest"),
                      grid_id = NULL,
                      x,
                      y,
                      training_frame = h2o.getFrame("hmda.train.hex"),
                      validation_frame = NULL,
                      hyper_params = list(),

                      # # HYPER PARAMETERS
                      # hyper_params = list(
                      #   ntrees = seq(10,50,2),
                      #   max_depth = c(10,20,30),
                      #   min_rows = c(1, 3),
                      #   sample_rate  = NULL,
                      #   col_sample_rate_per_tree = NULL,
                      #   col_sample_rate_change_per_level = NULL,
                      #   nbins = NULL,
                      #   nbins_cats = NULL
                      # ),

                      fold_assignment = "Modulo",
                      nfolds = 10,
                      seed = NULL,
                      fold_column = NULL,
                      weights_column = NULL,
                      keep_cross_validation_predictions = TRUE,

                      # STOPPING PARAMETERS
                      stopping_rounds = NULL,
                      stopping_metric = "AUTO",
                      stopping_tolerance = NULL,

                      # recovery and saving
                      recovery_dir = NULL,

                      ...) {


  # Grammar check
  # ===========================================================
  if (length(algorithm) > 1) stop("only one algorithm is supported at the time")
  algorithm <- match.arg(algorithm) # Match argument for safety

  if (class(training_frame) != "H2OFrame") {
    if (class(training_frame) == "character") {
      training_frame <- h2o.getFrame("hmda.train.hex")
    }
    else {
      stop("the 'training_frame' argument is not referenced to an existing H2O frame")
    }
  }

  # check and generate grid_id
  if (is.null(grid_id)) {
    grid_id <- paste0(algorithm, "_grid_", as.integer(Sys.time()))
    message(paste("an automatic 'grid_id' was generated:", grid_id))
  }

  # Define model IDs
  # ============================================================
  MODELIDS <- list()

  # Tuning
  grid <- h2o.grid(algorithm = algorithm,
                   y = y,
                   x = x,
                   training_frame = training_frame,
                   hyper_params = hyper_params,
                   grid_id = grid_id,

                   # this setting ensures the models are comparable
                   seed = seed,
                   nfolds = nfolds,
                   fold_assignment = fold_assignment,
                   keep_cross_validation_predictions = keep_cross_validation_predictions,
                   recovery_dir = recovery_dir,
                   ...)

  # Save the models if required
  # ============================================================
  if (!is.null(recovery_dir)) {
    h2o.saveGrid(
      grid_directory = paste0(recovery_dir,"/",grid_id),
      grid_id = grid_id,
      save_params_references = FALSE,
      export_cross_validation_predictions = TRUE
    )
  }

  # Get the IDs
  # ============================================================
  MODELIDS <- unlist(grid@model_ids)

  return(MODELIDS)
}
