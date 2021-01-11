
##########################################################
#### Set of utility functions  ####
##########################################################

#### loadRData ----
## Load RData objects into the workspace
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

#### max.cor ----
## Function to calculate maximum correlation coefficient between predictor variables, per variable
  max.cor <- function(glmer.model){
    COR <- cov2cor(vcov(glmer.model))
    COR <- COR[-1, -1]
    diag(COR) <- 0
    RES <- apply(COR, 1, function(x){max(abs(x))}) 
    return(RES)
  }

#### max.r ----
## Function to calculate maximum correlation coefficient between predictor variables, retrieved from each model
max.r <- function(x){
  if(class(x)[length(class(x))] == "lm"){
    corm <- summary(x, correlation=TRUE)$correlation}
  else if(class(x) =="lmerMod"){
    corm <- cov2cor(vcov(x))}
  else if(class(x) =="lmerModLmerTest"){
    corm <- cov2cor(vcov(x))}
  else if(class(x) =="glmerMod"){
    corm <- cov2cor(vcov(x))}
  else if(class(x)=="gls"){
    corm <- summary(x)$corBeta} 
  else if(class(x)=="lme"){
    corm <- summary(x)$corFixed}
  else { print("Error: Invalid model class")}
  corm <- as.matrix(corm)
  if (length(corm)==1){
    corm <- 0
    max(abs(corm))
  } else if (length(corm)==4){
    cormf <- corm[2:nrow(corm),2:ncol(corm)]
    cormf <- 0
    max(abs(cormf))
  } else {
    cormf <- corm[2:nrow(corm),2:ncol(corm)]
    diag(cormf) <- 0
    max(abs(cormf))
  }
}

#### overdisp_fun ----
## Bolker's function to assess overdispersion in glmers: http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#reml-for-glmms
overdisp_fun <- function(model) {
  ## number of variance parameters in 
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

#### plot.mar.fixed ----
## Plot marginal distributions of fixed effects from inla model
plot.mar.fixed <- function(inla.model){
  varnames <- names(inla.model$marginals.fixed)
  for(i in 1: length(varnames)){
    var.mar <- data.frame(inla.model$marginals.fixed[i])
    plot(x = var.mar[,1], y=var.mar[, 2], type="l", 
         xlab=paste(names(var.mar)[1]), ylab=paste(names(var.mar)[2]))
    abline(v=0, col="red")
  }
}

#### plot.mar.hyper ----
## Plot marginal distributions of hyperparameters from inla model
plot.mar.hyper <- function(inla.model){
  varnames <- names(inla.model$marginals.hyperpar)
  for(i in 1: length(varnames)){
    var.mar <- data.frame(inla.model$marginals.hyperpar[i])
    plot(x = var.mar[,1], y=var.mar[, 2], type="l", 
         xlab=paste(names(var.mar)[1]), ylab=paste(names(var.mar)[2]))
  }
}

#### inla.residuals.full ----
## Extract residuals from binomial inla model
inla.residuals.full <- function(stack, model, data){
  Index <- inla.stack.index(stack, tag  = "Fit")$data
  Pi <- model$summary.fitted.values[Index, "mean"]
  ExpY <- Pi * data$TotPix
  VarY <- data$TotPix * Pi * (1 - Pi)
  E <- (data$Def - ExpY) / sqrt (VarY)
  RES <- data.frame(Fitted=Pi, Residuals=E)
  return(RES)
}

#### residual.plots ----
## Plot inla model residuals agains each covariate
residual.plots <- function(stack.data, resid.fit.df){
  Xp <- stack.data %>%
    cbind(resid.fit.df)
  predictors <- names(Xp[, -c((ncol(Xp)-2):(ncol(Xp)))])
  for(i in 1:length(predictors)){
    plot(x = Xp[, predictors[i]], y=resid.fit.df$Residuals, 
                   ylab = "Residuals", xlab=predictors[i]); abline(0,0, col="red", lwd=2)
  }
}

#### inla.spatial.cv ----
## Spatial blocks cross validation using k-means
inla.spatial.cv <- function(data, formula, mesh, k){
  start <- Sys.time()
  # Create folds
  set.seed(123456) ## set seed to always create the same folds!
  data$fold <- kmeans(as.matrix(data[, c("Long", "Lat")]), centers = k)$cluster
  plot(data[, "Lat"] ~ data[, "Long"], col = data$fold)
  
  # Define penalised complexity priors for random field. 
  spde <- INLA::inla.spde2.matern(mesh, alpha=2)
  field.indices <- inla.spde.make.index("field", n.spde = mesh$n)
  
  # Make the A matrix 
  Aest <- INLA::inla.spde.make.A(mesh, loc = as.matrix(data[, c("Long", "Lat")]))
  
  # Make an empty column in the data frame read for the out of sample predictions
  data$preds <- NA
  
  ## Begin loop
  for(i in seq_len(k)){
    # Copy the data and then NA out the hold out data.
    d_subset <- data
    d_subset$Def[data$fold == i] <- NA
    
    ## Create data subset - Remove all non-predictors
    d_subset.cv <- d_subset %>%
      dplyr::select(-c(Long, Lat, Def, NDef, TotPix, fold, preds))
    
    # Put the data together
    stk.est <- inla.stack(tag = "estimation", ## tag
                          data = list(y = d_subset$Def, ## Number of deforested pixels
                                      Ntrials = d_subset$TotPix), ## Total number of pixels per grid
                          A = list(Aest, 1),  ## Projector matrix for space, fixed.
                          effects = list(field = field.indices,
                                         cbind(Intercept = 1, d_subset.cv)))
    
    # Run the model
    m <- inla(formula, family = "binomial", 
              data = INLA::inla.stack.data(stk.est), 
              Ntrials = INLA::inla.stack.data(stk.est)$Ntrials,
              control.predictor = list(compute = TRUE, link = 1, A = inla.stack.A(stk.est)),
              control.inla = list(int.strategy = "eb", strategy = "gaussian")) # This makes things quicker. Might be a good idea for cv and then do the most accurate you can for the final model
    
    # Pull out the predictions and put it in the data.frame
    data$preds[data$fold == i] <- m$summary.fitted.values[1:nrow(data), 1][data$fold == i]
  }
  rmse <- sqrt(mean((data$preds - data$Def/data$TotPix)^2))
  end <- Sys.time()
  print(end-start)
  return(rmse)
}

#### create.formulas ----
## Create formulas to run step-wise spatial blocks cross validation
create.formulas <- function(varnames){
  formulas_temp <- list()
  for(i in 1:length(varnames)){
    temp_cov <- paste(varnames[-i], "+", collapse=" ")
    formulas_temp[[i]] <- as.formula(paste("y", "~", -1, "+", "Intercept", "+", 
                                           temp_cov, "f(field, model=spde) + f(grid.ID, model='iid')"))
  }
  formulas_temp[[length(formulas_temp)+1]] <- as.formula(paste("y", "~", -1, "+", "Intercept", "+", 
                                                               paste(varnames, "+", collapse=" "), "f(field, model=spde) + f(grid.ID, model='iid')"))
  print(paste(length(formulas_temp), "formulas created"))
  return(formulas_temp)
}

#### inla.spatial.step ----
## Step-wise spatial blocks cross validation
inla.spatial.step <- function(varnames){
  varnames_rec <- NA
  
  for(k in 1:length(varnames)){
    varnames_new <- varnames[!varnames%in%varnames_rec]
    formulas_tempL <- create.formulas(varnames_new)
    INLA.RES_tempL <- data.frame(Formula = c(1:length(formulas_tempL)), RMSE = NA)
    
    for(i in 1: length(formulas_tempL)){
      INLA.RES_tempL[i, "RMSE"] <- inla.spatial.cv(data=past.def.cc, formula=formulas_tempL[[i]], mesh=m1, k=5)
      print(paste("Iteration # ", i))
    }
    write.csv(arrange(INLA.RES_tempL, RMSE), file=paste0("INLA_step/INLA.RES_", k, ".csv"), row.names=FALSE)
    save(formulas_tempL, file=paste0("INLA_step/formulas_", k, ".RData"))
    
    rmse.bestL <- min(INLA.RES_tempL[,2])
    rmse.fullL <- INLA.RES_tempL[nrow(INLA.RES_tempL),2]
    
    if(rmse.bestL < rmse.fullL){
      form.indexL <- arrange(INLA.RES_tempL, RMSE)[1,1]
      varnames_rec[k] <- varnames_new[form.indexL]
      } else{
        save(varnames_rec, file=paste0("INLA_step/Excluded_variables_", k, ".RData"))
      break
    }
  }
}

#### best.formula ----
## Retrieve formula for best model
best.formula <- function(dir, res, form){
  RMSE <- read.csv(paste0(dir, res, ".csv"), head=TRUE)
  FORMULAS <- loadRData(paste0(dir, form, ".RData"))
  RES <- FORMULAS[RMSE[1,1]]
  return(RES)
}
