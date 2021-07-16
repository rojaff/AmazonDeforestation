# AmazonDeforestation
Data and R scripts accompanying the paper "Forecasting deforestation in the Brazilian Amazon to prioritize conservation efforts". This repository contains an RStudio Project with the following folders:

- DataBase: Data sources and geospatial database.
- Metrics: Set of functions to compute grid-level metrics and parallelization of grid-level metrics.
- GridData: Example data used to run INLA-SPDE models.
- Spatial_CV: Full model containing all 20 predictor variables & spatial blocks cross-validation, and assessment of model accuracy across temporal and spatial scales.
- Predictions: Deforestation predictions based on the selected models, deforestation predictions in protected areas and indigenous lands, final effect plots, and interactive html map.
- Utilities: Set of utility functions.
- Subset_CV: Full models, spatial blocks cross-validation, and deforestation predictions for the Altamira region.
