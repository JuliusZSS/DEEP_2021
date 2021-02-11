# DEEP_2021
DEEP code (R), causal tree code (R), interaction tree code (R), uplift tree code (Python)

To run experiments using synthetic_data:
1. in synthetic_data folder, run sz102 to generate data and get causal tree results
2. in DEEP_R folder, run main_synthetic_self_training.R and main_synthetic_cross_validation.R to get DEEP results
3. in synthetic_data folder, run sz201 sz202 sz203 sz204 to analyse DEEP results
4. in interaction_trees folder, run Experiments_synthetic_self_training.R and Experiments_synthetic_cross_validation.R to get interaction tree results
5. in synthetic_data folder, run sz300 sz301 to analyse interaction tree results
6. in uplift folder, run main_synthetic_data.py to get uplift tree results
7. in synthetic_data folder, run sz400 sz401 to analyse uplift tree results

To run experiments using real-world data:
1. choose one of the real-world datasets. eg. US census
2. in real_world_data folder, run sz000 sz001 to get causal tree results
3. in DEEP_R folder, run main_real_world_bias.R to get DEEP results
4. in real_world_data folder, run sz100 sz101 to analyse DEEP results
5. in interaction_trees folder, run Experiments_real_world.R to get interaction tree results
6. in real_world_data folder, run sz200 sz201 to analyse interaction tree results
7. rename real_world_data/data folder as real_world_data/data_US_census
8. in uplift folder, run main_real_world_data_US_census.py to get uplift tree results 
9. in real_world_data folder, run sz300 to analyse uplift tree results

