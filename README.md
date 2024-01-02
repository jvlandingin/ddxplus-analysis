# ddxplus-analysis

### Overview

This repository contains the scripts used for analysis and model development trained using the differential diagnosis

### scripts

**1 - Libraries and Functions.R** - Contains all packages and functions used

**2 - Data import and preprocessing.R** - Pull all datasets for analysis. Contains initial preprocessing

#### 3. Feature Engineering (FE)

**3.1 - FE (evidences).R** - contains script for unnesting the EVIDENCES column and creating new columns for the purpose of becoming features for model development

**3.2 - FE (diff_diagnosis).R** - contains script for unnesting the DIFFERENTIAL_DIAGNOSIS column and separating each diagnosis as a new row

**3.3 - FE (final_prep).R** - contains minor processing before exploration and model development

**4 - Data Exploration.R** - contains exploratory analysis highlighting the need of using differential diagnosis for training and as an output for automated disease detection

**5 - Modeling.R** - Model development

**6 - Analyze modeling results.R** - Check model accuracy from within the DDXPlus training set

**7 - Analyzing validation set.R** - Check model accuracy using the DDXPlus validation set
