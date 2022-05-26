# Nudging NOAA: Seasonal Forecast Bias Correction with Machine Learning

##### Helpful commands:
-

##### Sub-directories:
- **./data** contains .tif for elevation data. curl and extract data from 1982-2010 into **./data/old** and 2012-2020 into **./data/new**
- **./deprecated** contains Rscripts we no longer use, but we're keeping as backup. Never delete!
- **./images** contains charts and graphs from tests/experiments

##### Rscripts:
- **climate_norm_experiment** tests climate norm versus quantile matching performance on all US pixels
- **CNN_Dev**
- **CNN Practice**
- **data_sampling_dev** 
- **EDA_state** exploratory data analysis for some other US states
- **EDA_Vermont** exploratory data analysis for Vermont
- **LSTM_dev**
- **make_data_dev**
- **make_data_slice**
- **make_data_with_state** is similar to _make_train_data_, but you can filter cells by state (rather than random sampling).
- **make_test_data** is similar to _make_train_, but requires a **train** data slice, and generates a similar slice from the _test_ data.
- **made_test_data_CNN** will generate a 10 x 10 pixel testing subset of US data (includes Vermont)
- **make_train_data** will clean, subset, and store a data slice from the _old_ data. No fancy formatting, you'll have to dig into the code...
- **made_train_data_CNN_vermont** will generate a 10 x 10 pixel training subset of US data (includes Vermont)
- **ML** contains ACF and LSTM resources.
- **PCA** conducts a principal components analysis and visualizes results for training + testing data
- **process_data_CNN** shapes train and test data to be the correct input shape for CNN
- **results_visualizations** visualizes model results
- **rf_testing_multi_cell** will take a dataframe of RFs as input and _train_ and _test_ datasets, then do MSE analysis on the _test_ data against GT, QM, and climate norm.
- **rf_training_multi_cell** can build and tune RFs on multiple points simultaneously. This is the alternate to:
- **rf_training_single_cell**, which builds and tunes a separate RF for every cell in the train data.
- **state-bias** determines bias on the full _old_ dataset by state using random sampling.
- **stats_testing** has some experimental statistical methods such as GAM.
