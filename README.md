# CFSv2 Bias Correction with Machine Learning
### _(Title Subject to Change)_

##### Sub-directories:
- **./data** contains .tif for elevation data. curl and extract data from 1982-2010 into **./data/old** and 2012-2020 into **./data/new**
- **./deprecated** contains Rscripts we no longer use, but we're keeping as backup. Never delete!
- **./images** contains charts and graphs from tests/experiments

##### Rscripts:
- **make_train_data.R** will clean, subset, and store a data slice from the _old_ data. No fancy formatting, you'll have to dig into the code...
- **make_test_data.R** is similar to _make_train_, but requires a **train** data slice, and generates a similar slice from the _test_ data.
- **random_forest_dev** has scripts for running a cell-by-cell RF.
- **rf_testing** will take a dataframe of RFs as input and _train_ and _test_ datasets, then do MSE analysis on the _test_ data against GT, QM, and climate norm.
- **make_data_with_state** is similar to _make_train_data_, but you can filter cells by state (rather than random sampling).
- **rf_multi_cell** can build and tune RFs on multiple points simultaneously. This is the alternate to:
- **rf_single_cell**, which builds and tunes a separate RF for every cell in the train data.
- **ML** contains ACF and LSTM resources.
- **stats_testing** has some experimental statistical methods such as GAM.
- **state-bias** determines bias on the full _old_ dataset by state using random sampling.