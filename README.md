# Strategies

This repo simply contains trading strategies to be run by other processes.

### Strategy Directory

Each strategy has its own directory.\
Strategies will leverage `get_all_data()` (from the `common-code` repo).\
Strategies can pull model files from S3, and run predictions.\
Strategies can create other new variables if needed.\
Strategies need to determine whether a row of data should result in a buy or sell.\
Preceeding rows can be used in buy/sell logic as well.
