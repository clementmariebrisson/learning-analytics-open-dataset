import os
import pandas as pd
from pandas_profiling import ProfileReport

"""
Automatic generation of a standardized univariate and multivariate report for data understanding of all csv datasets.
"""

files = os.listdir()

for file in files:
    if file.__contains__('.csv'):
        df_ = pd.read_csv(file)
        profile = ProfileReport(df_, title="Pandas Profiling Report for "+file.replace(".csv","") )
        profile.to_file("ProfileReport/" + file.replace(".csv","") + ".html")