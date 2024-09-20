import numpy as np
import pandas as pd
from datetime import datetime, timedelta
import matplotlib.pyplot as plt
import os
from pathlib import Path
from scipy.stats import zscore
from functools import reduce
import statsmodels.api as sm
import statsmodels.formula.api as smf
from statsmodels.tools.sm_exceptions import ConvergenceWarning

from Empatica_Functions import Empatica_data






folder_path = os.getcwd() + "\\Data\\Physio_d\\Empatica"

person_IDs = [folder for folder in os.listdir(folder_path) if os.path.isdir(os.path.join(folder_path, folder))]
### Empatica disconnected fo the following paticipant: 40122
person_IDs = [ID for ID in person_IDs if ID != "40122"]




## Process EDA and ACC for all participants
processed_e4 = dict()
for ID in person_IDs:
    empatica_folder = os.getcwd() + '\\Data\\Physio_d' + "\\Empatica\\" + ID + "\\"
    timing_folder = os.getcwd() + '\\Data\\Physio_d' + "\\Timing\\" + ID + "_timing_blocks.csv"


    empatica_wrapper = Empatica_data(empatica_folder, timing_folder)
    
    EDA = empatica_wrapper.eda
    ACC = empatica_wrapper.acc
    BVP = empatica_wrapper.bvp
    HR = empatica_wrapper.hr
    
    
    # downsample ACC, EDA, BVP to HR's 1Hz
    downsampled_ACC = ACC.iloc[:,0:3].resample('1S').mean()
    ds_EDA = EDA.iloc[:,0].resample('1S').mean()
    ds_BVP = BVP.iloc[:,0].resample('1S').mean()
    
    # merge physio data
    E4_pro = pd.merge(ds_EDA, ds_BVP, left_index=True, right_index=True, how='inner')
    E4_pro = pd.merge(E4_pro, downsampled_ACC, left_index=True, right_index=True, how='inner')
    E4_pro = pd.merge(E4_pro, HR, left_index=True, right_index=True, how='inner')
    
    
    # Drop rows that were not assigned any condition, i.e. data before and after experiment
    E4_pro.dropna(subset=["condition"], inplace=True)
    
    # z-score within participants, due to big interindividual differences in EDA
    E4_pro["EDA_zw"] = zscore(E4_pro.EDA)
    E4_pro["HR_zw"] = zscore(E4_pro.HR)
    E4_pro["BVP_zw"] = zscore(E4_pro.BVP)
    

    # Add participant's ID
    E4_pro["ID"] = np.full(len(E4_pro),ID)
    E4_pro.insert(0, "ID", E4_pro.pop("ID"))
    
    # Add counter for participant's data
    E4_pro["counter"] = np.arange(1,len(E4_pro)+1)
    
    
    # Start timestamp at 0 for each participant
    E4_pro.reset_index(level=0, inplace=True)
    E4_pro["timestamps"] = E4_pro["timestamps"] - E4_pro["timestamps"][0]
    E4_pro["timestamps"] = E4_pro["timestamps"].dt.total_seconds()
    
    
    
    processed_e4[ID] = E4_pro



### Merging seperate dfs into a single main df
empatica_main = pd.concat(processed_e4.values(), axis=0)
#EDA_main.to_csv(r"Data/Physio_d/EDA_main.csv", index=False)








