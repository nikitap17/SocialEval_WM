# -*- coding: utf-8 -*-
"""
Ceated on Sat Feb 17 13:50:25 2024

@autho: nikit
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import os
from scipy.stats import zscore

from Empatica_Functions import Empatica_data



## Data Preparation

folder_path = "../Data/raw_data/raw_EDA/"
person_IDs = [folder for folder in os.listdir(folder_path + "Empatica")]

#### Empatica disconnected fo the following paticipant: 40122
person_IDs = [ID for ID in person_IDs if ID != "40122"]


### Process EDA and ACC for all participants
processed_EDA = dict()
processed_EDA_onesec = dict()
for ID in person_IDs:
    empatica_folder = folder_path + "Empatica/" + ID + "/"
    timing_folder = folder_path + "Timing/" + ID + "_timing_blocks.csv"


    empatica_wrapper = Empatica_data(empatica_folder, timing_folder)
    
    EDA = empatica_wrapper.eda
    
    ACC = empatica_wrapper.acc
    
    
    # downsample ACC to EDA's 4Hz
    downsampled_ACC = ACC.iloc[:,0:3].resample('250L').mean()  # '250L' corresponds to 4 Hz (L stands for milliseconds)
    EDA_pro = pd.merge(EDA, downsampled_ACC, left_index=True, right_index=True, how='inner')
    
    # Drop rows that were not assigned any condition, i.e. data before and after experiment
    EDA_pro.dropna(subset=["condition"], inplace=True)
    
    # z-score within participants, due to big interindividual differences in EDA
    EDA_pro["EDA_zw"] = zscore(EDA_pro.EDA)
    
    
    # Create one dataset with data resampled to 1 Hz
    EDA_onesec = EDA_pro.drop(columns=["condition"])
    EDA_onesec = EDA_onesec.resample('1S').mean()
    EDA_onesec.dropna(inplace=True)
    
    # Add participant's ID
    EDA_pro["ID"] = np.full(len(EDA_pro),ID)
    EDA_pro.insert(0, "ID", EDA_pro.pop("ID"))
    
    EDA_onesec["ID"] = np.full(len(EDA_onesec),ID)
    EDA_onesec.insert(0, "ID", EDA_onesec.pop("ID"))
    
    # Add counter for participant's data
    EDA_pro["counter"] = np.arange(1,len(EDA_pro)+1)
    
    EDA_onesec["counter"] = np.arange(1,len(EDA_onesec)+1)
    
    # Start timestamp at 0 for each participant
    EDA_pro.reset_index(level=0, inplace=True)
    EDA_pro["timestamps"] = EDA_pro["timestamps"] - EDA_pro["timestamps"][0]
    EDA_pro["timestamps"] = EDA_pro["timestamps"].dt.total_seconds()
    
    EDA_onesec.reset_index(level=0, inplace=True)
    EDA_onesec["timestamps"] = EDA_onesec["timestamps"] - EDA_onesec["timestamps"][0]
    EDA_onesec["timestamps"] = EDA_onesec["timestamps"].dt.total_seconds()
    
    
    
    processed_EDA[ID] = EDA_pro
    processed_EDA_onesec[ID] = EDA_onesec



### Merging seperate dfs into a single main df (including all participants)
EDA_main = pd.concat(processed_EDA.values(), axis=0)
EDA_main.to_csv("../Data/clean_data/EDA_main.csv", index=False)

EDA_main_onesec = pd.concat(processed_EDA_onesec.values(),axis=0)
EDA_main_onesec.to_csv("../Data/clean_data/EDA_main_onesec.csv", index=False)


## Aggregate by common counter - Mean across participants and time

# In order to aggregate correcly, we have to make sure that each participant has the same number of seconds (same condition lengths)
number_counts = EDA_main_onesec['counter'].value_counts().reset_index()
number_counts.columns = ['number', 'count']

# trim data where counts is higher than 1088
EDA_main_onesec = EDA_main_onesec.loc[EDA_main_onesec.counter <= 1088,]
EDA_onesec_agg = EDA_main_onesec.groupby('counter')[["EDA", "ACCx","ACCy","ACCz","EDA_zw"]].mean().reset_index(level=0)



## Visualisation

### Plot EDA time series
plt.figure(figsize=(8, 4.5))

plt.plot(EDA_onesec_agg.counter,EDA_onesec_agg.EDA_zw, c="#FED789", linewidth=1.5)
#plt.plot(EDA_onesec_agg.counter,EDA_onesec_agg.EDA_pr, c="blue", alpha= 0.7, linewidth=1, label="Averaged EDA")

plt.axvline(max(EDA_onesec_agg.counter)/2, c="#3C304B", linestyle="--",
            linewidth = 3, label = "Condition Switch")
plt.ylabel("Electrodermal Activity (EDA)", fontsize = 11)
plt.xlabel("Social Evaluation", fontsize = 11)

ax = plt.gca()
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)

# Calculate positions for custom ticks
x_max = max(EDA_onesec_agg.counter)
tick_positions = [x_max / 4, 3 * x_max / 4]  # First quarter and third quarter

# Set custom x-axis ticks and labels
plt.xticks(tick_positions, ["Control", "Evaluation"], fontsize=9, color = "#202020")
plt.yticks(fontsize=9, color = "#202020")
plt.legend()

plt.savefig('TablesFigures/Figure 3 - EDA timeseries.png', bbox_inches="tight",
            dpi = 600)











