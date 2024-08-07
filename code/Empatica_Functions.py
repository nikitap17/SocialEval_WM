# -*- coding: utf-8 -*-
"""
Created on Sat Feb 17 11:52:30 2024

@author: nikit
"""

import numpy as np
import pandas as pd
from datetime import datetime, timedelta
import matplotlib.pyplot as plt
import os
from pathlib import Path


class Empatica_data():

    def __init__(self, path_to_folder, timing_folder, buffer_seconds=0):

        self.tags = timing_folder

        self.buffer = buffer_seconds

        self.bvp = self.read_data(path_to_folder + 'BVP.csv')
        #self.bvp = self.bvp.set_index("timestamps")
        self.bvp.rename(columns={self.bvp.columns[0]:"BVP"}, inplace=True)
        self.bvp = self.add_conditions(self.bvp, self.tags)
        #self.bvp.reset_index(level=0, inplace=True)

        self.eda = self.read_data(path_to_folder + 'EDA.csv')
        #self.eda = self.eda.set_index("timestamps")
        self.eda.rename(columns={self.eda.columns[0]:"EDA"}, inplace=True)
        self.eda = self.add_conditions(self.eda, self.tags)
        #self.eda.reset_index(level=0, inplace=True)

        self.hr = self.read_data(path_to_folder + 'HR.csv')
        #self.hr = self.hr.set_index("timestamps")
        self.hr.rename(columns={self.hr.columns[0]:"HR"}, inplace=True)
        self.hr = self.add_conditions(self.hr, self.tags)
        #self.hr.reset_index(level=0, inplace=True)
        
        self.acc = self.read_data(path_to_folder + 'ACC.csv')
        #self.acc = self.hr.set_index("timestamps")
        self.acc.rename(columns={self.acc.columns[0]:"ACCx",
                            self.acc.columns[1]:"ACCy",
                            self.acc.columns[2]:"ACCz"}, inplace=True)
        self.acc = self.add_conditions(self.acc, self.tags)
        #self.acc.reset_index(level=0, inplace=True)


    def read_data(self, path):
        self.path = path
     
        data = pd.read_csv(self.path, header=None)

        timestamp = float(data.iloc[0,0])       # marks beginning timestamp
        sampling_frequency = float(data.iloc[1,0])  # marks sampling rate in Hz
        
        data = data.iloc[2:,:]

        data['timestamps'] = pd.date_range(start=pd.to_datetime(timestamp, unit='s'),
                                           periods=len(data), freq=str(1 / sampling_frequency * 1000) + 'ms',
                                    tz='UTC') #name='timestamp',
        data['timestamps'] = pd.to_datetime(data['timestamps'], unit='s').dt.tz_localize(None)
        
        data["timestamps"] = data["timestamps"] + pd.Timedelta(hours=1)  # Data collection was in Vienna, which is +1 GMT
        data.set_index("timestamps", inplace=True)

        self.data = data

        return data

    def add_conditions(self, data, tags):
        timings = pd.read_csv(tags)
        timings.start_time = pd.to_datetime(timings.start_time)
        timings.end_time = pd.to_datetime(timings.end_time)
        
        

        data["condition"] = np.full(len(data), np.nan).astype("object")
                
        # Questionnaire condition
        #mask = (data.index > min(timings.start_time)) & (data.index < max(timings.end_time))
        #data.loc[mask, "condition"] = "questionnaire"
        
        # Conditions
        for idx, condition in enumerate(timings['Block']):
            
            if idx%2 ==0:
                # Resting condition
                mask = (data.index >= (timings.start_time[idx] - pd.Timedelta(seconds=65))) & (data.index <= timings.start_time[idx])
                data.loc[mask, 'condition'] = "resting"
                
            elif idx%2 != 0:
                # Break condition
                mask1 = (data.index > (timings.end_time[idx-1])) & ((data.index < timings.start_time[idx]))
                data.loc[mask1, 'condition'] = "break"

            
            # Primary conditions
            mask = (data.index >= timings.start_time[idx]) & (data.index <= timings.end_time[idx])
            data.loc[mask, 'condition'] = condition
            

        return data






