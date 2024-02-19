import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import statistics

from scipy import stats, signal

def VTC(filename, key: str = 'RT'):
    """
    Takes in a filename and a variable name and compute variance time course.

    Args:
        filename:
            Name of the file in xls form.
        key:
            Name of the variable of interest (RT, Dev, MT, etc.) in string form. Defaults to response time (RT).

    Returns:
        A dataset with new computed values for the variable of interest. 
        
    """
    
    file = pd.read_excel(filename)

    #Replace values with NA if values are smaller than 0.1
    if key != "Curv":
        file[key] = file[key].where(file[key] > 0.1, np.NaN) 
        
    file[key] = file[key].interpolate(method = 'linear')
    file = file.iloc[3:-1]

    #Find z-score and take absolute values
    file['z'+key] = abs(stats.zscore(file[key]))
    
    #Take 20 points in each computation and compute Gaussian function. We can play around with these parameters
    L = 20
    W = signal.windows.gaussian(L, 2.355)/7 

    #Apply zero phase filtering in each window
    VTC_smooth = signal.filtfilt(W, 1, file['z'+key], padtype = None, padlen= None)

    #Smoothed values lower than median scores are in-zone periods, and higher smoothed values are out-zone periods
    file['smoothed'+key] = VTC_smooth
    med = statistics.median(file['smoothed'+key])
    file['zone'] = np.where(file['smoothed'+key] < med,'in-zone','out-zone')

    return file[["Subject", "ACC", "TrialType", 'zone', key, 'smoothed'+key ]]
    
def error_rate(data, idx, var):

    """
    Calculates omission and commision error rate for each period.

    Args:
        data:
            Dataset with accuracy (1 = correct responses, 0 = incorrect responses), trial (1 = go trials, 0 = no-go trials)

        idx:
            Participant index. For example, participant 1 has an index of 1

    Returns:
        A dataframe of error rates for each participant.
        
    """
    ##In-zone periods
    in_zone = data.loc[(data['zone'] == 'in-zone')]

    #Omission rate for in-zone periods
    in_zone_go = in_zone.loc[in_zone['TrialType'] == 1]
    in_zone_go_incorrect = in_zone_go.loc[in_zone_go['ACC'] == 0]
    in_zone_om = len(in_zone_go_incorrect)/len(in_zone_go)

    #Comission rate for in-zone periods
    in_zone_nogo = in_zone.loc[in_zone['TrialType'] == 2]
    in_zone_nogo_incorrect = in_zone_nogo.loc[in_zone_nogo['ACC'] == 0]
    in_zone_com = len(in_zone_nogo_incorrect)/len(in_zone_nogo)

    ## Out-of-the-zone periods
    out_zone = data.loc[(data['zone'] == 'out-zone')]

    #Omission rate for out-of-the-zone periods
    out_zone_go = out_zone.loc[out_zone['TrialType'] == 1]
    out_zone_go_incorrect = out_zone_go.loc[out_zone_go['ACC'] == 0]
    out_zone_om = len(out_zone_go_incorrect)/len(out_zone_go)

    #Comission rate for out-of-the-zone periods
    out_zone_nogo = out_zone.loc[out_zone['TrialType'] == 2]
    out_zone_nogo_incorrect = out_zone_nogo.loc[out_zone_nogo['ACC'] == 0]
    out_zone_com = len(out_zone_nogo_incorrect)/len(out_zone_nogo)

    total = {var + "in_zone_om": in_zone_om, var + "out_zone_om":out_zone_om, var+"in_zone_com": in_zone_com, var+"out_zone_com":out_zone_com }
    table = pd.DataFrame(total,  index=[0])
    return table

def main():


    alldata = pd.DataFrame([])
    variables = ['RT', 'MT', 'Curv']

    for idx in range(1,33):
        try:
            name = 'Aim1E1Participant' + str(idx) + '.xls'
            person = pd.DataFrame([])
        
            for var in variables:
                data = VTC(name, var)
                data = error_rate(data, idx, var)
                person = pd.concat([person, data], axis = 1)
            person["Participant"] = idx
        except FileNotFoundError:
            print(f"File {name} is not found")
        except ZeroDivisionError:
            print(f"File {name} has zero division error")

        else:
            alldata = pd.concat([alldata, person])

    alldata.to_excel("AimE1_VTC.xlsx")
    print("All files are processed and written to Excel File successfully.")

if __name__ == "__main__":
    main()






















    
