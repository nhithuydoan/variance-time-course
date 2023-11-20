import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import statistics

from scipy import stats, signal


def VTC(filename, key: str = 'RT'):
    """
    This function takes in a filename and a variable name and compute variance time course.

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
    file[key] = file[key].where(file[key] > 0.1, np.NaN) 

    #Interpolate for NA points: to do: cut the first and last key
    #file[key] = file[key].interpolate(method='linear', limit_direction='backward', order = 1)
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
    file['smoothed'] = VTC_smooth
    med = statistics.median(file['smoothed'])
    file['zone'] = np.where(file['smoothed'] < med,'in-zone','out-zone')

    return file

def organize(in_zone_om, in_zone_com, out_zone_om, out_zone_com, idx):
    """
    This function organizes commission errors and omission errors of in and out of the zone periods for each participants

    Args:
        in_zone_om:
            Omission error rate when participant is in the zone.

        in_zone_com:
            Comission error rate when participant is in the zone.

        out_zone_om:
            Omission error rate when participant is out of the zone.

        out_zone_com:
            Comission error rate when participant is out of the zone.

        idx:
            participant index (idx = 1 for the first participant)

    Returns:
        A dataset with omission and coommision error rate for each participant.
    """
    
    total = [[in_zone_om, in_zone_com, idx],[out_zone_om, out_zone_com, idx]]
    df = pd.DataFrame(total, columns = ['Omission error', 'Commision error', 'Participant'], index = ['in-zone', 'out-zone'])
    return df

def error_rate(data, idx):

    """
    Calculates omission and commision error rate for each period.

    Args:
        data:
            Dataset with accuracy (1 = correct responses, 0 = incorrect responses) , trial (1 = go trials, 0 = no-go trials)

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
    in_zone_nogo = in_zone.loc[in_zone['TrialType'] == 0]
    in_zone_nogo_incorrect = in_zone_nogo.loc[in_zone_nogo['ACC'] == 0]
    in_zone_com = len(in_zone_nogo_incorrect)/len(in_zone_nogo)

    ## Out-of-the-zone periods
    out_zone = data.loc[(data['zone'] == 'out-zone')]

    #Omission rate for out-of-the-zone periods
    out_zone_go = out_zone.loc[out_zone['TrialType'] == 1]
    out_zone_go_incorrect = out_zone_go.loc[out_zone_go['ACC'] == 0]
    out_zone_om = len(out_zone_go_incorrect)/len(out_zone_go)

    #Comission rate for out-of-the-zone periods
    out_zone_nogo = out_zone.loc[out_zone['TrialType'] == 0]
    out_zone_nogo_incorrect = out_zone_nogo.loc[out_zone_nogo['ACC'] == 0]
    out_zone_com = len(out_zone_nogo_incorrect)/len(out_zone_nogo)

    table = organize(in_zone_om, in_zone_com, out_zone_om, out_zone_com, idx)
    
    return table




if __name__ == "__main__":
    alldata = pd.DataFrame([])

    for idx in range(1,101):
        name = str('HarryE1Participant'+ str(idx)+'.xls')
        data = VTC(name,'RT')
        df = error_rate(data, idx)
        alldata = pd.concat([alldata, df])

    alldata['zone'] = alldata.index
    #Export to excel file
    #alldata.to_excel("VTC_allparticipants.xlsx")
    #print("DataFrame is written to Excel File successfully.")
    
    


















    

