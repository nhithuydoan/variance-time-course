import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import random as rand
import statistics

from scipy import stats


def ttest_byerror(filename: str, var: str):
    #Filtering data
    file = pd.read_excel(filename).iloc[:,1:]
    error = file.loc[:, [var, 'zone', 'Participant']]
    var_inzone, var_outzone = error.loc[error['zone'] == 'in-zone'], error.loc[error['zone'] == 'out-zone']

    #t-test
    results = stats.ttest_ind(var_inzone[var], var_outzone[var])

    return error, results

def graph_results(data, var: str, pvalue: int):
    #Average data by zone
    sumdata = data.groupby('zone').agg('mean').reset_index()
    filtereddata = sumdata.loc[:, sumdata.columns != 'Participant']

    #Set parameters for graphing
    y = max(filtereddata[var])
    colors = ['tab:orange', 'tab:blue']
    fig, ax = plt.subplots(figsize=(6,6))
    ax.bar(filtereddata['zone'], filtereddata[var], color = colors, yerr = y/10, edgecolor = 'black', width = 0.5, align ='center' )
    ax.set_ylabel(var + 'rate', color = 'black', fontsize = 15)
    ax.set_xlabel("")
    ax.set_ylim(0, y + y/2)
    
    x1, x2 = 0, 1
    
    ax.plot([x1, x1, x2, x2], [y + y/6, y + y/5, y + y/5, y + y/6],lw=1.5, color = 'black')
    ax.set_xticklabels(['In the zone', 'Out of the zone'] )
    if pvalue >=0.05:
        sig = 'n.s'
    elif 0.01 <= pvalue < 0.5:
        sig = '*'
    elif 0.001 <= pvalue < 0.01:
        sig = '**'
    else:
        sig = '***'
    
    ax.text((x1+x2)/2, y + y/5, sig , ha='center', va='bottom', color='black', fontsize = 15)

    plt.show()


if __name__ == "__main__":
    for error in ['Commision error', "Omission error"]:
        data, results = ttest_byerror("VTC_allparticipants.xlsx", error)
        graph_results(data, error, results[1])
