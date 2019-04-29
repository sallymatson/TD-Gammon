#import pandas as pd
import pickle

def read_file(file):
    with open(file,'rb') as f:
        weights = pickle.load(f, encoding="latin1")
    return(weights)

read_file("weights0.bin")
