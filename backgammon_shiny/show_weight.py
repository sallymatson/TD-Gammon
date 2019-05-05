import cPickle as pickle
import matplotlib.pyplot as plt
weights = pickle.load(open('weights0.bin','r'))
print weights[0]
plt.imshow(weights[0])
plt.show()



import csv
from itertools import izip

with open('w1.csv', 'wb') as f:
    writer = csv.writer(f)
    writer.writerows(weights[0])
with open('b1.csv', 'wb') as f:
    writer = csv.writer(f)
    writer.writerows(weights[1])
with open('w2.csv', 'wb') as f:
    writer = csv.writer(f)
    writer.writerows(weights[2])
with open('b2.csv', 'wb') as f:
    writer = csv.writer(f)
    writer.writerows(weights[3])
