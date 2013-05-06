import matplotlib.pyplot as plt
import numpy as np
from mpl_toolkits.mplot3d import Axes3D

def loadData(fName):
    f = open(fName)
    lines = f.readlines()
    
    positions = np.zeros((3,len(lines)))
    for i,line in enumerate(lines):
        p = [float(e) for e in line.split()]
        positions[:,i] = p

    return positions

def visualize(positions):
    fig = plt.figure()
    ax = fig.add_subplot(111,projection='3d')
    ax.scatter(positions[0],positions[1],positions[2])

    plt.show()



fName = 'out_positions.txt'
p=loadData(fName)
visualize(p)
