import matplotlib.pyplot as plt
import numpy as np
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.animation as animation

def loadData(fName,N):
    f = open(fName)
    N=f.readline()
    N = int(N.split()[1])

    positions = np.loadtxt(f,unpack=True)
    positions = np.reshape(positions,(3,N,-1),order='F')
    
    return positions

def simData():
    for t in range(0,len(p[0,0,:])):
        yield p[:,:,t]

def simPoints(simData):
    positions = simData
    line.set_data(positions[0,:],positions[1,:])
    line.set_3d_properties(positions[2,:])
    return line


fName = 'out_positions.txt'
N = 512
p=loadData(fName,N)

fig = plt.figure()
ax = fig.add_subplot(111,projection='3d')
line, = ax.plot(p[0,:,0],p[1,:,0],p[2,:,0],"o")
ax.set_xlim3d((-15,15))
ax.set_ylim3d((-15,15))
ax.set_zlim3d((-5,25))

ani = animation.FuncAnimation(fig,simPoints, simData, blit=False,interval=10,
        repeat=True)
plt.show()
