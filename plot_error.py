''' parse the log file and plot error at each timestep '''

import sys
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import gridspec

logfile = sys.argv[1]

#print(logfile)

f = open(logfile, 'r')

timestep = []
P_err = []
U_err = []
V_err = []
P_itr = []
U_itr = []

# read until you find: Timestep =

for line in f:
	if 'Timestep =' in line:
		timestep.append(int(line.split()[3]))
	if 'Pressure error' in line:
		P_err.append(float(line.split()[3]))
	if 'Velocity U error' in line:
		U_err.append(float(line.split()[4]))
	if 'Velocity V error' in line:
		V_err.append(float(line.split()[4]))
		
	if 'Pressure solver iteration' in line:
		P_itr.append(int(line.split()[3]))
	if 'Velocity solver iteration' in line:
		U_itr.append(int(line.split()[3]))

#print(timestep)
#print(P_err)
#print(U_err)
#print(V_err)
#print(P_itr)
#print(U_itr)

timestep = np.array(timestep)

gs = gridspec.GridSpec(2,1,height_ratios=[2,1])

plt.subplot(gs[0])
plt.plot(timestep, P_err, label='Pressure error')
plt.plot(timestep, U_err, label='Velocity U error')
plt.plot(timestep, V_err, label='Velocity V error')
plt.ylabel('Error')
plt.legend()
plt.title('Run monitor')

bar_width = 3

plt.subplot(gs[1])
plt.bar(timestep, P_itr, bar_width, label='Pressure')
plt.bar(timestep+bar_width, U_itr, bar_width, label='Velocity')
plt.xlabel('Timesteps')
plt.ylabel('Solver iterations')
plt.legend()

plt.show()

f.close()
