import tkinter as tk
from subprocess import call, Popen
import os

def onClick():
    print('Running')
    #os.chdir(r'C:\Users\Swapnil\Desktop\CFD_app')
    global p
    p = Popen(['mpiexec', '-n', '2', 'channel.exe'], cwd=r'C:\Users\Swapnil\Desktop\CFD_app')

def sayHello():
    print('Hello')

def close():
    try:
        p.terminate()
    except NameError:
        pass
    root.destroy()

root = tk.Tk()
root.geometry('800x600')
frame = tk.Frame(master=root,width=800,height=600)
frame.pack()

b1 = tk.Button(frame, text='Run', command=onClick)
b1.grid(column=0)

b2 = tk.Button(frame, text='say Hello', command=sayHello)
b2.grid(column=0)

b3 = tk.Button(frame, text='Quit', command=close)
b3.grid(column=0)

cvs = tk.Canvas(master=frame, width=500, height=400, bg='cyan')
cvs.grid(column=1)

root.mainloop()
