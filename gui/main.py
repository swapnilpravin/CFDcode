#!/usr/bin/env python3

'''
Gui to set up setup.txt parameters

Controls needed:
	Button: Button
	textbox: Label
	entry box: Entry
	Positioning: grid
'''

from tkinter import *
from tkinter import messagebox
import subprocess
from tkinter.scrolledtext import ScrolledText
import io

#fields: (Data_field, unit, default_value)
fields = [('Length','m', '100e-6'), ('Height','m', '20e-6'), ('dx','m', '0.4e-6'), 
		('dy','m', '0.4e-6'), ('dt','s', '1e-7'), ('nt','','100000'), ('nit','','100'), 
		('rho','kg m^-3','1000'), ('nu','m^2 s^-1','1e-6'), ('omega_P', '', '0.5'),
		('omega_U', '', '0.2'), ('omega_V', '', '0.2'), ('tol', '', '1e-6'), ('U_0', 'm s^-1', '2e-3'),
		('nLog', '', '10'),	('nWrite', '', '1000'), ('RADIUS', 'm', '3e-6')]

def fetch(entries):
	for entry in entries:
		field = entry[0]
		text	= entry[1].get()
		print('%s: "%s"' % (field, text))


def clear(entries):
	for entry in entries:
		entry[1].delete(0,END)
		entry[1].config({'background': 'white'})


def defaults(entries):
	for i,entry in enumerate(entries):
		entry[1].delete(0,END)
		entry[1].insert(0,fields[i][2])
		entry[1].config({'background': 'white'})

def is_number(s):
	try:
		float(s)
		return (True,s)
	except ValueError:
		pass
	
	try:
		import unicodedata
		unicodedata.numeric(s)
		return (True, unicodedata.numeric(s))
	except (TypeError, ValueError):
		pass

	return (False, '')

def writeSetupFile(entries):
	for entry in entries:
		entry[1].config({'background':'white'})

	f = open('setup.txt', 'w')
	for entry in entries:
		if is_number(entry[1].get())[0]:
			s = is_number(entry[1].get())[1]
		else:
			messagebox.showinfo('Error', entry[0]+' is not a numerical value')
			entry[1].config({'background':'red'})
			f.close()
			return
		line = entry[0] + ',' + s + '\n'
		f.write(line)
	f.close()
	messagebox.showinfo('','Setup file written.')

def run():
	def onRun():
		Nproc = ent1.get()
		#run = subprocess.run(['mpirun', '-n', Nproc, './channel'], stdout=subprocess.PIPE)
		#log.insert(END, run.stdout.read())
		p = subprocess.Popen(['mpirun', '-n', Nproc, './channel'], stdout=subprocess.PIPE)
		#p = subprocess.Popen(['ls', '-l'], stdout=subprocess.PIPE)
	
		for i in range(100):
			line = p.stdout.readline()
		#print(line)
			log.insert(END,line)
			win.update_idletasks()
			log.update_idletasks()
		#	logstr.set(line)
		#win.update_idletaska()	
		#p.stdout.close()
		#p.wait()

	win = Toplevel()
	row = Frame(win)
	lab1 = Label(row, width=20, text='Number of processors', anchor='w')
	ent1 = Spinbox(row, width=5, from_=2, to=8)
	#lab1.grid(row=0, column=0)
	#ent1.grid(row=0, column=1)
	#b1.grid(row=1, column=0, rowspan=2)
	row.pack(side=TOP)
	lab1.pack(side=LEFT)
	ent1.pack(side=RIGHT)
	logstr = StringVar()
	log = ScrolledText(win, height=30, width=80)
	#log = Label(win, textvariable=logstr)
	#log.grid(row=2, column=0, rowspan=2)
	log.pack()
	b1 = Button(win, text='Run', command=onRun)
	b1.pack()
	
	
def makeform(root, fields):
	entries = []
	i=0
	#frame = Frame(root)
	#frame.pack(side=TOP, fill=X, padx=5, pady=5)
	for field in fields:
		lab = Label(root, width=15, text=field[0], anchor='w')
		ent = Entry(root)
		unitLab = Label(root, width=10, text=field[1], anchor='w')
		ent.insert(0, field[2])
		#lab.pack(side=LEFT)
		#ent.pack(side=RIGHT, expand=YES, fill=X)
		lab.grid(row=i, column=0)
		ent.grid(row=i, column=1)
		unitLab.grid(row=i, column=2)
		entries.append((field[0], ent))
		i=i+1
	return entries

if __name__ == '__main__':
	root = Tk()
	root.title('CFD Solver 0.1')
	ents = makeform(root, fields)
	root.bind('<Return>', (lambda event, e=ents: fetch(e)))
	b1 = Button(root, text='Show',
			command=(lambda e=ents: fetch(e)))
	b1.grid(row=0, column=3)
	b2 = Button(root, text='Quit', command=root.quit)
	b2.grid(row=1, column=3)
	b3 = Button(root, text='Defaults', command=(lambda e=ents: defaults(e)))
	b3.grid(row=2, column=3)
	b4 = Button(root, text='Clear', command=(lambda e=ents: clear(e)))
	b4.grid(row=3, column=3)
	b5 = Button(root, text='Write', command=(lambda e=ents: writeSetupFile(e)))
	b5.grid(row=4, column=3)
	b6 = Button(root, text='Run', command=run)
	b6.grid(row=5, column=3)
	root.mainloop()

