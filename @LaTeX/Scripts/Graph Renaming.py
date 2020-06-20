import sys, os, shutil

droppedPath	=	sys.argv[1].rsplit("\\", 1)[0] + "\\"

droppedGPU	=	droppedPath.split("OCAT Data")[1].split("\\")[1]
droppedOCAT	=	droppedPath.split("OCAT Data")[0] + "OCAT Data\\"

os.chdir(droppedPath)

Z	=	1
#	zero-padding width

if	"Locations.txt"	in	os.listdir(droppedOCAT):
	LOCs	=	open(droppedOCAT + "Locations.txt", 'r').readlines()
	LOCs	=	[line.rstrip('\n') for line in LOCs]

if	"APIs.txt"		in	os.listdir(droppedOCAT):
	APIs	=	open(droppedOCAT + "APIs.txt", 'r').readlines()
	APIs	=	[line.rstrip('\n') for line in APIs]
	APIs	=	[API + " -" for API in APIs]
#		this last bit makes it check for the hyphen that separates the parts of the filename
else:
	APIs	=	[""]

if	"Qualities.txt"	in	os.listdir(droppedOCAT):
	QUAs	=	open(droppedOCAT + "Qualities.txt", 'r').readlines()
	QUAs	=	[line.rstrip('\n') for line in QUAs]
else:
	QUAs =	[\
		"Minimum Acceptable",\
		"High",\
		"Max"]

GPUs	=	[\
'RX 580',\
'RX Vega 64',\
'GTX 770',\
'GTX 980',\
'GTX 1070',\
'GTX 1080',\
'RTX 2060',\
'RTX 2080']

DATAs	=	[\
'Frame',\
'Display',\
'Render',\
'Driver']

TYPEs	=	[\
'Means',\
'Course',\
'Freq',\
'QQ',\
'Diff',\
'Diff EXT',\
'Means Labeled']

def numFind	(filename, list):
	if list == [""]:
		return(0)
	for i in reversed(range(len(list))):
		if list[i] in filename:
			return(i+1)
	return(0)

def numGen (filename, GPU = droppedGPU):
	if GPU not in GPUs:
		gpu	=	""
	else:
		gpu	=	numFind(droppedGPU, GPUs)
	if APIs == [""]:
		api	=	""
	else:
		api		=	numFind(filename, APIs)
	loc		=	numFind(filename, LOCs)
	qua		=	numFind(filename, QUAs)
	data	=	numFind(filename, DATAs)
	type	=	numFind(filename, TYPEs)

	code	=	""
	for x in [gpu, api, qua, loc, data, type]:
		if x != "":
			code	=	code + str(x).zfill(Z)

	return(code)

if not os.path.exists("@Graphs"):
	os.mkdir("@Graphs")

for file in os.listdir(droppedPath):
	if file.endswith(".png"):
		shutil.copyfile(file, "@Graphs\\" + numGen(file) + ".png")

# os.system("pause")