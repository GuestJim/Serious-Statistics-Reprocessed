import sys, os, shutil

droppedPath	=	sys.argv[1].rsplit("\\", 1)[0] + "\\"

scriptPath	=	sys.argv[0].rsplit("\\", 1)[0] + "\\"

if "OCAT Data" in droppedPath.rsplit("\\", 3)[2:3]:
	TYPE	=	"HIGH"
else:
	TYPE	=	"GPU"

def	listclean	(list):
	return str(list).replace("[", "").replace("]", "").replace("\'", "\"").replace(", ", ",\n").replace(".csv", "");

def	CSVlistR	(GPU, API, QUA, CSVlist):
	if API	==	"NA":
		API	=	""
	return str("\
GPU	=	\"" + GPU + "\"\n\
CSV	=	c(\n"\
	+ listclean(CSVlist) + \
"\n)\n\
CSV	=	paste0(CSV, \".csv\")\n\
OCATcomb	=	READ(\"\", \"" + QUA + "\", \"" + API + "\")\n"
);

RelPath	=	droppedPath.split("OCAT Data")[0] + "OCAT Data\\"

listfile	=	[]

for paths, folders, files in os.walk(droppedPath):
	for file in files:
		if file.startswith("OCAT-"):
			listfile.append((str(paths).replace(RelPath, "") + "\\" + str(file)).replace("\\\\", "\\"))
#	produces a list of all OCAT CSVs with the directory information

listsplit	=	[file.split("\\") for file in listfile]

listmap	=	[]
for line in listsplit:
	listmapL	=	[line[0], "", "", ""]
	for i in range(1, len(line)):
		listmapL[len(listmapL) - i]	=	line[len(line) - i]
	listmap.append(listmapL)
#	this will map the values I need to the appropriate locations in a list
#		[GPU, API, Quality, File]
#	if no API change is made, the element will be blank

GPUs, APIs, QUAs	=	[], [], []

for item in listmap:
	GPUs.append(item[0])
	APIs.append(item[1])
	QUAs.append(item[2])

if		TYPE	==	"HIGH":
	GPUs	=	[\
	'RX 580',\
	'RX Vega 64',\
	'GTX 770',\
	'GTX 980',\
	'GTX 1070',\
	'GTX 1080',\
	'RTX 2060',\
	'RTX 2080']
	QUAs	=	["High"]
elif	TYPE	==	"GPU":
	GPUs	=	list(set(GPUs))
	QUAs	=	list(set(QUAs))

if "APIs.txt" in os.listdir(RelPath):
	APIs	=	open(RelPath + "APIs.txt", 'r').readlines()
	APIs	=	[line.rstrip('\n') for line in APIs]
else:
	APIs	=	list(set(APIs))

grouped	=	[]
out		=	""

for GPU in GPUs:
	for API in APIs:
		for QUA in QUAs:
			filelist	=	[]
			for file in listmap:
				if file[0] == GPU	and file[1] == API	and file[2] == QUA:
					filelist.append(file[3])
			if filelist != []:
				grouped.append([GPU, API, QUA, filelist])
				countCSV	=	len(filelist)
				out	=	out + "\n" + CSVlistR(GPU, API, QUA, filelist)


droppedGame	=	RelPath.rsplit("\\", 3)[1]	\
	.replace(" Performance Analysis", "")	\
	.replace(" Review", "")

if	"Locations.txt" in os.listdir(RelPath):
	loc	=	open(RelPath + "Locations.txt", 'r').readlines()
	loc	=	[line.strip('\n') for line in loc]
else:
	loc	=	["Recording "] * countCSV
	for i in range(countCSV):
		loc[i]	=	loc[i] + str(i+1)
locStr	=	listclean(loc)

if	"Locations Short.txt" in os.listdir(RelPath):
	locsho		=	open(RelPath + "Locations Short.txt", 'r').readlines()
	locsho		=	[line.strip('\n') for line in locsho]
	locshoStr	=	listclean(locsho)
else:
	locshoStr	=	"NULL"

if	"APIs Short.txt" in os.listdir(RelPath):
	APIsho		=	open(RelPath + "APIs Short.txt", 'r').readlines()
	APIsho		=	[line.strip('\n') for line in APIsho]
	APIshoStr	=	listclean(APIsho)
else:
	APIshoStr	=	"NULL"

if		TYPE	==	"HIGH":
	cGPU	=	"NULL"
elif	TYPE	==	"GPU":
	cGPU	=	"\"" + GPUs[0] + "\""


scriptFull	=	scriptPath + "OCAT - Combined - PA.r"

outputName	=	"Combined - PA - " + droppedGame + ".r"
outputFull	=	droppedPath + "@" + outputName

RPath		=	droppedPath.replace("\\", "/")

if not os.path.exists(outputFull):
	with open(scriptFull, 'r') as fref, open(outputFull, 'w') as fout:
		for line in fref:
			fout.write(line	\
				.replace("!PATH!",		RPath)			\
				.replace("!GAME!",		droppedGame)	\
				.replace("!LONG!",		out)			\
				.replace("!QUA!",		QUAs[0])		\
				.replace("!LOC!",		locStr)			\
			)
		fout.close()


scriptFull	=	scriptPath + "OCAT - Combined - Input.r"

outputName	=	"Combined - Input - " + droppedGame + ".r"
outputFull	=	droppedPath + "@" + outputName

if not os.path.exists(outputFull):
	with open(scriptFull, 'r') as fref, open(outputFull, 'w') as fout:
		for line in fref:
			fout.write(line
				.replace("!PATH!",		RPath)				\
				.replace("!GAME!",		droppedGame)		\
				.replace("!API!",		listclean(APIs))	\
				.replace("!APISHO!",	APIshoStr)			\
				.replace("!QUA!",		QUAs[0])			\
				.replace("!LOC!",		locStr)				\
				.replace("!LOCSHO!",	locshoStr)			\
				.replace("!GPU!",		cGPU)
			)
		fout.close()

if not os.path.exists(droppedPath + "OCAT - Combined - Output.r"):
	shutil.copyfile(scriptPath + "OCAT - Combined - Output.r", droppedPath + "@Combined - Output.r")

# os.system("pause")