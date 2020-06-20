import sys, os, shutil

droppedPath	=	sys.argv[1].rsplit("\\", 1)[0] + "\\"

scriptPath	=	sys.argv[0].rsplit("\\", 1)[0] + "\\"

if "Review" in droppedPath.rsplit("OCAT Data")[0].rsplit("\\", 2)[1]:
	TYPE	=	"SINGLE"
elif "OCAT Data" in droppedPath.rsplit("\\", 4)[2:4]:
	TYPE	=	"MULTI"
else:
	TYPE	=	"SINGLE"

mQUA	=	"High"

RelPath	=	droppedPath.split("OCAT Data")[0] + "OCAT Data\\"
listfile	=	[]

for paths, folders, files in os.walk(droppedPath):
	for file in files:
		if file.startswith("OCAT-") and file.endswith(".csv"):
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

GPUs, QUAs	=	[], []

for item in listmap:
	GPUs.append(item[0])
	QUAs.append(item[2])

GPUs	=	list(set(GPUs))
if		TYPE	==	"MULTI":
	QUAs	=	[mQUA]
elif	TYPE	==	"SINGLE":
	QUAs	=	list(set(QUAs))


droppedGame	=	RelPath.rsplit("\\", 3)[1]	\
	.replace(" Performance Analysis", "")	\
	.replace(" Review", "")

if		TYPE	==	"MULTI" and len(GPUs) != 1:
	cGPU	=	"NULL"
elif	TYPE	==	"SINGLE" or len(GPUs) == 1:
	cGPU	=	"\"" + str(GPUs[0]) + "\""

scriptFull	=	scriptPath + "OCAT - Search - PA.r"

outputName	=	"Search - PA - " + droppedGame + ".r"
outputFull	=	droppedPath + "@" + outputName

RPath		=	droppedPath.replace("\\", "/")

if not os.path.exists(outputFull):
	with open(scriptFull, 'r') as fref, open(outputFull, 'w') as fout:
		for line in fref:
			fout.write(line	\
				.replace("!PATH!",		RPath)			\
				.replace("!GAME!",		droppedGame)	\
				.replace("!QUA!",		QUAs[0])
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
				.replace("!QUA!",		QUAs[0])			\
				.replace("!GPU!",		cGPU)
			)
		fout.close()

if not os.path.exists(droppedPath + "@Combined - Output.r"):
	shutil.copyfile(scriptPath + "OCAT - Combined - Output.r", droppedPath + "@Combined - Output.r")

# os.system("pause")