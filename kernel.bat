SET helpMessage="kernel.sh: Usage: kernel.sh add /absolute/path/to/WolframKernel [/path/to/Jupyter/binary]
kernel.sh: Usage:	adds a Wolfram kernel to a Jupyter binary on PATH, or optional provided Jupyter binary path
kernel.sh: Usage: kernel.sh remove [/path/to/Jupyter/binary]
kernel.sh: Usage:	removes any Wolfram kernels found on a Jupyter binary on PATH, or optional provided Jupyter binary path"

SET wolframKernel="%2"
SET jupyterBinary="%3"

SET count=50
IF "%1"=="" SET count=0
IF "%2"=="" SET count=1
IF "%3"=="" SET count=2
IF "%3"=="" SET count=3

IF %count% LSS 1 (
	ECHO "kernel.sh: Missing subcommand. Try add or remove."
    ECHO "kernel.sh: "
	ECHO "%helpMessage%"
	EXIT /B
)

IF NOT %1% == "add" (
	IF NOT %1% == "remove" (
		ECHO "kernel.sh: Missing subcommand. Try add or remove."
	    ECHO "kernel.sh: "
		ECHO "%helpMessage%"
		EXIT /B
	)
)

IF NOT EXIST "%wolframKernel%" (
	ECHO "kernel.sh: Provided Wolfram Kernel path, %wolframKernel%, does not exist or is a directory."
	EXIT /B
)

IF %count% == 2 (
	ECHO "kernel.sh: Using %wolframKernel% as the Wolfram kernel."
    "%wolframKernel%" -script resources\install.wl
    EXIT /B
)

IF %count% == 3 (
	ECHO "kernel.sh: Using %wolframKernel% as the Wolfram kernel, and %jupyterBinary% as the Jupyer binary path."
    "%wolframKernel%" -script resources\install.wl "%jupyterBinary%"
    EXIT /B
)

ECHO "kernel.sh: Incorrect number of arguments provided. 1 or 2 expected. See usage below."
ECHO "kernel.sh: "
ECHO "%helpMessage%"