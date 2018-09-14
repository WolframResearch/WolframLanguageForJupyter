::This script sets up the folder structure and installs the jupyter kernel
::It should be called in the form: addKernel.bat "C:/Path/To/Kernel.wolfram.exe"

@echo off

:: Remove directory
RD /S /Q JupyterWolframKernel
MKDIR JupyterWolframKernel

:: Current location
set "pwd=%~dp0"
set "pwd=%pwd:\=/%"

:: Build kernel.json
(
echo {
echo  "argv": [%1, "-script", "%pwd%/resources/kernel.wl", "{connection_file}"],
echo  "display_name": "Wolfram Language",
echo  "language": "Wolfram Language"
echo }
) > JupyterWolframKernel/kernel.json

:: Final jupyter install command
jupyter kernelspec install JupyterWolframKernel