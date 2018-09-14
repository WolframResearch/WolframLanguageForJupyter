#!/usr/bin/env bash
helpMessage="kernel.sh: Usage: kernel.sh add /absolute/path/to/WolframKernel [/path/to/Jupyter/binary]
kernel.sh: Usage:	adds a Wolfram kernel to a Jupyter binary on PATH, or optional provided Jupyter binary path
kernel.sh: Usage: kernel.sh remove [/path/to/Jupyter/binary]
kernel.sh: Usage:	removes any Wolfram kernels found on a Jupyter binary on PATH, or optional provided Jupyter binary path"

wolframKernel="$2"
jupyterBinary="$3"

if [ "$#" -lt 1 ]
then
    echo "kernel.sh: Missing subcommand. Try add or remove."
    echo "kernel.sh: "
	echo "$helpMessage"
	exit
fi

if [ "$1" != "add" ] && [ "$1" != "remove" ]
then
    echo "kernel.sh: Missing subcommand. Try add or remove."
    echo "kernel.sh: "
	echo "$helpMessage"
	exit
fi

if [ ! -f "$wolframKernel" ]
then
	echo "kernel.sh: Provided Wolfram Kernel path, $wolframKernel, does not exist or is a directory."
fi

if [ "$#" -eq 2 ]
then
    echo "kernel.sh: Using $wolframKernel as the Wolfram kernel."
    "$wolframKernel" -script resources/install.wl "$1"
elif [ "$#" -eq 3 ]
then
	echo "kernel.sh: Using $wolframKernel as the Wolfram kernel, and $jupyterBinary as the Jupyer binary path."
	"$wolframKernel" -script resources/install.wl "$1" "$jupyterBinary"
else
	echo "kernel.sh: `expr $# - 1` arguments provided. 1 or 2 expected. See usage below."
	echo "kernel.sh: "
	echo "$helpMessage"
	exit
fi