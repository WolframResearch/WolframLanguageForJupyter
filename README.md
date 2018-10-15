# Wolfram Language kernel for Jupyter notebooks

Jupyter provides a protocol (ZMQ) to connect their notebooks to various languages. This project defines a Wolfram Language kernel which can be used in Jupyter notebooks.


# Installation

The below adds the Wolfram Language kernel to Jupyter's "database of kernels" and allow you to select "Wolfram Language" from the Jupyter notebook interface.

Run the following command in a terminal from the project root directory.
  
    wolframscript -file install.wls add

Then, run the following.

    jupyter notebook

**NOTE: If the location of Mathematica changes, you will have to re-run `install.wls`.**

**NOTE: `install.wls` automatically checks Jupyter's database of kernels to see if the Wolfram Language kernel was added, and gives an error if it was not.**

**NOTE: Run the below for more configuration options for `install.wls`.**

	wolframscript -file install.wls help

# Building

The below creates a paclet that, when installed within Mathematica, creates a Mathematica symbol that allows the user to add the Wolfram Language kernel to Jupyter's database of kernels and select "Wolfram Language" from the Jupyter notebook interface.

Run the following command in a terminal from the project root directory.

	wolframscript -file build.wls

## Steps in Mathematica

To install the paclet, run the following (in Mathematica).

	PacletInstall["<location of paclet>"]

After the paclet has been installed, all that needs to be done is the following, once every kernel session.

	<<WolframLanguageForJupyter`

Then, run the below in Mathematica to see how to evaluate the new command.

	?AddKernelToJupyter
	?RemoveKernelFromJupyter

# Usage

Once the "Wolfram Language" has been selected, and the wrapper has finished starting up, you should see an empty In[]: cell. Just type your input into that cell, as you would for the normal Mathematica Front End, and press Shift-Enter.

If the wrapper can determine for sure that the output can be properly represented as text, it will do so. If not, the output will be in the form of an image.

Any errors that occur during the evaluation of the input will be displayed.

Wrap your input with Interact[] to upload the output to the cloud, and embed that cloud object in the Jupyter Notebook.

**NOTE: This requires that you are signed in to the cloud within the Wolfram Kernel.**