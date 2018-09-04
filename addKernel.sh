#-f quiets errors
rm -rf JupyterWolframKernel;
mkdir JupyterWolframKernel;
echo "{
 \"argv\": [\"$1\", \"-script\", \"`pwd`/kernel.wl\", \"{connection_file}\"],
 \"display_name\": \"Wolfram Language\",
 \"language\": \"Wolfram Language\"
}" > JupyterWolframKernel/kernel.json;
cp jupyter_notebook_config.py JupyterWolframKernel;
jupyter kernelspec install JupyterWolframKernel;