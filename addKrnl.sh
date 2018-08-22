rm -rf JWKConfig;
mkdir JWKConfig;
echo "{
 \"argv\": [\"`pwd`/JupyterWolframKernel\", \"{connection_file}\"],
 \"display_name\": \"Wolfram Language\",
 \"language\": \"Wolfram Language\"
}" > JWKConfig/kernel.json;
cp jupyter_notebook_config.py JWKConfig;
jupyter kernelspec install JWKConfig;