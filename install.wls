Begin["WolframForJupyter`Private`"];

notfound = "install.wls: Jupyter installation on Environment[\"PATH\"] not found.";
isdir = "install.wls: Provided Jupyter binary path is a directory. Please provide the path to the Jupyter binary."
nobin = "install.wls: Provided Jupyter binary path does not exist.";
notadded = "install.wls: An error has occurred. There is still no Wolfram kernel in \"jupyter kernelspec list.\"";
notremoved = "install.wls: An error has occurred. There is a Wolfram kernel still in \"jupyter kernelspec list.\"";

globalKernelUUID = "11a8cf20-da0e-4976-83e5-27579d6360b3";

projectHome = Directory[];

mathBin := (defineGlobalVars; mathBin);
fileExt := (defineGlobalVars; fileExt);
pathSeperator := (defineGlobalVars; pathSeperator);

defineGlobalVars := 
	Switch[
		$OperatingSystem,
		"Windows",
		mathBin = FileNameJoin[{$InstallationDirectory, "wolfram.exe"}];
		fileExt = ".exe";
		pathSeperator = ";";,
		"MacOSX",
		mathBin = FileNameJoin[{$InstallationDirectory, "MacOS", "WolframKernel"}];
		fileExt = "";
		pathSeperator = ":";,
		"Unix",
		mathBin = FileNameJoin[{$InstallationDirectory, "MacOS", "Kernel", "Binaries", $SystemID, "WolframKernel"}];
		fileExt = "";
		pathSeperator = ":";
	];

findJupyerPath[] := 
	SelectFirst[
		StringSplit[Environment["PATH"], pathSeperator],
		FileExistsQ[FileNameJoin[{#1, StringJoin["jupyter", fileExt]}]]&
	];

addKernelToJupyter[] := 
	Module[{jupyterPath},
		jupyterPath = findJupyerPath[];
		If[MissingQ[jupyterPath],
			Print[notfound];
			Return[$Failed];
		];
		Return[addKernelToJupyter[FileNameJoin[{jupyterPath, StringJoin["jupyter", fileExt]}]]];
	];

removeKernelFromJupyter[] := 
	Module[{jupyterPath},
		jupyterPath = findJupyerPath[];
		If[MissingQ[jupyterPath],
			Print[notfound];
			Return[$Failed];
		];
		Return[removeKernelFromJupyter[FileNameJoin[{jupyterPath, StringJoin["jupyter", fileExt]}]]];
	];

getKernelspecAssoc[jupyterPath_String] := 
	Replace[
		ImportString[RunProcess[{jupyterPath, "kernelspec", "list", "--json"}, "StandardOutput"], "JSON"],
		part_List /; AllTrue[part, Head[#1] === Rule &] -> Association @ part, 
		{0, Infinity}
	];

addKernelToJupyter[jupyterPath_String] := 
	Module[{baseDir, tempDir, exitCode, kernelspecAssoc, kernelspecs, kernelUUID},
		If[DirectoryQ[jupyterPath],
			Print[isdir];
			Return[$Failed];
		];
		If[!FileExistsQ[jupyterPath],
			Print[nobin];
			Return[$Failed];
		];

		kernelUUID = CreateUUID[];
		tempDir = CreateDirectory[
					FileNameJoin[{
						projectHome,
						kernelUUID,
						(* Could Remove this part so that every evalution of addKernelToJupyter adds a new kernel with a different uuid *)
						globalKernelUUID
					}], CreateIntermediateDirectories -> True
				];
		Export[
			FileNameJoin[{tempDir, "kernel.json"}], 
			Association[
				"argv" -> {mathBin, "-script", FileNameJoin[{projectHome, "WolframForJupyter", "Resources", "kernel.wl"}], "{connection_file}"},
				"display_name" -> "Wolfram Language",
				"language" -> "Wolfram Language"
			]
		];
		exitCode = RunProcess[{
					jupyterPath,
					"kernelspec",
					"install",
					tempDir
				}, "ExitCode"];

		(* DeleteDirectory[tempDir, DeleteContents -> True]; *)
		DeleteDirectory[DirectoryName[tempDir], DeleteContents -> True];

		kernelspecAssoc = getKernelspecAssoc[jupyterPath];
		kernelspecs = Keys[kernelspecAssoc["kernelspecs"]];

		If[!MemberQ[
				kernelspecs,
				(* kernelUUID *)
				globalKernelUUID
			],
			Print[notadded];
			Return[$Failed];
		];

		(* Return[kernelUUID]; *)
	];

removeKernelFromJupyter[jupyterPath_String (*, kernelUUID_String *)] := 
	Module[{exitCode, kernelspecAssoc, kernelspecs},
		exitCode = RunProcess[{jupyterPath, "kernelspec", "remove", "-f", (* kernelUUID *) globalKernelUUID}, "ExitCode"];

		kernelspecAssoc = getKernelspecAssoc[jupyterPath];
		kernelspecs = Keys[kernelspecAssoc["kernelspecs"]];

		If[MemberQ[
				kernelspecs,
				(* kernelUUID *)
				globalKernelUUID
			],
			Print[notremoved];
			Return[$Failed];
		];

		(* Return[kernelUUID]; *)
	];

templateJupyterPath = FileNameJoin[{"path", "to", "Jupyter", "binary"}];

helpMessage = StringJoin[
				"install.wls: Usage: install.wls add [", templateJupyterPath, "]\ninstall.wls: Usage:\tadds a Wolfram kernel to a Jupyter binary on PATH, or optional provided Jupyter binary path\ninstall.wls: Usage: install.wls remove [", templateJupyterPath ,"]\ninstall.wls: Usage:\tremoves any Wolfram kernels found on a Jupyter binary on PATH, or optional provided Jupyter binary path"
			];

If[Length[$ScriptCommandLine] == 1 || $ScriptCommandLine[[2]] === "help",
	Print[helpMessage];
	,
	If[$ScriptCommandLine[[2]] === "remove",
		command = removeKernelFromJupyter;,
		command = addKernelToJupyter;
	];

	If[Length[$ScriptCommandLine] > 2,
		command[
			$ScriptCommandLine[[3]]
		];,
		command[];
	];
];

End[];