Begin["WolframForJupyter`Private`"];

notfound = "kernel.sh: Jupyter installation on Environment[\"PATH\"] not found.";
isdir = "kernel.sh: Provided Jupyter binary path is a directory. Please provide the path to the Jupyter binary."
nobin = "kernel.sh: Provided Jupyter binary path does not exist.";
notadded = "kernel.sh: An error has occurred. There is still no Wolfram kernel in \"jupyter kernelspec list.\"";
notremoved = "kernel.sh: An error has occurred. There is a Wolfram kernel still in \"jupyter kernelspec list.\"";

globalKernelUUID = "11a8cf20-da0e-4976-83e5-27579d6360b3";

pacletHome = DirectoryName[$InputFileName];

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
						pacletHome,
						kernelUUID,
						(* Could Remove this part so that every evalution of addKernelToJupyter adds a new kernel with a different uuid *)
						globalKernelUUID
					}], CreateIntermediateDirectories -> True
				];
		Export[
			FileNameJoin[{tempDir, "kernel.json"}], 
			Association[
				"argv" -> {mathBin, "-script", FileNameJoin[{pacletHome, "Resources", "kernel.wl"}], "{connection_file}"},
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

If[Length[$CommandLine] > 3,
	If[$CommandLine[[4]] === "remove",
		command = removeKernelFromJupyter;,
		command = addKernelToJupyter;
	];
	If[Length[$CommandLine] > 4,
		command[
			$CommandLine[[5]]
		];,
		command[];
	];
];

End[];