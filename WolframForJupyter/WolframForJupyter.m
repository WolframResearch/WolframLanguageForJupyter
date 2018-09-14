BeginPackage["WolframForJupyter`"];

AddKernelToJupyter::usage = "AddKernelToJupyter[] attempts to add the Wolfram kernel to the Jupyter installation on Environment[\"PATH\"].
AddKernelToJupyter[\"\!\(\*
StyleBox[\"path\", \"TI\"]\)\"] adds the Wolfram kernel to the location of the Jupyter binary at \!\(\*
StyleBox[\"path\", \"TI\"]\).";
AddKernelToJupyter::notfound = "Jupyter installation on Environment[\"PATH\"] not found.";
AddKernelToJupyter::isdir = "Provided path is a directory. Please provide the path to the Jupyter binary."
AddKernelToJupyter::nobin = "Provided path does not exist.";
AddKernelToJupyter::notadded = "An error has occurred. There is still no Wolfram kernel in \"jupyter kernelspec list.\"";

RemoveKernelFromJupyter::usage = "RemoveKernelFromJupyter[] attempts to remove any Wolfram kernels from the Jupyter installation on Environment[\"PATH\"].
RemoveKernelFromJupyter[\"\!\(\*
StyleBox[\"path\", \"TI\"]\)\"] removes any Wolfram kernels from the location of the Jupyter binary at \!\(\*
StyleBox[\"path\", \"TI\"]\).";
RemoveKernelFromJupyter::notfound = AddKernelToJupyter::notfound;
RemoveKernelFromJupyter::isdir = AddKernelToJupyter::isdir;
RemoveKernelFromJupyter::nobin = AddKernelToJupyter::nobin;
RemoveKernelFromJupyter::notremoved = "An error has occurred. There is a Wolfram kernel still in \"jupyter kernelspec list.\"";

Begin["`Private`"];

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

AddKernelToJupyter[] := 
	Module[{jupyterPath},
		jupyterPath = findJupyerPath[];
		If[MissingQ[jupyterPath],
			Message[AddKernelToJupyter::notfound];
			Return[$Failed];
		];
		Return[AddKernelToJupyter[FileNameJoin[{jupyterPath, StringJoin["jupyter", fileExt]}]]];
	];

RemoveKernelFromJupyter[] := 
	Module[{jupyterPath},
		jupyterPath = findJupyerPath[];
		If[MissingQ[jupyterPath],
			Message[RemoveKernelFromJupyter::notfound];
			Return[$Failed];
		];
		Return[RemoveKernelFromJupyter[FileNameJoin[{jupyterPath, StringJoin["jupyter", fileExt]}]]];
	];

getKernelspecAssoc[jupyterPath_String] := 
	Replace[
		ImportString[RunProcess[{jupyterPath, "kernelspec", "list", "--json"}, "StandardOutput"], "JSON"],
		part_List /; AllTrue[part, Head[#1] === Rule &] -> Association @ part, 
		{0, Infinity}
	];

AddKernelToJupyter[jupyterPath_String] := 
	Module[{baseDir, tempDir, exitCode, kernelspecAssoc, kernelspecs, kernelUUID},
		If[DirectoryQ[jupyterPath],
			Message[AddKernelToJupyter::isdir];
			Return[$Failed];
		];
		If[!FileExistsQ[jupyterPath],
			Message[AddKernelToJupyter::nobin];
			Return[$Failed];
		];

		kernelUUID = CreateUUID[];
		tempDir = CreateDirectory[
					FileNameJoin[{
						pacletHome,
						kernelUUID,
						(* Could Remove this part so that every evalution of AddKernelToJupyter adds a new kernel with a different uuid *)
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
			Message[AddKernelToJupyter::notadded];
			Return[$Failed];
		];

		(* Return[kernelUUID]; *)
	];

RemoveKernelFromJupyter[jupyterPath_String (*, kernelUUID_String *)] := 
	Module[{exitCode, kernelspecAssoc, kernelspecs},
		exitCode = RunProcess[{jupyterPath, "kernelspec", "remove", "-f", (* kernelUUID *) globalKernelUUID}, "ExitCode"];

		kernelspecAssoc = getKernelspecAssoc[jupyterPath];
		kernelspecs = Keys[kernelspecAssoc["kernelspecs"]];

		If[MemberQ[
				kernelspecs,
				(* kernelUUID *)
				globalKernelUUID
			],
			Message[RemoveKernelFromJupyter::notremoved];
			Return[$Failed];
		];

		(* Return[kernelUUID]; *)
	];

End[];
EndPackage[];