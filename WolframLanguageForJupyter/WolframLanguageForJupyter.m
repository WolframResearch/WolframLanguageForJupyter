BeginPackage["WolframLanguageForJupyter`"];

(* AddKernelToJupyter::usage = "AddKernelToJupyter[] attempts to add the Wolfram Language kernel to the Jupyter installation on Environment[\"PATH\"].
AddKernelToJupyter[\"\!\(\*
StyleBox[\"path\", \"TI\"]\)\"] adds the Wolfram Language kernel to the location of the Jupyter binary at \!\(\*
StyleBox[\"path\", \"TI\"]\)."; *)
AddKernelToJupyter::usage = "AddKernelToJupyter[] adds a Wolfram Language kernel to a Jupyter binary on Environment[\"PATH\"].
AddKernelToJupyter[\"jupyter\"] adds a Wolfram Language kernel to the provided Jupyter binary path.
AddKernelToJupyter[\"jupyter\", \"kernel\"] adds the provided absolute Wolfram Language kernel binary path to the provided Jupyter binary path.";
AddKernelToJupyter::notfound = "Jupyter installation on Environment[\"PATH\"] not found.";
AddKernelToJupyter::isdir = "Provided `1` binary path is a directory. Please provide the path to the `1` binary.";
AddKernelToJupyter::nobin = "Provided `1` binary path does not exist.";
AddKernelToJupyter::notadded = "An error has occurred. There is still no Wolfram Language kernel in \"jupyter kernelspec list.\"";

(* RemoveKernelFromJupyter::usage = "RemoveKernelFromJupyter[] attempts to remove any Wolfram Language kernels from the Jupyter installation on Environment[\"PATH\"].
RemoveKernelFromJupyter[\"\!\(\*
StyleBox[\"path\", \"TI\"]\)\"] removes any Wolfram Language kernels from the location of the Jupyter binary at \!\(\*
StyleBox[\"path\", \"TI\"]\)."; *)
RemoveKernelFromJupyter::usage = "RemoveKernelFromJupyter[] removes any Wolfram Language kernels found on a Jupyter binary on Environment[\"PATH\"].
RemoveKernelFromJupyter[\"jupyter\"] removes any Wolfram Language kernels found on the provided Jupyter binary path.";
RemoveKernelFromJupyter::notfound = AddKernelToJupyter::notfound;
RemoveKernelFromJupyter::isdir = AddKernelToJupyter::isdir;
RemoveKernelFromJupyter::nobin = AddKernelToJupyter::nobin;
RemoveKernelFromJupyter::notremoved = "An error has occurred. There is a Wolfram Language kernel still in \"jupyter kernelspec list.\"";

Begin["`Private`"];

(* globalKernelUUID = "11a8cf20-da0e-4976-83e5-27579d6360b3"; *)
hashedKernelUUID = Hash[$InstallationDirectory, "SHA", "HexString"];
names = StringCases[$Version, name___ ~~ " for " ~~ ("Mac" | "Microsoft" | "Windows" | "Linux") -> name];
If[Length[names] > 0, 
	globalKernelUUID =
		ToLowerCase[StringJoin[
			"WolframLanguage-",
			StringReplace[First[names], Whitespace -> "-"]
		]];
	displayName = 
		StringJoin[
			"Wolfram Language (",
			Capitalize[
				First[names],
				"AllWords"
			],
			")"
		];
	,
	globalKernelUUID = hashedKernelUUID;
	displayName = "Wolfram Language";
];

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
	Module[{json},
		json = Quiet[ImportString[RunProcess[{jupyterPath, "kernelspec", "list", "--json"}, "StandardOutput"], "JSON"]];
		If[
			FailureQ[json],
			Return[Association[]];
		];
		Return[
			Replace[
				json,
				part_List /; AllTrue[part, Head[#1] === Rule &] -> Association @ part, 
				{0, Infinity}
			]
		];
	];

AddKernelToJupyter[jupyterPath_String] := AddKernelToJupyter[jupyterPath, mathBin];

RemoveKernelFromJupyter[jupyterPath_String (*, kernelUUID_String *)] := 
	Module[{exitCodeOld, exitCode, kernelspecAssoc, kernelspecs, oldEnv},
		If[DirectoryQ[jupyterPath],
			Message[RemoveKernelFromJupyter::isdir, "Jupyter"];
			Return[$Failed];
		];
		If[!FileExistsQ[jupyterPath],
			Message[RemoveKernelFromJupyter::nobin, "Jupyter"];
			Return[$Failed];
		];

		oldEnv = Environment["PATH"];
		SetEnvironment["PATH" -> StringJoin[Environment["PATH"], pathSeperator, DirectoryName[jupyterPath]]];

		exitCodeOld = RunProcess[{jupyterPath, "kernelspec", "remove", "-f", hashedKernelUUID}, "ExitCode"];
		exitCode = RunProcess[{jupyterPath, "kernelspec", "remove", "-f", globalKernelUUID}, "ExitCode"];
		
		kernelspecAssoc = getKernelspecAssoc[jupyterPath];
		If[
			KeyExistsQ[kernelspecAssoc, "kernelspecs"],
			kernelspecs = Keys[kernelspecAssoc["kernelspecs"]];,
			kernelspecs = {};
		];		

		If[MemberQ[
				kernelspecs,
				(* kernelUUID *)
				globalKernelUUID
			],
			Message[RemoveKernelFromJupyter::notremoved];
			Return[$Failed];
		];

		SetEnvironment["PATH" -> oldEnv];

		(* Return[kernelUUID]; *)
	];

AddKernelToJupyter[jupyterPath_String, mathB_String] := 
	Module[{baseDir, tempDir, exitCode, kernelspecAssoc, kernelspecs, kernelUUID, oldEnv},
		If[DirectoryQ[jupyterPath],
			Message[AddKernelToJupyter::isdir, "Jupyter"];
			Return[$Failed];
		];
		If[!FileExistsQ[jupyterPath],
			Message[AddKernelToJupyter::nobin, "Jupyter"];
			Return[$Failed];
		];

		If[DirectoryQ[mathB],
			Message[AddKernelToJupyter::isdir, "Wolfram Language kernel"];
			Return[$Failed];
		];
		If[!FileExistsQ[mathB],
			Message[AddKernelToJupyter::nobin, "Wolfram Language kernel"];
			Return[$Failed];
		];

		oldEnv = Environment["PATH"];
		SetEnvironment["PATH" -> StringJoin[Environment["PATH"], pathSeperator, DirectoryName[jupyterPath]]];

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
				"argv" -> {mathB, "-script", FileNameJoin[{pacletHome, "Resources", "KernelForWolframLanguageForJupyter.wl"}], "{connection_file}"},
				"display_name" -> displayName,
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
		If[
			KeyExistsQ[kernelspecAssoc, "kernelspecs"],
			kernelspecs = Keys[kernelspecAssoc["kernelspecs"]];,
			kernelspecs = {};
		];

		If[!MemberQ[
				kernelspecs,
				(* kernelUUID *)
				globalKernelUUID
			],
			Message[AddKernelToJupyter::notadded];
			Return[$Failed];
		];

		SetEnvironment["PATH" -> oldEnv];

		(* Return[kernelUUID]; *)
	];

End[];
EndPackage[];