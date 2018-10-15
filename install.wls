Begin["WolframLanguageForJupyter`Private`"];

notfound = "install.wls: Jupyter installation on Environment[\"PATH\"] not found.";
isdir = "install.wls: Provided Jupyter binary path is a directory. Please provide the path to the Jupyter binary."
nobin = "install.wls: Provided Jupyter binary path does not exist.";
isdirMath = "install.wls: Provided Wolfram Language kernel binary path is a directory. Please provide the path to the Wolfram Language kernel binary."
nobinMath = "install.wls: Provided Wolfram Language kernel binary path does not exist.";
notadded = "install.wls: An error has occurred. There is still no Wolfram Language kernel in \"jupyter kernelspec list.\"";
notremoved = "install.wls: An error has occurred. There is a Wolfram Language kernel still in \"jupyter kernelspec list.\"";
nopaclet = "install.wls: WolframLanguageForJupyter paclet source not detected. Are you running the script in the root project directory?";

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

projectHome = Directory[];
kernelScript = FileNameJoin[{projectHome, "WolframLanguageForJupyter", "Resources", "kernel.wl"}];
If[
	!FileExistsQ[kernelScript],
	Print[nopaclet];
	Quit[];
];


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

splitPath := StringSplit[Environment["PATH"], pathSeperator];

attemptPathRegeneration[] := If[
		$OperatingSystem === "MacOSX" && FileExistsQ["~/.profile"] && !DirectoryQ["~/.profile"],
		Print["install.wls: Warning: Regenerating PATH ..."];
		SetEnvironment[
			"PATH" -> StringTrim[
						RunProcess[
							$SystemShell,
							"StandardOutput",
							StringJoin[Import["~/.profile", "String"], "\necho $PATH"],
							ProcessEnvironment -> {}
						], 
						"\n"
					]
		];
	];

If[
	FailureQ[RunProcess[$SystemShell, All, ""]],
	If[
		MemberQ[$CommandLine, "-script"],
		Print["install.wls: Please use -file instead of -script in WolframScript."];,
		Print["install.wls: An unknown error has occurred."];
		attemptPathRegeneration[];
	];
	Quit[];
];

defineGlobalVars;
If[
	Length[splitPath] == 1,
	Print["install.wls: Warning: This script has encountered a very small PATH environment variable."];
	Print["install.wls: Warning: This can occur due to a possible WolframScript bug."];
	attemptPathRegeneration[];
];


findJupyerPath[] := 
	SelectFirst[
		splitPath,
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

addKernelToJupyter[jupyterPath_String] := addKernelToJupyter[jupyterPath, mathBin];

removeKernelFromJupyter[jupyterPath_String (*, kernelUUID_String *)] := 
	Module[{exitCodeOld, exitCode, kernelspecAssoc, kernelspecs, oldEnv},
		If[DirectoryQ[jupyterPath],
			Print[isdir];
			Return[$Failed];
		];
		If[!FileExistsQ[jupyterPath],
			Print[nobin];
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
			Print[notremoved];
			Return[$Failed];
		];

		SetEnvironment["PATH" -> oldEnv];

		(* Return[kernelUUID]; *)
	];

addKernelToJupyter[jupyterPath_String, mathB_String] := 
	Module[{baseDir, tempDir, exitCode, kernelspecAssoc, kernelspecs, kernelUUID, oldEnv},
		If[DirectoryQ[jupyterPath],
			Print[isdir];
			Return[$Failed];
		];
		If[!FileExistsQ[jupyterPath],
			Print[nobin];
			Return[$Failed];
		];

		If[DirectoryQ[mathB],
			Print[isdirMath];
			Return[$Failed];
		];
		If[!FileExistsQ[mathB],
			Print[nobinMath];
			Return[$Failed];
		];

		oldEnv = Environment["PATH"];
		SetEnvironment["PATH" -> StringJoin[Environment["PATH"], pathSeperator, DirectoryName[jupyterPath]]];

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
				"argv" -> {mathB, "-script", kernelScript, "{connection_file}"},
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
			Print[notadded];
			Return[$Failed];
		];

		SetEnvironment["PATH" -> oldEnv];

		(* Return[kernelUUID]; *)
	];

templateJupyterPath = FileNameJoin[{"path", "to", "Jupyter", "binary"}];
templateWLPath = FileNameJoin[{"", "absolute", "path", "to", "Wolfram", "Language", "kernel", "binary"}];

helpMessage = StringJoin[
				"install.wls: Usage: install.wls add [", templateJupyterPath, "]\ninstall.wls: Usage:\tadds a Wolfram Language kernel to a Jupyter binary on PATH, or optional provided Jupyter binary path\n",
				"install.wls: Usage: install.wls add ", templateJupyterPath, " ", templateWLPath, "\ninstall.wls: Usage:\tadds the provided absolute Wolfram Language kernel path to the provided Jupyter binary path\n",
				"install.wls: Usage: install.wls remove [", templateJupyterPath ,"]\ninstall.wls: Usage:\tremoves any Wolfram Language kernels found on a Jupyter binary on PATH, or optional provided Jupyter binary path"
			];

If[Length[$ScriptCommandLine] == 1 || $ScriptCommandLine[[2]] === "help",
	Print[helpMessage];
	,
	If[$ScriptCommandLine[[2]] === "remove",
		command = removeKernelFromJupyter;,
		command = addKernelToJupyter;
	];

	Switch[
		Length[$ScriptCommandLine],
		4,
		command[
			StringTrim[$ScriptCommandLine[[3]], "*"],
			StringTrim[$ScriptCommandLine[[4]], "*"]
		];,
		3,
		command[
			StringTrim[$ScriptCommandLine[[3]], "*"]
		];,
		_,
		command[];
	];
];

End[];