Begin["WolframLanguageForJupyter`Private`"];

notfound = "install.wls: Jupyter installation on Environment[\"PATH\"] not found.";
isdir = "install.wls: Provided Jupyter binary path is a directory. Please provide the path to the Jupyter binary."
nobin = "install.wls: Provided Jupyter binary path does not exist.";
notadded = "install.wls: An error has occurred. There is still no Wolfram kernel in \"jupyter kernelspec list.\"";
notremoved = "install.wls: An error has occurred. There is a Wolfram kernel still in \"jupyter kernelspec list.\"";
nopaclet = "install.wls: WolframLanguageForJupyter paclet source not detected. Are you running the script in the root project directory?";

(* globalKernelUUID = "11a8cf20-da0e-4976-83e5-27579d6360b3"; *)
globalKernelUUID = Hash[$InstallationDirectory, "SHA", "HexString"];

kernelScript = FileNameJoin[{projectHome, "WolframLanguageForJupyter", "Resources", "kernel.wl"}];
projectHome = Directory[];
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
				"argv" -> {mathBin, "-script", kernelScript, "{connection_file}"},
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