#!/usr/bin/env wolframscript

Begin["WolframLanguageForJupyter`Private`"];

notfound = "configure-jupyter.wls: Jupyter installation on Environment[\"PATH\"] not found.";
isdir = "configure-jupyter.wls: Provided Jupyter binary path is a directory. Please provide the path to the Jupyter binary."
nobin = "configure-jupyter.wls: Provided Jupyter binary path does not exist.";
isdirMath = "configure-jupyter.wls: Provided Wolfram Engine binary path is a directory. Please provide the path to the Wolfram Engine binary."
nobinMath = "configure-jupyter.wls: Provided Wolfram Engine binary path does not exist.";
notadded = "configure-jupyter.wls: An error has occurred. There is still no Wolfram Engine in \"jupyter kernelspec list.\"";
notremoved = "configure-jupyter.wls: An error has occurred. There is a Wolfram Engine still in \"jupyter kernelspec list.\"";
nopaclet = "configure-jupyter.wls: WolframLanguageForJupyter paclet source not detected. Are you running the script in the root project directory?";

globalKernelUUID;
displayName;

processEnvironment;

getVersionFromKernel[mathB_String] :=
	Module[{link, res},
		link = LinkLaunch[First[$CommandLine] <> " -wstp"];
		If[FailureQ[link],
			Return["$Failed"];
		];
		LinkRead[link];
		LinkWrite[link, Unevaluated[$Version]];
		res = StringTrim[ToString[LinkRead[link]], "ReturnPacket[" | "]"];
		LinkClose[link];
		If[!StringContainsQ[res, "[" | "]"],
			Return[res];,
			Return["$Failed"];
		];
	];

getNames[mathB_String, wasProvided_?BooleanQ] := 
	Module[{version, installDir, names, displayNameTemp, globalKernelUUIDTemp, hashedKernelUUID},
		If[
			wasProvided,
			version = $Version;
			installDir = $InstallationDirectory;
			,
			version = getVersionFromKernel[mathB];
			installDir = mathB;
		];
		(* globalKernelUUID = "11a8cf20-da0e-4976-83e5-27579d6360b3"; *)
		hashedKernelUUID = Hash[installDir, "SHA", "HexString"];
		names = StringCases[version, name___ ~~ " for " ~~ ("Mac" | "Microsoft" | "Windows" | "Linux") -> name];
		If[Length[names] > 0, 
			globalKernelUUIDTemp =
				ToLowerCase[StringJoin[
					"WolframLanguage-",
					StringReplace[First[names], Whitespace -> "-"]
				]];
			displayNameTemp = 
				StringJoin[
					"Wolfram Language (",
					Capitalize[
						First[names],
						"AllWords"
					],
					")"
				];
			,
			globalKernelUUIDTemp = hashedKernelUUID;
			displayNameTemp = "Wolfram Language";
		];
		Return[{globalKernelUUIDTemp, displayNameTemp}];
	];

projectHome = Directory[];
kernelScript = FileNameJoin[{projectHome, "WolframLanguageForJupyter", "Resources", "kernel.wl"}];
If[
	!(FileType[kernelScript] === File),
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
		$OperatingSystem === "MacOSX" && FileType["~/.profile"] === File,
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
		Print["configure-jupyter.wls: Please use -file instead of -script in WolframScript."];,
		Print["configure-jupyter.wls: An unknown error has occurred."];
		attemptPathRegeneration[];
	];
	Quit[];
];

defineGlobalVars;
If[
	Length[splitPath] == 1,
	Print["configure-jupyter.wls: Warning: This script has encountered a very small PATH environment variable."];
	Print["configure-jupyter.wls: Warning: This can occur due to a possible WolframScript bug."];
	attemptPathRegeneration[];
];


findJupyerPath[] := 
	SelectFirst[
		splitPath,
		(FileType[FileNameJoin[{#1, StringJoin["jupyter", fileExt]}]] === File)&
	];

addKernelToJupyter[] := 
	Module[{jupyterPath},
		jupyterPath = findJupyerPath[];
		If[MissingQ[jupyterPath],
			Print[notfound];
			Return[$Failed];
		];
		Return[
			addKernelToJupyter[
				FileNameJoin[{jupyterPath, StringJoin["jupyter", fileExt]}],
				mathBin
			]
		];
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
		json = Quiet[ImportString[RunProcess[{jupyterPath, "kernelspec", "list", "--json"}, "StandardOutput", ProcessEnvironment -> processEnvironment], "JSON"]];
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

(* addKernelToJupyter[jupyterPath_String] := addKernelToJupyter[jupyterPath, mathBin]; *)
addKernelToJupyter[mathB_String] := 
	Module[{jupyterPath},
		jupyterPath = findJupyerPath[];
		If[MissingQ[jupyterPath],
			Print[notfound];
			Return[$Failed];
		];
		Return[
			addKernelToJupyter[
				FileNameJoin[{jupyterPath, StringJoin["jupyter", fileExt]}],
				mathB
			]
		];
	];

removeKernelFromJupyter[jupyterPath_String (*, kernelUUID_String *)] := 
	Module[{exitCodeOld, exitCode, kernelspecAssoc, kernelspecs, fileType},
		If[
			!((fileType = FileType[jupyterPath]) === File),
			Switch[
				fileType,
				Directory,
				Print[isdir];,
				None,
				Print[nobin];
			];
			Return[$Failed];
		];

		(* as an association for 11.3 compatibility *)
			processEnvironment = Association[GetEnvironment[]];
			processEnvironment["PATH"] = StringJoin[Environment["PATH"], pathSeperator, DirectoryName[jupyterPath]];

		exitCodeOld = RunProcess[{jupyterPath, "kernelspec", "remove", "-f", hashedKernelUUID}, "ExitCode", ProcessEnvironment -> processEnvironment];
		exitCode = RunProcess[{jupyterPath, "kernelspec", "remove", "-f", globalKernelUUID}, "ExitCode", ProcessEnvironment -> processEnvironment];

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
			Print["configure-jupyter.wls: See below for the message that Jupyter returned when attempting to remove the Wolfram Engine."];
			Print[exitCode["StandardError"]];
			Return[$Failed];
		];

		(* Return[kernelUUID]; *)
	];

removeKernelFromJupyter[jupyterPath_String, ___] := removeKernelFromJupyter[jupyterPath];

addKernelToJupyter[jupyterPath_String, mathB_String] := 
	Module[{baseDir, tempDir, exitCode, kernelspecAssoc, kernelspecs, kernelUUID, fileType},
		If[
			!((fileType = FileType[jupyterPath]) === File),
			Switch[
				fileType,
				Directory,
				Print[isdir];,
				None,
				Print[nobin];
			];
			Return[$Failed];
		];

		If[
			!((fileType = FileType[mathB]) === File),
			Switch[
				fileType,
				Directory,
				Print[isdirMath];,
				None,
				Print[nobinMath];
			];
			Return[$Failed];
		];

		(* as an association for 11.3 compatibility *)
			processEnvironment = Association[GetEnvironment[]];
			processEnvironment["PATH"] = StringJoin[Environment["PATH"], pathSeperator, DirectoryName[jupyterPath]];

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
					"--user",
					tempDir
				}, All, ProcessEnvironment -> processEnvironment];

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
			Print["configure-jupyter.wls: See below for the message that Jupyter returned when attempting to add the Wolfram Engine."];
			Print[exitCode["StandardError"]];
			Return[$Failed];
		];

		(* Return[kernelUUID]; *)
	];

templateJupyterPath = StringJoin["\"", FileNameJoin[{"path", "to", "Jupyter binary"}], "\""];
templateWLPath = StringJoin["\"", FileNameJoin[{"", "absolute", "path", "to", "Wolfram Engine binary"}], "\""]; 

(* helpMessage = StringJoin[
				"configure-jupyter.wls add [", templateJupyterPath, "]\n",
				"configure-jupyter.wls adds a Wolfram Engine to a Jupyter binary on PATH, or optional provided Jupyter binary path\n",
				"configure-jupyter.wls add ", templateJupyterPath, " ", templateWLPath, "\n",
				"\tadds the provided absolute Wolfram Engine binary path to the provided Jupyter binary path\n",
				"configure-jupyter.wls remove [", templateJupyterPath ,"]\n",
				"\tremoves any Wolfram Engines found on a Jupyter binary on PATH, or optional provided Jupyter binary path"
			]; *)

helpMessage = StringJoin[
				"configure-jupyter.wls add [", templateWLPath, "]\n",
				"\tadds a Wolfram Engine, either attached to the current invocation, or at the provided absolute Wolfram Engine binary path, to a Jupyter binary on PATH\n",
				"configure-jupyter.wls add ", templateWLPath, " ", templateJupyterPath, "\n",
				"\tadds the provided absolute Wolfram Engine binary path to the provided Jupyter binary path\n",
				"configure-jupyter.wls remove [", templateJupyterPath ,"]\n",
				"\tremoves any Wolfram Engines found on a Jupyter binary on PATH, or optional provided Jupyter binary path",
				"configure-jupyter.wls build\n",
				"\tbuilds the WolframLanguageForJupyter paclet in the project directory"
			];

If[Length[$ScriptCommandLine] == 1 || $ScriptCommandLine[[2]] === "help",
	Print[helpMessage];
	,
	Switch[
		$ScriptCommandLine[[2]],
		"remove",
		command = removeKernelFromJupyter;,
		"add",
		command = addKernelToJupyter;,
		"build",
		PackPaclet["WolframLanguageForJupyter"];
		Quit[];
		,
		_,
		Print[helpMessage];
	];

	Switch[
		Length[$ScriptCommandLine],
		4,
		{globalKernelUUID, displayName} = getNames[StringTrim[$ScriptCommandLine[[4]], "*"], True];
		command[
			StringTrim[$ScriptCommandLine[[4]], "*"],
			StringTrim[$ScriptCommandLine[[3]], "*"]
		];,
		3,
		If[
			command === addKernelToJupyter,
			{globalKernelUUID, displayName} = getNames[StringTrim[$ScriptCommandLine[[3]], "*"], False];,
			{globalKernelUUID, displayName} = getNames["", False];
		];
		command[
			StringTrim[$ScriptCommandLine[[3]], "*"]
		];,
		_,
		{globalKernelUUID, displayName} = getNames["", False];
		command[];
	];
];

End[];
