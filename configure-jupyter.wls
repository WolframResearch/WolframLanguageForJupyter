#!/usr/bin/env wolframscript

Begin["WolframLanguageForJupyter`Private`"];

notfound = "configure-jupyter.wls: Jupyter installation on Environment[\"PATH\"] not found.";
isdir = "configure-jupyter.wls: Provided Jupyter binary path is a directory. Please provide the path to the Jupyter binary."
nobin = "configure-jupyter.wls: Provided Jupyter binary path does not exist.";
isdirMath = "configure-jupyter.wls: Provided Wolfram Engine binary path is a directory. Please provide the path to the Wolfram Engine binary."
nobinMath = "configure-jupyter.wls: Provided Wolfram Engine binary path does not exist.";
notadded = "configure-jupyter.wls: An error has occurred. The desired Wolfram Engine is not in \"jupyter kernelspec list.\"";
notremoved = "configure-jupyter.wls: An error has occurred: Wolfram Engine(s) still in \"jupyter kernelspec list.\"";
nopaclet = "configure-jupyter.wls: WolframLanguageForJupyter paclet source not detected. Are you running the script in the root project directory?";
nolink = "configure-jupyter.wls: Communication with provided Wolfram Engine binary could not be established.";

(* START: Helper symbols  *)

projectHome = If[StringQ[$InputFileName] && $InputFileName != "", DirectoryName[$InputFileName], Directory[]];

(* establishes link with Wolfram Engine at mathB and evaluates $Version *)
getVersionFromKernel[mathB_String] :=
	Module[{link, res},
		link = LinkLaunch[mathB <> " -wstp"];
		If[FailureQ[link],
			Return[$Failed];
		];
		LinkRead[link];
		LinkWrite[link, Unevaluated[$Version]];
		res = StringTrim[ToString[LinkRead[link]], "ReturnPacket[" | "]"];
		LinkClose[link];
		If[!StringContainsQ[res, "[" | "]"],
			Return[res];,
			Return[$Failed];
		];
	];

(* determine display name for Jupyter installation from Wolfram Engine $Version *)
getNames[mathB_String, notProvidedQ_?BooleanQ] := 
	Module[{version, installDir, names, hashedKernelUUID},

		If[
			notProvidedQ,
			version = $Version;
			installDir = $InstallationDirectory;
			,
			version = Quiet[getVersionFromKernel[mathB]];
			If[
				FailureQ[version],
				Return[$Failed];
			];
			installDir = mathB;
		];

		hashedKernelUUID = StringJoin["wl-script-", Hash[installDir, "SHA", "HexString"]];

		names = StringCases[version, name___ ~~ " for " ~~ ("Mac" | "Microsoft" | "Windows" | "Linux") -> name];
		Return[
			If[Length[names] > 0, 
				{
					ToLowerCase[StringJoin[
						"WolframLanguage-script-",
						StringReplace[First[names], Whitespace -> "-"]
					]],
					StringJoin[
						"Wolfram Language (",
						Capitalize[
							First[names],
							"AllWords"
						],
						") | Script Install"
					]
				}
				,
				{hashedKernelUUID, "Wolfram Language | Script Install"}
			]
		];
	];

(* determine symbols related to finding Wolfram Engine and Jupyter installations *)
defineGlobalVars[] := 
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
		(* mathBin = FileNameJoin[{$InstallationDirectory, "Executables", "WolframKernel"}]; *)
		fileExt = "";
		pathSeperator = ":";
	];

mathBin := (defineGlobalVars[]; mathBin);
fileExt := (defineGlobalVars[]; fileExt);
pathSeperator := (defineGlobalVars[]; pathSeperator);

splitPath := StringSplit[Environment["PATH"], pathSeperator];

(* regenerate PATH when wolframscript bug encountered *)
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

(* find Jupyter installation path *)
findJupyerPath[] := 
	SelectFirst[
		splitPath,
		(FileType[FileNameJoin[{#1, StringJoin["jupyter", fileExt]}]] === File)&
	];

(* get information about installed kernels in Jupyter *)
getKernels[jupyterPath_String, processEnvironment_] := 
	Module[{json, kernelspecAssoc},
		json = Quiet[ImportString[RunProcess[{jupyterPath, "kernelspec", "list", "--json"}, "StandardOutput", ProcessEnvironment -> processEnvironment], "JSON"]];
		kernelspecAssoc = 
			If[
				FailureQ[json],
				Association[],
				Replace[
					json,
					part_List /; AllTrue[part, Head[#1] === Rule &] -> Association @ part, 
					{0, Infinity}
				]
			];
		Return[
			If[
				KeyExistsQ[kernelspecAssoc, "kernelspecs"],
				Keys[kernelspecAssoc["kernelspecs"]],
				{}
			]
		];
	];


(* END: Helper symbols  *)

(* main install command *)
configureJupyter[specs_Association, removeQ_?BooleanQ, removeAllQ_?BooleanQ] := 
	Module[
		{
			kernelScript,
			retrievedNames, kernelUUID, displayName,
			notProvidedQ,
			jupyterPath, mathB,
			fileType,
			processEnvironment,
			baseDir, tempDir,
			wlKernels,
			commandArgs,
			exitInfo, kernelspecAssoc, kernelspecs,
			failureMessage
		},

		kernelScript = FileNameJoin[{projectHome, "WolframLanguageForJupyter", "Resources", "KernelForWolframLanguageForJupyter.wl"}];
		If[
			!(FileType[kernelScript] === File),
			Print[nopaclet];
			Return[$Failed];
		];

		jupyterPath = specs["JupyterInstallation"];
		(* if no Jupyter installation path provided, determine it from PATH *)
		If[
			MissingQ[jupyterPath],
			jupyterPath = findJupyerPath[];
			If[MissingQ[jupyterPath],
				Print[notfound];
				Return[$Failed];
			];
			jupyterPath = FileNameJoin[{jupyterPath, StringJoin["jupyter", fileExt]}];
		];

		mathB = Lookup[specs, "WolframEngineBinary", (notProvidedQ = True; mathBin)];

		(* check that the Jupyter installation path is a file *)
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

		{kernelUUID, displayName} = {"", ""};
		(* check that the Wolfram Engine installation path is a file *)
		If[
			!(removeQ && removeAllQ),
			If[
				(fileType = FileType[mathB]) === File,
				retrievedNames = getNames[mathB, TrueQ[notProvidedQ]];
				If[FailureQ[retrievedNames], Print[nolink]; Return[$Failed]];
				{kernelUUID, displayName} = retrievedNames;,
				Switch[
					fileType,
					Directory,
					Print[isdirMath];,
					None,
					Print[nobinMath];
				];
				Return[$Failed];
			];
		];

		(* as an association for 11.3 compatibility *)
			processEnvironment = Association[GetEnvironment[]];
			processEnvironment["PATH"] = StringJoin[Environment["PATH"], pathSeperator, DirectoryName[jupyterPath]];

		wlKernels = {kernelUUID};
		tempDir = "";
		If[
			!removeQ,
			failureMessage = notadded;

			tempDir = CreateDirectory[
				FileNameJoin[{
					projectHome,
					CreateUUID[],
					(* removing this would cause every evalution of addKernelToJupyter adds a new kernel with a different uuid *)
					kernelUUID
				}], CreateIntermediateDirectories -> True
			];

			Export[
				FileNameJoin[{tempDir, "kernel.json"}], 
				Association[
					"argv" -> {mathB, "-script", kernelScript, "{connection_file}" (* , "-noprompt" *)},
					"display_name" -> displayName,
					"language" -> "Wolfram Language"
				]
			];

			commandArgs = {jupyterPath, "kernelspec", "install", "--user", tempDir};,
			failureMessage = notremoved;
			commandArgs = {jupyterPath, "kernelspec", "remove", "-f",
				If[
					!removeAllQ,
					kernelUUID,
					Sequence @@ (wlKernels = Select[getKernels[jupyterPath, processEnvironment], StringMatchQ[#1, ("wolframlanguage-" | "WL-") ~~ ___] &])
				]
			}
		];
		exitInfo = RunProcess[commandArgs, All, ProcessEnvironment -> processEnvironment];

		If[StringLength[tempDir] > 0, DeleteDirectory[DirectoryName[tempDir], DeleteContents -> True]];
		If[Length[wlKernels] == 0, Return[];];

		kernelspecs = getKernels[jupyterPath, processEnvironment];
		If[
			!Xor[removeQ, SubsetQ[kernelspecs, wlKernels]],
			Print[failureMessage];
			Print["configure-jupyter.wls: See below for the message that Jupyter returned when attempting to add the Wolfram Engine."];
			Print[StringTrim[exitInfo["StandardError"], Whitespace]];
			Return[$Failed];
		];
	];

If[
	FailureQ[RunProcess[$SystemShell, All, ""]],
	(* maybe remove *)
	If[
		MemberQ[$CommandLine, "-script"],
		Print["configure-jupyter.wls: Please use -file instead of -script in WolframScript."];
		Quit[];
		,
		Print["configure-jupyter.wls: An unknown error has occurred."];
		attemptPathRegeneration[];
		If[FailureQ[RunProcess[$SystemShell, All, ""]], Quit[]];
	];
];

defineGlobalVars[];

(* maybe remove *)
If[
	Length[splitPath] == 1,
	Print["configure-jupyter.wls: Warning: This script has encountered a very small PATH environment variable."];
	Print["configure-jupyter.wls: Warning: This can occur due to a possible WolframScript bug."];
	attemptPathRegeneration[];
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
				"configure-jupyter.wls remove [", templateWLPath ,"]\n",
				"\tremoves the Wolfram Engine, either attached to the current invocation, or at the provided absolute Wolfram Engine binary path, from a Jupyter binary on PATH\n",
				"configure-jupyter.wls remove ", templateWLPath, " ", templateJupyterPath, "\n",
				"\tremoves the provided absolute Wolfram Engine binary path from the provided Jupyter binary path\n",
				"configure-jupyter.wls clear [", templateJupyterPath ,"]\n",
				"\tremoves all Wolfram Engines found on a Jupyter binary on PATH, or optional provided Jupyter binary path\n",
				"configure-jupyter.wls build\n",
				"\tbuilds the WolframLanguageForJupyter paclet in the project directory"
			];

If[
	Length[$ScriptCommandLine] < 2 ||
		Length[$ScriptCommandLine] > 4 ||
			$ScriptCommandLine[[2]] === "help",
	Print[helpMessage];
	,
	Switch[
		$ScriptCommandLine[[2]],
		"add" | "Add",
		command = {False, False};,
		"remove" | "Remove",
		command = {True, False};,
		"clear" | "Clear",
		command = {True, True};,
		"build",
		PackPaclet["WolframLanguageForJupyter"];
		Quit[];
		,
		_,
		Print[helpMessage];
	];

	configureJupyter[
		Switch[
			Length[$ScriptCommandLine],
			4,
			Association[
				"WolframEngineBinary" -> $ScriptCommandLine[[3]],
				"JupyterInstallation" -> $ScriptCommandLine[[4]]
			],
			3,
			If[command === {True, True},
				Association["JupyterInstallation" -> $ScriptCommandLine[[3]]],
				Association["WolframEngineBinary" -> $ScriptCommandLine[[3]]]
			],
			2,
			Association[]
		],
		Sequence @@ command
	];
];

End[];
