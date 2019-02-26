BeginPackage["WolframLanguageForJupyter`"];

ConfigureJupyter::subcommand = "The first argument to ConfigureJupyter is the subcommand: either \"add\", \"remove\", or \"clear\".";
ConfigureJupyter::argx = "ConfigureJupyter called with `1` arguments; 1 argument is expected.";

ConfigureJupyter::notfound = "Jupyter installation on Environment[\"PATH\"] not found.";
ConfigureJupyter::isdir = "Provided `1` binary path is a directory. Please provide the path to the `1` binary.";
ConfigureJupyter::nobin = "Provided `1` binary path does not exist.";

ConfigureJupyter::notadded = "An error has occurred. The desired Wolfram Engine is not in \"jupyter kernelspec list.\" See WolframLanguageForJupyter`.`Errors`.`$ConfigureError for the message that Jupyter returned when attempting to add the Wolfram Engine.";
ConfigureJupyter::notremoved = "An error has occurred: Wolfram Engine(s) still in \"jupyter kernelspec list.\" See WolframLanguageForJupyter`.`Errors`.`$ConfigureError for the message that Jupyter returned when attempting to remove the Wolfram Engine.";

ConfigureJupyter::nolink = "An error has occurred: Communication with provided Wolfram Engine binary could not be established.";

ConfigureJupyter::usage = 
	"ConfigureJupyter[subcommand:\"add\"|\"remove\"|\"clear\"] evaluates the action associated with subcommand, relying on the current Wolfram Engine binary path and the first Jupyter installation on Environment[\"PATH\"] when relevant.
ConfigureJupyter[subcommand:\"add\"|\"remove\"|\"clear\", opts] evaluates the action associated with subcommand, using specified paths for \"WolframEngineBinary\" and \"JupyterInstallation\" when given as options.";

Begin["`Private`"];

(* START: Helper symbols  *)

projectHome = DirectoryName[$InputFileName];

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

		hashedKernelUUID = StringJoin["wl-", Hash[installDir, "SHA", "HexString"]];

		names = StringCases[version, name___ ~~ " for " ~~ ("Mac" | "Microsoft" | "Windows" | "Linux") -> name];
		Return[
			If[Length[names] > 0, 
				{
					ToLowerCase[StringJoin[
						"WolframLanguage-",
						StringReplace[First[names], Whitespace -> "-"]
					]],
					StringJoin[
						"Wolfram Language (",
						Capitalize[
							First[names],
							"AllWords"
						],
						")"
					]
				}
				,
				{hashedKernelUUID, "Wolfram Language"}
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

(* splitPath := StringSplit[Environment["PATH"], pathSeperator]; *)
splitPath := StringSplit[If[
	$OperatingSystem === "MacOSX" && FileType["~/.profile"] === File,
	StringTrim[
					RunProcess[
						$SystemShell,
						"StandardOutput",
						StringJoin[Import["~/.profile", "String"], "\necho $PATH"],
						ProcessEnvironment -> {}
					], 
					"\n"
				]
	,
	Environment["PATH"]
], pathSeperator];


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
			retrievedNames, kernelUUID, displayName,
			notProvidedQ,
			jupyterPath, mathB,
			fileType,
			processEnvironment,
			baseDir, tempDir,
			wlKernels,
			commandArgs,
			exitInfo, kernelspecAssoc, kernelspecs
		},

		(* just check that the REPL script is there *)
		If[
			!(
				FileType[
					FileNameJoin[{projectHome, "Resources", "KernelForWolframLanguageForJupyter.wl"}]
				] === File
			),
			Return[$Failed];
		];

		jupyterPath = specs["JupyterInstallation"];
		(* if no Jupyter installation path provided, determine it from PATH *)
		If[
			MissingQ[jupyterPath],
			jupyterPath = findJupyerPath[];
			If[MissingQ[jupyterPath],
				Message[ConfigureJupyter::notfound, "Jupyter"];
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
				Message[ConfigureJupyter::isdir, "Jupyter"];,
				None,
				Message[ConfigureJupyter::nobin, "Jupyter"];
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
				If[FailureQ[retrievedNames], Message[ConfigureJupyter::nolink]; Return[$Failed]];
				{kernelUUID, displayName} = retrievedNames;,
				Switch[
					fileType,
					Directory,
					Message[ConfigureJupyter::isdir, "Wolfram Engine"];,
					None,
					Message[ConfigureJupyter::nobin, "Wolfram Engine"];
				];
				Return[$Failed];
			];
		];

		(* as an association for 11.3 compatibility *)
			processEnvironment = Association[GetEnvironment[]];
			processEnvironment["PATH"] = StringJoin[Riffle[Append[splitPath, DirectoryName[jupyterPath]], pathSeperator]];

		wlKernels = {kernelUUID};
		tempDir = "";
		If[
			!removeQ,

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
					"argv" -> {
						mathB,
						"-run",
						"Get[FileNameJoin[{DirectoryName[FindFile[\"WolframLanguageForJupyter`\"],2],\"Resources\",\"KernelForWolframLanguageForJupyter.wl\"}]]",
						"{connection_file}"
						(* , "-noprompt" *)
					},
					"display_name" -> displayName,
					"language" -> "Wolfram Language"
				]
			];

			commandArgs = {jupyterPath, "kernelspec", "install", "--user", tempDir};,
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
			WolframLanguageForJupyter`Errors`$ConfigureError = exitInfo["StandardError"];
			Print[WolframLanguageForJupyter`Errors`$ConfigureError];
			If[!removeQ, Message[ConfigureJupyter::notadded];, Message[ConfigureJupyter::notremoved];];
			Return[$Failed];
		];
	];

ConfigureJupyter[
		args___,
		opts:OptionsPattern[] /; Length[{opts}] > 0
	] := ConfigureJupyter[args, Association[opts]];

ConfigureJupyter["Add", args___] := ConfigureJupyter["add", args];
ConfigureJupyter["add"] := ConfigureJupyter["add", Association[]];
ConfigureJupyter["add", assoc_Association] := configureJupyter[assoc, False, False];

ConfigureJupyter["Remove", args___] := ConfigureJupyter["remove", args];
ConfigureJupyter["remove"] := ConfigureJupyter["add", Association[]];
ConfigureJupyter["remove", assoc_Association] := configureJupyter[assoc, True, False];

ConfigureJupyter["Clear", args___] := ConfigureJupyter["clear", args];
ConfigureJupyter["clear"] := ConfigureJupyter["add", Association[]];
ConfigureJupyter["clear", assoc_Association] := configureJupyter[assoc, True, False];

ConfigureJupyter[sc_String, ___] /; !StringMatchQ[sc, "add" | "remove" | "clear" | "Add" | "Remove" | "Clear"] := Message[ConfigureJupyter::subcommand];
ConfigureJupyter[Except[_String], ___] := Message[ConfigureJupyter::subcommand];
ConfigureJupyter[args___] := Message[ConfigureJupyter::argx, Length[{args}]];

End[];
EndPackage[];
