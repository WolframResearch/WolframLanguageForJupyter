BeginPackage["WolframLanguageForJupyter`"];

ConfigureJupyter::subcommand = "The first argument to ConfigureJupyter is the subcommand: either \"add\" or \"remove\".";
(* ConfigureJupyter::configuredetails = "The optional second argument to ConfigureJupyter is an association or list of rules specifying details for configuring Jupyter."; *)
ConfigureJupyter::argx = "ConfigureJupyter called with `1` arguments; 1 argument is expected.";

ConfigureJupyter::addnotfound = "Jupyter installation on Environment[\"PATH\"] not found.";
ConfigureJupyter::addisdir = "Provided `1` binary path is a directory. Please provide the path to the `1` binary.";
ConfigureJupyter::addnobin = "Provided `1` binary path does not exist."; 
ConfigureJupyter::addnotadded = "An error has occurred. There is still no Wolfram Engine in \"jupyter kernelspec list.\" See WolframLanguageForJupyter`.`Errors`.`$ConfigureError for the message that Jupyter returned when attempting to add the Wolfram Engine.";

ConfigureJupyter::removenotfound = ConfigureJupyter::addnotfound;
ConfigureJupyter::removeisdir = ConfigureJupyter::addisdir;
ConfigureJupyter::removenobin = ConfigureJupyter::addnobin;
ConfigureJupyter::removenotremoved = "An error has occurred. There is a Wolfram Engine still in \"jupyter kernelspec list.\" See WolframLanguageForJupyter`.`Errors`.`$ConfigureError for the message that Jupyter returned when attempting to remove the Wolfram Engine.";

ConfigureJupyter::usage = 
	"ConfigureJupyter[subcommand:\"add\"|\"remove\"] evaluates the action associated with subcommand, relying on the current Wolfram Engine binary path and the first Jupyter installation on Environment[\"PATH\"] when relevant.
ConfigureJupyter[subcommand:\"add\"|\"remove\", opts] evaluates the action associated with subcommand, using specified paths for \"WolframEngineBinary\" and \"JupyterInstallation\" when given as options.";

Begin["`Private`"];

processEnvironment;

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

envPath := If[
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
];

findJupyerPath[] := 
	SelectFirst[
		StringSplit[envPath, pathSeperator],
		(FileType[FileNameJoin[{#1, StringJoin["jupyter", fileExt]}]] === File)&
	];

addKernelToJupyter[] := 
	Module[{jupyterPath},
		jupyterPath = findJupyerPath[];
		If[MissingQ[jupyterPath],
			Message[ConfigureJupyter::addnotfound];
			Return[$Failed];
		];
		Return[addKernelToJupyter[FileNameJoin[{jupyterPath, StringJoin["jupyter", fileExt]}]]];
	];

removeKernelFromJupyter[] := 
	Module[{jupyterPath},
		jupyterPath = findJupyerPath[];
		If[MissingQ[jupyterPath],
			Message[ConfigureJupyter::removenotfound];
			Return[$Failed];
		];
		Return[removeKernelFromJupyter[FileNameJoin[{jupyterPath, StringJoin["jupyter", fileExt]}]]];
	];

getKernelspecAssoc[jupyterPath_String] := 
	Module[{json},
		json = Quiet[ImportString[RunProcess[{jupyterPath, "kernelspec", "list", "--json"}, "StandardOutput"], "JSON", ProcessEnvironment -> processEnvironment]];
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
	Module[{exitCodeOld, exitCode, kernelspecAssoc, kernelspecs, fileType},
		If[
			!((fileType = FileType[jupyterPath]) === File),
			Switch[
				fileType,
				Directory,
				Message[ConfigureJupyter::removeisdir, "Jupyter"];,
				None,
				Message[ConfigureJupyter::removenobin, "Jupyter"];
			];
			Return[$Failed];
		];

		(* as an association for 11.3 compatibility *)
			processEnvironment = Association[GetEnvironment[]];
			processEnvironment["PATH"] = StringJoin[envPath, pathSeperator, DirectoryName[jupyterPath]];

		exitCodeOld = RunProcess[{jupyterPath, "kernelspec", "remove", "-f", hashedKernelUUID}, "ExitCode", ProcessEnvironment -> processEnvironment];
		exitCode = RunProcess[{jupyterPath, "kernelspec", "remove", "-f", globalKernelUUID}, All, ProcessEnvironment -> processEnvironment];
		
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
			WolframLanguageForJupyter`Errors`$ConfigureError = exitCode["StandardError"];
			Message[ConfigureJupyter::removenotremoved];
			Return[$Failed];
		];

		(* Return[kernelUUID]; *)
		Return[Success["ConfiguredJupyter", <|"Message" -> "\"Wolfram Language for Jupyter\" has been removed from Jupyter"|>]];
	];

removeKernelFromJupyter[jupyterPath_String, ___] := removeKernelFromJupyter[jupyterPath];

addKernelToJupyter[jupyterPath_String, mathB_String] := 
	Module[{baseDir, tempDir, exitCode, kernelspecAssoc, kernelspecs, kernelUUID, fileType},
		If[
			!((fileType = FileType[jupyterPath]) === File),
			Switch[
				fileType,
				Directory,
				Message[ConfigureJupyter::addisdir, "Jupyter"];,
				None,
				Message[ConfigureJupyter::addnobin, "Jupyter"];
			];
			Return[$Failed];
		];

		If[
			!((fileType = FileType[mathB]) === File),
			Switch[
				fileType,
				Directory,
				Message[ConfigureJupyter::addisdir, "Wolfram Engine"];,
				None,
				Message[ConfigureJupyter::addnobin, "Wolfram Engine"];
			];
			Return[$Failed];
		];

		(* as an association for 11.3 compatibility *)
			processEnvironment = Association[GetEnvironment[]];
			processEnvironment["PATH"] = StringJoin[envPath, pathSeperator, DirectoryName[jupyterPath]];

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
				"argv" -> {mathB, "-script", FileNameJoin[{pacletHome, "Resources", "KernelForWolframLanguageForJupyter.wl"}], "{connection_file}"},
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
			WolframLanguageForJupyter`Errors`$ConfigureError = exitCode["StandardError"];
			Message[ConfigureJupyter::addnotadded];
			Return[$Failed];
		];

		(* Return[kernelUUID]; *)
		Return[Success["ConfiguredJupyter", <|"Message" -> "\"Wolfram Language for Jupyter\" has been added to Jupyter"|>]];
	];

ConfigureJupyter[
		arg1_,
		arg2:KeyValuePattern[{}],
		argsRest___,
		opts:OptionsPattern[] /; Length[{opts}] > 0
	] := ConfigureJupyter[
			arg1,
			Merge[{arg2, Association[opts]}, Last],
			argsRest
		];
ConfigureJupyter[
		args___,
		opts:OptionsPattern[] /; Length[{opts}] > 0
	] := ConfigureJupyter[args, Association[opts]];

ConfigureJupyter["Add", args___] := ConfigureJupyter["add", args];
ConfigureJupyter["add"] := addKernelToJupyter[];
ConfigureJupyter["add", KeyValuePattern[{"WolframEngineBinary" -> wl_String, "JupyterInstallation" -> jup_String}]] := 
	addKernelToJupyter[jup, wl];
ConfigureJupyter["add", KeyValuePattern[{"WolframEngineBinary" -> wl_String}]] :=
	Module[{jupyterPath},
		jupyterPath = findJupyerPath[];
		If[MissingQ[jupyterPath],
			Print[notfound];
			Return[$Failed];
		];
		Return[
			addKernelToJupyter[
				FileNameJoin[{jupyterPath, StringJoin["jupyter", fileExt]}],
				wl
			]
		];
	];
ConfigureJupyter["add", KeyValuePattern[{"JupyterInstallation" -> jup_String}]] := addKernelToJupyter[jup];
ConfigureJupyter["add", KeyValuePattern[{}]] := addKernelToJupyter[];

ConfigureJupyter["Remove", args___] := ConfigureJupyter["remove", args];
ConfigureJupyter["remove"] := removeKernelFromJupyter[];
ConfigureJupyter["remove", KeyValuePattern[{"WolframEngineBinary" -> wl_String, "JupyterInstallation" -> jup_String}]] :=
	removeKernelFromJupyter[jup, wl];
ConfigureJupyter["remove", KeyValuePattern[{"WolframEngineBinary" -> wl_String}]] :=
	Module[{jupyterPath},
		jupyterPath = findJupyerPath[];
		If[MissingQ[jupyterPath],
			Print[notfound];
			Return[$Failed];
		];
		Return[
			removeKernelFromJupyter[
				FileNameJoin[{jupyterPath, StringJoin["jupyter", fileExt]}],
				wl
			]
		];
	];
ConfigureJupyter["remove", KeyValuePattern[{"JupyterInstallation" -> jup_String}]] := removeKernelFromJupyter[jup];
ConfigureJupyter["remove", KeyValuePattern[{}]] := removeKernelFromJupyter[];

ConfigureJupyter[sc_String, ___] /; !StringMatchQ[sc, "add" | "remove" | "Add" | "Remove"] := Message[ConfigureJupyter::subcommand];
ConfigureJupyter[Except[_String], ___] := Message[ConfigureJupyter::subcommand];
(* ConfigureJupyter[_, Except[KeyValuePattern[{}]], ___] := Message[ConfigureJupyter::configuredetails]; *)
ConfigureJupyter[args___] := Message[ConfigureJupyter::argx, Length[{args}]];

End[];
EndPackage[];