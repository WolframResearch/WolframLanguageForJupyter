BeginPackage["WolframLanguageForJupyter`"];

ConfigureJupyter::subcommand = "The first argument to ConfigureJupyter is the subcommand: either \"add\", \"remove\", or \"clear\".";
ConfigureJupyter::argx = "ConfigureJupyter called with `1` arguments; 1 argument is expected.";

ConfigureJupyter::notfound = "Jupyter installation on Environment[\"PATH\"] not found.";
ConfigureJupyter::isdir = "Provided `1` binary path is a directory. Please provide the path to the `1` binary.";
ConfigureJupyter::nobin = "Provided `1` binary path does not exist.";

ConfigureJupyter::notadded = "An error has occurred. The desired Wolfram Engine is not in \"jupyter kernelspec list.\" See WolframLanguageForJupyter`.`Errors`.`$ConfigureError for the message that Jupyter returned when attempting to add the Wolfram Engine.";
ConfigureJupyter::notremoved = "An error has occurred: Wolfram Engine(s) still in \"jupyter kernelspec list.\" See WolframLanguageForJupyter`.`Errors`.`$ConfigureError for the message that Jupyter returned when attempting to remove the Wolfram Engine.";

ConfigureJupyter::addconflict = "An error has occurred. A Wolfram Engine with the same $VersionNumber of the target Wolfram Engine is in \"jupyter kernelspec list.\" Attempting to overwrite ...";
(* ConfigureJupyter::removeconflict = "An error has occurred. The Wolfram Engine(s) to be removed is/are not in \"jupyter kernelspec list.\""; *)

ConfigureJupyter::nolink = "An error has occurred: Communication with provided Wolfram Engine binary could not be established.";

ConfigureJupyter::usage = 
	"ConfigureJupyter[subcommand:\"add\"|\"remove\"|\"clear\"] evaluates the action associated with subcommand, relying on the current Wolfram Engine binary path and the first Jupyter installation on Environment[\"PATH\"] when relevant.
ConfigureJupyter[subcommand:\"add\"|\"remove\"|\"clear\", opts] evaluates the action associated with subcommand, using specified paths for \"WolframEngineBinary\" and \"JupyterInstallation\" when given as options.";

Begin["`Private`"];

(*
	Dictionary:
		mathBin/mathBinSession = WolframKernel binary
		kernelspec = Kernel Specification; term used by Jupyter
		notProvidedQ = was a Wolfram Engine Binary explicitly specified?
*)

(* START: Helper symbols  *)

projectHome = DirectoryName[$InputFileName];

(* establishes link with Wolfram Engine at mathBin and evaluates $Version/$VersionNumber *)
(* returns string form *)
getVersionFromKernel[mathBin_String] :=
	Module[{link, res},
		link = 
			LinkLaunch[
				StringJoin[
					{
						"\"",
						mathBin,
						"\" -wstp"
					}
				]
			];
		If[FailureQ[link],
			Return[$Failed];
		];
		(* bleed link *)
		While[LinkReadyQ[link, 0.5], LinkRead[link];];
		LinkWrite[link, Unevaluated[$VersionNumber]];
		res = StringTrim[ToString[LinkRead[link]], "ReturnPacket[" | "]"];
		LinkClose[link];
		If[!StringContainsQ[res, "[" | "]"],
			Return[res];,
			Return[$Failed];
		];
	];

(* determine display name for Jupyter installation from Wolfram Engine $Version/$VersionNumber *)
(* returns {Kernel ID, Display Name} *)
getNames[mathBin_String, notProvidedQ_?BooleanQ] := 
	Module[{version, installDir, (* names, hashedKernelUUID *) versionStr},
		(* if Wolfram Engine binary not provided, just evaluate $Version in the current session *)
		(* otherwise, use MathLink to obtain $Version *)
		If[
			notProvidedQ,
			version = ToString[$VersionNumber];
			installDir = $InstallationDirectory;
			,
			version = Quiet[getVersionFromKernel[mathBin]];
			If[
				FailureQ[version],
				Return[$Failed];
			];
			installDir = mathBin;
		];
		
		versionStr = StringTrim[version, "."];
		Return[
			{
				(* Kernel ID *)
				StringJoin["wolframlanguage", versionStr],
				(* Display Name *)
				StringJoin["Wolfram Language ", versionStr]
			}
		];
	];

(* determine symbols related to finding Wolfram Engine and Jupyter installations *)
(* mathBinSession: WolframKernel location for the current session *)
(* fileExt: file extension for executables *)
(* pathSeperator: delimiter for directories on PATH *)
defineGlobalVars[] := 
	Switch[
		$OperatingSystem,
		"Windows",
		mathBinSession = FileNameJoin[{$InstallationDirectory, "wolfram.exe"}];
		fileExt = ".exe";
		pathSeperator = ";";,
		"MacOSX",
		mathBinSession = FileNameJoin[{$InstallationDirectory, "MacOS", "WolframKernel"}];
		fileExt = "";
		pathSeperator = ":";,
		"Unix",
		mathBinSession = FileNameJoin[{$InstallationDirectory, "Executables", "WolframKernel"}];
		fileExt = "";
		pathSeperator = ":";
	];

mathBinSession := (defineGlobalVars[]; mathBinSession);
fileExt := (defineGlobalVars[]; fileExt);
pathSeperator := (defineGlobalVars[]; pathSeperator);

(* a list of directories in PATH *)
splitPath := 
	StringSplit[
		(* restore PATH, if due to a bug, it becomes essentially empty; this is relevant to finding the Jupyter installation *)
		(* otherwise, just use PATH directly *)
		If[
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
		],
	pathSeperator];


(* find Jupyter installation path *)
(* returns above *)
findJupyterPath[] := 
	SelectFirst[
		splitPath,
		(* check every directory in PATH to see if a Jupyter binary is a member *)
		(FileType[FileNameJoin[{#1, StringJoin["jupyter", fileExt]}]] === File)&
	];

(* get information about installed kernels in Jupyter *)
(* returns kernel IDs in Jupyter *)
getKernels[jupyterPath_String, processEnvironment_] := 
	Module[{json, kernelspecAssoc},
		(* obtain information about "jupyter kernelspec list" in JSON *)
		json = Quiet[ImportString[RunProcess[{jupyterPath, "kernelspec", "list", "--json"}, "StandardOutput", ProcessEnvironment -> processEnvironment], "JSON"]];
		(* transform that JSON information into an Association *)
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
			(* if the above process worked, just return the kernel IDs of all the kernelspecs *)
			(* otherwise, return an empty list *)
			If[
				KeyExistsQ[kernelspecAssoc, "kernelspecs"],
				Keys[kernelspecAssoc["kernelspecs"]],
				{}
			]
		];
	];


(* END: Helper symbols  *)

(* main install command *)
(* specs: options \"WolframEngineBinary\" and \"JupyterInstallation\" in an Association, when provided *)
(* removeQ: remove a Jupyter installation or not *)
(* removeAllQ: clear all Jupyter installations or not *)
(* removeQ first, removeAllQ second: "add" is False, False; "remove" is True, False, and "clear" is True, True *)
(* returns action success status *)
configureJupyter[specs_Association, removeQ_?BooleanQ, removeAllQ_?BooleanQ] := 
	Module[
		{
			retrievedNames, kernelID, displayName,
			notProvidedQ,
			jupyterPath, mathBin,
			fileType,
			processEnvironment,
			baseDir, tempDir,
			wlKernels, (* wlKernelsL(owerCase) *) wlKernelsL,
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
			jupyterPath = findJupyterPath[];
			(* if Jupyter not on PATH, message *)
			If[MissingQ[jupyterPath],
				Message[ConfigureJupyter::notfound, "Jupyter"];
				Return[$Failed];
			];
			jupyterPath = FileNameJoin[{jupyterPath, StringJoin["jupyter", fileExt]}];
		];

		mathBin = 
			Lookup[
				specs,
				"WolframEngineBinary",
				(* if no "WolframEngineBinary" provided, use the session Wolfram Kernel location and set notProvidedQ to True *)
				(notProvidedQ = True; mathBinSession)
			];

		(* check that the Jupyter installation path is a file, and message appropriately *)
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

		{kernelID, displayName} = {"", ""};
		(* if not clearing, check that the Wolfram Engine installation path is a file, and message appropriately *)
		If[
			!(removeQ && removeAllQ),
			If[
				(fileType = FileType[mathBin]) === File,
				(* get the "Kernel ID" and "Display Name" for the new Jupyter kernel *)
				retrievedNames = getNames[mathBin, TrueQ[notProvidedQ]];
				If[FailureQ[retrievedNames], Message[ConfigureJupyter::nolink]; Return[$Failed]];
				{kernelID, displayName} = retrievedNames;
				,
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

		(* list of kernels in Jupyter to perform an action on *)
		wlKernels = {kernelID};
		tempDir = "";
		(* if adding, ...*)
		(* otherwise, when removing or clearing, ...*)
		If[
			!removeQ,

			(* create staging directory for files needed to register a kernel with Jupyter *)
			tempDir = CreateDirectory[
				FileNameJoin[{
					projectHome,
					CreateUUID[],
					kernelID
				}], CreateIntermediateDirectories -> True
			];

			(* export a JSON file to the staging directory that contains all the relevant information on how to run the kernel *)
			Export[
				FileNameJoin[{tempDir, "kernel.json"}], 
				Association[
					"argv" -> {
						mathBin,
						(* TODO: automatically find the kernel script
							(only) if the Wolfram Engine being installed is the same as the one used to execute this command *)
						"-script",
						FileNameJoin[{projectHome, "Resources", "KernelForWolframLanguageForJupyter.wl"}],
						"{connection_file}"
						(* , "-noprompt" *)
					},
					"display_name" -> displayName,
					"language" -> "Wolfram Language"
				]
			];

			(* create a list of arguments that directs Jupyter to install from the staging directory *)
			commandArgs = {jupyterPath, "kernelspec", "install", "--user", tempDir};,
			(* create a list of arguments that directs Jupyter to remove ... *)
			commandArgs = {jupyterPath, "kernelspec", "remove", "-f",
				If[
					!removeAllQ,
					(* just the specified kernel *)
					kernelID,
					(* all Wolfram Language Jupyter kernels *)
					(* select from all kernel IDs in Jupyter those that match the form used by this install *)
					Sequence @@ (wlKernels = Select[getKernels[jupyterPath, processEnvironment], StringMatchQ[#1, (* ("WolframLanguage-" | "wl-") *) "WolframLanguage" ~~ ___, IgnoreCase -> True] &])
				]
			}
		];
		(* if no kernels to act on, quit *)
		If[Length[wlKernels] == 0, Return[];];
		wlKernelsL = ToLowerCase /@ wlKernels;

		(* for error detection, get a snapshot of kernels before the action is performed *)
		kernelspecs = getKernels[jupyterPath, processEnvironment];
		(* when adding, if there is a kernel with the same id already in Jupyter, it will be replaced; thus, message, but continue *)
		If[SubsetQ[kernelspecs, wlKernelsL] && !removeQ, Message[ConfigureJupyter::addconflict]];

		(* perform the action *)
		exitInfo = RunProcess[commandArgs, All, ProcessEnvironment -> processEnvironment];
		(* remove temporary directory if it was created *)
		If[StringLength[tempDir] > 0, DeleteDirectory[DirectoryName[tempDir], DeleteContents -> True]];

		(* get list of kernels after the action was performed *)
		kernelspecs = getKernels[jupyterPath, processEnvironment];
		(* message about success with respect to the action that was performed *)
		If[
			!Xor[removeQ, SubsetQ[kernelspecs, wlKernelsL]],
			WolframLanguageForJupyter`Errors`$ConfigureError = exitInfo["StandardError"];
			Print[WolframLanguageForJupyter`Errors`$ConfigureError];
			If[!removeQ, Message[ConfigureJupyter::notadded];, Message[ConfigureJupyter::notremoved];];
			Return[$Failed];
		];
	];

(* convert options to an Association *)
ConfigureJupyter[
		args___,
		opts:OptionsPattern[] /; Length[{opts}] > 0
	] := ConfigureJupyter[args, Association[opts]];

(* mold ConfigureJupyter arguments to what is expected by the main install function, configureJupyter ... *)
ConfigureJupyter["Add", args___] := ConfigureJupyter["add", args];
ConfigureJupyter["add"] := ConfigureJupyter["add", Association[]];
ConfigureJupyter["add", assoc_Association] := configureJupyter[assoc, False, False];

ConfigureJupyter["Remove", args___] := ConfigureJupyter["remove", args];
ConfigureJupyter["remove"] := ConfigureJupyter["remove", Association[]];
ConfigureJupyter["remove", assoc_Association] := configureJupyter[assoc, True, False];

ConfigureJupyter["Clear", args___] := ConfigureJupyter["clear", args];
ConfigureJupyter["clear"] := ConfigureJupyter["clear", Association[]];
ConfigureJupyter["clear", assoc_Association] := configureJupyter[assoc, True, True];

ConfigureJupyter[sc_String, ___] /; !StringMatchQ[sc, "add" | "remove" | "clear" | "Add" | "Remove" | "Clear"] := Message[ConfigureJupyter::subcommand];
ConfigureJupyter[Except[_String], ___] := Message[ConfigureJupyter::subcommand];
ConfigureJupyter[args___] := Message[ConfigureJupyter::argx, Length[{args}]];

End[];
EndPackage[];
