(************************************************
				Initialization.wl
*************************************************
Description:
	Initialization for
		WolframLanguageForJupyter
Symbols defined:
	loopState,
	applyHook,
	$canUseFrontEnd,
	$outputSetToTraditionalForm,
	$outputSetToTeXForm,
	$trueFormatType,
	connectionAssoc,
	bannerWarning,
	keyString,
	baseString,
	heartbeatString,
	verticalEllipsis,
	unicodeNamedCharactersReplacements,
	ioPubString,
	controlString,
	inputString,
	shellString,
	ioPubSocket,
	controlSocket,
	inputSocket,
	shellSocket,
	heldLocalSubmit
*************************************************)

(************************************
	Get[] guard
*************************************)

If[
	!TrueQ[WolframLanguageForJupyter`Private`$GotInitialization],
	
	WolframLanguageForJupyter`Private`$GotInitialization = True;

(************************************
	get required paclets
*************************************)

	(* obtain ZMQ utilities *)
	Needs["ZeroMQLink`"]; (* SocketOpen *)

(************************************
	private symbols
*************************************)

	(* begin the private context for WolframLanguageForJupyter *)
	Begin["`Private`"];

(************************************
	set noise settings
*************************************)

	(* make Short[] work *)
	SetOptions[$Output, PageWidth -> 89];

(* 	do not output messages to the jupyter notebook invocation
	$Messages = {};
	$Output = {}; *)

(************************************
	discover the named unicode
		characters and their names
*************************************)

	(* the vertical ellipsis character *)
	verticalEllipsis = FromCharacterCode[8942, "Unicode"];

	(* pre-define the association of names and named characters as empty *)
	unicodeNamedCharactersReplacements = Association[];

	Block[
		{
			(* the absolute file name for "UnicodeCharacters.tr" *)
			unicodeCharactersTRFileName,

			(* raw data extracted from "UnicodeCharacters.tr" *)
			charactersAndTheirNames
		},

		(* attempt to get the full location of "UnicodeCharacters.tr" *)
		unicodeCharactersTRFileName = UsingFrontEnd[System`Dump`unicodeCharactersTR];

		(* try again if using System`Dump`unicodeCharactersTR does not work *)
		If[
			!StringQ[unicodeCharactersTRFileName],
			unicodeCharactersTRFileName =
				UsingFrontEnd[
					ToFileName[
						FrontEnd`FileName[
							{
								$InstallationDirectory,
								"SystemFiles",
								"FrontEnd",
								"TextResources"
							},
							"UnicodeCharacters.tr"
						]
					]
				];
		];

		If[
			StringQ[unicodeCharactersTRFileName],
			charactersAndTheirNames =
				(
					(* parse the third item of a row (for a named character) into a list *)
					ReplacePart[
						#1,
						3 ->
							StringCases[
								(* remove extraneous parentheses *)
								StringTrim[
									#1[[3]],
									"(" | ")"
								],
								(* extract the escape sequences for the named character *)
								Longest[escSeq : (Except[WhitespaceCharacter] ..)] :>
									StringJoin[verticalEllipsis, StringTrim[escSeq, "$"], verticalEllipsis]
							]
					] &
				) /@
					(* only use lists with at least three items *)
					Select[
						(* parse the rows into their items (where each item is separated by two tabs) *)
						(StringSplit[#1, "\t\t"] &) /@
							(
								(* split unicodeCharactersTRFileName into its lines *)
								StringSplit[
									Import[unicodeCharactersTRFileName, "String"],
									"\n"
								(* drop the first row *)
								][[2 ;;]]
							),
						(Length[#1] >= 3) &
					];

			(* parse the data into an association of names and the named characters they correspond to *)
			unicodeNamedCharactersReplacements =
				(* sort the keys by string length *)
				KeySort[
					(* sort the keys in the default manner *)
					KeySort[
						(* drop "empty names" *)
						KeyDrop[
							(* make an association *)
							Association[
								(* create a list of rules of names and named characters *)
								Thread[
									Rule[
										(Prepend[#1[[3]], #1[[2]]]),
										FromCharacterCode[FromDigits[StringDrop[#1[[1]], 2], 16], "Unicode"]
									]
								] & /@ charactersAndTheirNames
							],
							{
								StringJoin[Table[verticalEllipsis, {2}]],
								"\\[]"
							}
						]
					],
					(StringLength[#1] < StringLength[#2]) &
				];
		];
	];

(************************************
	various important symbols
		for use by
		WolframLanguageForJupyter
*************************************)

	(* create an association for maintaining state in the evaluation loop *)
	loopState = 
		(* schema for the Association *)
		Association[
			(* index for the execution of the next input *)
			"executionCount" -> 1,

			(* flag for if WolframLanguageForJupyter should shut down *)
			"doShutdown" -> False,

			(* local to an iteration *)
			(* a received frame as an Association *)
			"frameAssoc" -> Null,
			(* type of the reply message frame *)
			"replyMsgType" -> Null,
			(* content of the reply message frame *)
			"replyContent" -> Null,
			(* message relpy frame to send on the IO Publish socket, if it is not Null *)
			"ioPubReplyFrame" -> Null,
			(* the function Print should use *)
			"printFunction" -> Function[#;]
		];

	(* helper utility for applying hooks if they are set *)
	applyHook[hook_, value_] /; Length[OwnValues[hook]] != 0 := hook[value];
	applyHook[hook_, value_] := value;
	Attributes[applyHook] := HoldAll;

	(* can we use the Front End? *)
	$canUseFrontEnd := (UsingFrontEnd[$FrontEnd] =!= Null);

	$outputSetToTraditionalForm := (Lookup[Options[$Output], FormatType] === TraditionalForm);
	$outputSetToTeXForm := (Lookup[Options[$Output], FormatType] === TeXForm);
	$trueFormatType :=
		If[
			$outputSetToTraditionalForm,
			TraditionalForm,
			If[$outputSetToTeXForm, TeXForm, #&]
		];

	(* obtain details on how to connect to Jupyter, from Jupyter's invocation of "KernelForWolframLanguageForJupyter.wl" *)
	connectionAssoc = ToString /@ Association[Import[$CommandLine[[4]], "JSON"]];

	(* warnings to display in kernel information *)
	bannerWarning = 
		If[
			Length[$CommandLine] > 4,
			"\\n\\nThis Jupyter kernel was installed through the WolframLanguageForJupyter WolframScript script install option. Accordingly, updates to a WolframLanguageForJupyter paclet installed to a Wolfram Engine will not propagate to this installation.",
			""
		];

	(* key for generating signatures for reply message frames *)
	keyString = connectionAssoc["key"];

	(* base string using protocol and IP address from Jupyter *)
	baseString = StringJoin[connectionAssoc["transport"], "://", connectionAssoc["ip"], ":"];

	(* see https://jupyter-client.readthedocs.io/en/stable/messaging.html for what the following correspond to *)
	heartbeatString = StringJoin[baseString, connectionAssoc["hb_port"]];
	ioPubString = StringJoin[baseString, connectionAssoc["iopub_port"]];
	controlString = StringJoin[baseString, connectionAssoc["control_port"]];
	inputString = StringJoin[baseString, connectionAssoc["stdin_port"]];
	shellString = StringJoin[baseString, connectionAssoc["shell_port"]];

(************************************
	open all the non-heartbeat
		sockets
*************************************)

	(* open sockets using the set strings from above *)
	ioPubSocket = SocketOpen[ioPubString, "ZMQ_PUB"];
	controlSocket = SocketOpen[controlString, "ZMQ_ROUTER"];
	inputSocket = SocketOpen[inputString, "ZMQ_ROUTER"];
	shellSocket = SocketOpen[shellString, "ZMQ_ROUTER"];

	(* check for any problems *)
	If[FailureQ[ioPubSocket] || FailureQ[controlSocket] || FailureQ[inputSocket] || FailureQ[shellSocket],
		Quit[];
	];

(************************************
	spin off a new kernel
		that nullifies Jupyter's
		requirement for looping
		back arrving "heartbeats"
*************************************)

	(* start heartbeat thread *)
	(* see https://jupyter-client.readthedocs.io/en/stable/messaging.html#heartbeat-for-kernels *)
	heldLocalSubmit =
		Replace[
			Hold[
				(* submit a task for the new kernel *)
				LocalSubmit[
					(* get required ZMQ utilities in the new kernel *)
					Get["ZeroMQLink`"];
					(* open the heartbeat socket -- inserted with Replace and a placeholder *)
					heartbeatSocket = SocketOpen[placeholder1, "ZMQ_REP"];
					(* check for any problems *)
					If[
						FailureQ[heartbeatSocket],
						Quit[];
					];
					(* do this "forever" *)
					While[
						True,
						(* wait for new data on the heartbeat socket *)
						SocketWaitNext[{heartbeatSocket}];
						(* receive the data *)
						heartbeatRecv = SocketReadMessage[heartbeatSocket];
						(* check for any problems *)
						If[
							FailureQ[heartbeatRecv],
							Continue[];
						];
						(* and loop the data back to Jupyter *)
						socketWriteFunction[
							heartbeatSocket, 
							heartbeatRecv,
							"Multipart" -> False
						];
					];,
					HandlerFunctions-> Association["TaskFinished" -> Quit]
				]
			],
			(* see above *)
			placeholder1 -> heartbeatString,
			Infinity
		];
	(* start the heartbeat thread *)
	(* Quiet[ReleaseHold[heldLocalSubmit]]; *)

	(* end the private context for WolframLanguageForJupyter *)
	End[]; (* `Private`` *)

(************************************
	Get[] guard
*************************************)

] (* WolframLanguageForJupyter`Private`$GotInitialization *)
