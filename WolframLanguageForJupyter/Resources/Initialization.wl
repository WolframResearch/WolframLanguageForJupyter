(************************************************
				Initialization.wl
*************************************************
Description:
	Initialization for
		WolframLanguageForJupyter
Symbols defined:
	$defaultPageWidth,
	loopState,
	applyHook,
	$canUseFrontEnd,
	$outputSetToTraditionalForm,
	$outputSetToTeXForm,
	$trueFormatType,
	$truePageWidth,
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
	$defaultPageWidth = 89;
	SetOptions[$Output, PageWidth -> $defaultPageWidth];

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

			(* flag for whether to redirect messages, or to, at the end of the execution of an input,
				bundle them with the result *)
			"redirectMessages" -> True,

			(* flag for if an is_complete_request has ever been sent to the kernel *)
			"isCompleteRequestSent" -> False,

			(* OutputStream for Jupyter's stdout *)
			"WolframLanguageForJupyter-stdout" -> False,

			(* local to an iteration *)
			(* a received frame as an Association *)
			"frameAssoc" -> Null,
			(* type of the reply message frame *)
			"replyMsgType" -> Null,
			(* content of the reply message frame *)
			"replyContent" -> Null,
			(* message relpy frame to send on the IO Publish socket, if it is not Null *)
			"ioPubReplyFrame" -> Null,
			(* the redirect function Print should use *)
			"printFunction" -> False,
			(* flag for if the Jupyter console (if running under a Jupyter console)
				should ask the user if it should exit *)
			"askExit" -> False
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
	$truePageWidth :=
		Replace[
			Lookup[Options[$Output], PageWidth],
			Except[
				Alternatives[
					Infinity,
					pageWidth_ /; ((IntegerQ[pageWidth]) && (pageWidth > 0))
				]
			] ->
				$defaultPageWidth
		];

	(* hard-coded base64 rasterization of $Failed *)
	failedInBase64 = "iVBORw0KGgoAAAANSUhEUgAAADcAAAARCAIAAAD2TKM6AAAAhXpUWHRSYXcgcHJvZmlsZSB0eXBlIGV4aWYAAHjaVYvBDcMwDAP/mqIjyLJM2uMYiQNkg45fuu0n9yApgbT1vi97felp2dgxABc5csRU6P6juNciDXn+X9MfZGWgoRhTluEkhnJXkqKFN+LCAahYcIbnIV8gNQN3o86928QyPusLVffpbh/5eCey76LuBgAAAAlwSFlzAAALEwAACxMBAJqcGAAAADx0RVh0U29mdHdhcmUAQ3JlYXRlZCB3aXRoIHRoZSBXb2xmcmFtIExhbmd1YWdlIDogd3d3LndvbGZyYW0uY29tXKKmhQAAACF0RVh0Q3JlYXRpb24gVGltZQAyMDE5OjA3OjAyIDAzOjExOjExSFD8JQAAA8xJREFUSInVlk9IKl8Ux68ajjThLynMwCKLICSDjBbRxpVSFhFkCtFfBMMKAhcu3GXhQopaJBEFISjhsn9YUGE6zSJqkRURFVpKLQqNcYgZHectpszfez9exoP33u+zmnPm3HO/c865w2XRNA3+eth/WkBOfKjEMAzH8T8o5Sd8qJybm1tcXPz1jCaTyeVyMc8URXV3d2s0Go1GEwgEvppqZGQERdEPldfX1yiKIghydnaWCerp6WFn0dbW9mlekiTdbrfX62VMNpvd398/MDAQCATu7u6+qnJ9fT0SiQAA8gAAJycnzc3NnZ2dfD5foVA4nc7W1lYAQDqd1uv1drudWZOXl/dpXi6XG4lEOBwOY7JYLLVaDQDIz8//qsRs2AAAt9vd2NjodrsXFhYuLy9VKlXmNQRB/7wDwzAAIBaLmc1miUTC5/MHBwfPz8+ZSJvNJpVKpVKpTCazWq2fbkySpMViEYvFAoFAp9M9Pz8z/v39/bq6Oj6fr9FoMueEDQBQKpUoihqNxtvb2+Li4kwlAADxePz6nXQ6DQC4uroKBoPz8/MoimIYZjabmciuri6Hw+FwOMRi8ePj46cqLRaLx+NZXV1dW1u7v78fGxsDAESj0fb29qamJp/PV19f//Ly8hZN0zRN016vVy6Xs9nsoaGh19dXxqnT6QAAzFByOJxYLEa/Q5Lk6enp9PQ0BEE4jtNZ9Pb2Go1G+t9UVla6XK6MmUwmIQja3t5mzI2NjYKCApqm7Xa7RCJJpVKMv6yszOPx0DT9dnpUKtXx8fHW1tbm5ubk5GTmi0dHRymKoigqlUoVFhYCAAiCMBqNQqFweHj48PCQIIh4PP5p5b4jHA4TBNHS0sLlcrlcbkdHB0EQOI7f3NzU1tZmN/Oj4wcHBxiGMVpVKlVm1P6TqakpBEEuLi4QBJmdnc1RFovFoigqY4pEIhaL5ff7SZIkSTKVSpEkCcOwQCAIBoM/LmcDACYmJqqrq/v6+rRarcfjYRr9kzJUVVWJRKJEImGz2XJU2dDQsLu7m06nHx4eAAAwDKvVapPJxExwNBpFEAQAoFAoQqHQ0tISSZIrKytM8JvKnZ0dxoVhmM/n02q1mQL8uJ/BYPD7/UKhsKKioqioCIKgXFTq9fq9vT0YhuVyOVPU5eXlkpKS8vLy0tJSqVTK/POVSuX4+LjBYODxeE6ns6am5m19ZqKtVuvMzAydAwRBhEIhiqJyCc5AUVQoFEomk9lOHMfD4fB3qRKJxNPTU7aHRb/fiY6Ojng8nkwmy7GJv5MPlX8z/4+b2zdkknhkRbjZsAAAAABJRU5ErkJggg==";

	(* obtain details on how to connect to Jupyter, from Jupyter's invocation of "KernelForWolframLanguageForJupyter.wl" *)
	(* NOTE: We remove the extension of the file to be imported so as to avoid accidentally loading the format "MXNet," which has a bad interaction with the paclet SetReplace (https://github.com/maxitg/SetReplace) as of June 22, 2020 *) 
	Block[
		{noExtensionFile},
		noExtensionFile = CopyFile[$CommandLine[[4]], CreateFile[], OverwriteTarget -> True];
		connectionAssoc = ToString /@ Association[Import[noExtensionFile, "JSON"]];
		DeleteFile[noExtensionFile];
	];

	(* warnings to display in kernel information *)
	bannerWarning = 
		If[
			Length[$CommandLine] > 4,
			"\\n\\nNote: This Jupyter kernel was installed through the WolframScript install method. Accordingly, updates to a WolframLanguageForJupyter paclet will not affect this kernel.",
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

	Block[
		{
			(* for storing the result of defining a new OutputStream method *)
			customOutputStreamMethod
		},

		(* define an OutputStream method that will allow writing to Jupyter's stdout *)
		customOutputStreamMethod =
			DefineOutputStreamMethod[
				"for-WolframLanguageForJupyter-stdout",
				{
					"ConstructorFunction" -> Function[{name, isAppend, caller, opts}, {True, {}}],
					"WriteFunction" -> 
						Function[
							{state, bytes},
							If[
								loopState["frameAssoc"] =!= Null,
								redirectPrint[loopState["frameAssoc"], FromCharacterCode[bytes]];
							];
							{Length[bytes], {}}
						]
				}
			];

		(* if defining a new OutputStream method did not fail,
			open an OutputStream using the new method, and store it in loopState *)
		If[
			!FailureQ[customOutputStreamMethod],
			loopState["WolframLanguageForJupyter-stdout"] =
				OpenWrite["WolframLanguageForJupyter-stdout", Method -> "for-WolframLanguageForJupyter-stdout"];
			(* -- also, if opening the OutputStream failed, reset loopState["WolframLanguageForJupyter-stdout"] back to False *)
			If[
				FailureQ[loopState["WolframLanguageForJupyter-stdout"]],
				loopState["WolframLanguageForJupyter-stdout"] = False;
			];
		];
	];

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
