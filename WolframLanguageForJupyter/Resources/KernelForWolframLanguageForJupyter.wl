(* Do not output messages to the jupyter notebook invocation: *)
$Messages = {};
$Output = {};

Get["ZeroMQLink`"];

(* START: Cloud Interaction Functions *)

InteractQ[expr_] := MatchQ[expr, Hold[Interact[___]]];
Uninteract[Interact[expr___]] ^:= WolframLanguageForJupyter`Private`jupEval[expr];

SetAttributes[Interact, HoldAll];
Uninteract[expr___] := WolframLanguageForJupyter`Private`jupEval[expr];

SetAttributes[Uninteract, HoldAll];

(* END: Cloud Interaction Functions *)

Begin["WolframLanguageForJupyter`Private`"];

(* START: Evaluation Helper Functions *)

toOutText[output_] := 
	StringJoin[
		"<pre style=\"",
		StringJoin[{"&#", ToString[#1], ";"} & /@ ToCharacterCode["font-family: \"Courier New\",Courier,monospace;", "UTF-8"]], 
		"\">",
		(* the OutputForm (which ToString uses) of any expressions wrapped with, say, InputForm should
			be identical to the string result of an InputForm-wrapped expression itself *)
		StringJoin[{"&#", ToString[#1], ";"} & /@ ToCharacterCode[ToString[output], "UTF-8"]],
		"</pre>"
	];

toOutImage[output_] := 
	StringJoin[
		"<img alt=\"Output\" src=\"data:image/png;base64,",
		BaseEncode[
			ExportByteArray[
				If[Head[output] === Manipulate, output, Rasterize[output]],
				"PNG"
			]
		],
		"\">"
	];

textQ[expr_] := Module[{exprHead, pObjects}, 
	(* if the expression is wrapped with InputForm or OutputForm, automatically format as text *)
	exprHead = Head[expr];
	If[exprHead === InputForm || exprHead === OutputForm,
		Return[True]
	];

	pObjects = 
		GroupBy[
			Complement[
				Quiet[Cases[
					expr, 
					elem_ /; (Depth[elem] == 1), 
					{0, Infinity}, 
					Heads -> True
				]],
				{List, Association}
			],
			Head
		];

	If[
		ContainsOnly[Keys[pObjects], {Integer, Real, String}],
		Return[True];
   	];

	If[
		ContainsOnly[Keys[pObjects], {Integer, Real, String, Symbol}],
   		Return[
   			AllTrue[
   				pObjects[Symbol], 
   				(ToString[Definition[#1]] === "Null") &
   			]
   		];
   	];

   	Return[False];
];

(* review other methods: through EvaluationData or WSTP so we don't redefine Print and use a global *)
Unprotect[Print];
Print[args___, opts:OptionsPattern[]] :=
	Block[
		{
			$inPrint=True,
			$Output={OpenWrite[FormatType->StandardForm]}
		},
		If[
			!FailureQ[First[$Output]] && Length[WolframLanguageForJupyter`Private`srm] > 0,
				Print[args, opts];
				WolframLanguageForJupyter`Private`printFunction[
					Import[$Output[[1,1]], "String"]
				];
		];
		Close[First[$Output]];
		Null
	] /; !TrueQ[$inPrint];
Protect[Print];

(* other method: evaluate input through WSTP in another kernel *)
jupEval[expr___] := Module[{$oldMessages, stream, msgs, eval, evalExpr},
	eval = Association[];
	$oldMessages = $Messages;
	stream = OpenWrite[];
	Unprotect[$MessageList]; $MessageList = {}; Protect[$MessageList];
	$Messages = {stream};
	evalExpr = {expr};
	$Messages = $oldMessages;
	msgs = Import[stream[[1]], "String"];
	Close[stream];
	AssociateTo[eval, {"res" -> evalExpr, "msgs" -> msgs}];
	Return[eval];
];
SetAttributes[jupEval, HoldAll];

(* END: Evaluation Helper Functions *)

(* START: Messaging Helper Functions *)

If[TrueQ[$VersionNumber < 12.0],
	Options[socketWriteFunction] = {"Asynchronous"->False,"Multipart"->False};
	socketWriteFunction[sock_, data_List, opts:OptionsPattern[]] := ZeroMQLink`Private`ZMQWriteInternal[sock, data, opts];
	socketWriteFunction[sock_, data_ByteArray, rest___]:= socketWriteFunction[sock, Normal[data], rest]
	,
	socketWriteFunction = ZeroMQLink`ZMQSocketWriteMessage
];

sendFrame[socket_, frame_Association] := Module[{},

	socketWriteFunction[
		socket, 
		frame["ident"],
		"Multipart" -> True
	];

	socketWriteFunction[
		socket, 
		StringToByteArray[#1],
		"Multipart" -> True
	]& /@ Lookup[frame, {"idsmsg", "signature", "header", "pheader", "metadata"}];

	socketWriteFunction[
		socket, 
		StringToByteArray[frame["content"]],
		"Multipart" -> False
	];
];

(* cryptographic helper function *)
(* adapted from wikipedia article on HMAC's definition *)
hmac[key_String, message_String] :=
  Module[
   {
    method, blockSize, outputSize,
    baKey, baMessage,
    baKeyPrime,
    keyPrime,
    baOPadded, baIPadded
    },
   
   method = "SHA256";
   blockSize = 64;
   outputSize = 32;
   
   baKey = StringToByteArray[key];
   baMessage = StringToByteArray[message];
   
   If[Length[baKey] > blockSize,
    baKeyPrime = Hash[baKey, method, "ByteArray"];
    ];
   
   If[Length[baKey] < blockSize,
    baKeyPrime = Join[
       baKey,
       ByteArray[
        Table[0, {blockSize - Length[baKey]}]
        ]
       ];
    ];
   
   keyPrime = Normal[baKeyPrime];
   
   baOPadded = ByteArray[BitXor[#1, 92] & /@ Normal[keyPrime]];
   baIPadded = ByteArray[BitXor[#1, 54] & /@ Normal[keyPrime]];
   
   Hash[Join[
     baOPadded,
     Hash[
      Join[
       baIPadded,
       baMessage
       ], method, "ByteArray"
      ]
     ], method, "HexString"]
   ];

getFrameAssoc[frame_Association, replyType_String, replyContent_String, branchOff:(True|False)] := Module[{res = Association[], header, content},
	header = frame["header"];
	content = frame["content"];

	AssociateTo[res, {"header" -> Association[ImportByteArray[StringToByteArray[header], "JSON"]], "content" -> Association[ImportByteArray[StringToByteArray[content], "JSON"]]}];
	AssociateTo[
		res, 
		"replyMsg" -> 
			Association[
				"ident" -> If[KeyExistsQ[frame, "ident"], frame["ident"], ByteArray[{0, 0, 0, 0, 0}]],
				"idsmsg" -> "<IDS|MSG>",
				"header" -> ExportString[Append[res["header"], {"date" -> DateString["ISODateTime"], "msg_type" -> replyType, "msg_id" -> StringInsert[StringReplace[CreateUUID[], "-" -> ""], "-", 9]}], "JSON", "Compact" -> True],
				"pheader" -> If[branchOff, "{}", header],
				"metadata" -> ExportString[
								{"text/html" -> {}},
								"JSON",
								"Compact" -> True
							],
				"content" -> replyContent
			]
	];
	AssociateTo[
		res["replyMsg"],
		"signature" -> 
			hmac[
				keyString, 
				StringJoin[
					res["replyMsg"]["header"],
					res["replyMsg"]["pheader"],
					res["replyMsg"]["metadata"],
					res["replyMsg"]["content"]
				]
			]
	];

	Return[res];
];

getFrameAssoc[baFrame_ByteArray, replyType_String, replyContent_String, branchOff:(True|False)] := Module[{frameStr, res = Association[], identLen, header, pheader, metadata, content},
	frameStr = Quiet[ByteArrayToString[baFrame]];

	{identLen, header, pheader, metadata, content} = First[StringCases[frameStr,
			ident1___ ~~ "<IDS|MSG>" ~~ ___ ~~ "{" ~~ json1___ ~~ "}" ~~ "{" ~~ json2___ ~~ "}" ~~ "{" ~~ json3___ ~~ "}" ~~ "{" ~~ json4___ ~~ "}" ~~ EndOfString :> 
				Prepend[(StringJoin["{",#1,"}"] &) /@ {json1,json2,json3,json4}, StringLength[ident1]]
		]];

	Return[
		getFrameAssoc[
			Association["ident" -> baFrame[[;;identLen]], "header" -> header, "content" -> content],
			replyType,
			replyContent,
			branchOff
		]
	];
];

(* END: Messaging Helper Functions *)

connectionAssoc = ToString /@ Association[Import[$CommandLine[[4]], "JSON"]];

keyString = connectionAssoc["key"];

baseString = StringJoin[connectionAssoc["transport"], "://", connectionAssoc["ip"], ":"];

heartbeatString = StringJoin[baseString, connectionAssoc["hb_port"]];
ioPubString = StringJoin[baseString, connectionAssoc["iopub_port"]];
controlString = StringJoin[baseString, connectionAssoc["control_port"]];
inputString = StringJoin[baseString, connectionAssoc["stdin_port"]];
shellString = StringJoin[baseString, connectionAssoc["shell_port"]];

errorCode = 0;

prs = {};

(* start heartbeat thread *)
heldLocalSubmit = Replace[Hold[LocalSubmit[
	Get["ZeroMQLink`"];
	heartbeatSocket = SocketOpen[placeholder1, "ZMQ_REP"];
	If[
		FailureQ[heartbeatSocket],
		Quit[];
	];
	While[
		True,
		SocketWaitNext[{heartbeatSocket}];
		heartbeatRecv = SocketReadMessage[heartbeatSocket];
		If[
			FailureQ[heartbeatRecv],
			Continue[];
		];
		socketWriteFunction[
			heartbeatSocket, 
			heartbeatRecv,
			"Multipart" -> False
		];
	];,
	HandlerFunctions-> Association["TaskFinished" -> Quit]
]], placeholder1 -> heartbeatString, Infinity];
Quiet[ReleaseHold[heldLocalSubmit]];

ioPubSocket = SocketOpen[ioPubString, "ZMQ_PUB"];
controlSocket = SocketOpen[controlString, "ZMQ_ROUTER"];
inputSocket = SocketOpen[inputString, "ZMQ_ROUTER"];
shellSocket = SocketOpen[shellString, "ZMQ_ROUTER"];

If[FailureQ[ioPubSocket] || FailureQ[controlSocket] || FailureQ[inputSocket] || FailureQ[shellSocket],
	Quit[];
];

executeRequestHandler[srm_, frameAssoc_, executionCount_Integer] :=
	Module[
		{
			(* messaging *)
			replyMsgType, replyContent,
			ioPubReplyContent, ioPubReplyFrame,
			(* evaluation *)
			$jupResEval, $res, $msgs,
			toOut, errorMessage
		},
		replyMsgType = "execute_reply";
		replyContent = ExportString[Association["status" -> "ok", "execution_count" -> executionCount, "user_expressions" -> {}], "JSON", "Compact" -> True];

		(* print function is global under jupyterEvaluationLoop's block *)
		printFunction = (sendFrame[
			ioPubSocket,
			getFrameAssoc[
					srm,
					"stream",
					ExportString[
						Association[
								"name" -> "stdout",
								"text" -> #1
						], "JSON", "Compact" -> True
					]
					,
					False
			]["replyMsg"]
		]&);

		$jupResEval = ToExpression[frameAssoc["content"]["code"], InputForm, Uninteract];
		$res = $jupResEval["res"];
		$msgs = $jupResEval["msgs"];
		
		printFunction = Function[#;];

		If[FailureQ[$jupResEval],
			$res = $Failed;
			$msgs = jupEval[ToExpression[frameAssoc["content"]["code"], InputForm]]["msgs"];
		];

		(* format output as purely text, image, or cloud interface *)
		If[TrueQ[InteractQ[ToExpression[frameAssoc["content"]["code"], InputForm, Hold]]] && $CloudConnected,
			ioPubReplyContent = ExportString[
									Association[
										"execution_count" -> executionCount,
										"data" -> {"text/html" -> StringJoin[
																	"<div><img alt=\"\" src=\"data:image/png;base64,", 
																	BaseEncode[ExportByteArray[Rasterize[Style[$msgs, Darker[Red]]], "PNG"]],
																	"\">",
																	EmbedCode[CloudDeploy[$res], "HTML"][[1]]["CodeSection"]["Content"],
																	"</div>"
																]
													},
										"metadata" -> {"text/html" -> {}}
									],
									"JSON",
									"Compact" -> True
								];
			,
			(* if every output expression can be formatted as text, format as text *)
			(* TODO: allow for mixing text and image results *)
			If[AllTrue[$res, textQ],
				toOut = toOutText,
				toOut = toOutImage
			];
			errorMessage = 	If[StringLength[$msgs] == 0,
				{},
				{
					"<pre style=\"",
					StringJoin[{"&#",ToString[#1], ";"} & /@ ToCharacterCode["color:red; font-family: \"Courier New\",Courier,monospace;", "UTF-8"]], 
					"\">",
					StringJoin[{"&#", ToString[#1], ";"} & /@ ToCharacterCode[$msgs, "UTF-8"]],
					"</pre>"
				}
			];
			ioPubReplyContent = ExportString[
				Association[
					"execution_count" -> executionCount, 
					"data" -> {"text/html" -> 
											If[
												Length[$res] > 1,
												StringJoin[
													"<style>
														.grid-container {
															display: inline-grid;
															grid-template-columns: auto auto;
														}
													</style>

													<div>",
													errorMessage,
													"<div class=\"grid-container\">",
													Table[
														{
															"<div class=\"grid-item\">
																<div class=\"prompt output_prompt\" style=\"text-align:left;padding:0em;padding-right:20px;line-height:20px;\">
																	Out[", ToString[outIndex],"/", ToString[Length[$res]], "]:
																</div>
															</div>
															<div class=\"grid-item\">",
															toOut[$res[[outIndex]]],
															"</div>"
														},
														{outIndex, 1, Length[$res]}
													],
													"</div></div>"
												],
												StringJoin[
													"<div>",
													errorMessage,
													toOut[First[$res]],
													"</div>"
												]
											]
								},
					"metadata" -> {"text/html" -> {}}
				],
				"JSON",
				"Compact" -> True
			];
		];

		ioPubReplyFrame = getFrameAssoc[srm, "execute_result", ioPubReplyContent, False];
		
		Return[{replyMsgType, replyContent, ioPubReplyFrame}];
	];

jupyterEvaluationLoop[] :=
	Block[{srm = ByteArray[{}], printFunction = Function[#;]},
		Module[
			{
				(* messaging *)
				frameAssoc,
				replyMsgType, replyContent, ioPubReplyContent,
				statReplyFrame, shellReplyFrame, ioPubReplyFrame,
				
				(* evaluation *)
				executionCount, 
				
				doShutdown
			},

			executionCount = 1;

			ioPubReplyFrame = Association[];

			doShutdown = False;

			While[
				True,
				Switch[
					First[SocketWaitNext[{shellSocket}]], 
					shellSocket, 
					srm = SocketReadMessage[shellSocket, "Multipart" -> True];
					frameAssoc = getFrameAssoc[srm, "", "{}", False];
					Switch[
						frameAssoc["header"]["msg_type"], 
						"kernel_info_request",
						replyMsgType = "kernel_info_reply";
						replyContent = "{\"protocol_version\": \"5.3.0\",\"implementation\": \"WolframLanguageForJupyter\",\"implementation_version\": \"0.0.1\",\"language_info\": {\"name\": \"Wolfram Language\",\"version\": \"12.0\",\"mimetype\": \"application/vnd.wolfram.m\",\"file_extension\": \".m\",\"pygments_lexer\": \"python\",\"codemirror_mode\": \"python\"},\"banner\" : \"Wolfram Language/Wolfram Engine Copyright 2019\"}";,
						"is_complete_request",
						(* Add syntax-Q checking *)
						replyMsgType = "is_complete_reply";
						replyContent = "{\"status\":\"unknown\"}";,
						"execute_request",
						{replyMsgType, replyContent, ioPubReplyFrame} = executeRequestHandler[srm, frameAssoc, executionCount];
						executionCount++;,
						"shutdown_request",
						replyMsgType = "shutdown_reply";
						replyContent = "{\"restart\":false}";
						doShutdown = True;,
						_,
						Continue[];
					];
					statReplyFrame = getFrameAssoc[srm, "status", "{\"execution_state\":\"busy\"}", True]["replyMsg"];
					sendFrame[ioPubSocket, statReplyFrame];

					shellReplyFrame = getFrameAssoc[srm, replyMsgType, replyContent, False];
					sendFrame[shellSocket, shellReplyFrame["replyMsg"]];

					If[!(ioPubReplyFrame === Association[]),
						sendFrame[ioPubSocket, ioPubReplyFrame["replyMsg"]];
						ioPubReplyFrame = Association[];
					];

					sendFrame[ioPubSocket, getFrameAssoc[statReplyFrame, "status", "{\"execution_state\":\"idle\"}", False]["replyMsg"]];

					If[doShutdown, Quit[];];
					,
					_,
					Continue[];
				];
			];
		];
	];

End[];

(* This setup does not preclude dynamics or widgets. *)

WolframLanguageForJupyter`Private`jupyterEvaluationLoop[];
