Get["ZeroMQLink`"];

(* START: Cloud Interaction Functions *)

InteractQ[expr_] := MatchQ[expr, Hold[Interact[___]]];
Uninteract[Interact[expr___]] ^:= JupyterKernel`Private`jupEval[expr];

SetAttributes[Interact, HoldAll];
Uninteract[expr___] := JupyterKernel`Private`jupEval[expr];

SetAttributes[Uninteract, HoldAll];

(* END: Cloud Interaction Functions *)

Begin["JupyterKernel`Private`"];

(* START: Evaluation Helper Functions *)

doText[expr_] := Module[{pObjects}, 
	pObjects = 
		KeyDrop[GroupBy[
			DeleteCases[
				Cases[
					expr, 
					_?AtomQ, 
					{0, Infinity}, 
					Heads -> True
				],
				List | Association
			],
			Head
		], {Integer, String, Real}];
	If[
		Keys[pObjects] === {Symbol}, 
   		Return[
   			AllTrue[pObjects[Symbol], ToString[""] === "Null" &]
   		];, 
   		Return[Length[pObjects] == 0];
   	];
];

jupEval[expr_] := Module[{$oldMessages, stream, msgs, eval, evalExpr},
	eval = Association[];
	$oldMessages = $Messages;
	stream = OpenWrite[];
	Unprotect[$MessageList]; $MessageList = {}; Protect[$MessageList];
	$Messages = Append[$Messages, stream];
	evalExpr = expr;
	$Messages = $oldMessages;
	msgs = Import[stream[[1]], "String"];
	Close[stream];
	AssociateTo[eval, "res" -> evalExpr];
	AssociateTo[eval, "msgs" -> msgs];
	Return[eval];
];
SetAttributes[jupEval, HoldAll];

(* END: Evaluation Helper Functions *)

sendFrame[socket_, frame_Association] := Module[{},

	ZMQSocketWriteMessage[
		socket, 
		StringToByteArray[#1],
		"Multipart" -> True
	]& /@ Lookup[frame, {"uuid", "idsmsg", "signature", "header", "pheader", "metadata"}];

	ZMQSocketWriteMessage[
		socket, 
		StringToByteArray[frame["content"]],
		"Multipart" -> False
	];
];

getFrameAssoc[frame_Association, replyType_String, replyContent_String, branchOff:(True|False)] := Module[{res = Association[], header, content},
	header = frame["header"];
	content = frame["content"];

	AssociateTo[res, {"header" -> Association[ImportString[header, "JSON"]], "content" -> Association[ImportString[content, "JSON"]]}];
	AssociateTo[
		res, 
		"replyMsg" -> 
			Association[
				"uuid" -> res["header"]["session"],
				"idsmsg" -> "<IDS|MSG>",
				"signature" -> "sig",
				"header" -> ExportString[Append[res["header"], {"date" -> DateString["ISODateTime"], "msg_type" -> replyType, "msg_id" -> StringInsert[StringReplace[CreateUUID[], "-" -> ""], "-", 9]}], "JSON", "Compact" -> True],
				"pheader" -> If[branchOff, "{}", header],
				"metadata" -> "{}",
				"content" -> replyContent
			]
	];

	Return[res];
];

getFrameAssoc[baFrame_ByteArray, replyType_String, replyContent_String, branchOff:(True|False)] := Module[{frameStr, res = Association[], header, pheader, metadata, content},
	frameStr = Quiet[ByteArrayToString[baFrame]];

	{header, pheader, metadata, content} = First[StringCases[frameStr,
			"<IDS|MSG>" ~~ "{" ~~ json1___ ~~ "}" ~~ "{" ~~ json2___ ~~ "}" ~~ "{" ~~ json3___ ~~ "}" ~~ "{" ~~ json4___ ~~ "}" ~~ EndOfString :> 
				(StringJoin["{",#1,"}"] &) /@ {json1,json2,json3,json4}
		]];

	Return[
		getFrameAssoc[
			Association["header" -> header, "content" -> content],
			replyType,
			replyContent,
			branchOff
		]
	];
];

connectionAssoc = ToString /@ Association[Import[$CommandLine[[4]], "JSON"]];

baseString = StringJoin[connectionAssoc["transport"], "://", connectionAssoc["ip"], ":"];

heartbeatString = StringJoin[baseString, connectionAssoc["hb_port"]];
ioPubString = StringJoin[baseString, connectionAssoc["iopub_port"]];
controlString = StringJoin[baseString, connectionAssoc["control_port"]];
inputString = StringJoin[baseString, connectionAssoc["stdin_port"]];
shellString = StringJoin[baseString, connectionAssoc["shell_port"]];

errorCode = 0;

prs = {};

heldLocalSubmit = Replace[Hold[LocalSubmit[
	Get["ZeroMQLink`"];
	heartbeatSocket = SocketOpen[placeholder1, "ZMQ_REP"];
	If[
		FailureQ[heartbeatSocket],
		Quit[];
	];
	While[
		True,
		heartbeatRecv = SocketReadMessage[heartbeatSocket];
		If[
			FailureQ[heartbeatRecv],
			Continue[];
		];
		ZMQSocketWriteMessage[
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
			replyContent = "{\"protocol_version\": \"5.3.0\",\"implementation\": \"WL\"}";,
			"is_complete_request",
			(* Add syntax-Q checking *)
			replyMsgType = "is_complete_reply";
			replyContent = "{\"status\":\"unknown\"}";,
			"execute_request",

			replyMsgType = "execute_reply";
			replyContent = ExportString[Association["status" -> "ok", "execution_count" -> executionCount, "user_expressions" -> {}], "JSON", "Compact" -> True];

			$jupResEval = ToExpression[frameAssoc["content"]["code"], InputForm, Uninteract];
			$res = $jupResEval["res"];
			$msgs = $jupResEval["msgs"];
			If[FailureQ[$jupResEval],
				$res=$Failed;
				$msgs=jupEval[ToExpression[frameAssoc["content"]["code"], InputForm]]["msgs"];
			];

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
											"metadata" -> {}
										],
										"JSON",
										"Compact" -> True
									];
				,
				If[doText[$res],
					ioPubReplyContent = ExportString[
						Association[
							"execution_count" -> executionCount, 
							"data" -> {"text/html" -> StringJoin[
														"<div>",
														If[StringLength[$msgs] == 0,
															{},
															{
																"<pre style=\"",
																StringJoin[{"&#",ToString[#1], ";"} & /@ ToCharacterCode["color:red; font-family: \"Courier New\",Courier,monospace;"]], 
																"\">",
																StringJoin[{"&#", ToString[#1], ";"} & /@ ToCharacterCode[$msgs]],
																"</pre>"
															}
														],
														"<pre style=\"",
														StringJoin[{"&#",ToString[#1], ";"} & /@ ToCharacterCode["font-family: \"Courier New\",Courier,monospace;"]], 
														"\">",
														StringJoin[{"&#", ToString[#1], ";"} & /@ ToCharacterCode[ToString[$res]]],
														"</pre></div>"
													]
										},
							"metadata" -> {}
						],
						"JSON",
						"Compact" -> True
					];,
					ioPubReplyContent = ExportString[
						Association[
							"execution_count" -> executionCount,
							"data" -> {"text/html" -> StringJoin[
														"<div>",
														Sequence @@ If[StringLength[$msgs] == 0,
															{},
															{
																"<pre style=\"",
																StringJoin[{"&#",ToString[#1], ";"} & /@ ToCharacterCode["color:red; font-family: \"Courier New\",Courier,monospace;"]], 
																"\">",
																StringJoin[{"&#", ToString[#1], ";"} & /@ ToCharacterCode[$msgs]],
																"</pre>"
															}
														],
														"<img alt=\"Output\" src=\"data:image/png;base64,",
														BaseEncode[
															ExportByteArray[
																If[Head[$res] === Manipulate, $res, Rasterize[$res]],
																"PNG"
															]
														],
														"\"></div>"
													]
										},
							"metadata" -> {}
						],
						"JSON",
						"Compact" -> True
					];
				];
			];

			ioPubReplyFrame = getFrameAssoc[srm, "execute_result", ioPubReplyContent, False];

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

End[];