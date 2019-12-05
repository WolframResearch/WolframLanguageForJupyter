(************************************************
				KernelForWolframLanguage-
					ForJupyter.wl
*************************************************
Description:
	Entry point for WolframLanguageForJupyter
		kernels started by Jupyter
Symbols defined:
	loop
*************************************************)

(************************************
	begin the
		WolframLanguageForJupyter
		package
*************************************)

(* begin the WolframLanguageForJupyter package *)
BeginPackage["WolframLanguageForJupyter`"];

(************************************
	get required paclets
*************************************)

(* obtain ZMQ utilities *)
Needs["ZeroMQLink`"]; (* SocketReadMessage *)

(************************************
	load required
		WolframLanguageForJupyter
		files
*************************************)

Get[FileNameJoin[{DirectoryName[$InputFileName], "Initialization.wl"}]]; (* initialize WolframLanguageForJupyter; loopState, bannerWarning, shellSocket, controlSocket, ioPubSocket *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "SocketUtilities.wl"}]]; (* sendFrame *)
Get[FileNameJoin[{DirectoryName[$InputFileName], "MessagingUtilities.wl"}]]; (* getFrameAssoc, createReplyFrame *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "RequestHandlers.wl"}]]; (* isCompleteRequestHandler, executeRequestHandler, completeRequestHandler *)

(************************************
	private symbols
*************************************)

(* begin the private context for WolframLanguageForJupyter *)
Begin["`Private`"];

(* define the evaluation loop *)
loop[] := 
	Module[
		{
			(* the socket that has become ready *)
			readySocket,

			(* the raw byte array frame received through SocketReadMessage *)
			rawFrame,

			(* a frame for sending status updates on the IO Publish socket *)
			statusReplyFrame,

			(* a frame for sending replies on a socket *)
			replyFrame
		},
		While[
			True,
			Switch[
				(* poll sockets until one is ready *)
				readySocket = First[SocketWaitNext[{shellSocket, controlSocket}]],
				(* if the shell socket or control socket is ready, ... *)
				shellSocket | controlSocket,
				(* receive a frame *)
				rawFrame = SocketReadMessage[readySocket, "Multipart" -> True];
				(* check for any problems *)
				If[FailureQ[rawFrame],
					Quit[];
				];
				(* convert the frame into an Association *)
				loopState["frameAssoc"] = getFrameAssoc[rawFrame];
				(* handle this frame based on the type of request *)
				Switch[
					loopState["frameAssoc"]["header"]["msg_type"], 
					(* if asking for information about the kernel, ... *)
					"kernel_info_request",
					(* set the appropriate reply type *)
					loopState["replyMsgType"] = "kernel_info_reply";
					(* provide the information *)
					loopState["replyContent"] = 
						StringJoin[
							"{\"protocol_version\": \"5.3.0\",\"implementation\": \"WolframLanguageForJupyter\",\"implementation_version\": \"0.0.1\",\"language_info\": {\"name\": \"Wolfram Language\",\"version\": \"12.0\",\"mimetype\": \"application/vnd.wolfram.m\",\"file_extension\": \".m\",\"pygments_lexer\": \"mathematica\",\"codemirror_mode\": \"mathematica\"},\"banner\" : \"Wolfram Language/Wolfram Engine Copyright 2019",
							bannerWarning,
							"\"}"
						];,
					(* if asking if the input is complete (relevant for jupyter-console), respond appropriately *)
					"is_complete_request",
					(* isCompleteRequestHandler will read and update loopState *)
					isCompleteRequestHandler[];,
					(* if asking the kernel to execute something, use executeRequestHandler *)
					"execute_request",
					(* executeRequestHandler will read and update loopState *)
					executeRequestHandler[];,
					(* use the tab-completion functionality to rewrite named character names *)
					"complete_request",
					(* completeRequestHandler will read and update loopState *)
					completeRequestHandler[];,
					(* if asking the kernel to shutdown, set doShutdown to True *)
					"shutdown_request",
					loopState["replyMsgType"] = "shutdown_reply";
					loopState["replyContent"] = "{\"restart\":false}";
					loopState["doShutdown"] = True;,
					_,
					Continue[];
				];

				(* create a message frame to send on the IO Publish socket to mark the kernel's status as "busy" *)
				statusReplyFrame =
					createReplyFrame[
						(* use the current source frame *)
						loopState["frameAssoc"],
						(* the status message type *)
						"status",
						(* the status message content *)
						"{\"execution_state\":\"busy\"}",
						(* do not branch off *)
						False
					];
				(* send the frame *)
				sendFrame[ioPubSocket, statusReplyFrame];

				(* create a message frame to send a reply on the socket that became ready *)
				replyFrame = 
					createReplyFrame[
						(* use the current source frame *)
						loopState["frameAssoc"],
						(* the reply message type *)
						loopState["replyMsgType"],
						(* the reply message content *)
						loopState["replyContent"],
						(* do not branch off *)
						False
					];
				(* send the frame *)
				sendFrame[readySocket, replyFrame];

				(* if an ioPubReplyFrame was created, send it on the IO Publish socket *)
				If[
					loopState["ioPubReplyFrame"] =!= Association[],
					sendFrame[ioPubSocket, loopState["ioPubReplyFrame"]];
					(* -- also, reset ioPubReplyFrame *)
					loopState["ioPubReplyFrame"] = Association[];
				];

				(* send a message frame on the IO Publish socket that marks the kernel's status as "idle" *)
				sendFrame[
					ioPubSocket,
					createReplyFrame[
						(* use the current source frame *)
						loopState["frameAssoc"],
						(* the status message type *)
						"status",
						(* the status message content *)
						"{\"execution_state\":\"idle\"}",
						(* do not branch off *)
						False
					]
				];

				(* if the doShutdown flag is True, shut down *)
				If[
					loopState["doShutdown"],
					Block[{$inQuit = True}, Quit[]];
				];
				,
				_,
				Continue[];
			]
		];
	];

(* end the private context for WolframLanguageForJupyter *)
End[]; (* `Private` *)

(************************************
	end the
		WolframLanguageForJupyter
		package
*************************************)

(* end the WolframLanguageForJupyter package *)
EndPackage[]; (* WolframLanguageForJupyter` *)
(* $ContextPath = DeleteCases[$ContextPath, "WolframLanguageForJupyter`"]; *)

(************************************
	evaluate loop[]
*************************************)

(* start the loop *)
WolframLanguageForJupyter`Private`loop[];

(* This setup does not preclude dynamics or widgets. *)
