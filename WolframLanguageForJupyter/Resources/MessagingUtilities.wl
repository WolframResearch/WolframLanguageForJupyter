(************************************************
				MessagingUtilities.wl
*************************************************
Description:
	Higher-level utilities for sending
		and receiving messages from Jupyter
Symbols defined:
	getFrameAssoc,
	createReplyFrame
*************************************************)

(************************************
	Get[] guard
*************************************)

If[
	!TrueQ[WolframLanguageForJupyter`Private`$GotMessagingUtilities],
	
	WolframLanguageForJupyter`Private`$GotMessagingUtilities = True;

(************************************
	load required
		WolframLanguageForJupyter
		files
*************************************)

	Get[FileNameJoin[{DirectoryName[$InputFileName], "SocketUtilities.wl"}]]; (* hmac *)

(************************************
	private symbols
*************************************)

	(* begin the private context for WolframLanguageForJupyter *)
	Begin["`Private`"];

(************************************
	utilities for reading in, and
		writing out, message
		frames
*************************************)

	(* transform received frame into a structured Association *)
	getFrameAssoc[baFrame_ByteArray] :=
		Module[
			{
				(* string form of the byte array *)
				frameStr,
				(* storage for the various value fields of the received frame *)
				identLen,
				header,
				pheader,
				metadata,
				content
			},

			(* set frameStr to the string form of the byte array of the received frame *)
			frameStr = Quiet[ByteArrayToString[baFrame]];

			(* get the values (of key-value pairs) from the string frame *)
			{identLen, header, pheader, metadata, content} =
				First[
					(* pick out the values using the expected form of the frame *)
					(* see https://jupyter-client.readthedocs.io/en/stable/messaging.html *)
					StringCases[
						frameStr,
						Shortest[ident1___] ~~ "<IDS|MSG>" ~~ Shortest[___] ~~ 
							"{" ~~ Shortest[json2___] ~~ "}" ~~
								"{" ~~ Shortest[json3___] ~~ "}" ~~
									"{" ~~ Shortest[json4___] ~~ "}" ~~
										"{" ~~ Shortest[json5___] ~~ "}" ~~
											EndOfString :> 
							Prepend[
								(* add back in the brackets *)
								(
									Association[
										ImportByteArray[
											StringToByteArray[
												StringJoin["{", #1, "}"]
											],
											"JSON"
										]
									] &
								) /@ {json2,json3,json4,json5},
								(* use the length of ident1 *)
								StringLength[ident1]
							]
					]
				];

			(* return an association with:
				* an ident key with a byte array value
				* the header of the original frame imported as JSON
				* the content of the original frame imported as JSON
				*)
			Return[
				Association[
					"ident" -> baFrame[[;;identLen]],
					"header" -> header,
					"content" -> content
				]
			];
		];

	(* generate a reply message frame from using a source message frame, replyType, and replyContent *)
	createReplyFrame[
			(* the source frame to use, after it has been ran through getFrameAssoc *)
			sourceFrame_Association,
			(* the message type to be used for the reply message frame *)
			replyType_String,
			(* the content to be used for the reply message frame *)
			replyContent : (_String | _ByteArray),
			(* whether to list sourceFrame as a parent for the reply message frame *)
			branchOff:(True|False)
		] := 
			Module[
				{
					(* for storing the header and content of the source message frame *)
					header, content,

					(* the association for the generated reply message frame *)
					result
				},

				(* save the header and content of the source message frame *)
				header = sourceFrame["header"];
				content = sourceFrame["content"];

				(* build reply message *)
				(* see https://jupyter-client.readthedocs.io/en/stable/messaging.html for why the following are set as they are *)
				result = Association[
							"ident" -> If[KeyExistsQ[sourceFrame, "ident"], sourceFrame["ident"], ByteArray[{0, 0, 0, 0, 0}]],
							"idsmsg" -> "<IDS|MSG>",
							"header" -> ExportString[
											Append[
												header,
												{"date" -> DateString["ISODateTime"], "msg_type" -> replyType, "msg_id" -> StringInsert[StringReplace[CreateUUID[], "-" -> ""], "-", 9]}
											],
											"JSON",
											"Compact" -> True
										],
							"pheader" -> If[branchOff, "{}", ExportString[header, "JSON", "Compact" -> True]],
							"metadata" -> ExportString[
											{"text/html" -> {}},
											"JSON",
											"Compact" -> True
										],
							"content" -> replyContent
						];

				(* generate the signature of the reply message *)
				AssociateTo[
					result,
					"signature" -> 
						hmac[
							keyString, 
							StringJoin[
								result["header"],
								result["pheader"],
								result["metadata"],
								If[StringQ[result["content"]], result["content"], ByteArrayToString[result["content"]]]
							]
						]
				];

				(* return the built reply message frame *)
				Return[result];
			];

	(* end the private context for WolframLanguageForJupyter *)
	End[]; (* `Private` *)

(************************************
	Get[] guard
*************************************)

] (* WolframLanguageForJupyter`Private`$GotMessagingUtilities *)
