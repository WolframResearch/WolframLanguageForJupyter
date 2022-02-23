(************************************************
				RequestHandlers.wl
*************************************************
Description:
	Handlers for message frames of the form
		"*_request" arriving from Jupyter
Symbols defined:
	isCompleteRequestHandler,
	executeRequestHandler,
	completeRequestHandler
*************************************************)

(************************************
	Get[] guard
*************************************)

If[
	!TrueQ[WolframLanguageForJupyter`Private`$GotRequestHandlers],
	
	WolframLanguageForJupyter`Private`$GotRequestHandlers = True;

(************************************
	load required
		WolframLanguageForJupyter
		files
*************************************)

	Get[FileNameJoin[{DirectoryName[$InputFileName], "Initialization.wl"}]]; (* loopState *)

	Get[FileNameJoin[{DirectoryName[$InputFileName], "SocketUtilities.wl"}]]; (* sendFrame *)
	Get[FileNameJoin[{DirectoryName[$InputFileName], "MessagingUtilities.wl"}]]; (* createReplyFrame *)

	Get[FileNameJoin[{DirectoryName[$InputFileName], "EvaluationUtilities.wl"}]]; (* redirectPrint, redirectMessages, simulatedEvaluate *)

	Get[FileNameJoin[{DirectoryName[$InputFileName], "OutputHandlingUtilities.wl"}]]; (* textQ, toOutTextHTML, toOutImageHTML,
																							toText, containsPUAQ *)

	Get[FileNameJoin[{DirectoryName[$InputFileName], "CompletionUtilities.wl"}]]; (* rewriteNamedCharacters *)

(************************************
	private symbols
*************************************)

	(* begin the private context for WolframLanguageForJupyter *)
	Begin["`Private`"];

(************************************
	handler for is_complete_requests
*************************************)

	(* handle is_complete_request message frames received on the shell socket *)
	isCompleteRequestHandler[] :=
		Module[
			{
				(* the length of the code string to check completeness for *)
				stringLength,
				(* the value returned by SyntaxLength[] on the code string to check completeness for *)
				syntaxLength
			},

			(* mark loopState["isCompleteRequestSent"] as True *)
			loopState["isCompleteRequestSent"] = True;

			(* set the appropriate reply type *)
			loopState["replyMsgType"] = "is_complete_reply";

			(* determine the length of the code string *)
			stringLength = StringLength[loopState["frameAssoc"]["content"]["code"]];
			(* determine the SyntaxLength[] value for the code string *)
			syntaxLength = SyntaxLength[loopState["frameAssoc"]["content"]["code"]];

			(* test the value of syntaxLength to determine the completeness of the code string,
				setting the content of the reply appropriately *)
			Which[
				(* if the above values could not be correctly determined,
					the completeness status of the code string is unknown *)
				!IntegerQ[stringLength] || !IntegerQ[syntaxLength],
					loopState["replyContent"] = "{\"status\":\"unknown\"}";,
				(* if the SyntaxLength[] value for a code string is greater than its actual length,
					the code string is incomplete *)
				syntaxLength > stringLength,
					loopState["replyContent"] = "{\"status\":\"incomplete\"}";,
				(* if the SyntaxLength[] value for a code string is less than its actual length,
					the code string contains a syntax error (or is "invalid") *)
				syntaxLength < stringLength,
					loopState["replyContent"] = "{\"status\":\"invalid\"}";,
				(* if the SyntaxLength[] value for a code string is equal to its actual length,
					the code string is complete and correct *)
				syntaxLength == stringLength,
					loopState["replyContent"] = "{\"status\":\"complete\"}";
			];
		];

(************************************
	handler for execute_requests
*************************************)

	(* handle execute_request message frames received on the shell socket *)
	executeRequestHandler[] :=
		Module[
			{
				(* message formatter function *)
				messageFormatter,

				(* content of the desired frame to send on the IO Publish socket *)
				ioPubReplyContent,

				(* the HTML form for any generated message *)
				errorMessage,

				(* the total result of the evaluation:
					an association containing
						the result of evaluation ("EvaluationResult"),
						indices of the output lines of the result ("EvaluationResultOutputLineIndices"),
						the total number of indices consumed by this evaluation ("ConsumedIndices"),
						generated messages ("GeneratedMessages")
				*)
				totalResult,

				(* flag for if there are any unreported error messages after execution of the input *)
				unreportedErrorMessages
			},

			(* if an is_complete_request has been sent, assume jupyter-console is running the kernel,
				redirect messages, and handle any "Quit", "Exit", "quit" or "exit" inputs *)
			If[
				loopState["isCompleteRequestSent"],
				loopState["redirectMessages"] = True;
				If[
					StringMatchQ[
						loopState["frameAssoc"]["content"]["code"],
						"Quit" | "Exit" | "quit" | "exit"
					],
					loopState["replyMsgType"] = "execute_reply";
					(* NOTE: uses payloads *)
					loopState["replyContent"] = ExportString[Association["status" -> "ok", "execution_count" -> loopState["executionCount"], "user_expressions" -> {}, "payload" -> {Association["source" -> "ask_exit", "keepkernel" -> False]}], "JSON", "Compact" -> True];
					Return[];
				];
			];

			(* redirect Print so that it prints in the Jupyter notebook *)
			loopState["printFunction"] = (redirectPrint[loopState["frameAssoc"], #1] &);

			(* if loopState["redirectMessages"] is True,
				update Jupyter explicitly with any errors that occur DURING the execution of the input *)
			If[
				loopState["redirectMessages"],
				messageFormatter[messageName_, messageText_] :=
					redirectMessages[
						loopState["frameAssoc"],
						messageName,
						messageText,
						(* add a newline if loopState["isCompleteRequestSent"] *)
						loopState["isCompleteRequestSent"]
					];
				SetAttributes[messageFormatter, HoldAll];
				Internal`$MessageFormatter = messageFormatter;
			];

			(* evaluate the input, and store the total result in totalResult *)
			totalResult = simulatedEvaluate[loopState["frameAssoc"]["content"]["code"]];
			
			(* restore printFunction to False *)
			loopState["printFunction"] = False;

			(* unset messageFormatter and Internal`$MessageFormatter *)
			Unset[messageFormatter];
			Unset[Internal`$MessageFormatter];

			(* set the appropriate reply type *)
			loopState["replyMsgType"] = "execute_reply";

			(* set the content of the reply to information about WolframLanguageForJupyter's execution of the input *)
			loopState["replyContent"] = 
				ExportString[
					Association[
						"status" -> "ok",
						"execution_count" -> loopState["executionCount"],
						"user_expressions" -> {},
						(* see https://jupyter-client.readthedocs.io/en/stable/messaging.html#payloads-deprecated *)
						(* if the "askExit" flag is True, add an "ask_exit" payload *)
						(* NOTE: uses payloads *)
						"payload" -> If[loopState["askExit"], {Association["source" -> "ask_exit", "keepkernel" -> False]}, {}]
					],
					"JSON",
					"Compact" -> True
				];

			(* check if there are any unreported error messages *)
			unreportedErrorMessages =
				(
					(* ... because messages are not being redirected *)
					(!loopState["redirectMessages"]) &&
						(* ... and because at least one message was generated *)
						(StringLength[totalResult["GeneratedMessages"]] > 0)
				);

			(* if there are no results, or if the "askExit" flag is True,
				do not send anything on the IO Publish socket and return *)
			If[
				(Length[totalResult["EvaluationResultOutputLineIndices"]] == 0) ||
					(loopState["askExit"]),
				(* set the "askExit" flag to False *)
				loopState["askExit"] = False;
				(* send any unreported error messages *)
				If[unreportedErrorMessages,
					redirectMessages[
						loopState["frameAssoc"],
						"",
						totalResult["GeneratedMessages"],
						(* do not add a newline *)
						False,
						(* drop message name *)
						True
					];
				];
				(* increment loopState["executionCount"] as needed *)
				loopState["executionCount"] += totalResult["ConsumedIndices"];
				Return[];
			];

			(* generate an HTML form of the message text *)
			errorMessage =
				If[
					!unreportedErrorMessages,
					(* if there are no unreported error messages, there is no need to format them *)
					{},
					(* build the HTML form of the message text *)
					{
						(* preformatted *)
						"<pre style=\"",
						(* the color of the text should be red, and should use Courier *)
						StringJoin[{"&#", ToString[#1], ";"} & /@ ToCharacterCode["color:red; font-family: \"Courier New\",Courier,monospace;", "UTF-8"]], 
						(* end pre tag *)
						"\">",
						(* the generated messages  *)
						StringJoin[{"&#", ToString[#1], ";"} & /@ ToCharacterCode[totalResult["GeneratedMessages"], "UTF-8"]],
						(* end the element *)
						"</pre>"
					}
				];

			(* format output as purely text, image, or cloud interface *)
			If[
				(* check if the input was wrapped with Interact,
					which is used when the output should be displayed as an embedded cloud object *)
				TrueQ[totalResult["InteractStatus"]] && 
					(* check if we are logged into the Cloud *)
					$CloudConnected,
				(* prepare the content for a reply message frame to be sent on the IO Publish socket *)
				ioPubReplyContent =
					ExportString[
						Association[
							(* the first output index *)
							"execution_count" -> First[totalResult["EvaluationResultOutputLineIndices"]],
							(* HTML code to embed output uploaded to the Cloud in the Jupyter notebook *)
							"data" ->
								{
									"text/html" ->
										StringJoin[
											(* display any generated messages as inlined PNG images encoded in base64 *)
											"<div><img alt=\"\" src=\"data:image/png;base64,", 
											(* rasterize the generated messages in a dark red color, and convert the resulting image to base64*)
											BaseEncode[ExportByteArray[Rasterize[Style[totalResult["GeneratedMessages"], Darker[Red]]], "PNG"]],
											(* end image element *)
											"\">",
											(* embed the cloud object *)
											EmbedCode[CloudDeploy[totalResult["EvaluationResult"]], "HTML"][[1]]["CodeSection"]["Content"],
											(* end the whole element *)
											"</div>"
										],
									"text/plain" -> ""
								},
							(* no metadata *)
							"metadata" -> {"text/html" -> {}, "text/plain" -> {}}
						],
						"JSON",
						"Compact" -> True
					];
				,
				(* if every output line can be formatted as text, use a function that converts the output to text *)
				(* otherwise, use a function that converts the output to an image *)
				(* TODO: allow for mixing text and image results *)
				If[AllTrue[totalResult["EvaluationResult"], textQ],
					toOut = toOutTextHTML,
					toOut = toOutImageHTML
				];
				(* prepare the content for a reply message frame to be sent on the IO Publish socket *)
				ioPubReplyContent = ExportByteArray[
					Association[
						(* the first output index *)
						"execution_count" -> First[totalResult["EvaluationResultOutputLineIndices"]],
						(* the data representing the results and messages *)
						"data" ->
							{
								(* generate HTML for the results and messages *)
								"text/html" ->
									If[
										loopState["isCompleteRequestSent"],
										(* if an is_complete_request has been sent, assume jupyter-console is running the kernel,
											and do not generate HTML *)
										"",
										(* otherwise, output the results in a grid *)
										If[
											Length[totalResult["EvaluationResult"]] > 1,
											StringJoin[
												(* add grid style *)
												"<style>
													.grid-container {
														display: inline-grid;
														grid-template-columns: auto;
													}
												</style>

												<div>",
												(* display error message *)
												errorMessage,
												(* start the grid *)
												"<div class=\"grid-container\">",
												(* display the output lines *)
												Table[
													{
														(* start the grid item *)
														"<div class=\"grid-item\">",
														(* show the output line *)
														toOut[totalResult["EvaluationResult"][[outIndex]]],
														(* end the grid item *)
														"</div>"
													},
													{outIndex, 1, Length[totalResult["EvaluationResult"]]}
												],
												(* end the element *)
												"</div></div>"
											],
											StringJoin[
												(* start the element *)
												"<div>",
												(* display error message *)
												errorMessage,
												(* if there are messages, but no results, do not display a result *)
												If[
													Length[totalResult["EvaluationResult"]] == 0,
													"",
													(* otherwise, display a result *)
													toOut[First[totalResult["EvaluationResult"]]]
												],
												(* end the element *)
												"</div>"
											]
										]
									],
								(* provide, as a backup, plain text for the results *)
								"text/plain" ->
									StringJoin[
										Table[
											{
												toText[totalResult["EvaluationResult"][[outIndex]]],
												(* -- also, suppress newline if this is the last result *)
												If[outIndex != Length[totalResult["EvaluationResult"]], "\n", ""]
											},
											{outIndex, 1, Length[totalResult["EvaluationResult"]]}
										]
									]
							},
						(* no metadata *)
						"metadata" -> {"text/html" -> {}, "text/plain" -> {}}
					],
					"JSON",
					"Compact" -> True
				];
			];
			
			(* create frame from ioPubReplyContent *)
			loopState["ioPubReplyFrame"] = 
				createReplyFrame[
					(* use the current source frame *)
					loopState["frameAssoc"],
					(* the reply message type *)
					"execute_result",
					(* the reply message content *)
					ioPubReplyContent,
					(* do not branch off *)
					False
				];

			(* increment loopState["executionCount"] as needed *)
			loopState["executionCount"] += totalResult["ConsumedIndices"];
		];

(************************************
	handler for complete_requests
*************************************)

	(* handle complete_request message frames received on the shell socket *)
	completeRequestHandler[] :=
		Module[
			{
				(* for storing the code string to offer completion suggestions on *)
				codeStr
			},
			(* get the code string to rewrite the named characters of, ending at the cursor *)
			codeStr =
				StringTake[
					loopState["frameAssoc"]["content"]["code"],
					{
						1,
						loopState["frameAssoc"]["content"]["cursor_pos"]
					}
				];
			(* set the appropriate reply type *)
			loopState["replyMsgType"] = "complete_reply";
			(* set the content of the reply to a list of rewrites for any named characters in the code string *)
			loopState["replyContent"] = 
				ByteArrayToString[
					ExportByteArray[
						Association[
							"matches" ->
								DeleteDuplicates[
									Prepend[
										Select[
											rewriteNamedCharacters[codeStr],
											(!containsPUAQ[#1])&
										],
										codeStr
									]
								],
							"cursor_start" -> 0,
							"cursor_end" -> StringLength[codeStr],
							"metadata" -> {},
							"status" -> "ok"
						], 
						"JSON",
						"Compact" -> True
					]
				];
		];

	(* end the private context for WolframLanguageForJupyter *)
	End[]; (* `Private` *)

(************************************
	Get[] guard
*************************************)

] (* WolframLanguageForJupyter`Private`$GotRequestHandlers *)
