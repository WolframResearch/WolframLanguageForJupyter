(************************************************
				EvaluationUtilities.wl
*************************************************
Description:
	Utilities for evaluating input arriving
		from Jupyter, and for simulating
		the Mathematica REPL
Symbols defined:
	Print,
	simulatedEvaluate
*************************************************)

(************************************
	Get[] guard
*************************************)

If[
	!TrueQ[WolframLanguageForJupyter`Private`$GotEvaluationUtilities],
	
	WolframLanguageForJupyter`Private`$GotEvaluationUtilities = True;

(************************************
	load required
		WolframLanguageForJupyter
		files
*************************************)

	Get[FileNameJoin[{DirectoryName[$InputFileName], "Initialization.wl"}]]; (* loopState *)

(************************************
	wrapper for interacting
		with the cloud
*************************************)

	(* Interact is wrapper, open to the user, for asking that the result of the evaluation
		be displayed as an embedded cloud object  that can be interacted with *)
	(* set Interact to not evaluate its arguments *)
	SetAttributes[Interact, HoldAll];

(************************************
	private symbols
*************************************)

	(* begin the private context for WolframLanguageForJupyter *)
	Begin["`Private`"];

(************************************
	version of Print that
		sends output to Jupyter
*************************************)

	(* redirect Print calls into a message to Jupyter, in order to print in the Jupyter notebook *)
	(* TODO: review other methods: through EvaluationData or WSTP so we don't redefine Print *)
	Unprotect[Print];
	Print[args___, opts:OptionsPattern[]] :=
		Block[
			{
				$inPrint=True,
				$Output={OpenWrite[FormatType->OutputForm]}
			},
			If[
				!FailureQ[First[$Output]],
					Print[args, opts];
					loopState["printFunction"][
						Import[$Output[[1,1]], "String"]
					];
			];
			Close[First[$Output]];
			Null
		] /; !TrueQ[$inPrint];
	Protect[Print];

(************************************
	main evaluation command
*************************************)

	(* evaluate input, and capture required information such as generated messages *)
	(* TODO: review other method: evaluate input through WSTP in another kernel *)
	simulatedEvaluate[expr___] :=
		Module[
			{
				(* for saving $Messages before changing it *)
				oldMessages,
				(* the stream used to capture generated messages *)
				stream,
				(* the string form of the generated messages, obtained from stream *)
				generatedMessages,

				(* the length of expr *)
				exprLength,

				(* a new version of expr that simulates lines *)
				exprWithLines,

				(* the result of evaluation *)
				evaluationResult,

				(* the total result of the evaluation:
					an association containing
						the result of evaluation ("EvaluationResult"),
						indices of the output lines of the result ("EvaluationResultOutputLineIndices"),
						the total number of indices consumed by this evaluation ("ConsumedIndices"),
						generated messages ("GeneratedMessages")
				*)
				totalResult
			},

			(* create the association for the total result of the evaluation *)
			totalResult = Association[];

			(* save $Messages before overwrite *)
			oldMessages = $Messages;
			(* open stream to write messages into *)
			stream = OpenWrite[];

			(* clear the list of generated messages *)
			Unprotect[$MessageList]; $MessageList = {}; Protect[$MessageList];

			(* set $Messages to use the new stream *)
			$Messages = {stream};

			(* obtain the length of expr *)
			exprLength = Length[Unevaluated[{expr}]];

			(* predefine the In[n] for the input *)
			Unprotect[In];
			(* for every line input in the input, set In[n], using placeholders *)
			Table[
				ReleaseHold[
					(* replace index with the number of the input line,
						and replace placeHolder with the element in expr corresponding to inIndex'th input line using Extract *)
					Replace[
						Hold[
							SetDelayed[
								In[index],
								ReleaseHold[placeHolder]
							]
						],
						{
							index -> loopState["executionCount"] + inIndex - 1,
							placeHolder ->
								Extract[
									Hold[{expr}],
									{1, inIndex},
									Hold
								]
						},
						(* the level of index and placeHolder *)
						{3}
					]
				];,
				{inIndex, 1, exprLength}
			];
			Protect[In];
			
			(* create a new version of expr that simulates lines *)
			exprWithLines = 
				Table[
					$Line++;
					ReleaseHold[
						Extract[
							Hold[{expr}],
							{1, inIndex},
							Hold
						]
					],
					{inIndex, 1, exprLength}
				];

			(* evaluate the input from Jupyter, removing Nulls from the Output *)
			evaluationResult = DeleteCases[exprWithLines, Null];

			(* preserve the locations of the output lines with respect to the Nulls *)
			AssociateTo[
				totalResult,
				"EvaluationResultOutputLineIndices" -> 
					(
						(loopState["executionCount"] - 1) + 
							Flatten[Position[exprWithLines, Except[Null], {1}, Heads -> False]]
					)
			];

			(* set Out[n] *)
			Unprotect[Out];
			(* for every output line, set the Out with the corresponding index *)
			Table[
				Out[loopState["executionCount"] + outIndex - 1] = exprWithLines[[outIndex]];,
				{outIndex, 1, Length[exprWithLines]}
			];
			Protect[Out];

			(* restore $Messages *)
			$Messages = oldMessages;

			(* obtain generated messages *)
			generatedMessages = Import[stream[[1]], "String"];
			(* close the opened stream *)
			Close[stream];

			(* add the total number of indices consumed by this evaluation *)
			AssociateTo[
				totalResult,
				"ConsumedIndices" -> exprLength
			];

			(* add the result of the evaluation and any generated messages to totalResult *)
			AssociateTo[totalResult, {"EvaluationResult" -> evaluationResult, "GeneratedMessages" -> generatedMessages}];

			(* return totalResult *)
			Return[totalResult];
		];
	(* set simulatedEvaluate to not implicitly evaluate its arguments *)
	SetAttributes[simulatedEvaluate, HoldAll];

	(* end the private context for WolframLanguageForJupyter *)
	End[]; (* `Private` *)

(************************************
	Get[] guard
*************************************)

] (* WolframLanguageForJupyter`Private`$GotEvaluationUtilities *)
