(************************************************
				OutputHandlingUtilities.wl
*************************************************
Description:
	Utilities for handling the result
		of Wolfram Language expressions
		so that, as outputs, they are
		reasonably displayed in Jupyter
		notebooks
Symbols defined:
	textQ,
	toText,
	toOutTextHTML,
	toImageData,
	toOutImageHTML
*************************************************)

(************************************
	Get[] guard
*************************************)

If[
	!TrueQ[WolframLanguageForJupyter`Private`$GotOutputHandlingUtilities],
	
	WolframLanguageForJupyter`Private`$GotOutputHandlingUtilities = True;

(************************************
	load required
		WolframLanguageForJupyter
		files
*************************************)

	Get[FileNameJoin[{DirectoryName[$InputFileName], "Initialization.wl"}]]; (* $canUseFrontEnd, $outputSetToTeXForm,
																					$outputSetToTraditionalForm,
																					$trueFormatType, $truePageWidth,
																					failedInBase64 *)

(************************************
	private symbols
*************************************)

	(* begin the private context for WolframLanguageForJupyter *)
	Begin["`Private`"];

(************************************
	helper utility for converting
		an expression into a
		textual form
*************************************)

	(* convert an expression into a textual form,
		using as much of the options already set for $Output as possible for ToString *)
	(* NOTE: toOutTextHTML used to call toStringUsingOutput *)
	toStringUsingOutput[expr_] :=
		ToString[
			expr,
			Sequence @@
				Cases[
					Options[$Output],
					Verbatim[Rule][opt_, val_] /;
						MemberQ[
							Keys[Options[ToString]],
							opt
						]
				]
		];

(************************************
	helper utility for determining
		if a result should be
		displayed as text or an image
*************************************)

	(* check if a string contains any private use area characters *)
	containsPUAQ[str_] :=
		AnyTrue[
			ToCharacterCode[str, "Unicode"],
			(57344 <= #1 <= 63743 || 983040 <= #1 <= 1048575 || 1048576 <= #1 <= 1114111) &
		];

(************************************
	utility for determining if a
		result should be displayed
		as text or an image
*************************************)

	(* determine if a result does not depend on any Wolfram Language frontend functionality,
		such that it should be displayed as text *)
	textQ[expr_] := Module[
		{
			(* the head of expr *)
			exprHead,

			(* pattern objects *)
			pObjects
		}, 

		(* if we cannot use the frontend, use text *)
		If[
			!$canUseFrontEnd,
			Return[True];
		];

		(* save the head of the expression *)
		exprHead = Head[expr];

		(* if the expression is wrapped with InputForm or OutputForm,
			automatically format as text *)
		If[exprHead === InputForm || exprHead === OutputForm,
			Return[True]
		];

		(* if the FormatType of $Output is set to TeXForm, or if the expression is wrapped with TeXForm,
			and the expression has an acceptable textual form, format as text *)
		If[($outputSetToTeXForm || exprHead == TeXForm) && !containsPUAQ[ToString[expr]],
			Return[True];
		];

		(* if the FormatType of $Output is set to TraditionalForm,
			or if the expression is wrapped with TraditionalForm,
			do not use text *)
		If[$outputSetToTraditionalForm || exprHead === TraditionalForm,
			Return[False]
		];

		(* breakdown expr into atomic objects organized by their Head *)
		pObjects = 
			GroupBy[
				Complement[
					Quiet[Cases[
						expr, 
						elem_ /; (Depth[Unevaluated[elem]] == 1) -> Hold[elem], 
						{0, Infinity}, 
						Heads -> True
					]],
					(* these symbols are fine *)
					{Hold[List], Hold[Association]}
				],
				(
					Replace[
						#1,
						Hold[elem_] :> Head[Unevaluated[elem]]
					]
				) &
			];

	   	(* if expr just contains atomic objects of the types listed above, return True *)
		If[
			ContainsOnly[Keys[pObjects], {Integer, Real}],
			Return[True];
	   	];

	   	(* if expr just contains atomic objects of the types listed above, along with some symbols,
	   		return True only if the symbols have no attached rules *)
		If[
			ContainsOnly[Keys[pObjects], {Integer, Real, String, Symbol}],
	   		Return[
				AllTrue[
						Lookup[pObjects, String, {}], 
						(!containsPUAQ[ReleaseHold[#1]]) &
					] &&
		   			AllTrue[
		   				Lookup[pObjects, Symbol, {}], 
		   				(
							Replace[
								#1,
								Hold[elem_] :> ToString[Definition[elem]]
							] === "Null"
		   				) &
		   			]
	   		];
	   	];

	   	(* otherwise, no, the result should not be displayed as text *)
	   	Return[False];
	];

(************************************
	utilities for generating
		HTML for displaying
		results as text and images
*************************************)

	(* generate the textual form of a result using a given page width *)
	(* NOTE: the OutputForm (which ToString uses) of any expressions wrapped with, say, InputForm should
		be identical to the string result of an InputForm-wrapped expression itself *)
	toText[result_, pageWidth_] :=
		ToString[
			(* make sure to apply $trueFormatType to the result if the result is not already headed by TeXForm *)
			If[
				Head[result] === TeXForm,
				result,
				$trueFormatType[result]
			],
			(* also, use the given page width *)
			PageWidth -> pageWidth
		];
	(* generate the textual form of a result using the current PageWidth setting for $Output *)
	toText[result_] := toText[result, $truePageWidth];

	(* generate HTML for the textual form of a result *)
	toOutTextHTML[result_] := 
		Module[
			{
				(* if the result should be marked as TeX *)
				isTeX
			},
			(* check if the result should be marked as TeX *)
			isTeX = ((Head[result] === TeXForm) || ($outputSetToTeXForm));
			Return[
				StringJoin[

					(* mark this result as preformatted only if it isn't TeX *)
					If[
						!isTeX,
						{
							(* preformatted *)
							"<pre style=\"",
							(* use Courier *)
							StringJoin[{"&#", ToString[#1], ";"} & /@ ToCharacterCode["font-family: \"Courier New\",Courier,monospace;", "Unicode"]], 
							"\">"
						},
						{}
					],

					(* mark the text as TeX, if is TeX *)
					If[isTeX, "&#36;&#36;", ""],

					(* the textual form of the result *)
					({"&#", ToString[#1], ";"} & /@ 
						ToCharacterCode[
							If[
								isTeX,
								(* if the result is TeX, do not allow line breaks *)
								toText[result, Infinity],
								(* otherwise, just call toText *)
								toText[result]
							],
							"Unicode"
						]),

					(* mark the text as TeX, if is TeX *)
					If[isTeX, "&#36;&#36;", ""],

					(* mark this result as preformatted only if it isn't TeX *)
					If[
						!isTeX,
						{
							(* end the element *)
							"</pre>"
						},
						{}
					]
				]
			];
		];

	(* generate a byte array of image data for the rasterized form of a result *)
	toImageData[result_] :=
		Module[
			{
				(* the preprocessed form of a result *)
				preprocessedForm
			},
			(* preprocess the result *)
			If[
				Head[result] === Manipulate,
				preprocessedForm = result;
				,
				preprocessedForm = Rasterize[result];
			];
			(* if the preprocessing failed, return $Failed *)
			If[
				FailureQ[preprocessedForm],
				Return[$Failed];
			];
			(* now return preprocessedForm as a byte array corresponding to the PNG format *)
			Return[
				ExportByteArray[
					preprocessedForm,
					"PNG"
				]
			];
		];

	(* generate HTML for the rasterized form of a result *)
	toOutImageHTML[result_] := 
		Module[
			{
				(* the rasterization of result *)
				imageData,
				(* the rasterization of result in base 64 *)
				imageDataInBase64
			},

			(* rasterize the result *)
			imageData =
				toImageData[
					$trueFormatType[result]
				];
			If[
				!FailureQ[imageData],
				(* if the rasterization did not fail, convert it to base 64 *)
				imageInBase64 = BaseEncode[imageData];
				,
				(* if the rasterization did fail, try to rasterize result with Shallow *)
				imageData =
					toImageData[
						$trueFormatType[Shallow[result]]
					];
				If[
					!FailureQ[imageData],
					(* if the rasterization did not fail, convert it to base 64 *)
					imageInBase64 = BaseEncode[imageData];
					,
					(* if the rasterization did fail, try to rasterize $Failed *)
					imageData =
						toImageData[
							$trueFormatType[$Failed]
						];
					If[
						!FailureQ[imageData],
						(* if the rasterization did not fail, convert it to base 64 *)
						imageInBase64 = BaseEncode[imageData];
						,
						(* if the rasterization did fail, use a hard-coded base64 rasterization of $Failed *)
						imageInBase64 = failedInBase64;
					];
				];
			];

			(* return HTML for the rasterized form of result *)
			Return[
				StringJoin[
					(* display a inlined PNG image encoded in base64 *)
					"<img alt=\"Output\" src=\"data:image/png;base64,",
					(* the rasterized form of the result, converted to base64 *)
					imageInBase64,
					(* end the element *)
					"\">"
				]
			]
		];

	(* end the private context for WolframLanguageForJupyter *)
	End[]; (* `Private` *)

(************************************
	Get[] guard
*************************************)

] (* WolframLanguageForJupyter`Private`$GotOutputHandlingUtilities *)
