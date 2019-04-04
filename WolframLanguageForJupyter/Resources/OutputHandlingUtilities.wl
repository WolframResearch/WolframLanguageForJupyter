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
	toOutText,
	toOutImage
*************************************************)

(************************************
	Get[] guard
*************************************)

If[
	!TrueQ[WolframLanguageForJupyter`Private`$GotOutputHandlingUtilities],
	
	WolframLanguageForJupyter`Private`$GotOutputHandlingUtilities = True;

(************************************
	private symbols
*************************************)

	(* begin the private context for WolframLanguageForJupyter *)
	Begin["`Private`"];

(************************************
	utility for determining if a
		result should be displayed
		as text or an image
*************************************)

	(* determine if a result does not depend on any Wolfram Language frontend functionality,
		such that it should be displayed as text *)
	textQ[expr_] := Module[
		{
			(* pattern objects *)
			pObjects
		}, 

		(* if we cannot use the frontend, use text *)
		If[
			UsingFrontEnd[$FrontEnd] === Null,
			Return[True];
		];

		(* breakdown expr into atomic objects organized by their Head *)
		pObjects = 
			GroupBy[
				Complement[
					Quiet[Cases[
						expr, 
						elem_ /; (Depth[elem] == 1), 
						{0, Infinity}, 
						Heads -> True
					]],
					(* these symbols are fine *)
					{List, Association}
				],
				Head
			];
			
		(* if expr just contains atomic objects of the types listed above, return True *)
		If[
			ContainsOnly[Keys[pObjects], {Integer, Real, String}],
			Return[True];
	   	];

	   	(* if expr just contains atomic objects of the types listed above, along with some symbols,
	   		return True only if the symbols have no attached rules *)
		If[
			ContainsOnly[Keys[pObjects], {Integer, Real, String, Symbol}],
	   		Return[
	   			AllTrue[
	   				pObjects[Symbol], 
	   				(ToString[Definition[#1]] === "Null") &
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

	(* generate HTML for the textual form of a result *)
	toOutText[result_] := 
		StringJoin[
			(* preformatted *)
			"<pre style=\"",
			(* use Courier *)
			StringJoin[{"&#", ToString[#1], ";"} & /@ ToCharacterCode["font-family: \"Courier New\",Courier,monospace;", "UTF-8"]], 
			"\">",
			(* the textual form of the result *)
			StringJoin[{"&#", ToString[#1], ";"} & /@ ToCharacterCode[ToString[result], "UTF-8"]],
			(* end the element *)
			"</pre>"
		];

	(* generate HTML for the rasterized form of a result *)
	toOutImage[result_] := 
		StringJoin[
			(* display a inlined PNG image encoded in base64 *)
			"<img alt=\"Output\" src=\"data:image/png;base64,",
			(* the rasterized form of the result, converted to base64 *)
			BaseEncode[
				UsingFrontEnd[ExportByteArray[
					If[Head[result] === Manipulate, result, Rasterize[result]],
					"PNG"
				]]
			],
			(* end the element *)
			"\">"
		];

	(* end the private context for WolframLanguageForJupyter *)
	End[]; (* `Private` *)

(************************************
	Get[] guard
*************************************)

] (* WolframLanguageForJupyter`Private`$GotOutputHandlingUtilities *)
