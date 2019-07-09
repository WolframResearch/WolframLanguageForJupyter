(************************************************
				CompletionUtilities.wl
*************************************************
Description:
	Utilities for the aiding in the
		(auto-)completion of Wolfram Language
		code
Symbols defined:
	rewriteNamedCharacters
*************************************************)

(************************************
	Get[] guard
*************************************)

If[
	!TrueQ[WolframLanguageForJupyter`Private`$GotCompletionUtilities],
	
	WolframLanguageForJupyter`Private`$GotCompletionUtilities = True;

(************************************
	load required
		WolframLanguageForJupyter
		files
*************************************)

	Get[FileNameJoin[{DirectoryName[$InputFileName], "Initialization.wl"}]]; (* unicodeNamedCharactersReplacements,
																					verticalEllipsis *)

(************************************
	private symbols
*************************************)

	(* begin the private context for WolframLanguageForJupyter *)
	Begin["`Private`"];

(************************************
	utilities for rewriting
		Wolfram Language code
*************************************)

	(* rewrite names (in a code string) into named characters *)
	rewriteNamedCharacters[codeToAnalyze_?StringQ] :=
		Module[
			{codeUsingFullReplacements},
			codeUsingFullReplacements =
				StringReplace[
					codeToAnalyze,
					Normal @ unicodeNamedCharactersReplacements
				];
			If[
				StringCount[
					codeUsingFullReplacements,
					verticalEllipsis | "\\["
				] != 1,
				Return[{codeUsingFullReplacements}];
			];
			Return[
				Flatten[
					StringCases[
						codeUsingFullReplacements,
						before___ ~~ name : ((verticalEllipsis | "\\[") ~~ rest__ ~~ EndOfString) :>
							(
								(StringJoin[before, #1] &) /@
									Values[
										KeySelect[
											unicodeNamedCharactersReplacements,
											StringMatchQ[#1, name ~~ ___] &
										]
									]
							)
					]
				]
			];
		];

	(* end the private context for WolframLanguageForJupyter *)
	End[]; (* `Private` *)

(************************************
	Get[] guard
*************************************)

] (* WolframLanguageForJupyter`Private`$GotCompletionUtilities *)
