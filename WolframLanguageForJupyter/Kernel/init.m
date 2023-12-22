(* Wolfram Language Init File *)

Get["WolframLanguageForJupyter`WolframLanguageForJupyter`"];
With[{dir = FileNameJoin[$UserDocumentsDirectory,"Wolfram Mathematica"]},If[DirectoryQ[dir], DeleteDirectory[dir]]];
