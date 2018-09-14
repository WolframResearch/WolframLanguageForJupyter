If[
	Length[$ScriptCommandLine] > 1 && $ScriptCommandLine[[2]] === "help",
	helpMessage = StringJoin[
					"build.wls: Usage: build.wls\nbuild.wls: Usage:\tbuild the WolframForJupyter paclet in the project directory"
				];
	Print[helpMessage];
	Quit[];
];

PackPaclet["WolframForJupyter"];