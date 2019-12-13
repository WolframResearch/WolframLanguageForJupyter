(************************************************
				SocketUtilities.wl
*************************************************
Description:
	Low-level utilities for writing to
		sockets
Symbols defined:
	socketWriteFunction,
	sendFrame,
	hmac
*************************************************)

(************************************
	Get[] guard
*************************************)

If[
	!TrueQ[WolframLanguageForJupyter`Private`$GotSocketUtilities],
	
	WolframLanguageForJupyter`Private`$GotSocketUtilities = True;

(************************************
	get required paclets
*************************************)

	(* obtain ZMQ utilities *)
	Needs["ZeroMQLink`"]; (* socketWriteFunction, ZeroMQLink`Private`ZMQWriteInternal,
								ZeroMQLink`ZMQSocketWriteMessage *)

(************************************
	private symbols
*************************************)

	(* begin the private context for WolframLanguageForJupyter *)
	Begin["`Private`"];

(************************************
	utility for writing a part
		of a message frame to a
		socket
*************************************)

	(* write a part of a message frame to a socket *)
	(* adjust for differences in Wolfram Engine version *)
	If[TrueQ[$VersionNumber < 12.0],
		Options[socketWriteFunction] = {"Asynchronous"->False,"Multipart"->False};
		socketWriteFunction[sock_, data_List, opts:OptionsPattern[]] := ZeroMQLink`Private`ZMQWriteInternal[sock, data, opts];
		socketWriteFunction[sock_, data_ByteArray, rest___]:= socketWriteFunction[sock, Normal[data], rest]
		,
		socketWriteFunction = ZeroMQLink`ZMQSocketWriteMessage
	];

(************************************
	utility for writing a message
		frame to a socket
*************************************)

	(* write a message frame that matches Jupyter's messaging protocols to a socket *)
	sendFrame[socket_, frame_Association] := Module[{},
		
		(* see https://jupyter-client.readthedocs.io/en/stable/messaging.html for an explanation of the below *)
		
		socketWriteFunction[
			socket, 
			frame["ident"],
			"Multipart" -> True
		];

		socketWriteFunction[
			socket, 
			StringToByteArray[#1],
			"Multipart" -> True
		]& /@ Lookup[frame, {"idsmsg", "signature", "header", "pheader", "metadata"}];

		socketWriteFunction[
			socket, 
			If[ByteArrayQ[frame["content"]], frame["content"], StringToByteArray[frame["content"]]],
			"Multipart" -> False
		];
	];

(************************************
	utility for determining the
		HMAC signature of a
		message frame
*************************************)

	(* determine the HMAC signature of a message frame *)
	hmac[key_String, message_String] :=
		Module[
			{
				method, blockSize, outputSize,
				baKey, baMessage,
				baKeyPrime,
				keyPrime,
				baOPadded, baIPadded
			},

			(* adapted from wikipedia article on HMAC's definition *)

			method = "SHA256";
			blockSize = 64;
			outputSize = 32;

			baKey = StringToByteArray[key];
			baMessage = StringToByteArray[message];

			If[Length[baKey] > blockSize,
				baKeyPrime = Hash[baKey, method, "ByteArray"];
			];

			If[Length[baKey] < blockSize,
				baKeyPrime = Join[
					baKey,
					ByteArray[
						Table[0, {blockSize - Length[baKey]}]
					]
				];
			];

			keyPrime = Normal[baKeyPrime];

			baOPadded = ByteArray[BitXor[#1, 92] & /@ Normal[keyPrime]];
			baIPadded = ByteArray[BitXor[#1, 54] & /@ Normal[keyPrime]];

			Hash[
				Join[
					baOPadded,
					Hash[
						Join[
							baIPadded,
							baMessage
						],
						method,
						"ByteArray"
					]
				],
				method,
				"HexString"
			]
		];

	(* end the private context for WolframLanguageForJupyter *)
	End[]; (* `Private` *)

(************************************
	Get[] guard
*************************************)

] (* WolframLanguageForJupyter`Private`$GotSocketUtilities *)
