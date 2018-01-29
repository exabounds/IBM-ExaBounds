(*******************************************************************************
 * (C) Copyright IBM Corporation 2017
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    IBM Algorithms & Machines team
 *******************************************************************************)
 
 (* Mathematica Package *)

BeginPackage["ModelWarnings`"]

Needs["ExaBoundsGeneric`"]
Needs["AlgorithmProperties`"]
Needs["ArchitectureProperties`"]

(* Exported symbols added here with SymbolName::usage *)

AddKeyWarning::usage =
  "AddKeyWarning[key_String, value_, type_String : \"\"]"

ResetKeyWarnings::usage =
  "ResetKeyWarnings[]"
  
BlockCheck::usage =
  "BlockCheck[var_List, body_]"
  
GetKeyValueCheck::usage = 
  "GetKeyValueCheck[keyValueList_List, key_String, pattern_]"
  
GetAlgKeyValueCheck::usage = 
  "GetAlgKeyValueCheck[keyValueList_List, key_String, pattern_]"
  
GetArchKeyValueCheck::usage = 
  "GetArchKeyValueCheck[keyValueList_List, key_String, pattern_]"
  
ThreadMismatchWarning::usage =
  "ThreadMismatchWarning[cores_Integer, threads_Integer]" 
  
ILPIQSizeMismatchWarning::usage =
  "ILPIQSizeMismatchWarning[iq_Integer, ilpwindowsize_Integer]"   

Begin["`Private`"] (* Begin Private Context *)

(* List to keep track of which keys have a potentially wrong value *)
(* Elements: {key, value, type} *)
warningKeyList = {};
(* Update and reset functionality *)
AddKeyWarning[key_String, value_, type_String : ""] :=
Block[{},
	If[MissingQ[FirstPosition[warningKeyList[[;;,1]], key]],
		(* Key not present, add *)
		AppendTo[warningKeyList, {key, value, type}]
	];
];

ResetKeyWarnings[] := warningKeyList = {};

warningThreadMismatch = {};
ThreadMismatchWarning[cores_Integer, threads_Integer] := warningThreadMismatch = {cores, threads};
ResetThreadMismatchWarning[] := warningThreadMismatch = {};

warningILPIQSizeMismatch = {};
ILPIQSizeMismatchWarning[iq_Integer, ilpwindowsize_Integer] := warningILPIQSizeMismatch = {iq, ilpwindowsize};
ResetILPIQSizeMismatchWarning[] := warningILPIQSizeMismatch = {}; 

(* Keep track of nesting level *)
nestedCheck = 0;

(* We create a new Block-lyke structure that, when exiting on the lowest nesting-level displays the warnings that have been generated*)
BlockCheck[var_List, body_] :=
Block[{localBlock, ret, str},
	(* Initialization when nesting level is 0.*)
	If[nestedCheck == 0,
		(* Reset warnings on level 0 *)
		ResetKeyWarnings[];
		ResetThreadMismatchWarning[];
		ResetILPIQSizeMismatchWarning[]; 
	];
	(* Increase nesting level *)
	nestedCheck += 1;
	(* Create Block with user-defined variables and body *)
	localBlock[] := Block[var, body];
	(* Call user-defined Block *)
	ret = localBlock[];
	(* Decrease nesting level *)
	nestedCheck -= 1;
	(* Finish and show warnings if nesting level is 0 *)
	If[nestedCheck == 0,
		If[Length[warningKeyList] != 0,
			(* Create warning string *)
			str = "Warning: The following keys have a potential invalid value: ";
			Do[
				str = str <> elem[[1]] <> " " <> elem[[3]] <> " value: " <> ToString[elem[[2]]] <> ", ";
				, {elem, warningKeyList}
			];
			str = StringDrop[str, -2];
			str = str <> ". Please verify correctness of value and model result.";
			Message[BlockCheck::Warning, str];
		];
		If[Length[warningThreadMismatch] == 2,
			(* Create warning string *)
			str = "Warning: the workload specifies " <> ToString[warningThreadMismatch[[2]]] <> " threads while only " <> ToString[warningThreadMismatch[[1]]] <> " cores are configured.";
			Message[BlockCheck::Warning, str];
		];
		If[Length[warningILPIQSizeMismatch] == 2,
			(* Create warning string *)
			str = "Warning: ILP values of the workload are for a window size of " <> ToString[warningILPIQSizeMismatch[[2]]] <> " entries while the architecture is configured with an instruction queue of size " <> ToString[warningILPIQSizeMismatch[[1]]] <> " entries.";
			Message[BlockCheck::Warning, str];
		];
	];
	(* Return user value *)
	Return[ret];
];
SetAttributes[BlockCheck, HoldAll];
BlockCheck::Warning = "`1`";

(* Version of GetKeyValue that performs a check on the content and raises a warning if the value has certain properties. *)
(* This is useful to check if some key parameters are zero for example, which may lead to incomplete or incorrect modeling *)
Options[GetKeyValueCheck] = {
	"defaultValueFunction" -> None
}
GetKeyValueCheck[keyValueList_List, key_String, pattern : Except[_Rule] : (n_ /; n == 0), OptionsPattern[]] :=
Block[{val, defaultFun},
	val = GetKeyValue[keyValueList, key];
	defaultFun = OptionValue["defaultValueFunction"];
	(* Check and show default value warning first. If not default value, check for match with pattern *)
	If[Head[defaultFun] === Symbol && defaultFun =!= None,
		If[MatchQ[val, defaultFun[key]], (* Separate IF statement to prevent evaluation if defaultFun is not valid *)
			AddKeyWarning[key, val, "default"];
			Return[val]; (* Return to only generate one warning if deault matches pattern *)
		]
	];
	If[MatchQ[val, pattern],
			AddKeyWarning[key, val];
	];
	Return[val];
];
(*KeyValueListCheck::value = "Warning: Key \"`1`\" is set to \"`2`\". Please verify correctness of value and model result."
KeyValueListCheck::defaultvalue = "Warning: Key \"`1`\" is set to the default value \"`2`\". Please verify correctness of value and model result."*)

GetAlgKeyValueCheck[keyValueList_List, key_String, pattern : Except[_Rule] : (n_ /; n == 0)] := GetKeyValueCheck[keyValueList, key, pattern, defaultValueFunction -> GetAlgorithmDefaultValue]
(* we cannot check the default values of architecture properties as the likelyhood of them matching the defaults is too large. GetArchitectureDefaultValue would be valid though *)
GetArchKeyValueCheck[keyValueList_List, key_String, pattern : Except[_Rule] : (n_ /; n == 0)] := GetKeyValueCheck[keyValueList, key, pattern, defaultValueFunction -> None] 

End[] (* End Private Context *)

EndPackage[]