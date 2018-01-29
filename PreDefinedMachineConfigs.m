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
 
 (* ::Package:: *)

(* Mathematica Package *)

BeginPackage["PreDefinedMachineConfigs`"]
(* Exported symbols *)

Needs["ExaBoundsGeneric`"];

predefinedconfig::usage =
"predefinedconfig[ configname_ ]"

predefinedConfigID2Name::usage =
"Maps config ID to clear name"

ExportMachineConfigs::usage =
"ExportMachineConfigs[dir_String]"

LoadPreDefinedArch::usage =
"LoadPreDefinedArch[]"

LoadArchJSON::usage =
"LoadArchJSON[file_]"

Begin["`Private`"] (* Begin Private Context *) 

(* ::Text::Italic:: *)
(* Pre-defined Machine Configurations *)

predefinedConfigID2Name = {};
configs = {};

(* Load all predifined memory specifications in the Architectures\\DefaultMemory subdirectory *)
LoadPreDefinedArch[] :=
Block[{path, files},
	path = FileNameJoin[{ExaBoundsDirectory[], "Architectures", "DefaultCompute"}];
	files = FileNames["*.json", path];
	LoadArchJSON[#] & /@ files;
];

LoadArchJSON[file_] :=
Block[{data, id, name, config},
	data = Check[Import[file, "JSON"], Message[LoadJSON::invalidjson, file]; Return[]];
	id = OptionValue[data, "id"];
	name = OptionValue[data, "name"];
	config = OptionValue[data, "config"];
	config = config /. (a_ -> b_) -> {a, b}; (* Convert back fron replacement list to list of tuples *)
	(* TODO: check for valid configuration *)
	AppendTo[predefinedConfigID2Name, id -> name];
	AppendTo[configs, id -> config];
];

predefinedconfig[configname_] := 
  Module[{settings, n0, f0},
   settings = OptionValue[configs, configname];
   
   (* GED: set n1 through n3 -- why? *)
   (* JRI: This determines the sizes of n1,n2,n3 to make the total system performance one exa-op peak... Ugly
   I changed it such that it only does this if the configname starts with "Exa-OP" *)
   If[StringMatchQ[configname, "Exa-OP"~~___],
     f0 = GetKeyValue[ settings, "f0" ];
     settings = Join[settings, {{"n1", 1}, {"n2", 1}, {"n3", 10^18/(f0*n0)}}];
   ];
   Return[ settings ]
  ];
  
ExportMachineConfigs[dir_String] :=
Block[{settings, jsonobj},
	Do[
		ExportMachineConfigs[dir, arch[[1]]]
	, {arch, predefinedConfigID2Name}];
];

ExportMachineConfigs[dir_String, arch_String] :=
Block[{settings, jsonobj},
	settings = OptionValue[configs, arch]/.{a_,b_}->a->b; (* Get config and convert to replacement list *)
	jsonobj = {"id" -> arch, "name" -> OptionValue[predefinedConfigID2Name, arch], "config" -> settings};
	Export[FileNameJoin[{dir, arch<>".json"}], jsonobj, "JSON"];
];
  

End[] (* End Private Context *)

EndPackage[]

LoadPreDefinedArch[]