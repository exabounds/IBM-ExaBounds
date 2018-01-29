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

BeginPackage["PreDefinedMemorySpec`"]
(* Exported symbols *)

Needs["ExaBoundsGeneric`"];

(* SP what does this do? maybe not necessary here? *)
predefinedconfigMem::usage =
"predefinedconfigMem[ configname_ ]"

predefinedConfigID2NameMem::usage =
"Maps config ID to clear name"

LoadPreDefinedMem::usage =
"LoadPreDefinedMemh[]"

LoadMemJSON::usage =
"LoadMemJSON[file_]"

ExportMemConfigs::usage =
"ExportMemConfigs[dir_String]"

Begin["`Private`"] (* Begin Private Context *) 

predefinedConfigID2NameMem = {};
configs = {};

(* Load all predifined memory specifications in the Architectures\\DefaultMemory subdirectory *)
LoadPreDefinedMem[] :=
Block[{path, files},
	path = FileNameJoin[{ExaBoundsDirectory[], "Architectures", "DefaultMemory"}];
	files = FileNames["*.json", path];
	LoadMemJSON[#] & /@ files;
];

LoadMemJSON[file_] :=
Block[{data, id, name, config},
	data = Check[Import[file, "JSON"], Message[LoadJSON::invalidjson, file]; Return[]];
	id = OptionValue[data, "id"];
	name = OptionValue[data, "name"];
	config = OptionValue[data, "config"];
	config = config /. (a_ -> b_) -> {a, b}; (* Convert back fron replacement list to list of tuples *)
	(* TODO: check for valid configuration *)
	AppendTo[predefinedConfigID2NameMem, id -> name];
	AppendTo[configs, id -> config];
];

(* Select configuration with given name *)
predefinedconfigMem[configname_] := 
  Module[{settings},
   settings = OptionValue[configs, configname];
   
   Return[ settings ]
  ];
  
(* Export commands to generate JSON files from configs *)
ExportMemConfigs[dir_String] :=
Block[{settings, jsonobj},
	Do[
		ExportMemConfigs[dir, arch[[1]]]
	, {arch, predefinedConfigID2NameMem}];
];

ExportMemConfigs[dir_String, arch_String] :=
Block[{settings, jsonobj},
	settings = OptionValue[configs, arch]/.{a_,b_}->a->b; (* Get config and convert to replacement list *)
	jsonobj = {"id" -> arch, "name" -> OptionValue[predefinedConfigID2NameMem, arch], "config" -> settings};
	Export[FileNameJoin[{dir, arch<>".json"}], jsonobj, "JSON"];
];
  
End[] (* End Private Context *)

EndPackage[]

LoadPreDefinedMem[];