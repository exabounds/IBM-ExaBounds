(* Mathematica Package *)

BeginPackage["PreDefinedNetworkConfigs`"]
(* Exported symbols added here with SymbolName::usage *)  

Needs["ExaBoundsGeneric`"];

(* SP what does this do? maybe not necessary here? *)
GetNetworkConfig::usage =
"GetNetworkConfig[ configname_ ]"

predefinedConfigID2NameNetwork::usage =
"Maps config ID to clear name"

LoadPreDefinedNetwork::usage =
"LoadPreDefinedNetwork[]"

LoadNetworkJSON::usage =
"LoadNetworkJSON[file_]"

ExportNetworkConfigs::usage =
"ExportNetworkConfigs[dir_String]"

Begin["`Private`"] (* Begin Private Context *) 

predefinedConfigID2NameNetwork = {};
configs = {};

(* Load all predifined memory specifications in the Architectures\\DefaultMemory subdirectory *)
LoadPreDefinedNetwork[] :=
Block[{path, files},
	path = FileNameJoin[{ExaBoundsDirectory[], "Architectures", "DefaultNetwork"}];
	files = FileNames["*.json", path];
	LoadNetworkJSON[#] & /@ files;
];

LoadNetworkJSON[file_] :=
Block[{data, id, name, config},
	data = Check[Import[file, "JSON"], Message[LoadJSON::invalidjson, file]; Return[]];
	id = OptionValue[data, "id"];
	name = OptionValue[data, "name"];
	config = OptionValue[data, "config"];
	config = config /. (a_ -> b_) -> {a, b}; (* Convert back fron replacement list to list of tuples *)
	PrependTo[config, {"networkConfiguration", id}]; (* We add the id name as well to the config, as it is needed to identify it in the ExaBoundsState structure*)
	(* TODO: check for valid configuration *)
	AppendTo[predefinedConfigID2NameNetwork, id -> name];
	AppendTo[configs, id -> config];
];

(* Select configuration with given name *)
GetNetworkConfig[configname_] := 
  Block[{settings},
   settings = OptionValue[configs, configname];
   Return[ settings ];
  ];
  
(* Export commands to generate JSON files from configs *)
ExportNetworkConfigs[dir_String] :=
Block[{settings, jsonobj},
	Do[
		ExportNetworkConfigs[dir, arch[[1]]]
	, {arch, predefinedConfigID2NameNetwork}];
];

ExportNetworkConfigs[dir_String, arch_String] :=
Block[{settings, jsonobj},
	settings = OptionValue[configs, arch]/.{a_,b_}->a->b; (* Get config and convert to replacement list *)
	jsonobj = {"id" -> arch, "name" -> OptionValue[predefinedConfigID2NameNetwork, arch], "config" -> settings};
	Export[FileNameJoin[{dir, arch<>".json"}], jsonobj, "JSON"];
];
  
End[] (* End Private Context *)

EndPackage[]

LoadPreDefinedNetwork[]