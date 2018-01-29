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

BeginPackage["CommunicationPatternVisualization`",{ "ExaBoundsGeneric`", "AlgorithmProperties`"}]
(* Exported symbols added here with SymbolName::usage *)  
	
(* Visualize Graph from ExaBoundsAlgInfo *)
GenerateCommunicationGraph::usage =
	"GenerateCommunicationGraph[executionProfile_,
			\"WeightFunction\"->\"Messages\", \"Clustering\"->Null, \"Coloring\"->\"Cluster\"]"
	
(* Returns a function that, given a value within min and max, returns a color. *)
ColoringFunction::usage =
	"ColoringFunction[min_, max_]"

Begin["`Private`"] (* Begin Private Context *) 

(* Generate graph structure from executionProfile. TODO: generalize in the case each process has multiple threads *)
Options[GenerateCommunicationGraph] = {
	"WeightFunction" -> "Messages",	(* Either "Messages" or "Bytes" *)
	"Clustering" -> Null,			(* Clustering to be used for node coloring *)
	"ColoringMetric" -> "Cluster"	(* What if one whant to color nodes based on a metric, e.g. LSys? *)
};
GenerateCommunicationGraph[executionProfile_,OptionsPattern[]] := Module[{
			graphStructure,graph,mpiCommVector,k,e,from,to,weight,
			pIds,clusterIds,cId,coloring,
			weightId,clustering,coloringMetric,coloringFunction,maxPid},
	(* Check if MPIcommunicationVector is not available (either partial profile or PCA profile) *)
	If[!MemberQ[executionProfile[[1,All,1]],"MPIcommVector"],
		Return[Nothing];
	];
	
	(* Initialization *)		
	weightId = OptionValue["WeightFunction"];
	coloringMetric = OptionValue["ColoringMetric"];
	clustering = OptionValue["Clustering"];
	
	
	If[weightId  == "Messages",
		weightId = 3;
		,If[weightId  == "Bytes",
			weightId = 2;
			,
			weightId = 0;
		];
	];
	
	If[coloringMetric == "Cluster" && ListQ[clustering],
		coloringFunction = ColoringFunction[1,Length[clustering]];
		,
		coloringFunction = Null;
	];
	
	(* Compute the graph *)
	mpiCommVector = GetKeyValue[#,"MPIcommVector"]& /@ KeySort[executionProfile];
	
	maxPid = Max[Keys[mpiCommVector][[All,1]] ];
	
	graphStructure = Reap[Do[
		to = k[[1]];
		Do[(* We use relative ids. Read as (pId=to): OptionValue[#,"source"] - pId  *)
		   (* We use modulus operator not to have negative numbers in the ids  *)
			from = Mod[to + e[[1]] , maxPid+1 ];
			If[weightId>0,
				weight = e[[weightId]];
				,
				weight = 0;
			];
			Sow[{DirectedEdge[from,to],weight}];
		,{e,mpiCommVector[k]}];
	,{k,Keys[mpiCommVector]}];
	][[2]] ;
	If[Length[graphStructure]>=1,
		graphStructure = graphStructure[[1]] // Transpose;
		,
		graphStructure = {{},{}};
	];
	(*graphStructure = SortBy[graphStructure,First];*)
	
	(* Compute the coloring *)
	Style[coloringFunction];
	If[coloringFunction =!= Null,
		pIds = Keys[executionProfile] [[All,1]];
		
		clusterIds = clustering[[All,All,1]];
	
		
		coloring = Reap[
			Do[
				cId = SelectFirst[Range[Length[clusterIds]], MemberQ[clusterIds[[#]],k ] & ];
				Sow[k->coloringFunction[cId]];
			,{k,pIds}];
		][[2,1]];
		,
		coloring = Automatic;
	];
	
	(* Visualize the graph *)
	If[weightId>0,
		graph = Graph[graphStructure[[1]]
					,EdgeWeight->graphStructure[[2]] 
					,VertexLabels -> Table[i->i,{i,0,Max[ Keys[mpiCommVector][[All,1]] ]}]
					,VertexSize -> 0.3
					,VertexStyle->coloring
					(*,EdgeLabels -> "EdgeWeight"*)
				];
		,
		graph = Graph[graphStructure[[1]]];
	];
	
	Return[graph];
];

(*
PrintCommunicationGraph[algInfo_, algoIndex_: 1,scalingConfiguration_: 1, dataProfileId_: 1] := Module[{graph,scalingConfigurationAssociation,executionProfile},
	If[IntegerQ[scalingConfiguration],(* scalingParameter shall be either an association or an integer index. *) 
 		scalingConfigurationAssociation = GetScalingConfigurations[algInfo[[algoIndex]]] [[scalingConfiguration]];
 		,
 		scalingConfigurationAssociation = scalingConfiguration;
	];
	
	executionProfile = algInfo[[algoIndex]][scalingConfigurationAssociation][[dataProfileId]];
	
	graph = GenerateCommunicationGraph[executionProfile];
	Return[graph];
];
*)

ColoringFunction[min_, max_] := Function[value,
	If[IntegerQ[min] && IntegerQ[max] && ( (max-min+1) < 10 ),
		ColorData[3,"ColorList"][[value]]
		,
		ColorData["VisibleSpectrum"][(value - min)/(If[max!=min,max - min,1])
		* (750.-380.) + 380. ]
	]
];

End[] (* End Private Context *)

EndPackage[]