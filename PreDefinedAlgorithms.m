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

BeginPackage["PreDefinedAlgorithms`"]
(* Exported symbols added here with SymbolName::usage *)

Needs["ExaBoundsGeneric`"]; (* Generic package for all CDF/PDF functions *)
Needs["AlgorithmProperties`"];  

LoadPreDefinedAlgorithms::usage = 
  "LoadPreDefinedAlgorithms[algInfoStructure_]"

Begin["`Private`"] (* Begin Private Context *)

(* Append algorithms to alInfostructure *)
(* NOTE: we do not append data to ExaBoundsState as was don in the original notebook, this data does not belong there *)
LoadPreDefinedAlgorithms[algInfoStructure_] :=
Module[{},
	(* Load two default algorithms *)
	Quiet[AddAlgorithm[algInfoStructure, FileNameJoin[{ExaBoundsDirectory[], "Algorithms", "Default", "graph500-seq-csr-s15-e16.out"}]], {AlgorithmFromFileJSON`LoadAlgorithmJSON::missingproperties, InstructionMix`TestInstrMix::nm}];
	Quiet[AddAlgorithm[algInfoStructure, FileNameJoin[{ExaBoundsDirectory[], "Algorithms", "Default", "graph500-seq-list-s15-e16.out"}]], {AlgorithmFromFileJSON`LoadAlgorithmJSON::missingproperties, InstructionMix`TestInstrMix::nm}];
];
SetAttributes[LoadPreDefinedAlgorithms,HoldFirst];

End[] (* End Private Context *)

EndPackage[]