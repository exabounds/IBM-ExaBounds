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

BeginPackage["CommunicationModel`"]
(* Exported symbols added here with SymbolName::usage *)

Needs["ExaBoundsGeneric`"]
Needs["Support`"]

ExampleFunction::usage =
  "ExampleFunction[par1_]"



Begin["`Private`"] (* Begin Private Context *) 

(* Clear downvalues of a function *)
ExampleFunction[par1_] := Module[{},
	Print[ToString[par1]];
	Return[1];
]

End[] (* End Private Context *)

EndPackage[]