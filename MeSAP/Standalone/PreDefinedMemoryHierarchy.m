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

BeginPackage["PreDefinedMemoryHierarchy`"]
(* Exported symbols *)

(*Needs["ExaBoundsGeneric`"];*)

(* SP what does this do? maybe not necessary here? *)
predefinedconfigArch::usage =
"predefinedconfigArch[ configname_ ]"

predefinedConfigID2NameArch::usage =
"Maps config ID to clear name"

Begin["`Private`"] (* Begin Private Context *) 

(* ::Text::Italic:: *)
(* Pre-defined Memory CPUlevel Specifications *)

predefinedConfigID2NameArch = {
  "Spec1" -> "Specification1 (CPU freq and memory hierarchy)",
  "Spec2" -> "same as Specification1 (CPU freq and memory hierarchy)"
}

(* list of rules to substitute a config for its name *)
configs = {
	
	"Spec1" ->
     {
        {"MemorySizeGB", 32 }, (* GB *)
     	{"nDIMMs", 4 },
     	{"nRanksPerDIMM", 2 }, 
     	{"CacheLineSizeBytes", 64 }, (* bytes *)
     	{"CpuFreqMHz", 2600 } (* MHz *)	
     },
	
   
     (* EMPTY Spec
     "" ->
     {
     	{"MemorySizeGB", }, (* GB *)
     	{"nDIMMs", },
     	{"nRanksPerDIMM", }, 
     	{"CacheLineSizeBytes", }, (* bytes *)
     	{"CpuFreqMHz", } (* MHz *)
     },*)
     
	"Spec2" ->
     {
     	{"MemorySizeGB", 32 }, (* GB *)
     	{"nDIMMs", 4 },
     	{"nRanksPerDIMM", 2 }, 
     	{"CacheLineSizeBytes", 64 }, (* bytes *)
     	{"CpuFreqMHz", 2600 } (* MHz *)
     }
       

     (*,
     
     _ ->*) (* default *)
     (*{"ERROR: config name not found", ""}*)
}

(* SP what does this do? *)
predefinedconfigArch[configname_] := 
  Module[{settings},
   settings = OptionValue[configs, configname];
   
   Return[ settings ]
  ];
  
End[] (* End Private Context *)

EndPackage[]