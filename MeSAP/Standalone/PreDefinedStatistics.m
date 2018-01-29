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

BeginPackage["PreDefinedStatistics`"]
(* Exported symbols *)

(*Needs["ExaBoundsGeneric`"];*)

(* SP what does this do? maybe not necessary here? *)
predefinedconfigStat::usage =
"predefinedconfigStat[ configname_ ]"

predefinedConfigID2NameStat::usage =
"Maps config ID to clear name"

Begin["`Private`"] (* Begin Private Context *) 

(* ::Text::Italic:: *)
(* Pre-defined Statistics *)

predefinedConfigID2NameStat = {
  "adi_ref" -> "adi_ref (polybench)",
  "atax_ref" -> "atax_ref (polybench)"
}

(* list of rules to substitute a config for its name *)
configs = {
	
	"adi_ref" ->
     {
  		{"TotalCycles", 18946600000},
     	{"nReadBytes", 9121820000},
     	{"nWriteBytes", 2724930000 }
     },
	
   
     (* EMPTY Spec
     "" ->
     {
     	{"TotalCycles", },
     	{"nReadBytes", },
     	{"nWriteBytes", }
     },*)
     
     "nothing" ->
     {
  		{"TotalCycles", 1171704041},
     	{"nReadBytes", 0},
     	{"nWriteBytes", 0}
     },

	"atax_ref" ->
     {
  		{"TotalCycles",427019000 },
     	{"nReadBytes",121286000 },
     	{"nWriteBytes",70890400 }
     },
     
    "bicg_ref" ->
     {
     	{"TotalCycles",647974000 },
     	{"nReadBytes",259147000 },
     	{"nWriteBytes",129606000 }
     },
     
    "covariance_ref" ->
     {
     	{"TotalCycles",5134330000 },
     	{"nReadBytes",12873000 },
     	{"nWriteBytes",6468640 }
     },

    "doitgen_ref" ->
     {
     	{"TotalCycles",1164270000 },
     	{"nReadBytes",24051900 },
     	{"nWriteBytes",12226200 }
     },
     
    "durbin_ref" ->
     {
     	{"TotalCycles",693130000 },
     	{"nReadBytes",160792000 },
     	{"nWriteBytes",192984000 }
     },
     	
    "dynprog_ref" ->
     {
     	{"TotalCycles",765156000 },
     	{"nReadBytes",3313060 },
     	{"nWriteBytes",1411230 }
     },
     
    "fdtd-2d_ref" ->
     {                 
     	{"TotalCycles",1176460000},
     	{"nReadBytes",18580200 },
     	{"nWriteBytes",5467690}
     },
 
     "floyd-warshall_ref" ->
     {
     	{"TotalCycles",4568560000 },
     	{"nReadBytes", 20615800 },
     	{"nWriteBytes",6878660 }
     },
     
     "gemm_ref" ->
     {
     	{"TotalCycles",26819600000},
     	{"nReadBytes",235242000},
     	{"nWriteBytes",118023000}
     },
     
     "gemver_ref" ->
     {
     	{"TotalCycles",1123390000 },
     	{"nReadBytes",243643000 },
     	{"nWriteBytes",162511000 }
     },
     
     "gesummv_ref" ->
     {
     	{"TotalCycles",831137000 },
     	{"nReadBytes",384630000 },
     	{"nWriteBytes",256474000 }
     },

     "jacobi-1d-imper_ref" ->
     {
     	{"TotalCycles",28442800 },
     	{"nReadBytes",5718990 },
     	{"nWriteBytes",2973430 }
     },

     "jacobi-2d-imper_ref" ->
     {
     	{"TotalCycles",295553000 },
     	{"nReadBytes",11469700 },
     	{"nWriteBytes",4577360}
     },

     "ludcmp_ref" ->
     {
     	{"TotalCycles",2494850000 },
     	{"nReadBytes",9204930 },
     	{"nWriteBytes",26896 }
     },

     "lu_ref" ->
     {
     	{"TotalCycles",1325520000 },
     	{"nReadBytes",6868590 },
     	{"nWriteBytes",2297360 }
     },

     "mvt_ref" ->
     {
     	{"TotalCycles",554861000 },
     	{"nReadBytes",68193800 },
     	{"nWriteBytes",51156000 }
     },

     "reg_detect_ref" ->
     {
     	{"TotalCycles",52076500 },
     	{"nReadBytes",174720 },
     	{"nWriteBytes",174721 }
     },

     "symm_ref" ->
     {
     	{"TotalCycles",4968200000 },
     	{"nReadBytes",34299400 },
     	{"nWriteBytes",6905520 }
     },

     
     "syr2k_ref" ->
     {
     	{"TotalCycles",12226900000},
     	{"nReadBytes",27494500 },
     	{"nWriteBytes",13764900}
     },
     
     "syrk_ref" ->
     {
     	{"TotalCycles",5746620000 },
     	{"nReadBytes",13750600 },
     	{"nWriteBytes",1886210 }
     },
     "trisolv_ref" -> (*PISA could not profile it*)
     {
     	{"TotalCycles", 953783170},
     	{"nReadBytes", 192303040},
     	{"nWriteBytes", 128065984}
     },

     "trmm_ref" ->
     {
     	{"TotalCycles",2878410000 },
     	{"nReadBytes",13711300 },
     	{"nWriteBytes",6875750 }
     },

     "seidel-2d_ref" ->
     {
     	{"TotalCycles",535307000 },
     	{"nReadBytes",6131710 },
     	{"nWriteBytes",2142220 }
     },
    (* further not required for ExaBounds *)
     
       "h263encode" ->
     {
     	{"TotalCycles", 525817104},
     	{"nReadBytes", 418808},
     	{"nWriteBytes", 94048}
     },
     
       "core1" ->
     {
     	{"TotalCycles", 2600000000},
     	{"nReadBytes", 5539094000},
     	{"nWriteBytes", 2344293000}
     }
     ,
     
       "core2" ->
     {
     	{"TotalCycles", 2600000000},
     	{"nReadBytes", 9780708000},
     	{"nWriteBytes", 4138209000}
     },
     
       "core4" ->
     {
     	{"TotalCycles", 2600000000},
     	{"nReadBytes", 17620830000},
     	{"nWriteBytes", 7470252000}
     },
     
       "core8" ->
     {
     	{"TotalCycles", 2600000000},
     	{"nReadBytes", 26139180000},
     	{"nWriteBytes", 11319550000}
     },
     
       "core12" ->
     {
     	{"TotalCycles", 2600000000},
     	{"nReadBytes", 27225340000},
     	{"nWriteBytes", 11858460000}
     },
     
       "core14" ->
     {
     	{"TotalCycles", 2600000000},
     	{"nReadBytes", 27587470000},
     	{"nWriteBytes", 12036570000}
     },
     
       "core16" ->
     {
     	{"TotalCycles", 2600000000},
     	{"nReadBytes", 27409650000},
     	{"nWriteBytes", 11957420000}
     },
     
       "core24" ->
     {
     	{"TotalCycles", 2600000000},
     	{"nReadBytes", 28113600000},
     	{"nWriteBytes", 12284710000}
     },
     
       "core28" ->
     {
     	{"TotalCycles", 2600000000},
     	{"nReadBytes", 28115550000},
     	{"nWriteBytes", 12282030000}
     }








     
     (*,
     
     _ ->*) (* default *)
     (*{"ERROR: config name not found", ""}*)
}

(* SP what does this do? *)
predefinedconfigStat[configname_] := 
  Module[{settings},
   settings = OptionValue[configs, configname];
   

   Return[ settings ]
  ];
  
End[] (* End Private Context *)

EndPackage[]
