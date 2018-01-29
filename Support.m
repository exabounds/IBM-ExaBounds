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

BeginPackage["Support`"]
(* Exported symbols added here with SymbolName::usage *)

ClearCache::usage =
  "ClearCache[f_]"

ExportImageAsVectorDialogButton::usage = 
  "ExportImageAsSVGDialogButton[graphic_]"
  
ExportImageAsVectorDialog::usage = 
  "ExportImageasSVGDialog[graphic_]"  
  
LinearInterpolation::usage =
	"LinearInterpolation[monotonicSortedPointList_,xIndex_,at_].
	Points are two dimensions: {x,y} if xIndex=1, or {y,x} otherwise.
	It returns the value of y at any value of x (at_) by interpolating the list of points.
	"

Begin["`Private`"] (* Begin Private Context *) 

(* Clear downvalues of a function *)
ClearCache[f_] :=  DownValues[f] = DeleteCases[DownValues[f], _?(FreeQ[First[#], Pattern] &)]

(* Routine which displays an export as vector button *)
ExportImageAsVectorDialogButton[graphic_] := Button["Export as vector graphic", ExportImageAsVectorDialog[graphic], ImageSize->150, Method->"Queued"];

(* Simple routine which accepts a graphic object, displays a file save dialog and exports an PDF or SVG *)
ExportImageAsVectorDialog[graphic_] :=
Block[{filename},
	filename = SystemDialogInput["FileSave", {"", {"Portable document format (pdf)"->{"*.pdf"}, "Scalable vector graphics (SVG)"->{"*.svg"}}}, WindowTitle->"Export vector graphic"];
	
	If[filename === $Canceled, Return[]];
	
	Export[filename, graphic];
];

(* linear interpolation for managing the memFunctions *)
LinearInterpolation[monotonicSortedPointList_,xIndex_,at_] :=
Module[{before,after,yIndex,iterBefore,iterAfter},
	yIndex = If[xIndex==1,2,1];
	
	iterAfter = SelectFirst[Range[Length[monotonicSortedPointList]],monotonicSortedPointList[[#,xIndex]] >= at&];
	If[MissingQ[iterAfter],
		after = monotonicSortedPointList[[-1]];
		iterAfter = Length[monotonicSortedPointList]+1;
		,
		after = monotonicSortedPointList[[iterAfter]];
	];
	
	Which[iterAfter === 1,
		iterBefore = iterAfter;
		
		,
		iterAfter == Length[monotonicSortedPointList]+1,
		iterAfter = Length[monotonicSortedPointList];
		iterBefore = iterAfter;
		
		,
		True,
		iterBefore = iterAfter -1;
	];
	before = monotonicSortedPointList[[iterBefore]];
	
					
	If[before[[xIndex]] == after[[xIndex]],
		before[[yIndex]]
		,
		before[[yIndex]] + ((at-before[[xIndex]])*(after[[yIndex]]-before[[yIndex]]) / (after[[xIndex]]-before[[xIndex]]))
	]
];

End[] (* End Private Context *)

EndPackage[]