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

BeginPackage["DSEVisualization`"]
(* Exported symbols added here with SymbolName::usage *)

ShowTooltips::usage = 
  "ShowTooltips[x_, tooltips_]"
  
ExtractParetoPoints::usage =
  "ExtractParetoPoints[list_, tooltip_List : {}]"

ParetoPlot::usage = 
  "ParetoPlot[data_List, keys_List : {}]"  

Begin["`Private`"] (* Begin Private Context *)

(* Add tooltips to plot data *)
Options[ShowTooltips] = {
	"TooltipPrefix" -> "Point"
};
ShowTooltips[data_, tooltips_, OptionsPattern[]] :=
Block[{prefix, list},
	prefix = OptionValue["TooltipPrefix"];
	If[!StringQ[prefix], Message[ShowTooltips::notstring]; Return[data]];
	
	list = {};	
	AppendTo[list, Tooltip[#[[1]],prefix <> ": " <> ToString[#[[2]]]]] & /@ Transpose[{data,tooltips}];
	Return[list];
];
(* Warnings *)
ShowTooltips::notstring = "Prefix is not of type String"

(* Extract pareto points from list: return a tuple of lists. One with pareto points, one without. Also, split tooltips if needed*)
ExtractParetoPoints[list_, tooltip_List : {}] :=
Block[{paretoPoints, otherPoints, tooltipList, paretoTooltip, otherTooltip},
	paretoPoints = {};
	otherPoints = {};
	tooltipList = tooltip;
	paretoTooltip = {};
	otherTooltip = {};
	Function[{p},
		(* Find all pareto poins *)
		If[NoneTrue[list, (#[[1]] <= p[[1]] && #[[2]] <= p[[2]] && # != p ) &],
			AppendTo[paretoPoints, p]; (* Add to pareto point list *)
			If[tooltip != {}, AppendTo[paretoTooltip, First[tooltipList]]; tooltipList = Rest[tooltipList]]; (* add tooltip text to pareto tooltip list *)
			, 
			AppendTo[otherPoints, p];
			If[tooltip != {}, AppendTo[otherTooltip, First[tooltipList]]; tooltipList = Rest[tooltipList]];
		]
	] /@ list;
		
	If[tooltip == {},
		Return[{paretoPoints, otherPoints}],
		Return[{paretoPoints, otherPoints, paretoTooltip, otherTooltip}]
	];
];

Options[ParetoPlot] = {
	"PlotType" -> "ListLogLogPlot",
	"ShowParetoPoints" -> True,
	"ParetoPointsMarker" -> {"\[Cross]", 18},
	
	"TooltipPrefix" -> "Point",

	(* All default options for the plots, such that they can be controlled by the user *)	
	"AspectRatio" -> 1/GoldenRatio,
	"Axes" -> True,
	"AxesLabel" -> None,
	"AxesOrigin" -> Automatic,
	"BaseStyle" -> Automatic,
	"DataRange" -> Automatic,
	"Filling" -> None,
	"FillingStyle" -> Automatic,
	"Frame" -> False,
	"FrameLabel" -> None,
	"FrameTicksStyle" -> Automatic,
	"Joined" -> False,
	"LabelingFunction" -> Automatic,
	"PerformanceGoal" -> $PerformanceGoal,
	"PlotLabel" -> "",
	"PlotLegends" -> Automatic,
	"PlotMarkers" -> Automatic, (*{"\[FilledCircle]", 15},*)
	"PlotRange" -> Automatic,
	"PlotRangeClipping" -> True,
	"PlotStyle" -> Automatic,
	"PlotTheme" -> $PlotTheme,
	"TargetUnits" -> Automatic
	
};
ParetoPlot[data_List, tooltip_List : {}, OptionsPattern[]] :=
Block[{plotType, showParetoPoints, paretoPointsMarker, paretoData, paretoPoints, otherPoints, basePlot, paretoPointPlot, plotSequence, paretoTooltip, otherTooltip, axesOrigin, plotLegends,otherLegend, paretoLegend},
	(* Get options and check values *)
	plotType = OptionValue["PlotType"];
	If[!AnyTrue[{"ListPlot", "ListLogPlot", "ListLogLinearPlot", "ListLogLogPlot"}, StringMatchQ[plotType, #] &], Message[ParetoPlot::invalidplottype, plotType]; Return[Nothing]];
	showParetoPoints = OptionValue["ShowParetoPoints"];
	If[!(showParetoPoints === True || showParetoPoints === False), Message[ParetoPlot::nottruefalse, "ShowParetoPoints"]; Return[Nothing]];
	paretoPointsMarker = OptionValue["ParetoPointsMarker"];
	axesOrigin = OptionValue["AxesOrigin"];
	plotLegends = OptionValue["PlotLegends"];
	
	(* Extract pareto points *)
	If[showParetoPoints,
		paretoData = ExtractParetoPoints[data, tooltip];
		paretoPoints = paretoData[[1]];
		otherPoints = paretoData[[2]];
		If[tooltip != {},
			paretoTooltip = paretoData[[3]];
			otherTooltip = paretoData[[4]];
			,
			paretoTooltip = {};
			otherTooltip = {};
		];
		,
		otherPoints = data;
		otherTooltip = tooltip;
	];
	
	(* Override Automatic AxesOrigin for pareto plot as it otherwise can show a line through the plots for ListLogLogPlot or ListLogPlot*)
	If[axesOrigin === Automatic,
		axesOrigin = Switch[plotType,
							"ListLogPlot",
							{Min[data[[;;,1]]], Log[Min[data[[;;,2]]]]},
							"ListLogLogPlot",
							{Log[Min[data[[;;,1]]]], Log[Min[data[[;;,2]]]]},					
							_,
							Automatic
		]
	];
	
	(* Override Automatic PlotLegends for pareto plots and split up normal legend in two*)
	If[showParetoPoints,
		If[plotLegends === Automatic,
			otherLegend = {"Points"};
			paretoLegend = {"Pareto-optimal points"};
			,
			
			otherLegend = {plotLegends[[1]]};
			paretoLegend = {plotLegends[[2]]};
		],
		otherLegend = plotLegends;
	];

	(* Create the base plot with non-pareto points (but only if we have those points*)
	If[otherPoints != {},
		basePlot = Symbol[plotType][
						If[otherTooltip == {}, otherPoints, ShowTooltips[otherPoints, otherTooltip, TooltipPrefix -> OptionValue["TooltipPrefix"]]],
						DataRange -> OptionValue["DataRange"],
						Filling -> OptionValue["Filling"],
						FillingStyle -> OptionValue["FillingStyle"],
						Joined -> OptionValue["Joined"],
						LabelingFunction -> OptionValue["LabelingFunction"],
						PerformanceGoal -> OptionValue["PerformanceGoal"],
						PlotMarkers -> OptionValue["PlotMarkers"],
						PlotLegends -> otherLegend,
						AxesOrigin -> axesOrigin,
						PlotStyle -> OptionValue["PlotStyle"],
						PlotTheme -> OptionValue["PlotTheme"],
						TargetUnits -> OptionValue["TargetUnits"]
					]		
	];
					
	(* Create the pareto point plot *)
	If[showParetoPoints,
		paretoPointPlot = Symbol[plotType][
					If[paretoTooltip == {}, paretoPoints, ShowTooltips[paretoPoints, paretoTooltip, TooltipPrefix -> OptionValue["TooltipPrefix"]]],
					PlotMarkers -> paretoPointsMarker,
					DataRange -> OptionValue["DataRange"],
					Filling -> OptionValue["Filling"],
					FillingStyle -> OptionValue["FillingStyle"],
					Joined -> OptionValue["Joined"],
					LabelingFunction -> OptionValue["LabelingFunction"],
					PerformanceGoal -> OptionValue["PerformanceGoal"],
					PlotLegends -> paretoLegend,
					AxesOrigin -> axesOrigin,
					PlotStyle -> OptionValue["PlotStyle"],
					PlotTheme -> OptionValue["PlotTheme"],
					TargetUnits -> OptionValue["TargetUnits"]
		];
					
	];
	
	(* Select which plots to show based on availability such that we can combine it in a Show[] statement *)
	If[showParetoPoints,
		If[otherPoints == {},
			plotSequence = paretoPointPlot,
			plotSequence = Sequence[basePlot, paretoPointPlot]
		],
		plotSequence = basePlot
	];

	Return[
		Show[
			plotSequence,
			(* Pass options *)
			AspectRatio -> OptionValue["AspectRatio"],
			Axes -> OptionValue["Axes"],
			AxesLabel -> OptionValue["AxesLabel"],
			AxesOrigin -> axesOrigin,
			BaseStyle -> OptionValue["BaseStyle"],
			Frame -> OptionValue["Frame"],
			FrameLabel -> OptionValue["FrameLabel"],
			FrameTicksStyle -> OptionValue["FrameTicksStyle"],
			PlotLabel -> OptionValue["PlotLabel"],
			PlotRange -> OptionValue["PlotRange"],
			PlotRangeClipping -> OptionValue["PlotRangeClipping"]
		]
	];
];  
(* Warnings *)
ParetoPlot::invalidplottype = "Invalid plot type \"`1`\" for ParetoPlot"
ParetoPlot::nottruefalse = "Value for `1` is not True or False"

End[] (* End Private Context *)

EndPackage[]
