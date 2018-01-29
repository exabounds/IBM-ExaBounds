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

BeginPackage["CPUPerformanceVisualization`"]
(* Exported symbols added here with SymbolName::usage *)  

Needs["ExaBoundsGeneric`"]
Needs["ModelWarnings`"]
Needs["CPUPerformanceModels`"]
Needs["CPUMultithreadedModels`"]
Needs["CPUPowerModels`"]
Needs["Support`"]

VisualizeCPUCorePerformance::usage =
  "VisualizeCPUCorePerformance[archProperties_, algProperties_, homogeneousThreadsOrThreadIndex_]"

SummaryCPUCorePerformance::usage =
  "SummaryCPUCorePerformance[archProperties_, algProperties_, homogeneousThreadsOrThreadIndex_]"

Begin["`Private`"] (* Begin Private Context *)

(* List all constraints + a description. Constraints added are automatically added to the graph *)
PerformanceConstraints = {	
							{LPIssue, "Issue width"},
							{LPMemMSHR, "Miss-status holding registers"},
							{LPILP, "Frequent events saturate ILP"},
							{LPMemFU, "Load/store units"},
							{LPIntFU, "Integer units"},
							{LPFPFU, "Floating-point units"},
							{LPControlFU, "Branch units"},
							{LPVectorFU, "Vector units"},
							{LPDispatch, "Dispatch width"},
							{LPBandwidthCoreToL1, "Core to L1 bandwidth"},
							{LPBandwidthL2ToL3, "L2 to L3 bandwidth"},
							{LPBandwidthL3ToDRAM, "L3 to DRAM bandwidth"},
							(* Last element should always be IQ, ROB, and branch penalty *)
							{Null, "IQ, ROB, and branch misprediction stalls"}
						};
						
ILPTypeConstraints = {	
							{LPILPType, "ILP type int", "int"},
							{LPILPType, "ILP type mem", "mem"},
							{LPILPType, "ILP type control", "control"},
							{LPILPType, "ILP type fp", "fp"}
						};

(*ipcConstraints = {};
penaltyBranch = {};
ipcBranchConstraint = {};
penaltyIQROB = {};
ipcIQROBConstraint = {};
ipcIQROB = {};
ipcBranch = {};
plotCorePerformance = None;*)

PerfConstraints = Join[Drop[PerformanceConstraints, -1], ILPTypeConstraints];

(* Visualize the CPU performance bottlenecks in a nice graph *)
VisualizeCPUCorePerformance[archProperties_List, algProperties_List, homogeneousThreadsOrThreadIndex_Integer : 1] :=
DynamicModule[{threadArchProperties, threadAlgProperties, cachePressures, DRAMPressures, ipc, zoomipc, scrollipc, ipcConstraints, penaltyBranch, ipcBranchConstraint, penaltyIQROB, ipcIQROBConstraint, ipcIQROB, ipcBranch, plotCorePerformance, sel},

(* First we calculate the per-thread properties. As we solve for IPC in this function, we cannot use the appropriate CPUMultithreadedModels function *)
Which[
		AllTrue[algProperties, Head[#] === List &] && homogeneousThreadsOrThreadIndex == 1, (* List + homogeneousThreads == 1 means: 1 thread running on core 1 *)
			threadArchProperties = archProperties; (* Nothing to do, just set the properties as they are passed *)
			threadAlgProperties = algProperties;
		,
		AllTrue[algProperties, Head[#] === List &], (* List + homogeneousThreads != 1: means: we have N homogeneous threads running on cores 1 to N *)
			cachePressures = SolveCachePressure[archProperties, algProperties, homogeneousThreadsOrThreadIndex]; (* Solve effective cache size *)
			If[cachePressures === Null, Return[Null]]; 
			threadArchProperties = SetKeyValue[archProperties, "M0L1", "M0L1" /. cachePressures[[1,2]]];	(* Set correct cache size, all threads are the same *)
			threadArchProperties = SetKeyValue[threadArchProperties, "M0L2", "M0L2" /. cachePressures[[1,2]]];
			threadArchProperties = SetKeyValue[threadArchProperties, "M1", "M1" /. cachePressures[[1,2]]];
			(* Next, we solve for the DRAM BW *)
			DRAMPressures = SolveDRAMBandwidthPressure[threadArchProperties, algProperties, homogeneousThreadsOrThreadIndex];
			Print[DRAMPressures];
			If[DRAMPressures === Null, Return[Null]];
			threadArchProperties = SetKeyValue[threadArchProperties, "B0Dmo", "B0Dmo" /. DRAMPressures[[1,2]]];	(* Set correct BW *)
			threadArchProperties = SetKeyValue[threadArchProperties, "B1Dmo", "B1Dmo" /. DRAMPressures[[1,2]]];
			threadArchProperties = SetKeyValue[threadArchProperties, "B2Dmo", "B2Dmo" /. DRAMPressures[[1,2]]];
			threadAlgProperties = algProperties; (* Set the algorithm*) 
		, (* Now, the replacement list cases *)
		AllTrue[algProperties, Head[#] === Rule &] && Length[algProperties] == 1, (* Length 1 = one thread *)
			threadArchProperties = archProperties;
			threadAlgProperties = algProperties[[1,2]]; (* Set the algorithm*)
		,
		AllTrue[algProperties, Head[#] === Rule &], (* The general case for a replacement rule; heterogeneous threads *)
			sel = FirstPosition[algProperties[[;;,1]], homogeneousThreadsOrThreadIndex];
			If[MissingQ[sel], sel = {1}]; (* Default to one *)
			cachePressures = SolveCachePressure[archProperties, algProperties]; (* Solve effective cache size *) 
			If[cachePressures === Null, Return[Null]];
			threadArchProperties = SetKeyValue[archProperties, "M0L1", "M0L1" /. cachePressures[[sel[[1]],2]]];	(* Set correct cache size *)
			threadArchProperties = SetKeyValue[threadArchProperties, "M0L2", "M0L2" /. cachePressures[[sel[[1]],2]]];
			threadArchProperties = SetKeyValue[threadArchProperties, "M1", "M1" /. cachePressures[[sel[[1]],2]]];
			(* Next, we solve for the DRAM BW *)
			DRAMPressures = SolveDRAMBandwidthPressure[threadArchProperties, algProperties];
			If[DRAMPressures === Null, Return[Null]];
			threadArchProperties = SetKeyValue[threadArchProperties, "B0Dmo", "B0Dmo" /. DRAMPressures[[sel[[1]],2]]];	(* Set correct BW *)
			threadArchProperties = SetKeyValue[threadArchProperties, "B1Dmo", "B1Dmo" /. DRAMPressures[[sel[[1]],2]]];
			threadArchProperties = SetKeyValue[threadArchProperties, "B2Dmo", "B2Dmo" /. DRAMPressures[[sel[[1]],2]]];
			threadAlgProperties = algProperties[[sel[[1]],2]]; (* Set the algorithm*)
		, (* Otherwise, something is wrong *)
		_,
			Return[Null];
	];

ipcConstraints = {};
penaltyBranch = {};
ipcBranchConstraint = {};
penaltyIQROB = {};
ipcIQROBConstraint = {};
ipcIQROB = {};
ipcBranch = {};
plotCorePerformance = None;
	(* Step one: maximize each individual constraint. We set the y coordinate to 1 for now *)
	ipcConstraints = {};
	AppendTo[ipcConstraints, Quiet[Check[{NMaximize[{ipc, #[[1]][ipc, threadArchProperties, threadAlgProperties]}, ipc][[1]], 0.75 (*y*)}, {9.*10^99, 0.75} ,{NMaximize::ubnd}]]] & /@ Drop[PerformanceConstraints, -1]; (* random groot nummer als de constraint unbounded is (Met Infinity faalt de plot) *)
	AppendTo[ipcConstraints, Quiet[Check[{NMaximize[{ipc, #[[1]][ipc, #[[3]], threadArchProperties, threadAlgProperties]}, ipc][[1]], 0.75 (*y*)}, {9.*10^99, 0.75}, {NMaximize::ubnd}]]] & /@ ILPTypeConstraints;
	(* Separately account for the IQ and ROB, and branch penalty *)
	ipcIQROB = Min[ipcConstraints[[1;;7,1]]];
	penaltyIQROB = Total[CalculateIQROBStallCycles[ipcIQROB, threadArchProperties, threadAlgProperties]];
	ipcIQROBConstraint = 1/(1/ipcIQROB+penaltyIQROB);
	ipcBranch = Min[Flatten[{ipcIQROBConstraint, ipcConstraints[[;;,1]]}]];
	penaltyBranch = d0branch[threadArchProperties, threadAlgProperties, ipcBranch];
	ipcBranchConstraint = 1/(1/ipcBranch+penaltyBranch);
	(*AppendTo[ipcConstraints, {1/(1/ipcIQROBBranch+TotalIQROBBranchStallCycles[ipcIQROBBranch, archProperties, algProperties]), 1 (*y*)}];*)
	
	(* Create a nice graph *)
	(*Return[*)Manipulate[
			Column[{
				plotCorePerformance = Show[{
					ListPlot[{ipcConstraints},
						Filling->Axis,
						BaseStyle->Medium,
						PlotRange->{{scrollipc+0, scrollipc+zoomipc},{0, 4}},
						(*PlotLegends->{"Absolute constraints"},*)
						Frame->{True, False, False, False},
						FrameLabel->{"Instructions per cycle [IPC]"},
						AspectRatio->0.4,
						ImageSize->500
					],
					Graphics[Table[
							Text[Rotate[PerfConstraints[[i,2]], 45 Degree], ipcConstraints[[i]], {If[i==6,-1(*.15*),-1], -1.1} ], (* IF[] = fix to make nice plot for ICCD paper *)
						{i, Length[ipcConstraints]}
					]],
					ListPlot[{{ipcBranchConstraint, 1.25}, {ipcIQROBConstraint, 1.25}},		
						Filling->Axis,
						PlotStyle->Orange,
						(*PlotLegends->{"Relative constraints"},*)
						PlotMarkers->{"\[Times]",20},
						PlotRange->{{scrollipc+0, scrollipc+zoomipc},{0, 4}}
					],
					(*ListLinePlot[{{ipcBranchConstraint, 0.5}, {ipcBranch, 0.5}},	
						PlotStyle->Orange,
						PlotRange->{{scrollipc+0, scrollipc+zoomipc},{0, 4}}
					],
					ListLinePlot[{{ipcIQROBConstraint, 0.75}, {ipcIQROB, 0.75}},		
						PlotStyle->Orange,
						PlotRange->{{scrollipc+0, scrollipc+zoomipc},{0, 4}}
					],*)
					If[Abs[ipcBranch-ipcBranchConstraint] <= 0.01, Graphics[{Orange, Thick, Line[{{ipcBranch, 0.5},{ipcBranchConstraint, 0.5}}]}], Graphics[{Orange, Thick, Arrowheads[Medium], Arrow[{{ipcBranch, 0.5},{ipcBranchConstraint, 0.5}}]}]],
					If[Abs[ipcIQROB-ipcIQROBConstraint] <= 0.01, Graphics[{Orange, Thick, Line[{{ipcIQROB, 0.5}, {ipcIQROBConstraint, 0.5}}]}], Graphics[{Orange, Thick, Arrowheads[Medium], Arrow[{{ipcIQROB, 0.5}, {ipcIQROBConstraint, 0.5}}]}]],
					Graphics[Text[Rotate["Branch misprediction penalty", 45 Degree], {ipcBranchConstraint, 1.25}, {-1, -1.1} ]],
					Graphics[Text[Rotate["IQ and ROB stall penalty", 45 Degree], {ipcIQROBConstraint, 1.25}, {-1, -1.1} ]] (*-0.85*)
				}],
				ExportImageAsVectorDialogButton[plotCorePerformance]
			}],
			{{zoomipc, 20, "Zoom"}, 1, 100, 1},
			{{scrollipc, 0, "Scroll"}, 0, 50, 0.1}
		](*];*)
];

(* Summary of core performance *)
SummaryCPUCorePerformance[archProperties_List, algProperties_List, enablePower_Symbol : True, homogeneousThreadsOrThreadIndex_Integer : 1 ] :=
BlockCheck[{homogeneousThreads, corePerformance,coreFlopsPerformance,socketDRAMBandwidthUsage, socketFlopsPerformance, systemRuntime, systemFlopsPerformance},
	homogeneousThreads = If[AllTrue[algProperties, Head[#] === Rule &], 1, homogeneousThreadsOrThreadIndex];
	corePerformance  = If[AllTrue[algProperties, Head[#] === Rule &],
						MCd0[archProperties, algProperties, homogeneousThreads][[homogeneousThreadsOrThreadIndex]],
						MCd0[archProperties, algProperties, homogeneousThreads]
	];
	coreFlopsPerformance = If[AllTrue[algProperties, Head[#] === Rule &],
								MCAchievedFLOPSi[CoreLayer[], archProperties, algProperties, homogeneousThreads][[homogeneousThreadsOrThreadIndex]],
								MCAchievedFLOPSi[CoreLayer[], archProperties, algProperties, homogeneousThreads]
	];
	socketDRAMBandwidthUsage = If[AllTrue[algProperties, Head[#] === Rule &],
								Total[MCBappiDmo[SocketLayer[], archProperties, algProperties, homogeneousThreads]],
								MCBappiDmo[SocketLayer[], archProperties, algProperties, homogeneousThreads]*homogeneousThreads (*MC is for one thread*)
	];
	socketFlopsPerformance = If[AllTrue[algProperties, Head[#] === Rule &],
								Total[MCAchievedFLOPSi[SocketLayer[], archProperties, algProperties, homogeneousThreads]],
								MCAchievedFLOPSi[SocketLayer[], archProperties, algProperties, homogeneousThreads]*homogeneousThreads
	];
	systemRuntime = If[AllTrue[algProperties, Head[#] === Rule &],
								Max[MCExecutionSeconds[archProperties, algProperties, homogeneousThreads]],
								MCExecutionSeconds[archProperties, algProperties, homogeneousThreads]
	];
	systemFlopsPerformance = If[AllTrue[algProperties, Head[#] === Rule &],
								Total[MCAchievedFLOPSi[SystemLayer[], archProperties, algProperties, homogeneousThreads]],
								MCAchievedFLOPSi[SystemLayer[], archProperties, algProperties, homogeneousThreads]*homogeneousThreads
	];
	Return[
		Grid[{
																{"Core statistics:"},
																{"","Core power: ", If[enablePower, Poweri[CoreLayer[], archProperties, algProperties, homogeneousThreads], "-"], " W"},
																{"","Performance: ", corePerformance, " CPI"},
																{"","Peak floating-point performance: ", PeakFLOPSi[CoreLayer[], archProperties]/10^9, " GFLOP/s"}, 
																{"","Achieved floating-point performance: ", coreFlopsPerformance/10^9, " GFLOP/s", " Efficiency: ", coreFlopsPerformance * 100 / PeakFLOPSi[CoreLayer[], archProperties], " %"},
																{"Socket statistics:"},
																{"","Processor power: ", If[enablePower, Poweri[SocketLayer[], archProperties, algProperties, homogeneousThreads] - (PoweriMemory[SocketLayer[], archProperties, algProperties, homogeneousThreads]+PoweriMemory[CardLayer[], archProperties, algProperties, homogeneousThreads]), "-"], " W"},
																{"","Memory power: ", If[enablePower, (PoweriMemory[SocketLayer[], archProperties, algProperties, homogeneousThreads]+PoweriMemory[CardLayer[], archProperties, algProperties, homogeneousThreads]), "-"], " W"},
																{"","Peak DRAM bandwidth: ", 1.*GetKeyValue[archProperties, "B2Dmo"]/10^9, " GB/s"},
																{"","DRAM bandwidth usage: ", 1.*socketDRAMBandwidthUsage/10^9, " GB/s", " Efficiency: ", socketDRAMBandwidthUsage * 100 / GetKeyValue[archProperties, "B2Dmo"], " %"},
																{"","Peak floating-point performance: ", PeakFLOPSi[SocketLayer[], archProperties]/10^9, " GFLOP/s"}, 
																{"","Achieved floating-point performance: ", socketFlopsPerformance/10^9, " GFLOP/s", " Efficiency: ", socketFlopsPerformance * 100 / PeakFLOPSi[SocketLayer[], archProperties], " %"}, 
																{"System statistics: "},
																{"","Power: ", If[enablePower, Poweri[SystemLayer[], archProperties, algProperties, homogeneousThreads], "-"], " W"},
																(*{"","Power-Delay product: ", If[enablePower, MCd0[archProperties, algProperties, homogeneousThreads] * Poweri[SystemLayer[], archProperties, algProperties, homogeneousThreads] / GetKeyValue[archProperties, "f0"], "-"], " J/I"},*)
																{"","Runtime: ", systemRuntime, " s"},
																{"","Peak system floating-point performance: ", PeakFLOPSi[SystemLayer[], archProperties]/10^9, " GFLOP/s"},
																{"","Achieved system floating-point performance: ", systemFlopsPerformance/10^9, " GFLOP/s", " Efficiency: ", systemFlopsPerformance * 100 / PeakFLOPSi[SystemLayer[], archProperties], " %"}
															},Alignment->Left]
	];
];


End[] (* End Private Context *)

EndPackage[]