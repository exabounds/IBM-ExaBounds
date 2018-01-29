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
 
 (* Mathematica package *)

ClearAll["Global`*"] (* Clear everything to always start with a blank slate. *)

(* Directory structure *)
rootPathExaBounds = DirectoryName[FindFile["ExaBoundsLite.nb"]];
AppendTo[$Path, FileNameJoin[{rootPathExaBounds, "AlgorithmExtrapolation"}]]; 
AppendTo[$Path, FileNameJoin[{rootPathExaBounds, "Algorithms"}]];
AppendTo[$Path, FileNameJoin[{rootPathExaBounds, "CacheModel"}]];  
AppendTo[$Path, FileNameJoin[{rootPathExaBounds, "DataInOut"}]];   
AppendTo[$Path, FileNameJoin[{rootPathExaBounds, "CommunicationModel"}]];
AppendTo[$Path, FileNameJoin[{rootPathExaBounds, "MeSAP"}]];    
AppendTo[$Path, FileNameJoin[{rootPathExaBounds, "DSE"}]];

<< "Units`"
<< "NumericalCalculus`" 

(* Include ExaBounds packages *)
<< "ExaBoundsGeneric`"
<< "ModelWarnings`"
<< "ArchitectureProperties`"
<< "AlgorithmProperties`"
<< "AlgorithmFromFileJSON`"
<< "VectorSizeConvert`" 
<< "Support`"
<< "DSEVisualization`"

(* Initialization of the ExaBoundsState variable*)
ExaBoundsState = InitArchitectureProperties[];
AlgorithmMinMaxValues = InitAlgorithmMinMax[];
