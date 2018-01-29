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

BeginPackage["CommunicationPowerModels`"]
(* Exported symbols added here with SymbolName::usage *)

Needs["ExaBoundsGeneric`"]
Needs["CommunicationPerformanceModels`"]


GetNumberElectricalLinks::usage =
"GetNumberElectricalLinks[archProperties_,algProperties_,level_]"

GetNumberOpticalLinks::usage =
"GetNumberOpticalLinks[archProperties_,algProperties_,level_]"
  
GetNetworkDynamicEnergy::usage =
"GetNetworkDynamicEnergy[archProperties_,algProperties_]"

GetTotalNumberSwitches::usage =
"GetTotalNumberSwitches[archProperties_]"

GetTotalNumberLinks::usage =
"GetTotalNumberLinks[archProperties_]"

GetNetworkStaticEnergy::usage =
"GetNetworkStaticEnergy[archProperties_,algProperties_,appTime_]"

                         
Begin["`Private`"]

(* Energy consumption-related network models *)

(* This function is currently not used in the ExaBounds models. 
   It is still here because it includes some formulas that might be useful in the future. *)
GetTotalNumberLinks[archProperties_] :=
  Module[{ topology, topoDescription, links, a, p, h, w0, w1, w2, m1, m2, m3, 
            d1, d2, d3, nGroups, nNodes, nSwitches, nodeToSwitchLinks, intraGroupLinks,
           interGroupLinks, switchToSwitchLinks },

    topology = GetKeyValue[archProperties,"networkConfiguration"];
    topoDescription = GetKeyValue[archProperties,"topologyDescription"];
    
    links = 1;

    If[topology == "full-mesh",
    (* Network parameters: (p, a) *)

        a = topoDescription[["a"]];
        p = topoDescription[["p"]];

        nodeToSwitchLinks = a*p;
        switchToSwitchLinks = a*(a-1)/2;

        links = nodeToSwitchLinks + switchToSwitchLinks;

        Return[ links ],

    If[topology == "torus3D",
    (* Network parameters: (p, d1, d2, d3) *)
    
        p = topoDescription[["p"]];
        d1 = topoDescription[["d1"]];
        d2 = topoDescription[["d2"]];
        d3 = topoDescription[["d3"]];

        links = d1*d2*d3*(p+3) - d1*d2 - d2*d3 - d3*d1;

        Return[ links ],

    If[topology == "torus2D",
    (* Network parameters: (p, d1, d2) *)

        p = topoDescription[["p"]];
        d1 = topoDescription[["d1"]];
        d2 = topoDescription[["d2"]];

        links = d1*d2*p + (d1-1)*d2 + (d2-1)*d1;
      
        Return[ links ];

    If[topology == "torus1D",
    (* Network parameters: (p, d1) *)

        p = topoDescription[["p"]];
        d1 = topoDescription[["d1"]];   

        links = d1*p + d1 - 1;      
        
        Return[ links ],

    If[topology == "2DhyperX",
    (* Network parameters: (p, d1, d2) *)

        p = topoDescription[["p"]];
        d1 = topoDescription[["d1"]];
        d2 = topoDescription[["d2"]];

        links = d1*d2*p + d1*(d1-1)/2*d2 + d2*(d2-1)/2*d1;

        Return[ links ],

    If[topology == "fat-tree-3L",
    (* Network parameters: (w0, w1, w2, m1, m2, m3) *)

        w0 = topoDescription[["w0"]];
        w1 = topoDescription[["w1"]];
        w2 = topoDescription[["w2"]];
        m1 = topoDescription[["m1"]];
        m2 = topoDescription[["m2"]];
        m3 = topoDescription[["m3"]];

        links = m1*w0*m2*m3 + m2*w1*m3 + m3*w2;

        Return[ links ],

    If[topology == "fat-tree-2L",
    (* Network parameters: (w0, w1, m0, m1) *)

        w0 = topoDescription[["w0"]];
        w1 = topoDescription[["w1"]];
        m1 = topoDescription[["m1"]];
        m2 = topoDescription[["m2"]];

        links = m1*w0*m2 + m2*w1;

        Return[ links ],

    If[topology == "dragonfly",
    (* Network parameters: (p, a, h) *)

        a = topoDescription[["a"]];
        p = topoDescription[["p"]];
        h = topoDescription[["h"]];

        nGroups = a*h+1;
        nNodes = nGroups*a*p;
        nSwitches = nGroups*a;

        nodeToSwitchLinks = nNodes;
        intraGroupLinks = nGroups*a*(a-1)/2;
        interGroupLinks = nGroups*a*h/2;

        links = nodeToSwitchLinks + intraGroupLinks + interGroupLinks;
        
        Return[ links ];
   ];
   ];
   ];
   ];
   ];
   ];
   ];
   ];
   
   Message[archProperties::missingmodel, "total number links", topology, "---"];
   Return[ links ];
];

(* Average number of electrical links traversed by a message *)
GetNumberElectricalLinks[archProperties_, algProperties_, level_] :=
  Module[{ links },
    
    If[level == 1, links = 2, 
        links = GetAverageNetworkLinkLatency[archProperties, algProperties, 1];
    ];
    
    Return[ links ];
];

(* Average number of optical links traversed by a message *)
GetNumberOpticalLinks[archProperties_, algProperties_, level_] :=
  Module[{ links },
    
    links = GetAverageNetworkLinkLatency[archProperties, algProperties, 1] - 
            GetNumberElectricalLinks[archProperties, algProperties, level];
              
    Return[ links ];
];

(* Network dynamic energy per process or class of processes *)
GetNetworkDynamicEnergy[archProperties_, algProperties_] :=
  Module[{  totalNumberBytes, mpiSentBytes, mpiReceivedBytes, commVector, messages,
            msgSize, numberElectricalLinks, numberOpticalLinks, numberSwitches,
            energyElectricalLinks, energyOpticalLinks, energySwitches, topology, pattern,
            communicationPattern, energyElectricalLinkPerBit, energyOpticalLinkPerBit, 
            energySwitchPerBit, nodeSwitchLinkType, level, recvMessages},
       
        mpiSentBytes = GetKeyValue[algProperties,"MPIsentBytes"];
        mpiReceivedBytes = GetKeyValue[algProperties,"MPIrecvBytes"];
        
        (*
          commVector = GetKeyValue[algProperties,"MPIcommVector"];
          messages = 0;
          For[i=1, i<= Length[commVector], i++, messages = messages + commVector[[i]][[3]]];
        *)

	    (* The extrapolated profile with ExtraX should have the MPIrecvMessages field defined. *)
        recvMessages = GetKeyValue[algProperties,"MPIrecvMessages"];
    	If[recvMessages==0,
        (* Check whether there are also MPIIrecv calls. *)
        (* messages = GetKeyValue[algProperties,"MPIrecv"] + GetKeyValue[algProperties,"MPIirecv"]; *)
	       messages = GetKeyValue[algProperties,"MPIrecv"],
           messages = recvMessages;
	    ];

        msgSize = mpiReceivedBytes/messages*1.0;
        totalNumberBytes = messages*msgSize;
               
        topology = GetKeyValue[archProperties,"networkConfiguration"];
        (* Print["GetNetworkDynamicEnergy:topology"]; Print[topology]; *)

        communicationPattern = GetKeyValue[algProperties, "CommunicationPattern"];
        pattern = communicationPattern[["type"]];
        
        energyElectricalLinkPerBit = GetKeyValue[archProperties, "electricalLinkEnergyPerBit"];        
        energyOpticalLinkPerBit = GetKeyValue[archProperties, "opticalLinkEnergyPerBit"];        
        energySwitchPerBit = GetKeyValue[archProperties, "switchLogicEnergyPerBit"];
        
        nodeSwitchLinkType = GetKeyValue[archProperties, "nodeSwitchLinkType"];
        If[nodeSwitchLinkType == "electrical", level = 1, level=0];
    
        numberElectricalLinks = GetNumberElectricalLinks[archProperties,algProperties,level];
        (* Print["GetNetworkDynamicEnergy: number of electrical links"]; Print[numberElectricalLinks]; *)
       
        numberOpticalLinks = GetNumberOpticalLinks[archProperties,algProperties,level];
        (* Print["GetNetworkDynamicEnergy: number of optical links"]; Print[numberOpticalLinks]; *)
       
        numberSwitches = GetAverageNetworkLinkLatency[archProperties,algProperties,1] - 1;
        (* Print["GetNetworkDynamicEnergy: number of switches"]; Print[numberSwitches]; *)

        (* The energies per bit are in pJ *)
        energyElectricalLinks = 8*totalNumberBytes*numberElectricalLinks*energyElectricalLinkPerBit*10^(-12);
        (* Print["GetNetworkDynamicEnergy: number of electrical links"]; Print[energyElectricalLinks]; *) 

        (* The energies per bit are in pJ *)       
        energyOpticalLinks = 8*totalNumberBytes*numberOpticalLinks*energyOpticalLinkPerBit*10^(-12);
        (* Print["GetNetworkDynamicEnergy: number of optical links"]; Print[energyOpticalLinks]; *)
       
        (* The energies per bit are in pJ *)
        energySwitches = 8*totalNumberBytes*numberSwitches*energySwitchPerBit*10^(-12);
        (* Print["GetNetworkDynamicEnergy: switch energy"]; Print[energySwitches]; *)
       
        (* The dynamic energy is in W *)
        Return[ energyElectricalLinks + energyOpticalLinks + energySwitches ];
];

(* Total number of switches in the network - used by the static power model *)
GetTotalNumberSwitches[archProperties_] :=
   Module[{topology, topoDescription, switches, a, p, h, w0, w1, w2, m1, m2, m3, 
            d1, d2, d3, d4, d5},

        topology = GetKeyValue[archProperties,"networkConfiguration"];
        topoDescription = GetKeyValue[archProperties,"topologyDescription"];
      
        switches = 1;

        If[topology == "full-mesh",
        (* Network parameters: (p, a) *)

            a = topoDescription[["a"]];
     
            switches = a;
            
            Return[ switches ],

        If[topology == "dragonfly",
        (* Network parameters: (p, a, h) *)

            a = topoDescription[["a"]];
            p = topoDescription[["p"]];
            h = topoDescription[["h"]];

            switches = a*(a*h+1);
        
            Return[ switches ],
            
        If[topology == "2DhyperX",
        (* Network parameters: (p, d1, d2) *)

            d1 = topoDescription[["d1"]];
            d2 = topoDescription[["d2"]];       
    
            switches = d1*d2;
     
            Return[ switches ],

        If[topology == "fat-tree-3L",
        (* Network parameters: (w0, w1, w2, m1, m2, m3) *)

            w0 = topoDescription[["w0"]];
            w1 = topoDescription[["w1"]];
            w2 = topoDescription[["w2"]];
            m1 = topoDescription[["m1"]];
            m2 = topoDescription[["m2"]];
            m3 = topoDescription[["m3"]];
            
            switches = m2*m3+m3*w1+w1*w2;
           
            Return[ switches ],

        If[topology == "fat-tree-2L",
        (* Network parameters: (w0, w1, m0, m1) *)

            w0 = topoDescription[["w0"]];
            w1 = topoDescription[["w1"]];
            m1 = topoDescription[["m1"]];
            m2 = topoDescription[["m2"]];
            
            switches = m2 + w1;

            Return[ switches ],

        If[topology == "torus1D",
        (* Network parameters: (p, d1) *)
        
            d1 = topoDescription[["d1"]];

            switches = d1;
            
            Return[ switches ],

        If[topology == "torus2D",
        (* Network parameters: (p, d1, d2) *)

            d1 = topoDescription[["d1"]];
            d2 = topoDescription[["d2"]];
           
            switches = d1*d2;
           
            Return[ switches ],

        If[topology == "torus3D",
        (* Network parameters: (p, d1, d2, d3) *)

            d1 = topoDescription[["d1"]];
            d2 = topoDescription[["d2"]];
            d3 = topoDescription[["d3"]]; 
            
            switches = d1*d2*d3;
            
            Return[ switches ],

        If[topology == "torus5D",
        (* Network parameters: (p, d1, d2, d3, d4, d5) *)

            d1 = topoDescription[["d1"]];
            d2 = topoDescription[["d2"]];
            d3 = topoDescription[["d3"]]; 
            d4 = topoDescription[["d4"]];
            d5 = topoDescription[["d5"]]; 

            switches = d1*d2*d3*d4*d5;
            
            Return[ switches ];
        ];
        ];
        ];
        ];
        ];
        ];
        ];
        ];
        ];
        
        Message[archProperties::missingmodel, "total number of switches", topology, "---"];

        Return[ switches ];
];

(* Network static energy - static power *)
GetNetworkStaticEnergy[archProperties_, algProperties_, appTime_] :=
   Module[{totalNumberSwitches, staticPowerPerSwitch},

    totalNumberSwitches = GetTotalNumberSwitches[archProperties];
    staticPowerPerSwitch = GetKeyValue[archProperties,"switchStaticPower"];
    (* Print["GetNetworkStaticEnergy: total number of switches"]; Print[totalNumberSwitches]; *)

    Return[staticPowerPerSwitch * appTime * totalNumberSwitches];

];


End[] (* End Private Context *)

EndPackage[]
