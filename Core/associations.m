(* ::Package:: *)

AN$KeyReplace[ass_, key_, val_] := Union[KeyDrop[ass,key],Association[key->val] ];

AmplitudesMap[ass_Association, func_, ind_:All] := Module[{amps = ass[Amplitudes], vals, newAmps},
  
   indices = If[ind=!=All, Flatten[ind /. Span[a_, b_] :> Range[a, b] ], ind ];

   If[ListQ[amps] && AllTrue[amps, MatchQ[_Rule] ],

      newAmps = MapIndexed[ If[MatchQ[indices, All] || MemberQ[indices, First[#2] ],  #[[1]] -> func[#[[2]]],   #] &, amps],
      newAmps = func[amps];

   ];

   AN$KeyReplace[ass, Amplitudes, newAmps]
 ];

AmplitudesSimplify[ass_Association] := AmplitudesMap[ass,Simplify];
AmplitudesFullSimplify[ass_Association] := AmplitudesMap[ass,FullSimplify];

(* ::Section:: *)
(*Conversion between rules and associations*)


AN$AssociationConversionRules = {"Total number of amplitudes" -> Total,"Process Name"->Name,"Number of Loops"->LoopOrder};


ConvertToAmplitudeAssociation[list_List]:=Module[{lisi=list},
    lisi=lisi//.AN$AssociationConversionRules;
    lisi=Prepend[Rest[lisi],Process->lisi[[1]]];
    Association[lisi]
    ];


ConvertFromAmplitudeAssociation[list_Association]:=Module[{lisi=list,diag},
    diag = Normal[KeySelect[lisi,Head[#]===DiagramID&]];
    lisi = Normal[KeySelect[lisi,Head[#]=!=DiagramID&]];
    lisi=lisi//.Reverse/@AN$AssociationConversionRules;
    lisi=Prepend[Rest[lisi],lisi[[1,2]]];
    lisi=Append[lisi,diag]
    ];


ConvertToProjectedAmplitudeAssociation[pamp_] := Association[pamp];
ConvertFromProjectedAmplitudeAssociation[pamp_] := Normal[pamp];


(* ::Section:: *)
(*Define kinematics*)


(** Kinematics **)


DefineKinematics[name_String,momConservation_Rule,momSquare_List,crossProducts_List]:= Module[{inverted,momSquare2},

              Kinematics[name] = Association[{MomentumSquare -> momSquare, CrossProducts -> crossProducts, MomentumConservation -> {momConservation} }]; 
              AN$Kinematics = Join[momSquare,crossProducts];

              momSquare2 = momSquare/.{Dot[pi_, pi_] :> Power[pi, 2]};
              AN$Kinematics = Join[AN$Kinematics,momSquare2];

              (* Complement the Kinematics provided by the user with the ratios of the kind 1/p1.p2 *)

              inverted = Cases[AN$Kinematics, Rule[Dot[p1_, p2_], y_] /; (y =!= 0)];
              inverted = inverted/.{Rule[Dot[p1_, p2_], y_]/;(y =!= 0) :>  Rule[1/Dot[p1, p2], 1/y]};

              AN$Kinematics = Join[AN$Kinematics, inverted,{momConservation}];
              
              ];



(* ::Section:: *)
(*Topologies*)


If[ Length[AllTopologies[] ] === 0, AllTopologies[] = {}];


DefineTopology[name_, loop_, ext_, props_] := Module[{},

    name::usage = SymbolName[Unevaluated[name] ] <> " is assigned as the name of a topology.";

    If[Head[name]===Symbol, Protect[name], Protect[#]&@Head[name ] ];

   Topology[name] = Association[{Name -> name, 
           LoopOrder -> Length[loop],
           LoopMomenta -> loop,
           ExternalMomenta -> ext,
           Propagators -> props}];
   AllTopologies[]=Union[AllTopologies[], {name}];
   ];


(* ::Section:: *)
(*Projectors*)


If[ Length[AllProjectors[] ] === 0, AllProjectors[]  = {}];

DefineProjector[name_, spins_, colRep_, coefficient_:1, pLor_, pColour_] := Module[{},
    
    name::usage = SymbolName[Unevaluated[name] ] <> " is assigned as the name of a projector.";

    If[Head[name]===Symbol, Protect[name], Protect[#]&@Head[name ] ];

   Projector[name] = Association[{Name -> name, 
           Spins                -> spins,
           ColourRepresentation -> colRep,
           PCoefficient         -> coefficient,
           PLorentz             -> pLor,
           PColour              -> pColour,
           Operator             -> coefficient*pLor*pColour}];
   AllProjectors[] =Union[AllProjectors[] , {name}];
   ];

ProjectorsBySpins[spin_] := Select[AllProjectors[], Projector[#][Spins] === spin &];
ProjectorsByColours[colour_] := Select[AllProjectors[], Projector[#][ColourRepresentation]  === colour &];

DefineLorentzProjector[name_, spins_List, coefficient_:1, pLor_] := DefineProjector[name, spins, None, coefficient, pLor, 1];
DefineColourProjector[name_, colRep_List, coefficient_:1, pColour_] := DefineProjector[name, None, colRep, coefficient, 1, pColour];


(* ::Section:: *)
(*Manipulating outputs*)


GetLoopIntegrals[expr_] := Cases[expr, _TopInts, Infinity];


(* ::Subsection:: *)
(*SumDiagrams*)


Clear[SumDiagrams];
SumDiagrams[amp_List] := Module[{temp = amp},
      Which[Head[temp] === Rule,
            temp /. _DiagramID :> DiagramID[Sum],
          Head[temp[[1,1]]] === DiagramID,
            {Rule[DiagramID[Sum], Total[Last/@temp]]},
          (Head[temp[[1,1]]] === List) && Length[temp[[1,1]]] == 1,
            {Rule[DiagramID[Sum], Total[Last/@temp]]},
          (Head[temp[[1,1]]] === List) && Length[temp[[1,1]]] == 2,
              temp = GatherBy[temp, #[[1,2]]&];
              Rule[{DiagramID[Sum],#[[1,1,2]]}, Total[#[[All,2]]]]& /@ temp
       ]]


SumDiagrams[amp_Association] := Module[{temp = SumDiagrams[amp[Amplitudes]],tempamp=amp},
    tempamp[Amplitudes] = temp;
    tempamp 
    ];


(* ::Subsection:: *)
(*Form factors*)


FormFactor[pamp_Association, i_] := {DiagramID[Sum],i} /. SumDiagrams[pamp][Amplitudes];
FormFactor[pamp_List, i_] := {DiagramID[Sum],i} /. SumDiagrams[pamp];


(* ::Subsection:: *)
(*Collect*)


Clear[GatherTopInts];
CollectTopIntOperator[a_. t_TopInt^n_., b_] := CollectTopIntOperator[a, t^n b];
CollectTopIntOperator[sum_Plus, b_] := CollectTopIntOperator[#, b]&/@sum;
GatherTopInts[expr_,func_:Identity] := Module[{temp},

    If[Head[expr] === List,
       Return[GatherTopInts[#,func]& /@ expr]
       ];
       
    If[Head[expr] === Rule,
       Return[Rule[expr[[1]],GatherTopInts[expr[[2]],func]]]
       ];
       
    If[Head[expr] =!= Plus,
       Return[func[expr]]
       ];

    temp = CollectTopIntOperator[Expand[expr], 1];
    temp = {func[Total[#[[All,1]]]], #[[1,2]]}& /@ GatherBy[List @@ temp, Last]
 ]; 


Clear[CollectIntegrals];
CollectIntegrals[expr_,func_:Identity] := Module[{tempexpr},

    Which[Head[expr] === List,
          Return[CollectIntegrals[#,func]&/@expr],
       Head[expr] === Rule,
          Return[expr [[1]] -> CollectIntegrals[expr[[2]],func]],
       Head[expr] === RuleDelayed,
          Return[RuleDelayed @@ {expr [[1]], CollectIntegrals[expr[[2]],func]}]
          ];

    If[Head[expr] =!= Association,
       Return[Total[Times@@@(MapAt[func,#,1]&/@GatherTopInts[expr])]]
       ];
   
   tempexpr = expr;
   tempexpr[Amplitudes] = CollectIntegrals[expr[Amplitudes], func];
   tempexpr   
   ]


(* ::Section:: *)
(*InsertCouplings*)


InsertCouplings[expr_, rules_:Automatic] := If[Head[expr] === Association,
             AmplitudesMap[expr, # /. If[rules === Automatic, AN$GCouplings, rules]&],
             expr /. If[rules === Automatic, AN$GCouplings, rules]
             ];
