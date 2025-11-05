(* ::Package:: *)

(* ::Title:: *)
(* ANATAR  *)

(*********************************************************)
(*********************************************************)
(***                                                   ***)
(***     oo    oo   o    oo    oooooo    oo    oooo    ***)
(***    oooo   ooo  o   oooo     oo     oooo   oo  o   ***)
(***   oo  oo  o  ooo  oo  oo    oo    oo  oo  oooo    ***)
(***  oo    oo o   oo oo    oo   oo   oo    oo oo  oo  ***)
(***                                                   ***)
(*********************************************************)
(*********************************************************)


(* ::Text:: *)
(*Authors : C. Duhr, P. Mukherjee, A. Vasquez 2025*)
(*    *)


(* ::Section:: *)
(*Some printout*)

If[$AnatarLoaded===True, Print["Package already loaded..."]&&Abort[] ];

$AnatarLoaded = True;

BeginPackage["Anatar`"];

Anatar$Version="1.0.1";


Print[" - ANATAR - "];
Print["Version: "<>Anatar$Version<>", (5 Nov 2025)"];
Print["Authors: C. Duhr, P. Mukherjee, A. Vasquez"];
Print["Please cite  "];
Print["https://github.com/ANATAR-hep/ANATAR"];

(* Space-time dimension is set as d. To set a different value, it is enough change the value of $DimensionST. *)

$DimensionST = d;
Update[$DimensionST];

(* ::Section:: *)
(*Check if gzip exists, and can be called*)


AN$GZip = True;
GZip::NotFound = "Apparently gzip cannot be called from Mathematica. Intermediate files will not be compressed. Note that this will not affect the running of ANATAR!";

Module[{
sourceFile = FileNameJoin[{Global`$AnatarPath, "Anatar.m"}],
targetFile = FileNameJoin[{Global`$AnatarPath, "gzip_test.m"}],
compressedFile = FileNameJoin[{Global`$AnatarPath, "gzip_test.m.gz"}]
},

(*Copy the file to the target location*)
CopyFile[sourceFile, targetFile];

(*Remove the compressed file if it already exists to avoid conflicts*)
If[FileExistsQ[compressedFile], DeleteFile[compressedFile] ];

(*Run the gzip command and handle errors*)
If[Run["gzip " <> targetFile] =!=
  0, (If[FileExistsQ[targetFile], DeleteFile[targetFile] ];
  AN$GZip = False;
  Message[GZip::NotFound]), (If[FileExistsQ[targetFile],
   DeleteFile[targetFile] ];(*Delete the.m file*)
  If[FileExistsQ[compressedFile], DeleteFile[compressedFile] ]; )];
];



(* ::Section:: *)
(*Checks*)


(* ::Subsection:: *)
(*Qraf*)


QGPathError::NotFound = "The path to QGraf has not been provided. Please set the variable $QGrafPath to the QGraf location, otherwise ANATAR will not be able to generate amplitudes.";

If[ Head[Global`$QGrafPath] === Symbol,
  Message[QGPathError::NotFound]
];




(* ::Subsection:: *)
(*Form*)


Form::NotFound = "FORM excecuatable was not found. Please make sure that you call form by using the command \"form\" from the shell.";

If[Run["which form"] =!= 0,
   Message[Form::NotFound]
   ];


(* ::Section:: *)
(**)


(* ::Subsubtitle:: *)
(* Package symbols definitions *)

d::usage="d stands for the space-time dimension. "

QGWrite::usage="QGWrite[lag,mod] computes the Feynman rules from the Lagrangian lag and writes them in the QGraf model's format in the file mod.qgraf."

QGDiagrams::usage="QGDiagrams[mod,{inc1,inc2,...}->{out1,out2,...},file] uses QGraf to generate all the Feynman diagrams according to the Model mod with incoming fields {inc1,inc2,...} and outgoing  {out1,out2,...} and writes the output in File file."

Loops::usage="Loops -> 0 is an option of QGDiagrams indicating the number of loops allowed in the diagrams, 0 indicates tree-level, 1 corresponds to one-loop, '1 to 4' generates diagrams for a number of consecutive loop levels."

Dimension::usage="Dimension -> d, is an option of GenerateAmplitude. It allows the selection the space-time dimension. The default value is d for amplitudes at any loop order."

QGStyle::usage="QGStyle -> \"array.sty\" is an option of QGDiagrams and allows to change the style for presenting the diagram listing. The style 'array.sty' is the easiest to be read by Mathematica."

QGMessage::usage="QGMessage -> \"msg.txt\" is an option of QGDiagrams. It saves warnings and error messages in the file msg.txt."

QGPartition::usage="QGPartition -> \"None\" is an option of QGDiagrams. It restricts the diagram generation to some subsets of the provided partitions."

QGrafOptions::usage="QGrafOptions is an option of GenerateAmplitudes. QGrafOptions -> \"None\" is an option of QGDiagrams. It selects the diagrams according to the specified topologies."

GenerateAmplitude::usage="GenerateAmplitude[{in1,in2,...}->{out1,out2,...}] generates Feynman diagrams using QGraf according to the selected Model via LoadModel function with incoming fields {in1,in2,...} and outgoing  {out1,out2,...}. "

GenerateAmplitudeConjugate::usage="GenerateAmplitudeConjugate[{in1,in2,...}->{out1,out2,...}] generates Feynman diagrams using QGraf according to the selected Model via LoadModel function with incoming fields {in1,in2,...} and outgoing  {out1,out2,...}. The options of GenerateAmplitudeConjugate are the same of GenerateAmplitude."

GenerateAmplitudeSquare::usage="GenerateAmplitudeSquare[{in1,in2,...}->{out1,out2,...},amp,ampC] contracts amplitude amp with the conjugate amplitude ampC and sums over polarizations and spins of external fields."

ProjectAmplitude::usage="ProjectAmplitude[OUTAmp,{projID1,projID2,...}] applies projectors in the list {projID1,projID2,...} on the amplitude OUTAmp generated by the amplitude-generation functions."

LoadModel::usage="LoadModel[\"model\"] reads the input model and detects any missing definition.\n Models can also be selected via the option Model of GenerateAmplitude and GenerateAmplitudeConjugate."

Model::usage="Model is an option of GenerateAmplitude. It allows the selection of a model."

OutputName::usage="OutputName -> \"None\" allows to give a specific name to the output of GenerateAmplitude. This option is useful when the horizontal limit is exceeded in QGraf. "

CouplingsOrder::usage="CouplingsOrder -> {{\[Alpha]1,min1,max1},{\[Alpha]2,min2,max2},...} is an option of GenerateAmplitude. It specifies the coupling orders for the diagrams where a generic coupling \[Alpha] is selected to appear from powers min to max."

PolarizationTrim::usage="PolarizationTrim is an option of GenerateAmplitude and sets removes spinors and polarization vectos from the amplitudes when set to True."

Gauges::usage="Gauges is an option of GenerateAmplitude. It specifies the gauge of preference, the default choice is {\"QCD\"->\"Feynman\",\"EW\"->\"Feynman\"}."

RemovePropagator::usage="RemovePropagator -> {{psi1, a1, a2},{psi2, b1, b2},...} is an option of GenerateAmplitude and keeps diagrams with a1 to a2 number of propagators of field psi1.\nWhen written as RemovePropagator -> {psi1,psi2,...} the fields psi1, psi2, ... are completely forbidden in the diagrams."

SubstitutionRules::usage="SubstitutionRules -> {x1 -> x2, y1 -> y2, ...} is an option of GenerateAmplitude and allows to provide to FORM additional identities to be used to simplify expressions."

SimplifiedOutput::usage="SimplifiedOutput is an option of GenerateAmplitude. SimplifiedOutput -> True performs simplifications on the colour and Lorentz structures at the amplitude generation level. Default value is False."

RecycleAmplitude::usage="RecycleAmplitude is an option of GenerateAmplitude. RecycleAmplitude -> True imports a previous result if a computation with the same output name is found. Default value is False."

SelectCouplingsOrder::usage="SelectCouplingsOrder is an option of GenerateAmplitude. SelectCouplingsOrder -> {{param1,a1},...} selects terms in the amplitudes, yielding the coefficient
of param1^a1."

SelectAmplitudes::usage="SelectAmplitudes is an option of GenerateAmplitude. It allows the selection of the diagrams to be processed under Feynman Rules. The syntax is based on the mathematica notation of the function Part ([[...]]), for example SelectAmplitudes -> {m;;n} gives amplitudes m through n."

SetOffShell::usage="SetOffShell is a variable that keeps the external momenta off-shell while set to True. Default value is False."

CasimirValues::usage="CasimirValues is an option of GenerateAmplitudeSquare and ProjectAmplitude. When set to True, the values corresponding to SU(3) are subsituted in the colour factors."

OverallFactor::usage="OverallFactor->Expr is an option of GenerateAmplitudeSquare. It allows to introduce the overall factor Expr into the contraction of the two input amplitudes."

nnVectorSimplification::usage="nnVectorSimplification is an option of GenerateAmplitudeSquare. When set to True simplifications are performed to eliminate the dependence of squared amplitudes on the auxiliary four-vectors nn."

TopInt::usage="TopInt[Topo,\!\(\*SubscriptBox[\(a\), \(1\)]\),...,\!\(\*SubscriptBox[\(a\), \(n\)]\)] stands for the scalar Feynman integrals with the n-tuple (\!\(\*SubscriptBox[\(a\), \(1\)]\),...,\!\(\*SubscriptBox[\(a\), \(n\)]\)) being the powers of the denominators that define the topology Topo."

Amplitudes::usage="Amplitudes is a symbol used as key in the associations generated by various functions of ANATAR."

DiagramID::usage="DiagramID is a symbol used as a label in the Amplitudes key of the associations generated by various functions of ANATAR."

Process::usage="Process is a symbol used as key in the associations generated by various functions of ANATAR."

OnlyDiagrams::usage="OnlyDiagrams is an option of GenerateAmplitudes that when set to true generates only the diagrams using QGraf, but not the respective amplitudes."

If[!StringContainsQ[Total::usage, "ANATAR"],
    Total::usage = StringJoin["In ANATAR context, Total is a symbol used as key in the associations generated by various functions.\n", Total::usage];
  ];

Name::usage="Name is a symbol used as key in the associations generated by various functions of ANATAR."

LoopOrder::usage="LoopOrder is a symbol used as key in the associations generated by various functions of ANATAR."

Projectors::usage="Projectors is a symbol used as key in the associations generated by various functions of ANATAR."

Kinematics::usage="Kinematics is a symbol used as key in the associations generated by various functions of ANATAR."

DefineKinematics::usage="DefineKinematics[Name, momCons, momSqrs, crossProd] allows the definition of a custom kinematics. \n The Name of the Kinematics must be the same of the amplitudes. \n momCons is given as a rule representing momentum conservation. \n momSqrs is a list of rules defining the squared momenta. \n crossProd is a list of rules defining the cross products of the independent external momenta. "

Projector::usage="Projector[ProjID] returns an association with all the information of the projector named ProjID"

AllProjectors::usage="AllProjectors[] contains a list of all the defined projectors used in ANATAR."

ProjectorsBySpins::usage="ProjectorsBySpins[{spin1,spin2,...}] lists all the projectors corresponding to the selection of spins {spin1,spin2,...}."

ProjectorsByColours::usage="ProjectorsByColours[{colRep1,colRep2,...}] lists all the projectors corresponding to the selection of fields in the respresentations {colRep1,colRep2,...}."

Spins::usage="Spins is a symbol used as key in the associations generated in the definitions of projectors."

ColourRepresentation::usage="ColourRepresentation is a symbol used as key in the associations generated in the definitions of projectors."

PCoefficient::usage="PCoefficient is a symbol used as key in the associations generated in the definitions of projectors."

PLorentz::usage="PLorentz is a symbol used as key in the associations generated in the definitions of projectors."

PColour::usage="PColour is a symbol used as key in the associations generated in the definitions of projectors."

Operator::usage="Operator is a symbol used as key in the associations generated in the definitions of projectors."

DefineProjector::usage="DefineProjector[Name, ExtFields, Coeff, LorentzStr, ColourStr] allows the definition of custom projectors which then can be used by ProjectAmplitude via the option Projectors -> Name. \n The projectors are defined by a coefficient Coeff, a Lorentz or Dirac structure which depends on the external fields ExtFields, and the colour structured ColourStr."

DefineLorentzProjector::usage="DefineProjector[Name, Coeff, LorentzStr] allows the definition of custom projectors to be applied only on the Lorentz structure of the amplitude."

DefineColourProjector::usage="DefineProjector[Name, Coeff, ColourStr] allows the definition of custom projectors to be applied only on the Colour structure of the amplitude."

GammaM::usage="GammaM[Lor1,Spin,Spin2] is an object corresponding to the Dirac matrix \!\(\*SubsuperscriptBox[\(\[Gamma]\), RowBox[{\"Spin1\", \",\", \"Spin2\"}], \(Lor1\)]\). The syntax of identity matrix is GammaM[1,Spin,Spin2]."

SpinorU::usage="SpinorU corresponds to the Dirac spinor of an initial particle."

SpinorUbar::usage="SpinorUbar corresponds to the Dirac spinor of a final particle."

SpinorV::usage="SpinorV corresponds to the Dirac spinor of a final anti-particle."

SpinorVbar::usage="SpinorVbar corresponds to the Dirac spinor of a initial anti-particle."

SpinorUC::usage="SpinorUC corresponds to the Hermitian conjugate of the Dirac spinor of an initial particle."

SpinorUbarC::usage="SpinorUbarC corresponds to the Hermitian conjugate of the Dirac spinor of a final particle."

SpinorVC::usage="SpinorVC corresponds to the Hermitian conjugate of the Dirac spinor of a final anti-particle."

SpinorVbarC::usage="SpinorVbarC corresponds to the Hermitian conjugate of the Dirac spinor of a initial anti-particle."

PolV::usage="PolV corresponds to the polarization vectors of external vector bosons."

PolVC::usage="PolVC corresponds to the Hermitian conjugate of the polarization vectors of external vector bosons."

f::usage="f[gl1,gl2,gl3] stands for the QCD structure constant  \!\(\*SuperscriptBox[\(f\), \(gl1, gl2, gl3\)]\)"

T::usage="T[col1,col2,gl1] stands for the QCD structure constant  \!\(\*SubsuperscriptBox[\(T\), RowBox[{\"col1\", \",\", \"col2\"}], \(gl1\)]\)"

Metric::usage="Metric stands for the Kronecker's delta and Minkowski metric."

AN$LeviCivita::usage="AN$LeviCivita stands for the Levi-Civita symbol."

Den::usage="Den[k,m] is a function standing for 1/(k^2 - m^2)."

AmplitudeToTopologies::usage="AmplitudeToTopologies[amp] maps the input amplitude amp into the already defined topologies listed in AllTopologies[]."

AnatarModel::usage="AnatarModel[] returns the information about the current loaded model."

Particle::usage="Particle[psi] returns the information about a specific field psi defined in the current loaded model."

SumDiagrams::usage="SumDiagrams[amp] returns an association identical to the input one, amp, but where the value of the Amplitudes key now contains a single entry with the sum of analytical expressions of all the diagrams."

AmplitudesMap::usage="AmplitudesMap[amp, func] returns an association identical to amp, but with the value of the Amplitudes key replaced by the function func acting on the analytical expressions of the individual diagrams.\n AmplitudesMap[amp, func, {i1,i2,...,j1;;j2,...}] maps the function func over the diagrams i1,i2,... and j1 through j2."

AmplitudesSimplify::usage="AmplitudesSimplify[amp] maps the function Simplify[] over the amplitudes expression contained in the association amp."

AmplitudesFullSimplify::usage="AmplitudesFullSimplify[amp] maps the function FullSimplify[] over the amplitudes expression contained in the association amp."

InsertCouplings::usage="InsertCouplings[amp] inserts the expressions for the abbreviations of the couplings in the value of the Amplitudes key of the association amp."

MomentumSquare::usage="MomentumSquare is a symbol used as key in the association generated by DefineKinematics"

CrossProducts::usage="CrossProducts is a symbol used as key in the association generated by DefineKinematics"

MomentumConservation::usage="MomentumConservation is a symbol used as key in the association generated by DefineKinematics"

AllTopologies::usage="AllTopologies[] contains a list of all the defined topologies in the current kernel."

DefineTopology::usage="DefineTopology[name, {k1,k2,...}, {p1,p2,...}, denominators] defines integral families according to the list denominators. The loop (k1,k2,...) and external (p1,p2,...) momenta present in the denominators should also be provided. \n\nDefineTopology->True is also an option of FindTopology, that defines automatically the found topologies when set to True."

FindTopology::usage="FindTopology[amp] returns the topologies needed to classify all the scalar integrals present in the amplitude amp. By default it defines the found topologies. This can be avoided by setting the option DefineTopology->False."

Topologies::usage="Topologies is an option of the functions AmplitudeToTopologies and IntegralReduceKira. Topologies->{Topo1,Topo2,...} allows the user to select a subset of the topologies listed in AllTopologies[] to be used in the handling of the input amlitudes of AmplitudeToTopologies and IntegralReduceKira."

Topology::usage="Topology[TopoID] returns an association with all the information of the topology named TopoID."

LoopMomenta::usage="LoopMomenta is a symbol used as key in the associations generated by various functions of ANATAR."

ExternalMomenta::usage="ExternalMomenta is a symbol used as key in the associations generated by various functions of ANATAR."

Propagators::usage="Propagators is a symbol used as key in the associations generated by various functions of ANATAR."

LiteRedFile::usage="LiteRedFile is a symbol used as key in the associations generated by various functions of ANATAR."

RunLiteRed::usage="RunLiteRed[topoList] defines and reduces the topologies contained topoList by using LiteRed. If the input is All, then the list AllTopologies[] is used. Notice that the LiteRed package must be loaded before loading ANATAR."

If[!StringContainsQ[Save::usage, "ANATAR"],
    Save::usage = StringJoin["In ANATAR context, Save is an option of RunLiteRed and WriteLiteRedFile. When set to True, the result of the LiteRed reduction is saved on file.\n", Save::usage];
  ];

If[!StringContainsQ[Directory::usage, "ANATAR"],
    Directory::usage = StringJoin["In ANATAR context, Directory is an option of RunLiteRed and WriteLiteRedFile. It sets the name of the directory where the result of LiteRed reduction should be saved.\n", Directory::usage];
  ];

Scalars::usage="Scalars is an option of RunLiteRed and WriteLiteRedFile that lists the names of the scalar variables to be declared for LiteRed."

If[!StringContainsQ[Vectors::usage, "ANATAR"],
    Vectors::usage = StringJoin["In ANATAR context,Scalars is an option of RunLiteRed and WriteLiteRedFile that lists the names of the vector variables to be declared for LiteRed, other than the loop and external momenta that appear in the definition of the topology.\n", Vectors::usage];
  ];

LiteRedOptions::usage="LiteRedOptions is an option of RunLiteRed and WriteLiteRedFile that lists the rules which should be passed as options to the function NewDsBasis in LiteRed. Default is SolvejSector -> True."

IntegralReduceKira::usage="IntegralReduceKira[amp] reduces the defined topologies, identifies the loop integrals in the amplitude amp and substitutes back their respective reductions to master integrals."

WriteLiteRedFile::usage="WriteLiteRedFile writes the input file for LiteRed without executing it."

LiteRedMIs::usage="LiteRedMIs contains the list of master integrals obtained by LiteRed."

MasterIntegrals::usage="MasterIntegrals[] contains the list of master integrals in the last output amplitude generated by the function IntegralReduceKira."

BasisMI::usage="BasisMI is an option of IntegralReduceKira that allows the definition of the preferred basis of master integrals."

SeedSelection::usage="SeedSelection -> {r,s} is an option of IntegralReuceKira that allows the manual setting of the parameters r and s. "

OnlyWriteFiles::usage="OnlyWriteFiles is an option of IntegralReduceKira that, when set to True, writes all the required input files by Kira without executing them. "

IdentifyIntegrals::usage="IdentifyIntegrals[amp[Amplitudes]] returns the Feynman integrals present in the amplitudes expressions."

KiraSectors::usage="KiraSectors->{{topo1,sector1},{topo2,sector2},...} is an option of IntegralReduceKira that allows the selection of a sector for a given topology. The default value is Automatic."

OnlyImport::usage="OnlyImport->PathToFile is an option of IntegralReduceKira that imports, without the need to run Kira, the reduction of the integrals provided in the file located in PathToFile. When set to True, ANATAR imports the reduction in the predetermined location for the Kira files, without executing any job."

CutPropagators::usage="CutPropagators -> { Den[a1, b1], Den[a2,b2],...} specifies the set of cut propagators in the phase-space integrals passed to Kira for reduction to master integrals."

Block[{$Path = {Global`$AnatarPath, 
                Global`$AnatarPath<>"/Core",
                Global`$AnatarPath<>"/Interfaces"}
       },

<< "associations.m";
<< "LiteRed.m";
<< "QGraf.m";
<< "Kira.m";
<< "TemplatesForm.m";
<< "Amplitudes.m";
<< "Miscellaneous.m";
<< "Kinematics.m";
<< "Projectors.m";
<< "IntegralDecomposition.m";
<< "ProjectorsLib.m";

];


(* ::Section:: *)
(*Loading models*)


(* ::Subsection:: *)
(*Associations*)


GenerateParticleInformation[field_,s_]:=Module[{$dum},
   Association[Spin->s,
               Mass->(Last/@Cases[AN$Masses,{field,_}]/.{}->{0})[[1]],
               ParticleIndices-> First/@Cases[Flatten[$dum@@@#&/@AN$FieldsClassified],$dum[_,field] ]
               ]
          ];


GenerateAllParticleInformation[]:=Module[{work},
  Clear[Particle]; (* This avoids errors when loading models without quitting the kernel in between *)
   work={Particle[#],GenerateParticleInformation[#,1/2]}&/@AN$Fermions;
   Set@@@work;
   work={Particle[#],GenerateParticleInformation[#,0]}&/@AN$ScalarFields;
   Set@@@work;
   work={Particle[#],GenerateParticleInformation[#,1]}&/@AN$VectorFields;
   Set@@@work;
];


GenerateAnatarModelInformation[]:=
   Association[Fields->AN$Fields,
               Name->AN$ModelName,
               FermionFields->AN$Fermions,
               VectorFields->AN$VectorFields,
               ScalarFields->AN$ScalarFields,
               ModelParameters->AN$Parameters
               ];


(* ::Subsection:: *)
(*Main function*)


LoadModel::NoModelFolder="Model folder `1` not found.";
LoadModel::missingFiles="The files `1` were not found. Not possible to load the model `2`";
LoadModel::missingLists="The information associated to `1` was not found or is provided with the wrong syntax. Not possible to load the model `2`.";

LoadModel[model1_String] := Module[{folder,listFiles,missingFiles,missingLists,headsLists},

  Clear[ AN$ModelName, AN$Fields, AN$antiFields, AN$Fermions, AN$Bosons, AN$VectorFields, AN$ScalarFields, AN$xFermions, AN$yFermions, AN$NoCouplings, AN$Parameters, AN$Masses, AN$IndicesInFR, AN$FieldsClassified, AN$StructureConstantsList,  AN$GeneratorsList, AN$ExternalParameters ]; 

  folder = Global`$AnatarPath<>"/Models/"<>model1;

  If[DirectoryQ[folder], 

      AN$Model = model1;
      Get[folder<>"/General.m"];
      Print["The folder for the model "<>MakeGreen[ToString[model1],Boldmg->True]<>" was found."] , 

      Message[LoadModel::NoModelFolder,model1]&&Abort[];
  ];

  (* Check if all the files for the model exist *)

  listFiles = {"Couplings.frm","General.m","Polarizations.frm","Propagators.frm",model1<>"QG.qgraf","Vertices.frm"};
  missingFiles = Select[listFiles, !FileExistsQ[FileNameJoin[{folder, #}] ] &];

  If[ Length[missingFiles] === 0, 
      Print["All the required files were found."], 
      Message[LoadModel::missingFiles,missingFiles,model1]&&Abort[];
  ];

  (* Check if all the lists in General.m are correctly defined *)

  headsLists = {{"AN$ModelName", String}, {"AN$Fields", List}, {"AN$antiFields", List}, {"AN$Fermions", List}, 
 {"AN$Bosons", List}, {"AN$VectorFields", List}, {"AN$ScalarFields", List}, {"AN$xFermions", List}, 
 {"AN$yFermions", List}, {"AN$NoCouplings", Integer}, {"AN$Parameters", List}, {"AN$Masses", List}, 
 {"AN$IndicesInFR", List}, {"AN$FieldsClassified", List}, {"AN$StructureConstantsList", List}, 
 {"AN$GeneratorsList", List}, {"AN$ExternalParameters", List}};

  missingLists = Select[headsLists, (Head[Symbol[#[[1]] ] ] =!= #[[2]]) &];
  missingLists = missingLists[[;;,1]];

  If[ Length[missingLists] === 0, 
      Print["Model "<>MakeGreen[ToString[model1],Boldmg->True]<>" is correctly loaded." ];
      Print["The fields content can be found in the list"<>MakeGreen[" AN$Fields."] ], 
      Message[LoadModel::missingLists,missingLists,model1]&&Abort[];
  ];
  
    AnatarModel = GenerateAnatarModelInformation[];
    GenerateAllParticleInformation[];

];

FieldsInModel[model1_String] := Module[{folder},
	folderGen = Get[Global`$AnatarPath<>"/Models/"<>model1<>"/General.m"];
        Return[AN$Fields];];



(* ::Section:: *)
(* Off Shell *)


If[SetOffShell=!=True||SetOffShell=!=False, SetOffShell = False];

EndPackage[];

Protect[LoadModel, QGWrite, QGDiagrams, QGLoops, QGStyle, QGMessage, QGPartition, QGrafOptions, GenerateAmplitude,
     GenerateAmplitudeConjugate, GenerateAmplitudeSquare, OutputName, QGCouplingsSpecification, AllShifts,
     Loops, CouplingsOrder, RemovePropagator, Model, Dimension, SimplifiedOutput, PolarizationTrim,
     SelectAmplitudes, RecycleAmplitude, SelectCouplingsOrder, SubstitutionRules, Gauges, OffShell,
     OnlyDiagrams, SumDiagrams, AmplitudesMap, AmplitudesFullSimplify, AmplitudesSimplify, InsertCouplings,
     nnVectorSimplification, OverallFactor, CasimirValues, CouplingExpand, Amplitudes, Process, Name, 
     DiagramID, Den, ProjectorsBySpins, ProjectAmplitude, DefineTopology, AmplitudeToTopologies,
     IntegralReduceKira, Topologies, BasisMI, KiraSectors, SeedSelection, OnlyWriteFiles, OnlyImport,
     CutPropagators, FindTopology, GammaM, RXi, Metric, LeviCivita, T, f, SpinorU, SpinorUbar, SpinorV,
     SpinorVbar, SpinorUC, SpinorUbarC, SpinorVC, SpinorVbarC, PolV, PolVC, AN$LoopInt, TopInt
  ];
