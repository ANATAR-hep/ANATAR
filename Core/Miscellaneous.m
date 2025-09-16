(* ::Package:: *)

(*********************************************)
(*             Format of objects             *)
(*********************************************)

(* Format of Amplitude objects *)

(* Format of Gamma Matrices *)

ItalicBox[arg_]:=StyleBox[arg, FontSlant -> "Italic"];


MakeBoxes[GammaM[args___], TraditionalForm] := Module[{argList = {args}, n = Length[{args}], gammaArgs, spinIndices,
    gammaBoxes, timesBox, subscriptBoxes}, 

  gammaArgs = Take[argList, n - 2];
  spinIndices = Take[argList, -2];

  (* Classify arguments *)
  MomentumSymbolQ[s_Symbol] := StringMatchQ[SymbolName[s], ("k" | "p") ~~ DigitCharacter ...];
  LorentzSymbolQ[s_Symbol] := ! MomentumSymbolQ[s];

  (*Format*)
  formatGamma[arg_] := Which[
    
    (* Lorentz index *)
    AtomQ[arg] && LorentzSymbolQ[arg] || arg===5, SuperscriptBox["\[Gamma]",MakeBoxes[arg,StandardForm] ],
    (* Single momentum *)
    AtomQ[arg] && MomentumSymbolQ[arg], RowBox[{"(", "\[Gamma]", "\[CenterDot]", MakeBoxes[arg, StandardForm], ")"}],
    (* sums of momenta *)
    True, RowBox[{"(", "\[Gamma]", "\[CenterDot]", RowBox[{"(", MakeBoxes[arg, StandardForm], ")"}], ")"}] 
    
    ];
  gammaBoxes = formatGamma /@ gammaArgs;
  (* Here we could at whatever symbol we want in between Gammas *)
  timesBox = If[Length[gammaBoxes] == 1, First[gammaBoxes], RowBox@Riffle[gammaBoxes, ""] ];

  (* Spin indices for subscript *)
  getCleanName[s_Symbol] := SymbolName[s];
  getCleanName[expr_] := ToString[expr, InputForm];

  subscriptBoxes = Riffle[ItalicBox[#]&/@(getCleanName/@spinIndices), " "];

  (* Final format *)
  If[MatchQ[timesBox, RowBox[{ "(", ___, ")" }] ], SubscriptBox[RowBox[{ timesBox}], RowBox[subscriptBoxes] ], SubscriptBox[RowBox[{"(", timesBox, ")"}], RowBox[subscriptBoxes] ] ]
];

GammaM/:MakeBoxes[GammaM[1,sp1_,sp2_],TraditionalForm]:=RowBox[{SubscriptBox[ "\[DoubleStruckCapitalI]", RowBox[{ItalicBox[ToString[sp1] ],ItalicBox[ToString[sp2] ] }] ] }] ;


RXi/:MakeBoxes[RXi,TraditionalForm]:=RowBox[{SubscriptBox["\[Xi]",RowBox[{ToString[G]}] ]}];

Metric/:MakeBoxes[Metric[gluon___],TraditionalForm]/;(StringContainsQ[#, "Gluon"]&@ToString[{gluon}[[1]] ] ) :=RowBox[{SuperscriptBox[ "\[Delta]", RowBox[Map[ToString,{gluon}] ] ] }] ; 
Metric/:MakeBoxes[Metric[colour___],TraditionalForm]/;(StringContainsQ[#, "Colour"]&@ToString[{colour}[[1]] ] ) :=RowBox[{SuperscriptBox[ "\[Delta]", RowBox[Map[ToString,{colour}] ] ] }] ; 
Metric/:MakeBoxes[Metric[lorentz___],TraditionalForm]/;(Not@StringContainsQ[#, "Gluon"] &@ToString[{lorentz}[[1]]]):=RowBox[{SuperscriptBox[ "g", RowBox[Map[ToString,{lorentz}] ] ] }] ; 

LeviCivita/:MakeBoxes[LeviCivita[lorentz___],TraditionalForm] := RowBox[{SubscriptBox["\[CurlyEpsilon]", RowBox[Map[ToString,{lorentz}] ] ]}];


T/:MakeBoxes[T[clr1_,clr2_,gluons___],TraditionalForm]:=RowBox[{SubsuperscriptBox[ "T", RowBox[{ItalicBox[ToString[clr1] ],ItalicBox[ToString[clr2] ]}], RowBox[Map[ToString,{gluons}] ] ] }] ;
f/:MakeBoxes[f[gluons___],TraditionalForm]:=RowBox[{SuperscriptBox[ "f", RowBox[Map[ToString,{gluons}] ] ] }] ; 

SpinorU/:MakeBoxes[SpinorU[aa_,p_,m_,spn_],TraditionalForm]:=RowBox[{SubscriptBox[ RowBox[{"u"}] , RowBox[{ItalicBox[ToString[spn] ]}] ],"(", RowBox[{ToString[p]}],",", RowBox[{ToString[m]}],")"  }] ;
SpinorUbar/:MakeBoxes[SpinorUbar[aa_,p_,m_,spn_],TraditionalForm]:=RowBox[{SubscriptBox[RowBox[{"\*OverscriptBox[\(u\), \(_\)]"} ],RowBox[{ItalicBox[ToString[spn] ]}] ],"(", RowBox[{ToString[p]}],",", RowBox[{ToString[m]}],")"  }] ;
SpinorV/:MakeBoxes[SpinorV[aa_,p_,m_,spn_],TraditionalForm]:=RowBox[{SubscriptBox[ RowBox[{"v"}] , RowBox[{ItalicBox[ToString[spn] ]}] ],"(", RowBox[{ToString[p]}],",", RowBox[{ToString[m]}],")"  }] ;
SpinorVbar/:MakeBoxes[SpinorVbar[aa_,p_,m_,spn_],TraditionalForm]:=RowBox[{SubscriptBox[RowBox[{"\*OverscriptBox[\(v\), \(_\)]"} ],RowBox[{ItalicBox[ToString[spn] ]}] ],"(", RowBox[{ToString[p]}],",", RowBox[{ToString[m]}],")"  }] ;

SpinorU/:MakeBoxes[SpinorU[aa_,p_,m_,spn_,clr_],TraditionalForm]:=RowBox[{SubsuperscriptBox[ RowBox[{"u"}] , RowBox[{ItalicBox[ToString[spn] ]}], RowBox[{ToString[clr]}] ],"(", RowBox[{ToString[p]}],",", RowBox[{ToString[m]}],")"  }] ;
SpinorUbar/:MakeBoxes[SpinorUbar[aa_,p_,m_,spn_,clr_],TraditionalForm]:=RowBox[{SubsuperscriptBox[ RowBox[{"\*OverscriptBox[\(u\), \(_\)]"} ] , RowBox[{ItalicBox[ToString[spn] ]}], RowBox[{ToString[clr]}] ],"(", RowBox[{ToString[p]}],",", RowBox[{ToString[m]}],")"  }] ;
SpinorV/:MakeBoxes[SpinorV[aa_,p_,m_,spn_,clr_],TraditionalForm]:=RowBox[{SubsuperscriptBox[ RowBox[{"v"}] , RowBox[{ItalicBox[ToString[spn] ]}], RowBox[{ToString[clr]}] ],"(", RowBox[{ToString[p]}],",", RowBox[{ToString[m]}],")"  }] ;
SpinorVbar/:MakeBoxes[SpinorVbar[aa_,p_,m_,spn_,clr_],TraditionalForm]:=RowBox[{SubsuperscriptBox[ RowBox[{"\*OverscriptBox[\(v\), \(_\)]"} ] , RowBox[{ItalicBox[ToString[spn] ]}], RowBox[{ToString[clr]}] ],"(", RowBox[{ToString[p]}],",", RowBox[{ToString[m]}],")"  }] ;

SpinorUC/:MakeBoxes[SpinorUC[aa_,p_,m_,spn_],TraditionalForm]:=RowBox[{SubscriptBox[ RowBox[{"\*SuperscriptBox[\(u\), \(\[Dagger]\)]"}] , RowBox[{ItalicBox[ToString[spn] ]}] ],"(", RowBox[{ToString[p]}],",", RowBox[{ToString[m]}],")"  }] ;
SpinorUbarC/:MakeBoxes[SpinorUbarC[aa_,p_,m_,spn_],TraditionalForm]:=RowBox[{SubscriptBox[RowBox[{"\*SuperscriptBox[\( \*OverscriptBox[\(u\), \(_\)]\), \(\[Dagger]\)]"} ],RowBox[{ItalicBox[ToString[spn] ]}] ],"(", RowBox[{ToString[p]}],",", RowBox[{ToString[m]}],")"  }] ;
SpinorVC/:MakeBoxes[SpinorVC[aa_,p_,m_,spn_],TraditionalForm]:=RowBox[{SubscriptBox[ RowBox[{"\*SuperscriptBox[\(v\), \(\[Dagger]\)]"}] , RowBox[{ItalicBox[ToString[spn] ]}] ],"(", RowBox[{ToString[p]}],",", RowBox[{ToString[m]}],")"  }] ;
SpinorVbarC/:MakeBoxes[SpinorVbarC[aa_,p_,m_,spn_],TraditionalForm]:=RowBox[{SubscriptBox[RowBox[{"\*SuperscriptBox[\( \*OverscriptBox[\(v\), \(_\)]\), \(\[Dagger]\)]"} ],RowBox[{ItalicBox[ToString[spn] ]}] ],"(", RowBox[{ToString[p]}],",", RowBox[{ToString[m]}],")"  }] ;

SpinorUC/:MakeBoxes[SpinorUC[aa_,p_,m_,spn_,clr_],TraditionalForm]:=RowBox[{SubsuperscriptBox[ RowBox[{"\*SuperscriptBox[\(u\), \(\[Dagger]\)]"}] , RowBox[{ItalicBox[ToString[spn] ]}], RowBox[{ToString[clr]}] ],"(", RowBox[{ToString[p]}],",", RowBox[{ToString[m]}],")"  }] ;
SpinorUbarC/:MakeBoxes[SpinorUbarC[aa_,p_,m_,spn_,clr_],TraditionalForm]:=RowBox[{SubsuperscriptBox[ RowBox[{"\*SuperscriptBox[\( \*OverscriptBox[\(u\), \(_\)]\), \(\[Dagger]\)]"} ] , RowBox[{ItalicBox[ToString[spn] ]}], RowBox[{ToString[clr]}] ],"(", RowBox[{ToString[p]}],",", RowBox[{ToString[m]}],")"  }] ;
SpinorVC/:MakeBoxes[SpinorVC[aa_,p_,m_,spn_,clr_],TraditionalForm]:=RowBox[{SubsuperscriptBox[ RowBox[{"\*SuperscriptBox[\(v\), \(\[Dagger]\)]"}] , RowBox[{ItalicBox[ToString[spn] ]}], RowBox[{ToString[clr]}] ],"(", RowBox[{ToString[p]}],",", RowBox[{ToString[m]}],")"  }] ;
SpinorVbarC/:MakeBoxes[SpinorVbarC[aa_,p_,m_,spn_,clr_],TraditionalForm]:=RowBox[{SubsuperscriptBox[ RowBox[{"\*SuperscriptBox[\( \*OverscriptBox[\(v\), \(_\)]\), \(\[Dagger]\)]"} ] , RowBox[{ItalicBox[ToString[spn] ]}], RowBox[{ToString[clr]}] ],"(", RowBox[{ToString[p]}],",", RowBox[{ToString[m]}],")"  }] ;

PolV/:MakeBoxes[PolV[aa_,lor_,p_,m_,gl_],TraditionalForm]:=RowBox[{ToBoxes[Subscript[\[CurlyEpsilon],lor]^gl],"(", RowBox[{ToString[p]}],",", RowBox[{ToString[m]}],")"  }] ;
PolVC/:MakeBoxes[PolVC[aa_,lor_,p_,m_,gl_],TraditionalForm]:=RowBox[{SubsuperscriptBox[ RowBox[{"\*SuperscriptBox[\(\[CurlyEpsilon]\), \(\[Dagger]\)]"}] , RowBox[{ToString[lor] }], RowBox[{ToString[gl]}] ],"(", RowBox[{ToString[p]}],",", RowBox[{ToString[m]}],")"  }] ;

PolV/:MakeBoxes[PolV[aa_,lor_,p_,m_],TraditionalForm]:=RowBox[{ToBoxes[Subscript[\[CurlyEpsilon],lor] ],"(", RowBox[{ToString[p]}],",", RowBox[{ToString[m]}],")"  }] ;
PolVC/:MakeBoxes[PolVC[aa_,lor_,p_,m_],TraditionalForm]:=RowBox[{SubsuperscriptBox[ RowBox[{"\*SuperscriptBox[\(\[CurlyEpsilon]\), \(\[Dagger]\)]"}] , RowBox[{ToString[lor] }], "" ],"(", RowBox[{ToString[p]}],",", RowBox[{ToString[m]}],")"  }] ;


(* Format of LoopIntegrals *)

AN$LoopInt/:MakeBoxes[AN$LoopInt[topo_[args___] ],TraditionalForm]/;(topo=!=Plus&&topo=!=Times):=RowBox[{SuperscriptBox["\[ScriptCapitalJ]",RowBox[{ToString[topo]}] ],"(",Sequence@@Riffle[Map[ToBoxes,{args}],","],")"}];

TopInt/:MakeBoxes[TopInt[topo_, args___ ],TraditionalForm]/;(topo=!=Plus&&topo=!=Times):=RowBox[{SuperscriptBox["\[ScriptCapitalJ]",RowBox[{ToString[topo]}] ],"(",Sequence@@Riffle[Map[ToBoxes,{args}],","],")"}];


(* Linear properties of TopInt Loop Integrals *)

TopInt/:TopInt[a_+b_]:=TopInt[a]+TopInt[b];
TopInt/:TopInt[Times[c_,expr_] ]/;(Head[expr]=!=Symbol):=c TopInt[expr];
TopInt/:TopInt[0]:=0 ;
TopInt[x_] /; NumberQ[x] := x;
TopInt[x_ -> y_] := (TopInt[x] -> TopInt[y]);
TopInt[TopInt[x___] ] := TopInt[x];


(* Properties of the Levi Civita *)

LeviCivita[___, a_, ___, b_, ___] /; a === b = 0;



(*********************************************)
(*     Function that does trace computation  *)
(*********************************************)

Options[GenerateTrace4] := {Dimension -> "None", DiracMatrices -> {}, DiracMomenta -> {}, Symbols -> {}};

GenerateTrace4[expr__, opts : OptionsPattern[] ] := Block[{Formfile, TraceExpr, TraceDimension, TraceMatrices, TraceMomenta, TraceSymbols},

  If[ OptionValue[Dimension] === "None", TraceDimension = 4, TraceDimension = OptionValue[Dimension] ];
  If[OptionValue[Dimension] === D, TraceDimension = n];

  If[ OptionValue[DiracMatrices] === {}, TraceMatrices = StringTrim[#, ("{" | "}")] &@ToString@expr, TraceMatrices = StringTrim[#, ("{" | "}")] &@
     ToString@OptionValue[DiracMatrices] ];

  If[OptionValue[DiracMomenta] != {}, TraceMomenta = StringTrim[#, ("{" | "}")] &@ToString@OptionValue[DiracMomenta] ];
  If[OptionValue[Symbols] != {}, TraceSymbols = StringTrim[#, ("{" | "}")] &@ToString@OptionValue[Symbols] ];

  ( OperatorTr[ Expr1_] := (Intersection[Expr1, OptionValue[Symbols] ] /. {List :> Times}) (GammaM @@ (Complement[Expr1, OptionValue[Symbols] ]) /. {List :> Times});

     TraceExpr = expr; 
   If[MemberQ[TraceExpr, 5] === True, {Pos2 = Position[TraceExpr, 5]; Expr2 = TraceExpr;
          
   Table[ If[Pos2[[i, 1]] === 1, Expr2 = SubsetReplace[ Expr2, {TraceExpr[[-1]], TraceExpr[[1]]} :> Gamma5[TraceExpr[[-1]]] ],
                Expr2 = SubsetReplace[ Expr2, {TraceExpr[[Pos2[[i, 1]] - 1]], TraceExpr[[Pos2[[i, 1]]]]} :> Gamma5[TraceExpr[[Pos2[[i, 1]] - 1]] ] ] ], {i, 1, Length[Pos2]}];
          TraceExpr = Flatten@Expr2}];
    
      ExpandNCM[(h : NonCommutativeMultiply)[a___, b_Plus, c___] ] := Distribute[h[a, b, c], Plus, h, Plus, ExpandNCM[h[##] ] &] ;
      ExpandNCM[a_] := ExpandAll[a];
      Expr1 = ExpandNCM@# &@( TraceExpr /. {List -> NonCommutativeMultiply}) /.  Power[a_, b_] :> (Inactive[Times] @@ ConstantArray[a, b]);
      If[ Length[Expr1] === 0, {TraceExpr = GammaM@Expr1; Goto["LabelExpr"]}];
      If[(Head[Expr1] === Plus), ConExpr = List @@ Expr1, TraceExpr = GammaM @@ Expr1];
      If[(Head[Expr1] === Plus), Expr2 = Table[ GammaM[(ConExpr[[i]]) /.  NonCommutativeMultiply -> List /. {Times[a_, Levieps[b_, c_, d_, e_]] -> List@{a, Levieps[b, c, d, e]}}], {i, 1, Length[ConExpr]}];
       Expr3 = Table[Expr2[[i]] /. {GammaM -> List}, {i, 1, Length[Expr2]}];
       TraceExpr = Table[OperatorTr[#] & @@ Expr3[[i]], {i, 1, Length[Expr3]}] /.  List -> Plus /. Inactive[Times] :> Sequence];


     Label["LabelExpr"];
     Gamma5Subs = {GammaM[Gamma5[5]] :> GammaM[1],
                   GammaM[ Gamma5[beta_]] :>  (im/6*Levieps[ beta, #1, #2, #3] GammaM[#1, #2, #3]) & @@ Unique[{"indexfive", "indexfive", "indexfive"}],
	           GammaM[a__, Gamma5[5]] :> GammaM[a],
                   GammaM[a__, Gamma5[5], b__] :> GammaM[a, b],
                   GammaM[Gamma5[5], b__] :> GammaM[b], 
                   GammaM[Gamma5[beta_], b__] :> (im/6*Levieps[ beta, #1, #2, #3] GammaM[#1, #2, #3, b]) & @@ Unique[{"indexfive", "indexfive", "indexfive"}],
                   GammaM[a__, Gamma5[beta_], b__] :> (im/6*Levieps[ beta, #1, #2, #3] GammaM[a, #1, #2, #3, b]) & @@ Unique[{"indexfive", "indexfive", "indexfive"}],
                   GammaM[a__, Gamma5[beta_]] :> (im/6*Levieps[ beta, #1, #2, #3] GammaM[ a, #1, #2, #3]) & @@ Unique[{"indexfive", "indexfive", "indexfive"}]};
      TraceExpr = InputForm[TraceExpr //. Gamma5Subs];
      Formfile = Global`$AnatarPath <> "/Trace.frm";
      OpenWrite[Formfile];
      WriteString[Formfile, "#-\n"];
      WriteString[Formfile, "off statistics,finalstats,allwarnings;\n"];
      WriteString[Formfile, "FunPowers allfunpowers;\n"];
      If[IntegerQ[TraceDimension] == False, WriteString[Formfile, "Symbols " <> ToString[TraceDimension] <> ";\n"]];
      If[IntegerQ[TraceDimension] == False, WriteString[Formfile, "dimension " <> ToString[TraceDimension] <> " ;\n"]];
      If[OptionValue[DiracMomenta] != {}, WriteString[Formfile, "Vectors   " <> ToString[TraceMomenta] <> " ;\n"]];
      WriteString[Formfile, "Symbols   x,n,im;\n"];
      WriteString[Formfile, "Indices   " <> ToString[TraceMatrices] <> " ;\n"];
      WriteString[Formfile, "Autodeclare index    mu,nu,alpha,beta,la,si,indexfive;\n"];
      If[OptionValue[Symbols] != {}, WriteString[Formfile, "Symbols   " <> ToString[TraceSymbols] <> " ;\n"]];
      WriteString[Formfile, "CF  GammaM, Levieps(antisymmetric);\n"];
      WriteString[Formfile, "\n"];
      WriteString[Formfile, "g TraceExpr =  " <> StringReplace[#, {"[" -> "(", "]" -> ")"}] &@ ToString[TraceExpr] <> ";\n"];
      WriteString[Formfile, ".sort;\n"];
      WriteString[Formfile, "\n"];
      WriteString[Formfile, "id GammaM(?a) = g_(1,?a);\n"];
      WriteString[Formfile, "Trace" <> ToString[TraceDimension] <> ",1;\n"];
      WriteString[Formfile, ".sort;\n"];
      WriteString[Formfile, "id im^4 = 1;\n"];
      WriteString[Formfile, "id im^3 = -im;\n"];
      WriteString[Formfile, "id im^2 = -1;\n"];
      WriteString[Formfile, ".sort\n"];
      WriteString[Formfile, "id Levieps(mu1?,nu1?,la1?,si1?)*Levieps(mu2?,nu2?,la2?,si2?) = \n"];
      WriteString[Formfile, "-d_(mu1,mu2)*(d_(nu1,nu2)*d_(la1,la2)*d_(si1,si2)\n"];
      WriteString[Formfile, "                + d_(si1,nu2)*d_(nu1,la2)*d_(la1,si2)\n"];
      WriteString[Formfile, "                + d_(la1,nu2)*d_(si1,la2)*d_(nu1,si2)\n"];
      WriteString[Formfile, "                - d_(si1,nu2)*d_(la1,la2)*d_(nu1,si2)\n"];
      WriteString[Formfile, "                - d_(la1,nu2)*d_(nu1,la2)*d_(si1,si2)\n"];
      WriteString[Formfile, "                - d_(nu1,nu2)*d_(si1,la2)*d_(la1,si2))\n"];
      WriteString[Formfile, "    +d_(nu1,mu2)*(d_(mu1,nu2)*d_(la1,la2)*d_(si1,si2)\n"];
      WriteString[Formfile, "                + d_(si1,nu2)*d_(mu1,la2)*d_(la1,si2)\n"];
      WriteString[Formfile, "                + d_(la1,nu2)*d_(si1,la2)*d_(mu1,si2)\n"];
      WriteString[Formfile, "                - d_(si1,nu2)*d_(la1,la2)*d_(mu1,si2)\n"];
      WriteString[Formfile, "                - d_(la1,nu2)*d_(mu1,la2)*d_(si1,si2)\n"];
      WriteString[Formfile, "                - d_(mu1,nu2)*d_(si1,la2)*d_(la1,si2))\n"];
      WriteString[Formfile, "    -d_(la1,mu2)*(d_(mu1,nu2)*d_(nu1,la2)*d_(si1,si2)\n"];
      WriteString[Formfile, "                + d_(si1,nu2)*d_(mu1,la2)*d_(nu1,si2)\n"];
      WriteString[Formfile, "                + d_(nu1,nu2)*d_(si1,la2)*d_(mu1,si2)\n"];
      WriteString[Formfile, "                - d_(si1,nu2)*d_(nu1,la2)*d_(mu1,si2)\n"];
      WriteString[Formfile, "                - d_(nu1,nu2)*d_(mu1,la2)*d_(si1,si2)\n"];
      WriteString[Formfile, "                - d_(mu1,nu2)*d_(si1,la2)*d_(nu1,si2))\n"];
      WriteString[Formfile, "    +d_(si1,mu2)*(d_(mu1,nu2)*d_(nu1,la2)*d_(la1,si2)\n"];
      WriteString[Formfile, "                + d_(la1,nu2)*d_(mu1,la2)*d_(nu1,si2)\n"];
      WriteString[Formfile, "                + d_(nu1,nu2)*d_(la1,la2)*d_(mu1,si2)\n"];
      WriteString[Formfile, "                - d_(la1,nu2)*d_(nu1,la2)*d_(mu1,si2)\n"];
      WriteString[Formfile, "                - d_(nu1,nu2)*d_(mu1,la2)*d_(la1,si2)\n"];
      WriteString[Formfile, "                - d_(mu1,nu2)*d_(la1,la2)*d_(nu1,si2));\n"];
      WriteString[Formfile, ".sort\n"];
      WriteString[Formfile, "\n"];
      WriteString[Formfile, "format Mathematica;\n"];
      If[FileExistsQ[FileNameJoin[{Global`$AnatarPath, "TraceResult.m"}] ] ===True, DeleteFile[FileNameJoin[{Global`$AnatarPath, "TraceResult.m"}] ] ];
      WriteString[Formfile, "#write<" <> Global`$AnatarPath <> "/TraceResult.m> \"( %E )\", TraceExpr;\n"];
      WriteString[Formfile, ".sort\n"];
      WriteString[Formfile, "print+s TraceExpr;\n"];
      WriteString[Formfile, ".sort \n"];
      WriteString[Formfile, ".end "];
      Close[Formfile];
      Run["form Trace.frm >" <> "Trace.log"];
      FilePrint[FileNameJoin@"TraceResult.m"];
      Run["rm  Trace.log"];)
    ]



(* ::Section:: *)
(*Some useful functions*)


DeleteFileIfExistsQ[file_String] := If[FileExistsQ[file], DeleteFile[file] ];
MoveFileIfExistsQ[file1_String, file2_String] := If[FileExistsQ[file1],
                                                     DeleteFileIfExistsQ[file2];
                                                     CopyFile[file1, file2];
                                                     DeleteFile[file1];
                                                     ];


subsOnShell[statement_] := Block[{output},
                          
                            If[SetOffShell === False, 
                              output = ReplaceRepeated[statement, AN$Kinematics], 
                              output = statement];

                            Return[output]
                          ]

(* To check the polynomial degree of numerator *)

NumeratorDegree[expr_, var_] := Module[{expandedExpr, newExpr, powers},
  
  newExpr = Expand[expr /. Dot[a_, b_] :> Times[a, b] ];
  powers = Exponent[#, var] & /@ List @@ newExpr;
  DeleteDuplicates[powers] 
  
  ]

(*******************************************************************)
(*             Finding Fermion Loops from Qgraf output             *)
(*******************************************************************)

ExtractProTerms[expr_] := Cases[expr, pro[__], Infinity]
Options[FindFermionLoop] := {SelectDiagrams -> "All"}

FindFermionLoop[amp_, opts : OptionsPattern[]] := Module[{content, parsedExpressions, splitTerms, vrtxlist, 
   filteredvrtxlist, selectdiagrams, iRange, exprList, labels, rawExpressions, proTerms, Fermions, AntiFermions, AllFermions, 
   VrtxNumlist, Fermionlist, Proplist, pairs, Invpairs, Invvrtxlist, fermionCount, fermionCount1, fermionLoopCount, TotalfermionCount, 
   fermionLoopCountsList = {}, oneloopfermionlist, twoloopfermionlist, threeloopfermionlist, fourloopfermionlist, filteredMatches, 
   matchingElements, filteredMatchingElementsAll, cleanedContent,foldername,looporder}, 
   Get[Global`$AnatarPath <> "/Models/" <> AN$Model <> "/General.m"];
   AllFermions = AN$Fermions;
   Fermions = AN$xFermions;
   AntiFermions = AN$yFermions;
   foldername=amp[Name];
   looporder=ToString[amp[LoopOrder]];
   content = Import[Global`$AnatarPath <> "/Outputs/" <> foldername<>"/"<>foldername<>"_"<>looporder, "Text"];
   cleanedContent = StringJoin@ StringRiffle[ Select[StringSplit[content, "\n"], ! StringStartsQ[#, "*"] &], " "];
   rawExpressions = Select[StringSplit[cleanedContent, ";"], StringTrim[#] =!= "" &];
   exprList = rawExpressions;
   labels = StringCases[rawExpressions, "id " ~~ name : ("a" ~~ NumberString) ~~ " =" ~~ __ :> name];
   exprList = StringReplace[#, {"id " -> "", "(" -> "[", ")" -> "]", "*" -> " ", " +" -> "", "- " -> "-", "=" -> "", 
        StartOfString ~~ (DigitCharacter ..) ~~ " " -> "$1 * "}] & /@exprList;
   parsedExpressions = ToExpression["HoldComplete[" <> # <> "]", StandardForm, HoldComplete] & /@ exprList;
   proTerms = ExtractProTerms /@ parsedExpressions;
   splitTerms = Flatten /@ (proTerms /.  pro[x_[a_, b_, k_]] :> If[MemberQ[Fermions, x], {x[a, k], 
          ToExpression[ToString[x] <> "bar"][b, -k]}, Nothing]);
   vrtxlist = Cases[#, vrtx[x__], Infinity] & /@ parsedExpressions;
   filteredvrtxlist = vrtxlist /.  vrtx[args___] :> Select[{args}, MemberQ[AllFermions, Head[#]] &];
   selectdiagrams = OptionValue[SelectDiagrams];
   iRange = If[selectdiagrams === "All", Range[Length@splitTerms], Flatten[List @@ (selectdiagrams /. Span -> Range)]];
   Do[If[splitTerms[[i]] === {}, AppendTo[fermionLoopCountsList, {i, 0}], 
     fermionLoopCount = fermionCount = fermionCount1 = 0;
     VrtxNumlist = Fermionlist = {};
     TotalfermionCount = Length[splitTerms[[i]]];
     pairs = Thread[splitTerms[[i]][[;; ;; 2]] -> splitTerms[[i]][[2 ;; ;; 2]]];
     Invpairs = Thread[splitTerms[[i]][[2 ;; ;; 2]] -> splitTerms[[i]][[;; ;; 2]]];
     Proplist = List @@@ pairs;
     filteredvrtxlist[[i]] = DeleteCases[filteredvrtxlist[[i]], {}];
     If[filteredvrtxlist[[i]] === {}, AppendTo[fermionLoopCountsList, {i, 0}], 
      While[fermionCount < TotalfermionCount, 
       VrtxNumlist = Union[VrtxNumlist, Select[filteredvrtxlist[[i, 1]], #[[1]] > 0 &][[All, 1]]];
       Invvrtxlist = Select[filteredvrtxlist[[i, 1]], #[[1]] > 0 &] /.  Flatten[{pairs, Invpairs}];
       VrtxNumlist = Union[VrtxNumlist, Invvrtxlist[[All, 1]]];
       Fermionlist = Union[Fermionlist, VrtxNumlist];
       If[ AnyTrue[filteredvrtxlist[[i]], Sort[#] === Sort[Invvrtxlist] &] || MemberQ[Sort /@ Proplist, Sort[Invvrtxlist]], 
        fermionLoopCount++;
        fermionCount1 = Length@Fermionlist;
        filteredvrtxlist[[i]] = 
         Select[filteredvrtxlist[[i]], FreeQ[VrtxNumlist, #[[1, 1]]] &];
        VrtxNumlist = {}, fermionCount1 = Length@Fermionlist];
       While[ fermionCount1 < TotalfermionCount || Length[VrtxNumlist] > 0, 
        If[Length[VrtxNumlist] == 0, Break[]];
        filteredMatchingElementsAll = {};
        Do[ matchingElements = Select[filteredvrtxlist[[i]], MemberQ[#, Invvrtxlist[[m]]] &];
         filteredMatches = Table[Select[match, # =!= Invvrtxlist[[m]] &], {match, matchingElements}];
         filteredMatchingElementsAll = Join[filteredMatchingElementsAll, Flatten[filteredMatches]], {m, Length[Invvrtxlist]}];
        VrtxNumlist = Union[VrtxNumlist, Select[filteredMatchingElementsAll, #[[1]] > 0 &][[All, 1]]];
        Invvrtxlist = Select[filteredMatchingElementsAll, #[[1]] > 0 &] /.  Flatten[{pairs, Invpairs}];
        If[Invvrtxlist === {} && Length[filteredvrtxlist[[i]]] > 0, filteredvrtxlist[[i]] = Select[filteredvrtxlist[[i]], 
           Intersection[{#[[1, 1]], #[[2, 1]]}, VrtxNumlist] === {} &];
         If[Length[filteredvrtxlist[[i]]] > 0, VrtxNumlist = Union[VrtxNumlist, Select[filteredvrtxlist[[i, 1]], #[[1]] > 0 &][[All, 1]]];
          Invvrtxlist = Select[filteredvrtxlist[[i, 1]], #[[1]] > 0 &] /.  Flatten[{pairs, Invpairs}], Break[]]];
        VrtxNumlist = Union[VrtxNumlist, Invvrtxlist[[All, 1]]];
        Fermionlist = Union[Fermionlist, VrtxNumlist];
        fermionCount1 = Length@Fermionlist;
        If[ AnyTrue[filteredvrtxlist[[i]], Sort[#] === Sort[Invvrtxlist] &] || MemberQ[Sort /@ Proplist, Sort[Invvrtxlist]], 
         fermionLoopCount++;
         fermionCount1 = Length@Fermionlist;
         filteredvrtxlist[[i]] = 
          Select[filteredvrtxlist[[i]], 
           FreeQ[VrtxNumlist, #[[1, 1]]] &];
         VrtxNumlist = {}, 
         If[fermionCount1 === TotalfermionCount, VrtxNumlist = {}]];];
       fermionCount += fermionCount1;];
      AppendTo[fermionLoopCountsList, {i, fermionLoopCount}]]], {i, iRange}];
   If[AllTrue[fermionLoopCountsList[[All, 2]], # == 0 &], Print["There is no fermion loop in any diagram"]];
   oneloopfermionlist = Select[fermionLoopCountsList, #[[2]] == 1 &];
   If[oneloopfermionlist =!= {}, Print["Diagrams ", oneloopfermionlist[[All, 1]], " contains one fermion loop"]];
   twoloopfermionlist = Select[fermionLoopCountsList, #[[2]] == 2 &];
   If[twoloopfermionlist =!= {}, Print["Diagrams ", twoloopfermionlist[[All, 1]], " contains two fermion loops"]];
   threeloopfermionlist = Select[fermionLoopCountsList, #[[2]] == 3 &];
   If[threeloopfermionlist =!= {}, Print["Diagrams ", threeloopfermionlist[[All, 1]], " contains three fermion loops"]];
   fourloopfermionlist = Select[fermionLoopCountsList, #[[2]] == 4 &];
   If[fourloopfermionlist =!= {}, Print["Diagrams ", fourloopfermionlist[[All, 1]], " contains four fermion loops"]];]
