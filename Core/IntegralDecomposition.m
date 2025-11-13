(*********************************************)
(*     Function for topology classification  *)
(*********************************************)
FilterLoopDependentDen[expr_, loopMomenta_List] := Module[{factors, base, keepQ},
  factors = If[Head[expr] === Times, List @@ expr, {expr}];
  base[term_] := If[Head[term] === Power, First[term], term];
  keepQ[term_] := ! FreeQ[base[term], Alternatives @@ loopMomenta];
  Times @@ Select[factors, keepQ]]

canonicalDen[a_, b_] := Module[{expanded, terms, extractKey, sortedTerms, expr, leading},
        expanded = Expand[a];
        terms = If[Head[expanded] === Plus, List @@ expanded, {expanded}];
  extractKey[term_] := Module[{symbol, name, base, index},
        symbol = If[Head[term] === Times, Last[List @@ term], term];
        name = SymbolName[symbol];
        {base, index} = First@(StringCases[name, x : LetterCharacter .. ~~ y : DigitCharacter .. :> {x,
            ToExpression[y]}] /. {} -> {{name, 0}});
     {base, index}];
        sortedTerms = SortBy[terms, extractKey];
        expr = Total[sortedTerms];
        leading = First[sortedTerms];
        If[MatchQ[leading, Times[-1, ___]], Den[-expr, b], Den[expr, b]]];


GenerateTopologies::invalidAmplitude   = "invalidAmplitude:`1`";
GenerateTopologies::invalidTopology    = "invalidTopology:`1`";
GenerateTopologies::invalidAmplitude   = "Invalid format of the amplitude for topology indentification";
GenerateTopologies::invalidTopology    = "Invalid format of the reference topology";

GenerateTopologies[Amp_, topo_,loopMom_] := 
  Module[{SortedTOPORules, ListofDen, SubTopoInv, SubTopo, ProdDen, 
    denTotopo, TOPORules, TOPOList, result,ProdDen0,kVars,absRules,signFlipRule, withoutTopInt, withTopInt, finalresult},


    If[!MemberQ[{Plus, Times}, Head[Amp] ], Message[GenerateTopologies::invalidAmplitude ];Abort[] ]; 
  
    If[Head[topo]=!=Rule, Message[GenerateTopologies::invalidTopology ];Abort[] ]; 
   
   ListofDen = topo[[2]];
   SubTopoInv = {
         nameoftopo_[arg___]^(n_) :>  (nameoftopo @@ (-{arg}))^(-n)/;n < 0,
         nameoftopo_[arg___]^-1 :>  (nameoftopo @@ (-{arg}))};

   SubTopo = {
         nameoftopo_[arg___]^(n_) :>  (nameoftopo @@ (-{arg}))^(-n)/;n < 0,
         nameoftopo_[arg___]^-1 :>  (nameoftopo @@ (-{arg})),
         nameoftopo_[arg1___]^n_*nameoftopo_[arg2___]^m_ :> nameoftopo  @@ (n*{arg1} + m*{arg2}),
         nameoftopo_[arg1___]^n_*nameoftopo_[arg2___] :> nameoftopo  @@ (n*{arg1} + {arg2}),
         nameoftopo_[arg1___]*nameoftopo_[arg2___] :> nameoftopo @@ ({arg1} + {arg2}),
         nameoftopo_[arg1___]^n_ :> nameoftopo  @@ (n*{arg1}),
         nameoftopo_[arg1___]*nameoftopo_[arg2___] :> nameoftopo @@ ({arg1} + {arg2})};
   
   denTotopo = Table[{ListofDen[[i]] ->  topo[[1]] @@ (UnitVector[Length[ListofDen], i]),
                              (-ListofDen[[i]]) -> -topo[[1]] @@ (UnitVector[Length[ListofDen], i]),
                          
       Power[ListofDen[[i]], 
         n_] -> (topo[[1]] @@ (UnitVector[Length[ListofDen], i]))^n,
                  
       Power[-ListofDen[[i]], 
         n_] -> (-1)^n* (topo[[1]] @@ (UnitVector[Length[ListofDen], i]))^n}, {i,
        1, Length[ListofDen]}] // Flatten;
   denTotopo = Join[denTotopo, denTotopo /. Den[a_, b_] :> canonicalDen[a, b]]; 
   If[Head[Expand[Amp] ]===Plus,
      
      ProdDen0 =  DeleteDuplicates[ Select[#, ! FreeQ[#, Den] &] & /@ Level[Expand[Amp], 1] ],
      ProdDen0 =  {DeleteDuplicates[ Select[#, ! FreeQ[#, Den] &] & @ Amp ] };

   ];

   kVars = loopMom;
     
   ProdDen = FilterLoopDependentDen[#, kVars]&/@ProdDen0;
   
   absRules = {Den[a_, b_] :> canonicalDen[a, b]};

   signFlipRule = Den[a_, b_] :> Den[Simplify[-a], b];

   TOPORules = Thread[# -> TopInt[(temp = (# /.  absRules  //.
            denTotopo //. SubTopoInv //. SubTopo); temp2 = (temp /. signFlipRule //. denTotopo //. SubTopoInv //.
          SubTopo))]] & /@ ProdDen;

   TOPORules = TOPORules/.{TopInt[ topox_[x___] ] :> TopInt[topox,x]}; (* Due to change of syntax from AN$LoopInt to TopInt *)
   
   TOPOList = (#  /. {Den[a_, b_] :> Den[Simplify@Abs[a], b]} /. Den[Abs[a_], b_] :> Den[a, b] //. denTotopo //. SubTopoInv //. SubTopo) & /@ ProdDen;
   SortedTOPORules = SortBy[TOPORules, -Count[#, Den[__], Infinity] &];

   result = Expand[Amp] //. SortedTOPORules;
   withoutTopInt=result /. TopInt[__] -> 0 ;
   withTopInt = Expand[result - withoutTopInt];
   finalresult = withTopInt + ( withoutTopInt*TopInt[topo[[1]], Sequence @@ ConstantArray[0, Length[topo[[2]]]]] );

   Return[{SortedTOPORules,finalresult}];
   ];

(*********************************************)
(* Function for same particle for external states*)
(*********************************************)

SameExternalQ[process_, extMomenta_List] :=
  Module[{in, outh, allParticles, fieldsForMomenta, fieldsList,
    sameExternal},
   in = process[[1, 1]];
          outh = process[[1, 2]];
   allParticles = Join[in, outh];
   fieldsForMomenta =
    Association[
     Table[mom -> (matched =
         SelectFirst[allParticles, #[[1]] === mom &];
        If[MissingQ[matched], Missing[], Head[matched]]), {mom,
       extMomenta}]];
   fieldsList = DeleteCases[Values[fieldsForMomenta], Missing[]];
   sameExternal =
    Length[fieldsList] > 0 &&
     Length[DeleteDuplicates[fieldsList]] === 1;
   sameExternal];


(*********************************************)
(*     Function for finding plus den         *)
(*********************************************)

MultipleDenFunc[PlusDen_] :=
 Module[{PlusDenList, PlusDenVars, PlusDenList1, PlusDenList2,
   transformationRules = {}, elem, PlusDensortedlist,
   FinalPlusDenlist, PlusDensortedlist1},
  PlusDenList = List @@ PlusDen;
  PlusDenVars = Variables[PlusDen];
  DenSubs = {Den[a_, b_] :> (a^2 - b^2)};
  PlusDenList1 =
          Union[PlusDenVars, SameTest -> (Expand[#1 /. DenSubs] ===
                 Expand[#2 /. DenSubs] &)];
  PlusDenList2 = Complement[PlusDenVars, PlusDenList1];
  Do[sameElement =
             SelectFirst[PlusDenVars,
                Expand[# /. DenSubs] === Expand[elem /. DenSubs] &,
                Missing["NotFound"]];
          If[sameElement =!= Missing["NotFound"],

    AppendTo[transformationRules, elem -> sameElement]], {elem,
             PlusDenList2}];
  PlusDensortedlist = PlusDenList //. transformationRules;
  PlusDensortedlist1 =
   Union[PlusDensortedlist /. Den[a_, b_]^n_ :> Den[a, b]];
  getFactors[term_] := List @@ term;
  isSubsetQ[superset_, subset_] := (SubsetQ[superset, subset]);
  eliminateSubsets[PlusDensortedlist1_] :=
   Module[{factors, uniqueTerms},
    factors = getFactors /@ PlusDensortedlist1;
    uniqueTerms =
     Select[PlusDensortedlist1,
      Not[Or @@ (isSubsetQ[getFactors[#],
              getFactors[#2]] && # =!= #2 &) @@@
          Tuples[{PlusDensortedlist1, {#}}]] &];
    Return[uniqueTerms]];
  FinalPlusDenlist = eliminateSubsets[PlusDensortedlist1];
  Return[FinalPlusDenlist]
  ]

(*********************************************)
(*     Function for finding topologies       *)
(*********************************************)

   FindTopology::invalidinput = " Amplitudes are not given in correct format, check the manual";
   FindTopology::noAmplitude  = " Amplitude square is zero. No topologies can be found.";

Options[FindTopology]={DefineTopology->True,int->Automatic,ext->Automatic};

FindTopology[amplitude00_Association, opts: OptionsPattern[] ] :=
   Module[{amplitudelist0,amplitude0,sameElement, denlist2, list1, list2, masses,
       transformationRules, tmpList2, tmpList1, DenSubs, denoAmp,DenList,denoAmp0, ProductofDen1, DenList1, result = {}, seen = <||>, result1 = {},result2={},topos={},nProj1,nLoops,AmpName,nDiagrams,process,intOpt,extOpt},
    
     amplitudelist0 = Normal[amplitude00];

   If[Head[amplitudelist0] != List && (Length[amplitudelist0] === 7 || Length[amplitudelist0] === 5), Message[FindTopology::invalidinput] && Abort[] ];

   If[Length[amplitudelist0] === 7, 
    {Print[ "Finding the topologies for the projected amplitude ..."];
     process = amplitudelist0[[1, 2]];
     nDiagrams = amplitudelist0[[2, 2]];
     nLoops = amplitudelist0[[4, 2]];
     nProj1 = amplitudelist0[[5, 2]];
     amplitude0 = amplitudelist0[[7, 2]];
     intOpt = OptionValue[int];
     extOpt = OptionValue[ext];
     If[intOpt===Automatic,internal = Table[Symbol["k" <> ToString[i]], {i, 1, nLoops}],internal=OptionValue[int]];
     If[extOpt===Automatic,external  = Most[Cases[process, h_Symbol[p_Symbol] :> p, {0, Infinity}]],external=OptionValue[ext]];
     
     If[nProj1 == 1, amplitude = Select[amplitude0, (#[[2]] =!= 0) &]];
     If[nProj1 > 1, amplitude = DeleteCases[Map[Select[#, #[[2]] =!= 0 &] &, amplitude0], {}]];
     If[nProj1 == 1 && amplitude === {}, Message[FindTopology::noAmplitude, Style["All amplitudes are zero. No topologies can be found", 
      Bold, Darker@Red]] && Abort[]];
     If[nProj1 > 1 && AllTrue[amplitude, AllTrue[#, #[[2]] === 0 &] &], Message[FindTopology::noAmplitude, 
     Style["All amplitudes are zero. No topologies can be found", Bold, Darker@Red]] && Abort[]];
     denoAmp = DeleteDuplicates@Select[Flatten@Map[If[Head[Expand[#]] === Plus,(Times@@MultipleDenFunc@(Cases[#, Den[__] | Power[Den[__], _], {0, Infinity}]/.Den[a_, b_]^_. :> Den[a, b])) & /@
       Level[Expand[#], 1],{Times @@ DeleteDuplicates@MultipleDenFunc@Cases[#, Den[a_, b_]^_. :> Den[a, b], {0, Infinity}]}] &, Cases[amplitude, Rule[{DiagramID[_], _}, rhs_] :> rhs]], # =!= 1 &]}];
	   
    If[Length[amplitudelist0] == 5, {Print["Finding the topologies for the amplitude square ..."];
     process = amplitudelist0[[1, 2]];
     nLoops = amplitudelist0[[3, 2]];
     amplitude0 = amplitudelist0[[5, 2]];
     intOpt = OptionValue[int];
     extOpt = OptionValue[ext];
     If[intOpt===Automatic,internal = Table[Symbol["k" <> ToString[i]], {i, 1, nLoops}],internal=OptionValue[int]];
     If[extOpt===Automatic,external  = Most[Cases[process, h_Symbol[p_Symbol] :> p, {0, Infinity}]],external=OptionValue[ext]];


    If[amplitude0 == 0, Message[FindTopology::noAmplitude] && Abort[] ];

     denoAmp0 = List @@@ DeleteDuplicates[ Flatten[MultipleDenFunc[ Select[#, ! FreeQ[#, Den] &] & /@ Level[Expand[amplitude0], 1]], 2]];
     denoAmp = Select[#, ! FreeQ[#, Alternatives @@ internal] &]&/@denoAmp0}];
     DenList = Variables[denoAmp];
     DenSubs = {Den[a_, b_] :> (a^2 - b^2)};
     tmpList1 =
       Union[DenList, SameTest -> (Expand[#1 /. DenSubs] === Expand[#2 /. DenSubs] &)];
     tmpList2 = Complement[DenList, tmpList1];
     transformationRules = {};
     Do[sameElement = SelectFirst[tmpList1, Expand[# /. DenSubs] === Expand[elem /. DenSubs] &, Missing["NotFound"]];
       If[sameElement =!= Missing["NotFound"], AppendTo[transformationRules, elem -> sameElement]], {elem, tmpList2}];
     ProductofDen1 = denoAmp //. transformationRules;
     DenList1 = DenList //. transformationRules;
     masses = Select[Cases[DenList1, Den[a_, b_] :> b, Infinity] // DeleteDuplicates, FreeQ[#, 0] &];
     list1 = If[MatchQ[ProductofDen1, {Den[__]}], Flatten[List @ (#), 1] & /@ ProductofDen1, Flatten[List @@ (#), 1] & /@ ProductofDen1];
     If[masses === {}, list2 = SortBy[list1, {-Count[#, _Den]} &],
       list2 = SortBy[list1, {-Count[#, _Den], ((-Count[#, Den[_, masses[[1]]]]))  } &]];
     Table[If[! KeyExistsQ[seen, i], independenttopo =  {list2[[i]]};
         seen[i] = True;
         TOPO = ReplaceAll[#, List -> Times] & /@ independenttopo;
              TOPOforrules = List @@@ TOPO /. {Den[a_, b_] :> a};
         Rules1 = GenerateTransformationRules[TOPOforrules[[1]], internal];
         Rules2 = GenerateTransformationRuleswExt[TOPOforrules[[1]], internal, external];
         topolist = Flatten[List @@@ (TOPO /. {Den[a_, b_] :> a^2 - b^2}), 1];
         denlist2 = list2 /. {Power[Den[a_, b_], c_] :> Den[a, b]} /. {Den[a_, b_] :> a^2 - b^2};
     Do[found = False; Do[If[SubsetQ[Sort@Expand@(topolist), Sort@Expand@(denlist2[[k]] /. Rules1[[jj]])] === True && !
                   KeyExistsQ[seen, k], seen[k] = True; found = True; Break[];], {jj, Length[Rules1]}];
           If[! found && SameExternalQ[process, external], 
               Do[ If[SubsetQ[Sort@Expand@(topolist), Sort@Expand@(denlist2[[k]] /. Rules2[[jj]])] === 
           True && !  KeyExistsQ[seen, k], seen[k] = True; Break[];], {jj, Length[Rules2]}]], {k, Length[denlist2]}];
         AppendTo[result, list2[[i]]] ], {i, Length[list2]}];
      result1 = Flatten[AppendTo[result1, Table[{ToExpression["TOPO" <> ToString[i] ], result[[i]]}, {i, 1, Length[result]}]], 1];
     result2 = Flatten[result1,1];
     topos = Thread[result2[[1 ;; ;; 2]] -> result2[[2 ;; ;; 2]]];
     
     If[OptionValue[DefineTopology]===True, MapThread[DefineTopology[#1, internal, external, #2] &, 
  {result2[[1 ;; ;; 2]], result2[[2 ;; ;; 2]]}];
  
      Print[Style["Topologies can be accessed via AllTopologies[] list.",Bold,Darker@Green] ];, 
      Print[Style["The following topologies are found: ",Bold,Darker@Green] ] ];
      Print[Style["Done!", Bold, Darker@Green] ];
      Return[topos]
       ];



(*********************************************)
(*     Function for momentum shift   *)
(*********************************************)

wrongTransformation[rule_List, internal_] :=
  Module[{selectvars, conditions},
   selectvars =
    Table[List[
      Variables@rule[[1, 2]], ((Variables@rule[[jj, 2]]))], {jj, 2,
      Length[internal]}];
   conditions =
    Table[Function[{perm},
       AllTrue[Flatten@{Rest[perm]},
        MemberQ[selectvars[[i, 1]], First[perm]] &&
          FreeQ[selectvars[[i, 1]], #] &&
          MemberQ[selectvars[[i, 2]], First[perm]] &&
          FreeQ[selectvars[[i, 2]], #] &]] /@
      DeleteDuplicatesBy[Permutations[internal],
       Sort[{#[[Length[internal]]]}] &], {i, Length[selectvars]}];
   If[Or @@ Flatten[conditions],
(*    wrongrules = AppendTo[wrongrules, rule],*)
    Return[rule],
    Unevaluated[Sequence[]]];
   ];


GenerateTransformationRules[topolist_, internal_List] := 
  Module[{orderlist, numofinternal, combinedList, pairs, 
    internalrules, externalrules, rules,wrongrules={},wrongelements}, 
   combinedList = Join[topolist, -topolist];
   orderlist = Join[internal];
   numofinternal = Length[internal];
   pairs = Subsets[combinedList, {numofinternal}];
   rotateList = 
    Table[RotateLeft[internal, i], {i, 0, Length[internal] - 1}];
   rules = 
    Flatten[Table[
      Thread[rotateList[[i]] -> #] & /@ pairs, {i, 1, 
       Length[rotateList]}], 1];
  (****Extra piece added to delete the wrong transformations****)

   AppendTo[wrongrules, wrongTransformation[#, internal] & /@ rules];
   wrongelements = DeleteCases[#, Null] & /@ wrongrules;
   rules = Flatten[DeleteElements[rules, #]&/@wrongelements,1];
   rules = 
    Map[SortBy[#, 
       Function[{rule}, 
        If[MemberQ[orderlist, rule[[1]]], 
         Position[orderlist, rule[[1]]][[1, 1]], Infinity]]] &, rules];
   rules];

GenerateTransformationRuleswExt[topolist_, internal_List, 
   external_List] := 
  Module[{orderlist, numofinternal, combinedList, pairs, 
    internalrules, externalrules, rules,wrongrules ={}, wrongelements}, 
   combinedList = Join[topolist, -topolist];
   orderlist = Join[internal, external];
   numofinternal = Length[internal];
   pairs = Subsets[combinedList, {numofinternal}];
   rotateList = 
    Table[RotateLeft[internal, i], {i, 0, Length[internal] - 1}];
   externalrules = Thread[external -> #] & /@ Permutations[external];
   internalrules = 
    Flatten[Table[
      Thread[rotateList[[i]] -> #] & /@ pairs, {i, 1, 
       Length[rotateList]}], 1];
  (****Extra piece added to delete the wrong transformations****)
  
   AppendTo[wrongrules, wrongTransformation[#, internal] & /@ internalrules];
   wrongelements = DeleteCases[#, Null] & /@ wrongrules;
   internalrules = Flatten[DeleteElements[internalrules, #]&/@wrongelements,1];
   rules = Flatten[Outer[Join, internalrules, externalrules, 1], 1];
   rules = 
    Map[SortBy[#, 
       Function[{rule}, 
        If[MemberQ[orderlist, rule[[1]]], 
         Position[orderlist, rule[[1]]][[1, 1]], Infinity]]] &, rules];
   rules];


(* Find Transformation for a denominator list or an amplitude into a given topology  *)


FindShiftTransformations[topology_Rule, x___ ]:= FindShiftTransformations[List[topology], x ]; 


Options[FindShiftTransformations] := {AllShifts -> False};

FindShiftTransformations[topologies_List, denominator_, internal_List, external_List, opts: OptionsPattern[] ] := Module[{TOPOref, TOPO, topoNames, RuleswExt, topolist, denlist, Lentopo, NumSub, Rules, i = 1, Found = False, result = {}, noresult = {}}, 

   FindShiftTransformations::invalidTopo = "invalidTopo:`1'";
   FindShiftTransformations::invalidDen = "invalidDen:`1'";
   FindShiftTransformations::noTopo = "noTopo:`1'";
   FindShiftTransformations::noDen = "noDen:`1'";

   topoNames=topologies[[;;,1]];

   If[Head[topologies[[;;,2]]] != List, Message[FindShiftTransformations::invalidTopo, Style["Topologies are not given in correct format, check the manual", Bold, Darker@Red]&&Abort[]]];
   TOPOref = topologies[[;;,2]];
   TOPO = Select[#, ! FreeQ[#, Alternatives @@ internal] &]&/@TOPOref;
   If[TOPO == {}, Message[FindShiftTransformations::noTopo, Style["The given topologies do not depend on loop momenta" , Bold, Darker@Red]&&Abort[]]];
  

   If[FreeQ[denominator,Rule]===True&&Head[denominator] === Den,
      denlist = {denominator},
      denlist = List @@ denominator];
   
   If[FreeQ[denominator,Rule]===False,
      denominator1 = (Select[#, ! FreeQ[#, Den] &] & /@ denominator)[[2]];
      denlist = (Flatten@(List @@ denominator1))];

   If[Head[denlist] != List&&Head@@denlist != Times, Message[FindShiftTransformations::invalidDen, Style["Denominator/Amplitude is not given in correct format, check the manual",Bold,Darker@Red]&&Abort[]]];

   denlist = (Select[(denlist), ! FreeQ[#, Alternatives @@ internal] &]) /. {Power[Den[a_, b_], c_] :> Den[a, b]} /. {Den[a_, b_] :> a^2 - b^2};
   
   If[denlist=={}, Message[FindShiftTransformations::noDen, Style["The given denominator  do not depend on loop momenta" , Bold, Darker@Red]&&Abort[]]];

   Lentopo = Length[TOPO];

   TOPOforrules = List @@ TOPO /. {Den[a_, b_] :> a};

   While[i <= Lentopo && ! Found, 

      Rules = GenerateTransformationRules[List @@ TOPOforrules[[i]],internal];

      topolist = TOPO[[i]] /. {Den[a_, b_] :> a^2 - b^2};

      Table[
            If[SubsetQ[Sort@Expand@(topolist), 
               Sort@Expand@(denlist /. Rules[[jj]])] === True && ! Found, 
               If[ OptionValue[AllShifts]=== False,  Found = True; ];
               AppendTo[result, 
                        {topoNames[[i]],Pick[Rules[[jj]], SubsetQ[Sort@Expand@(topolist), Sort@Expand@(denlist /. Rules[[jj]])], True] }]; 
               ], 
         {jj, Length[Rules]}];
      i++;];      

   If[result === {}, i = 1];

   If[result === {}, 
   
      {While[i <= Lentopo && ! Found && SameExternalQ[process, external], 

            RuleswExt = GenerateTransformationRuleswExt[List @@ TOPOforrules[[i]], internal, external];
            topolist = List @@ TOPO[[i]] /. {Den[a_, b_] :> a^2 - b^2};

            Table[
                  If[SubsetQ[Sort@Expand@(topolist), 
                     Sort@Expand@(denlist /. RuleswExt[[jj]])] === True && !Found, Found = True;

                   AppendTo[result, 
                           {topoNames[[i]],Pick[RuleswExt[[jj]], SubsetQ[Sort@Expand@(topolist), Sort@Expand@(denlist /. RuleswExt[[jj]])], True]}];
                       ], 
               {jj, Length[RuleswExt]}];
       i++;]};
      ];
   result = Flatten[result, 1];
   Return[result];];



(*****************************************)
(* Function for rewriting the numerator  *)
(*****************************************)


NumeratorToDen::argDen=": The amplitude `1` does not follow the conventions or it does not have denominators. ";

Options[NumeratorToDen] := {AN$MomentumShift -> {} };

NumeratorToDen[projAmp0_,topology_,loopMomenta_,extMomenta_,  opts: OptionsPattern[] ]:=Block[{projAmp,topo,topo0,denominator,numerator,factor,newAmplitude,subsToVariables,allMomenta,assumpt,dimension,polyDegree,kinvariants,numeratorTopo,CCicoeff,CCicoeff0,kList,pList,coeffArrayNumerator,coeffArrayTopo,Eqs,CCsolutions,CC,zeroSubs,CCsolutionsRight,newNumerator,complCCi,zeroSubs2,finalAmp,basisTopoExpl,basisTopoDen,invariantsCoeffList,subsInvariants,basisList,shifts,tensorExpandsubs},

   subsToVariables={
      Power[k1_,a_]/;(MemberQ[loopMomenta,k1] && a===2):>ToExpression[ToString[k1]<>"sq"],
      Power[k1_,a_]/;(MemberQ[loopMomenta,k1] && EvenQ[a] && a=!=2):>Power[ToExpression[ToString[k1]<>"sq"],a-2],
      Dot[k1_,k1_]/;MemberQ[loopMomenta,k1]:>ToExpression[ToString[k1]<>"sq"],
      Dot[k1_,p1_]/;MemberQ[loopMomenta,k1]:>ToExpression[ToString[k1]<>ToString[p1] ],
      Dot[p1_,k1_]/;MemberQ[loopMomenta,k1]:>ToExpression[ToString[k1]<>ToString[p1] ]};

   shifts=OptionValue[AN$MomentumShift];
   If[shifts === {},  shifts = Thread[loopMomenta -> loopMomenta] ];
   allMomenta=Join[loopMomenta,extMomenta];
   kList=DeleteDuplicates[Sort/@Tuples[{loopMomenta,loopMomenta}] ];
   pList=Tuples[{loopMomenta,extMomenta}];
   kinvariants=Join[kList,pList];
   kinvariants=ReplaceAll[#,{{x1_,x1_}:>ToExpression[ToString[x1]<>"sq"],{x1_,x2_}:>ToExpression[ToString[x1]<>ToString[x2] ] }]&/@kinvariants;
   (* TensorExpand IS SAFER BUT MUCH SLOWER, USE THE tensorExpandsubs *)

   assumpt=Element[Alternatives@@allMomenta,Vectors[3,Reals] ];

   tensorExpandsubs = {
      Dot[x_ + y_, z_] :> Dot[x, z] + Dot[y, z], 
      Dot[z_, x_ + y_] :> Dot[z, x] + Dot[z, y],
      Dot[x_,b_ y_] :> b*Dot[x, y],
      Dot[a_ x_,y_] :> a*Dot[x, y],
      Dot[a_ x_,b_ y_] :> a*b*Dot[x, y], 
      Dot[p1_, k1_] /; (MemberQ[loopMomenta, k1]&& Not@MemberQ[loopMomenta, p1]) :> Dot[k1, p1]
   (*Dot[a_, b_] :> Dot@@Sort[{a, b}]*)};
   safeDotCanonicalize[expr_] := FixedPoint[ # /. Dot[args__] :> Dot @@ SortBy[{args}, ToString] &, expr ];

   projAmp=projAmp0/.{Dot[x1_,Power[x2_,a_] ]:>(x1.x2)^a}//Expand;
   projAmp=(projAmp/.shifts);
   projAmp = projAmp /. Dot -> dotTemp;
   projAmp = projAmp /. a_ . b_ :> Dot[a, b];
   projAmp = projAmp /. dotTemp -> Dot;
   projAmp =safeDotCanonicalize[projAmp];
   projAmp=projAmp//.tensorExpandsubs;
   projAmp =safeDotCanonicalize[projAmp];
   projAmp =projAmp //. { a_ . b_ /; OrderedQ[{ToString[a], ToString[b]}] === False :> b.a};


   (* denominator is obtained to check syntax error, [[2]] is an arbitrary choice *)
   If[Head[projAmp]===Plus, 
      denominator=Select[#,!FreeQ[#,Den ] &]&@projAmp[[2]],
      denominator=Select[#,!FreeQ[#,Den ] &]&@projAmp
      ];
   If[denominator===0,Message[NumeratorToDen::argDen,projAmp];(denominator=1 )];

   newAmplitude=projAmp;

   newAmplitude=newAmplitude/.subsToVariables;



   (*rewrite the topo*)

   topo0=Level[topology,1];

   topo=TensorExpand[(topo0/.{Den[p_,m_]:>p.p-m^2}),Assumptions->assumpt]//Simplify;
   topo=topo/.subsToVariables;

   
   (*write the basis*)

   CCicoeff0=Table[ToExpression["CC"<>ToString[ii] ],{ii,1,Length[topo]}];
   CCicoeff=Join[{CC0},CCicoeff0];
   basisTopoExpl=CC0+Total@Thread[CCicoeff0*topo];
   basisTopoDen=CC0+Total@Thread[CCicoeff0*1/topo0];
   
   invariantsCoeffList=Normal@CoefficientArrays[#,kinvariants]&/@kinvariants;
   basisList=Normal@CoefficientArrays[basisTopoExpl,kinvariants];
   
   Eqs=Flatten/@Table[basisList-invariantsCoeffList[[ii]],{ii,1,Length[invariantsCoeffList]}];
   Eqs=Thread[#==0]&/@Eqs;
   
   CCsolutions=First/@(Solve[#,CCicoeff]&/@Eqs);
   
   subsInvariants=Table[kinvariants[[ii]]->basisTopoDen/.CCsolutions[[ii]],{ii,1,Length[kinvariants]}];

   finalAmp=newAmplitude/.subsInvariants;
   Return[finalAmp]

]

(*************************************************)
(*              Removing odd k integrals         *)
(*************************************************)

CheckForOddInt[expr_,loopMom_] := 
 Module[{ ProdDen = {}, kVars, negDen, Numexpr, negNum, symList = {}, 
   notSymList = {}, symList2 = {}, simplifiedDiff, result, DenVar,ComkVarsSub}, 
  kVars = loopMom;
  If[Head[Expand[expr]] === Plus, 
   ProdDen = 
    DeleteDuplicates[
     Select[#, ! FreeQ[#, Den] &] & /@ Level[Expand[expr], 1]], 
   ProdDen = {DeleteDuplicates[
      Select[#, ! FreeQ[#, Den] &] &@expr]}];
  DenVar=Variables[ProdDen];
  Table[negDen = ProdDen[[i]] /. Thread[kVars -> -kVars];
   simplifiedDiff = 
    Simplify[(negDen - ProdDen[[i]]) /. {Den[a_, b_] :> a^2 - b^2}];
   If[simplifiedDiff === 0, AppendTo[symList, i]];, {i, 
    Length[ProdDen]}];
  Table[ComkVarsSub=Thread[Complement[DenVar,List@ProdDen[[i]]]->0];
	Numexpr = Coefficient[expr, ProdDen[[i]]]/.ComkVarsSub;
   If[MemberQ[symList, i], 
    negNum = 
     Numexpr /. 
       Thread[kVars -> -kVars] /. {Dot[-a_, -b_] :> Dot[a, b], 
       Dot[-a_, b_] :> -Dot[a, b], Dot[a_, -b_] :> -Dot[a, b]};
    If[Simplify[negNum + Numexpr] === 0, AppendTo[symList2, 0], 
     AppendTo[symList2, 
      ProdDen[[i]]*(Numexpr/.ComkVarsSub) ]], 
    AppendTo[notSymList, 
     ProdDen[[i]]*(Numexpr/.ComkVarsSub) ]];, {i, 
    Length[ProdDen]}];
  result = Total[Join[symList2, notSymList]];
  result]

(***********************************************************)
(* Collecting Function for amplitude in term of Integrals  *)
(***********************************************************)

AmplitudeToTopologies::noShifts  = " Given the defined topologies, no shifts were found for the mapping of the amplitude tagged as `1` ";

Options[AmplitudeToTopologies]:={Topologies-> {}}

AmplitudeToTopologies[amplitudeProj_Association,opts: OptionsPattern[] ]:=Block[{amplitude,amplitudeProjlist,topologieslist = {},amplitudeExpanded,finalList,denoAmp,denoAmpwPower,listAllToposDen,shifts,newAmplitude,currentAmp,listAmp,currentTopo,denoAmpcurrent,currentTopo2,currentAmp2,rules,nProj,currentampExpanded,process,nDiagrams,AmpName,nLoops,amplitudeProjlist0,nProj1,TopoPropRules,PowersNumerator,amplitude1,denoAmp0,ComkVarsSub,DenVar,currentAmpTag,printed1=False,printed2=False},

   finalList = {};
   If[Length[amplitudeProj]===7, amplitudeProjlist0 = GatherProjectors[amplitudeProj], amplitudeProjlist0 = amplitudeProj];
   amplitudeProjlist0   =  Normal[amplitudeProjlist0];
   If[Head[amplitudeProj] != List && (Length[amplitudeProj] === 7 || Length[amplitudeProj] === 5), Message[FindTopology::invalidinput,
   Style["Amplitudes are not given in correct format, check the manual", Bold, Darker@Red]] && Abort[]];
   If[Length[amplitudeProj]===7,
   {process              =  amplitudeProjlist0[[1,2]];
   nDiagrams            =  amplitudeProjlist0[[2,2]];
   processName          =  amplitudeProjlist0[[3,2]];
   nLoops               =  amplitudeProjlist0[[4,2]];
   nProj1               =  amplitudeProjlist0[[5,2]];
   kine                 =  amplitudeProjlist0[[6,2]];
   amplitudeProjlist    =  amplitudeProjlist0[[7,2]];}];
   
   If[Length[amplitudeProj]===5,
   {process              =  amplitudeProjlist0[[1,2]];
   processName          =  amplitudeProjlist0[[2,2]];
   nLoops               =  amplitudeProjlist0[[3,2]];
   kine                 =  amplitudeProjlist0[[4,2]];
   nProj1               =  0;
   amplitudeProjlist    =  amplitudeProjlist0[[5,2]];}];
   TopoPropRules[list_List] := TopoPropRules /@ list;

   loopMomenta = Union@Flatten[Topology[#][LoopMomenta] & /@ AllTopologies[], 1];
   extMomenta = Union@Flatten[Topology[#][ExternalMomenta] & /@ AllTopologies[], 1]; 
   TopoPropRules[top_] := TopoPropRules[top] = Rule[top, Topology[top][Propagators] ];
   If[OptionValue[Topologies] === {}, topologieslist = TopoPropRules[AllTopologies[] ],
   topologieslist = TopoPropRules[OptionValue[Topologies] ] ];

   If[Length[amplitudeProj]===7, nProj=Dimensions[amplitudeProjlist] ];
   If[Length[amplitudeProj]===5, nProj=0];
   If[Length[amplitudeProj]===7,If[Length[nProj]===1, nProj=1, nProj=nProj[[1]] ] ];
   
   If[ Length[amplitudeProj]===5&&nProj===0 ,
      {amplitude = amplitudeProjlist;
       amplitude = CheckForOddInt[amplitude,loopMomenta];


      (* With general gauges the amplitudes are not fully factored in Den[...] *)

      denoAmpwPower = {};
      amplitudeExpanded = (amplitude);
      denoAmpwPower = DeleteDuplicates[Select[#,!FreeQ[#,Den]&]&/@Level[Expand[amplitudeExpanded],1]];
      Table[

      amplitudeExpanded = List@denoAmpwPower[[ii]]/.{Power[Den[k___, m_],a_] :> Den[k, m]};

      denoAmp0 = {};
      currentampExpanded = amplitudeExpanded;

      denoAmp0=AppendTo[denoAmp0, currentampExpanded];
      If[!printed1,
         Print["Finding loop-momentum shifts for the amplitude square..."];printed1=True;];
      denoAmp = (# /. {List -> Times}) & /@ (Select[#, !  FreeQ[#, Alternatives @@ loopMomenta] &] & /@ (List @@@ Flatten[denoAmp0,1]));
      denoAmp=DeleteDuplicates@denoAmp;

      If[denoAmp==={},shifts={},shifts=FindShiftTransformations[topologieslist,#,loopMomenta,extMomenta]&/@denoAmp];
      If[!printed2, Print["Rewriting the numerators for the amplitude square..."];printed2=True;];
      listAmp={};
      DenVar=Variables[denoAmpwPower];

            ComkVarsSub={};
            ComkVarsSub=Thread[Complement[DenVar,List@denoAmpwPower[[ii]]]->0];
            currentAmp =denoAmpwPower[[ii]]*(Coefficient[amplitude,denoAmpwPower[[ii]] ]/.ComkVarsSub);
            If[shifts=!={},
            currentTopo=shifts[[1,1]]/.topologieslist;
            currentTopo2 = shifts[[1,1]] -> (shifts[[1,1]]/.topologieslist);

            currentAmp = NumeratorToDen[currentAmp,currentTopo,loopMomenta,extMomenta,AN$MomentumShift -> shifts[[1,2]]];

            currentAmp = GenerateTopologies[currentAmp,currentTopo2,loopMomenta];


            currentAmp =  currentAmp[[2]]/.kine;, currentAmp=currentAmp];
            listAmp=AppendTo[listAmp, currentAmp];
            listAmp = Flatten[listAmp];
            finalList = AppendTo[finalList,listAmp];
          ,{ii,Length[denoAmpwPower]}];


      finalList={Total[Flatten[finalList]]};}];

   Do[

      If[ nProj===1 , amplitude = amplitudeProjlist , amplitude = amplitudeProjlist[[jj]] ];
      
      amplitude = Select[amplitude,(#[[2]]=!=0)& ];
      amplitude = amplitude/. (lhs_ -> rhs_) :> (lhs -> (CheckForOddInt[rhs,loopMomenta]));
      amplitude = Select[amplitude,(#[[2]]=!=0)& ];

      amplitudeExpanded = (amplitude[[;;,2]])//Expand;
      amplitudeExpanded = amplitudeExpanded/.{Power[Den[k___, m_],a_] :> Den[k, m]};

      denoAmp = {};
      Do[
         currentampExpanded = amplitudeExpanded[[ii]];

         If[ Head[currentampExpanded] === Plus,
             currentampExpanded = Select[#,!FreeQ[#,Den]&]&@currentampExpanded[[1]], (* This selects only the very first term of the amplitude expanded, faster and works for gluon RXi gauge *)
             currentampExpanded = Select[#,!FreeQ[#,Den]&]&@currentampExpanded;
         ];
         denoAmp=AppendTo[denoAmp, currentampExpanded];
      ,{ii,1,Length[amplitudeExpanded]}]

      If[ nProj===1 ,
         Print["Finding loop-momentum shifts for the amplitudes..."],
         Print["Finding loop-momentum shifts for the amplitudes given by projector number "<>ToString[jj]<>"..."];
      ];
      shifts=FindShiftTransformations[topologieslist,#,loopMomenta,extMomenta]&/@denoAmp;
      If[ nProj===1 ,
         Print["Rewriting the numerators..."],
         Print["Rewriting the numerators for the case of projector number "<>ToString[jj]<>"..."];
      ];

      listAmp={};
      Do[
              
            tagAmp = amplitude[[ii,1]];
            currentAmp = amplitude[[ii,2]];
            currentAmpTag = amplitude[[ii,1]];

            If[shifts[[ii]]==={}, Message[AmplitudeToTopologies::noShifts,currentAmpTag]&&Abort[] ];

            denoAmpcurrent=Select[#,!FreeQ[#,Den]&]&@currentAmp;

            currentTopo=shifts[[ii,1]]/.topologieslist;
            currentTopo2 = shifts[[ii,1]] -> (shifts[[ii,1]]/.topologieslist);
            
            currentAmp = NumeratorToDen[currentAmp,currentTopo,loopMomenta,extMomenta,AN$MomentumShift -> shifts[[ii,2]]];

            currentAmp = GenerateTopologies[currentAmp,currentTopo2,loopMomenta];

            currentAmp =  currentAmp[[2]]/.kine;

            currentAmp={tagAmp -> currentAmp};

            listAmp=AppendTo[listAmp, currentAmp];

        ,{ii,Length[amplitude]}];

      listAmp = Flatten[listAmp];

      finalList=AppendTo[finalList, listAmp];
   ,{jj,nProj}]

   
    Do[If[finalList[[ii]] === {}, Print[Style["Note: all the amplitudes for projector "<>ToString[ii]<>" are zero because of zero projection or odd-loop momenta integral", Bold, Darker@Blue] ] ] , {ii,nProj}];

   Print[MakeGreen["Done!",Boldmg->True] ];

   If[nProj===1, finalList = Flatten[finalList] ];

   If[nProj===0, finalAss = Association[{Process -> process, "Name" ->  processName,  "LoopOrder" ->  nLoops, Kinematics -> kine, Amplitudes -> finalList}] ,
   finalAss = Association[{Process -> process,"Total" -> nDiagrams, "Name" ->  processName,  "LoopOrder" ->  nLoops, Kinematics -> kine, Amplitudes -> finalList}]  ];

   finalAss = FlattenProjectors[finalAss];
   Return[finalAss]
]




(*************************************************)
(*  Writing of the integrals for Kira to reduce  *)
(*************************************************)

(* this function takes an expression as input and returns a list of integrals under the envelope AN$LoopInt *)

  (* Function for identifying the MI in expression *)
 
   
   IdentifyIntegrals[expr_Plus]:= IdentifyIntegrals[List[expr] ];
   IdentifyIntegrals[expr_Times]:= IdentifyIntegrals[List[expr] ];
   IdentifyIntegrals[expr_Rational]:= IdentifyIntegrals[List[expr] ]; 
   IdentifyIntegrals[TopInt[x___] ]:= {TopInt[x]}; 

IdentifyIntegrals[expr0_List]:= Module[{myMI},

   expr = Flatten[expr0];

   If[ expr==={}, Return[{}] ];

   If[Head[expr[[1]] ]===Rule, myMI = expr[[;;,2]], myMI = expr ];

   myMI = Expand /@ myMI;
   myMI = If[Head[#] =!= Times && Head[#] =!= TopInt , (Level[# // Expand, 1]), # ] & /@ myMI;
   myMI = Flatten[myMI];
   myMI = If[Head[#] =!= TopInt, If[Head[#] =!= Symbol, Select[#, ! FreeQ[#, TopInt[___] ] &], 1], # ] & /@ myMI;
   myMI = DeleteCases[#, 1]&@DeleteDuplicates[myMI];

   Return[myMI];
  ]

(* Writes the integrals in the amplitude in a file *)

Options[WriteAmpIntegrals] := {OutputName -> "integrals_amp" };

WriteAmpIntegrals[outh_, Amplitude_List, opts: OptionsPattern[] ] := Block[{outfile, topoStrings,lineLength,wsp,comStr},
  
  outfile = OptionValue[OutputName];
  outfile = outh <>"/"<> outfile;

  If[Head[Amplitude[[1]] ]===TopInt, 
      topoStrings = Amplitude,  

      topoStrings = Flatten[Amplitude];
      If[Length[topoStrings]===1&&Head[topoStrings[[1]] ]=!=Rule, topoStrings=topoStrings, topoStrings = topoStrings[[;;,2]] ]; 
   ];

  topoStrings = IdentifyIntegrals[topoStrings];
  topoStrings = topoStrings/.{TopInt[topo_,x___]:>topo[x]};
  topoStrings = DeleteDuplicates@Flatten[topoStrings];
  topoStrings = ToString /@ topoStrings;

  lineLength=50;
  wsp="                                                                                                                             ";   
  comStr="###########################################################################################";
  OpenWrite[outfile];
   WriteString[outfile,StringTake[comStr,lineLength]<>"\n" ];
   WriteString[outfile,StringTake["#    File automatically generated by ANATAR    "<>wsp,lineLength]<>"\n" ];
   WriteString[outfile,StringTake[comStr,lineLength]<>"\n\n" ];
  	(WriteString[outfile, # <> "\n"] &) /@ topoStrings;
  Close[outfile];

   If[Head[Amplitude[[1]] ]===TopInt,
      Print["Integrals have been written in the file "<>MakeGreen[outfile] ],
      Print["The integrals in the amplitudes have been written in the file "<>MakeGreen[outfile] ];
   ];
  ]

