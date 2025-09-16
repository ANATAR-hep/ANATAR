

Kine[initial_,final_,InitialMom_,FinalMom_]:=Block[{nInit,nFin,PP,nTot,subsets,SInvariants,SS,TT,SqMom,AllParticles,listSS,listTT,SInvariantsInverse,ListKinematics},


   ListKinematics={};
   SqMom={};

   nInit=Length@InitialMom;
   nFin=Length@FinalMom;
   PP=Join[InitialMom,FinalMom];
   nTot=Length@PP-1;

   (*  Momentum conservation *)


   ListKinematics=Append[ListKinematics,FinalMom[[nFin]]->Total@InitialMom-Total@Drop[FinalMom,-1] ];

   (*  Square of momenta *)

   AllParticles=Join[initial,final];

    Do[

         SqMom=If[MemberQ[AN$Masses,AllParticles[[i]],2],Append[SqMom,PP[[i]]^2->Cases[AN$Masses,{AllParticles[[i]],m_} :>m^2][[1]]],Append[SqMom,PP[[i]]^2->0] ];

         SqMom=If[MemberQ[AN$Masses,AllParticles[[i]],2],Append[SqMom,PP[[i]].PP[[i]]->Cases[AN$Masses,{AllParticles[[i]],m_} :>m^2][[1]]],Append[SqMom,PP[[i]].PP[[i]]->0] ];

         SqMom=If[MemberQ[AN$Masses,AllParticles[[i]],2]&&nTot>2,Append[SqMom,PP[[i]].PP[[i]]^(-1)->Cases[AN$Masses,{AllParticles[[i]],m_} :>(m^2)^(-1) ][[1]]], SqMom ];

      ,{i,1,Length@PP}];

   (* Keep off-shell values for two, three and four-point amplitudes *)

   Which[
      
      nInit===1 && nFin === 1,   
         SqMom = { p1^2 -> SS, p1.p1 -> SS, (p1.p1)^(-1) -> SS^(-1) },

      nInit===1 && nFin === 2,   
         SqMom = { p1^2 -> SS, p1.p1 -> SS,  p2^2 -> p2.p2, p3^2 -> p3.p3, (p1.p1)^(-1) -> SS^(-1) },

      nInit===2 && nFin === 1,   
         SqMom = { p3^2 -> SS, p3.p3 -> SS, p1^2 -> p1.p1, p2^2 -> p2.p2, (p3.p3)^(-1) -> SS^(-1) },

      nTot>=3 && SetOffShell === True, 
         SqMom = Table[PP[[ii]]^2 -> PP[[ii]].PP[[ii]], {ii,1,nTot+1}]
   ];

   (* Scalar products in terms of S's and T's *)

   Which[

      (* Two-point amplitude *)

      nTot === 1 ,

      subsets = Subsets[Table[i,{i,nTot}],{1}];

      listSS = {SS};

      SInvariants = Drop[#,1]&/@Table[If[(i<=nInit&&jj<=nInit)||(i>nInit&&jj>nInit),PP[[i]].PP[[jj]]->1/2 (SS-PP  [[i]]^2-PP[[jj]]^2)],{i,1,nTot-1},{jj,i,nTot}]/.SqMom// Flatten;

      SInvariantsInverse = Drop[#,1]&/@Table[If[(i<=nInit&&jj<=nInit)||(i>nInit&&jj>nInit),PP[[i]].PP[[jj]]^(-1)->(1/2 (SS-PP[[i]]^2-PP[[jj]]^2) )^(-1) ],{i,1,nTot-1},{jj,i,nTot}]/.SqMom// Flatten;

      listTT={},

      (* Three-point amplitude 1->2 *)

      nTot === 2 && nFin > nInit ,

      subsets=Subsets[Table[i,{i,nTot}],{2}];

      listSS={SS};

      SInvariants = { PP[[1]].PP[[2]]-> -1/2 (PP[[3]]^2-PP[[1]]^2-PP[[2]]^2) }/.SqMom;

      SInvariantsInverse = Select[SInvariants, (#[[2]] =!= 0) &];
      SInvariantsInverse = SInvariantsInverse/. {(x_ -> y_) :> (x^(-1) -> y^(-1) ) }/.SqMom;

      listTT={},

      (* Three-point amplitude 2->1 *)

       nTot === 2 && nInit > nFin ,

      subsets=Subsets[Table[i,{i,nTot}],{2}];

      listSS={SS};

      SInvariants=Drop[#,1]&/@Table[If[(i<=nInit&&jj<=nInit)||(i>nInit&&jj>nInit),PP[[i]].PP[[jj]]->1/2 (SS-PP[[i]]^2-PP[[jj]]^2)],{i,1,nTot-1},{jj,i,nTot}]/.SqMom// Flatten;

      SInvariantsInverse = Select[SInvariants, (#[[2]] =!= 0) &];
      SInvariantsInverse = SInvariantsInverse/. {(x_ -> y_) :> (x^(-1) -> y^(-1) ) }/.SqMom;


      listTT={},
      
      (* Mandelstam variables for 2 -> 2 amplitude *)

      nInit===2&&nFin===2,

      listSS={SS};
      listTT={TT,UU};

      SS[x_,y_]:= SS;
      TT[x_,y_]:= listTT[[x]];

      SInvariants=Drop[#,1]&/@Table[If[(i<=nInit&&jj<=nInit)||(i>nInit&&jj>nInit),PP[[i]].PP[[jj]]->1/2 (SS[i,jj]-PP  [[i]]^2-PP[[jj]]^2),PP[[i]].PP[[jj]]->-(1/2)(TT[i,jj]-PP[[i]]^2-PP[[jj]]^2)],{i,1,nTot-1},{jj,i,nTot}]/.SqMom// Flatten;
      
      SInvariantsInverse = Select[SInvariants, (#[[2]] =!= 0) &];
      SInvariantsInverse = SInvariantsInverse/. {(x_ -> y_) :> (x^(-1) -> y^(-1) ) }/.SqMom;,

       (* Higher point amplitudes *)

      nTot>3||nInit===1&&nFin===3||nInit===3&&nFin===1, 
     
      subsets=Subsets[Table[i,{i,nTot}],{2}];

      listSS=Table[(SS[subsets[[k,1]],subsets[[k,2]]]=(ToExpression["S"<>ToString@subsets[[k,1]]<>ToString@subsets[[k,2]]])),{k,Binomial[nTot,2]}];

      listTT=Table[(TT[subsets[[k,1]],subsets[[k,2]]]=(ToExpression["T"<>ToString@subsets[[k,1]]<>ToString@subsets   [[k,2]]])),{k,Binomial[nTot,2]}];

      SInvariants=Drop[#,1]&/@Table[If[(i<=nInit&&jj<=nInit)||(i>nInit&&jj>nInit),PP[[i]].PP[[jj]]->1/2 (SS[i,jj]-PP  [[i]]^2-PP[[jj]]^2),PP[[i]].PP[[jj]]->-(1/2)(TT[i,jj]-PP[[i]]^2-PP[[jj]]^2)],{i,1,nTot-1},{jj,i,nTot}]/.SqMom// Flatten;

      SInvariantsInverse = Select[SInvariants, (#[[2]] =!= 0) &];
      SInvariantsInverse = SInvariantsInverse/. {(x_ -> y_) :> (x^(-1) -> y^(-1) ) }/.SqMom;


   ];




   If[ nTot === 2 , KinematicInvariants=listSS, KinematicInvariants=Join[listSS,listTT] ];

   If[SetOffShell === False, 

      AN$Kinematics = Join[SqMom,SInvariants,SInvariantsInverse,ListKinematics];
      AN$KinematicInvariants = Join[listSS,listTT],

      
      AN$Kinematics = Join[SqMom,{FinalMom[[-1]]->Total@InitialMom-Total@Drop[FinalMom,-1]}]
   ];

   Return[AN$Kinematics];
]
