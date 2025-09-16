(*****  This file contains the Projectors in Mathematica format *****)


(* Massive fermion propagator *)

DefineProjector[
   projTT1, 
   {1/2,1/2},
   {3,3}, 
   1/(4*Dot[p1,p2]),
   GammaM[p1,Spin1,Spin2],
   Metric[Colour1,Colour2]  
]

DefineProjector[
   projTT2, 
   {1/2,1/2}, 
   {3,3}, 
   1/(4*MT),
   GammaM[1,Spin1,Spin2],
   Metric[Colour1,Colour2]  
]
 
(* Gluon propagator *)

DefineProjector[
   projGG, 
   {1,1},
   {8,8}, 
   1/(8*(d-1)*p1.p2),
   Metric[Lor1,Lor2]-p1[Lor2]*p2[Lor1]/p1.p2,
   Metric[Gluon1,Gluon2]  
]

(* Gluon - Gluon - Higgs *)

DefineProjector[
   projGGH, 
   {1,1,0},
   {8,8,1},
   1/(d-2),
   Metric[Lor1,Lor2]-p1[Lor2]*p2[Lor1]/p1.p2,
   Metric[Gluon1,Gluon2]  
]

(* t - tbar - Gluon *)

DefineProjector[
   projGTT1, 
   {1,1/2,1/2}, 
   {8,3,3},
   1/4,
   1/(4*d)*GammaM[Lor1,Spin2,Spin3],
   T[Colour2, Colour3, Gluon1]
   ];

DefineProjector[
   projGTT2, 
   {1,1/2,1/2}, 
   {8,3,3},
   1/4,
   1/(4*(p2.p2 + p3.p3 - 2*p2.p3 ))*(p2[Lor1]*GammaM[1,Spin2,Spin3] - p3[Lor1]*GammaM[1,Spin2,Spin3]),
   T[Colour2, Colour3, Gluon1]
   ];

DefineProjector[
   projGTT3, 
   {1,1/2,1/2}, 
   {8,3,3},
   1/4,
   1/(4*p1.p1)*((p2[Lor1]+p3[Lor1])*GammaM[1,Spin2,Spin3] ),
   T[Colour2, Colour3, Gluon1]
   ];

DefineProjector[
   projGTT4, 
   {1,1/2,1/2}, 
   {8,3,3},
   1/4,
   -1/(16*(d-1)*(p2.p2 + p3.p3 - 2 p2.p3))*(GammaM[Lor1,p2,Spin2,Spin3] - GammaM[p2,Lor1,Spin2,Spin3] - GammaM[Lor1,p3,Spin2,Spin3] + GammaM[p3,Lor1,Spin2,Spin3] ),
   T[Colour2, Colour3, Gluon1]
   ];
   
DefineProjector[
   projGTT5, 
   {1,1/2,1/2}, 
   {8,3,3},
   1/4,
   -1/(16*(d-1)*(p2.p2 + p3.p3 + 2 p2.p3))*(GammaM[Lor1,p2,Spin2,Spin3] + GammaM[Lor1,p3,Spin2,Spin3]  - GammaM[p2,Lor1,Spin2,Spin3] - GammaM[p3,Lor1,Spin2,Spin3] ),
   T[Colour2, Colour3, Gluon1]
   ];


(* Gluon - Gluon - Z boson *)

DefineProjector[
   projGGZ, 
   {1,1,1}, 
   {8,8,1},
   -1/((-3 + d)*(-2 + d)*(-1 + d)*d)*1/(Dot[p1,p1]+Dot[p2,p2]+2*Dot[p1,p2]),
   LeviCivita[Lor1,Lor2,p1,p2]*(p1[Lor3]+p2[Lor3]),
   Metric[Gluon1,Gluon2]
   ];



(* H - t - tbar *)


DefineProjector[
   projHTT1, 
   {0,1/2,1/2}, 
   {1,3,3},
   1/4,
   GammaM[1,Spin2,Spin3],
   Metric[Colour2, Colour3]
   ];

DefineProjector[
   projHTT2, 
   {0,1/2,1/2}, 
   {1,3,3},
   1/4,
   GammaM[p2,Spin2,Spin3] - GammaM[p3,Spin2,Spin3],
   Metric[Colour2, Colour3]
   ];

DefineProjector[
   projHTT3, 
   {0,1/2,1/2}, 
   {1,3,3},
   1/4,
   GammaM[p2,Spin2,Spin3] + GammaM[p3,Spin2,Spin3],
   Metric[Colour2, Colour3]
   ];



(* u - ubar - t - tbar *)

DefineProjector[
   projUUTT1, 
   {1/2,1/2,1/2,1/2},
   {3,3,3,3},
   1,
   (p1.p1+p2.p2+2*p1.p2)*GammaM[iLor99,Spin1,Spin2]*GammaM[iLor99,Spin4,Spin3],
   1
   (* T[Colour1, Colour2, iGluon99]*T[Colour3, Colour4, iGluon99] *)
   ];

DefineProjector[
   projUUTT2, 
   {1/2,1/2,1/2,1/2},
   {3,3,3,3},
   1,
   (p1[iLor99]+p2[iLor99])*GammaM[iLor99,Spin1,Spin2]*GammaM[p1+p2,Spin4,Spin3],
   1
   (* T[Colour1, Colour2, iGluon99]*T[Colour3, Colour4, iGluon99] *)
   ];
