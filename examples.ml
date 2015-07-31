open Formula
open Printf

let _ =
  let p = {fm| p /\ ~(q \/ r) |fm} in
  print p;
  print (dual p);
  print (dual (dual p))

let _ =
  let p = {fm| (true ==> (x <=> false)) ==> ~(y \/ false /\ z) |fm} in
  print p;
  print (psimplify p); (* ~x ==> ~y *)
  let q = {fm| ((x ==> y) ==> true) \/ ~false |fm} in
  print q;
  print (psimplify q) (* true *)

let _ =
  let p = {fm| (p <=> q) <=> ~(r ==> s) |fm} in
  print p;
  print (nnf p); (* (p /\ q \/ ~p /\ ~q) /\ r /\ ~s \/
                    (p /\ ~q \/ ~p /\ q) /\ (~r \/ s) *)
  printf "%b\n" (tautology (mk_equiv p (nnf p)))
