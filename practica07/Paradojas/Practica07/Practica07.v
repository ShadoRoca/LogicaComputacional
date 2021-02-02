Section Naturales.

(*Definicion de los numeros naturales, la O representa al 0 y S es el constructor sucesor*)
Inductive natural: Type :=
  | O : natural
  | S : natural -> natural.

(*Definicion de la suma*)
Fixpoint suma (n : natural) (m : natural) : natural :=
  match n with
  | O => m
  | S p => S (suma p m)
  end.

(*Teorema auxiliar para probar que n + 0 = 0 + n*)
Theorem conmutatividad_suma_con_O: forall n : natural, suma n O = suma O n.
Proof.
induction n.
+ simpl. reflexivity.
+ simpl. rewrite IHn. simpl. reflexivity. 
Qed.

(*Teorema auxiliar para probar que m + S(n) = S(m + n)*)
Theorem conmutatividad_suma_con_S: forall n m : natural, suma m (S n) = S (suma m n).
Proof.
induction m.
+ intros. simpl. reflexivity.
+ intros. simpl. rewrite <- IHm. reflexivity.    
Qed.

(*Demostracion de la conmutatividad de la suma, use teoremas auxiliares ya que con mi
  definicion de suma no bastaba así que investigué un poco y me di cuenta que tenia que probar
  los teoremas auxiliares para que saliera*)
Theorem conmutatividad_suma: forall n m : natural, suma n m = suma m n.
Proof.
induction m.
+ apply conmutatividad_suma_con_O.
(*n + 0 = 0 + m , se prueba con el teorema de conmutatividad de la suma con 0*)
+ rewrite conmutatividad_suma_con_S. rewrite  IHm. simpl. reflexivity.    
(*suma n (Sm) = suma (Sm) n => S(suma n m) = suma (S m) n => 
  S (suma m n) = suma (S m) n => S (suma m n) = S (suma m n)*)     
Qed.

(*Definicion de la multiplicacion*)
Fixpoint multi (n: natural) (m:natural) : natural :=
  match n with
  | O => O
  | S p => suma m (multi p m)
  end.

Theorem asociatividad_suma: forall n m r: natural, suma (suma n m) r = suma n (suma m r).
Proof.
induction n.
-intros. simpl. reflexivity.
-intros. simpl. rewrite IHn. reflexivity.
Qed.

(*1- DISTRIBUTIVIDAD DEL PRODUCTO SOBRE LA SUMA*)
Theorem distributividad_producto_suma: forall m n r: natural, multi n (suma m r) = suma (multi n m) (multi n r).
Proof.
induction n.
-intros. simpl. reflexivity.
-intros. simpl. rewrite IHn. rewrite <- asociatividad_suma. rewrite <- asociatividad_suma. rewrite conmutatividad_suma. rewrite asociatividad_suma. rewrite -> (conmutatividad_suma r (multi n m)). rewrite conmutatividad_suma. rewrite <- asociatividad_suma. reflexivity.
Qed.

Require Import Setoid.
Section Grupos.
(* Un grupo es un conjunto G junto con una operacion binaria sobre G que combina dos elementos en otro de G. Hay un elemento especial que es llamado el elemento identidad y para cada elemento en G existe uno inverso que bajo la operacion obtiene a la identidad *)

Variable G: Set.

Hypotheses (e:G)  (* Neutro *)
           (g:G -> G -> G) (* Operacion binaria*)
           (h:G -> G). (* Inverso *)

(* Las condiciones anteriores satisfacen las siguientes propiedades: *)
Hypothesis Asoc : forall x y z : G, g x (g y z) = g (g x y) z.

Hypothesis Inv : forall x:G, g (h x) x = e.

Hypothesis Neut : forall x:G, g e x = x.

(* notacion para hacer infija a la operacion binaria *)
Infix "<+>" := g (at level 50, left associativity).


Theorem NeutIdem: forall x:G, g x x = x -> x = e.
Proof.
intros.
rewrite <- Inv with x.
rewrite <- H at 3.
rewrite Asoc.
rewrite Inv.
rewrite Neut.
trivial.
(*reflexivity.*)
Qed.

Theorem Cancel: forall x y z:G, g x y = g x z -> y = z.
Proof.
intros.
rewrite <- Neut with y.
rewrite <- Inv with x.
rewrite <- Asoc.
rewrite H.
rewrite Asoc.
rewrite Inv.
rewrite Neut.
reflexivity.
Qed. 


Theorem InvDer: forall x:G, g x (h x) = e.
Proof.
intros.
apply NeutIdem.
rewrite <- Asoc.
rewrite Asoc with (h x) (x) (h x).
rewrite Inv.
rewrite Neut.
trivial.
Qed.


Theorem NeutroDer: forall x:G, g x e = x.
Proof.
intros.
cut (e = g (h x) x).
intro.
rewrite H.
rewrite Asoc.
rewrite InvDer.
rewrite Neut.
trivial.
rewrite Inv.
trivial.
Qed.

(*2*)
Theorem inverso_de_inverso: forall x:G, h(h(x)) = x.
Proof.
intros.
apply (Cancel) with (h(x)).
rewrite InvDer.
rewrite Inv.
reflexivity.
Qed.


(*3*)
Theorem Inverso_operacion_binaria: forall x y:G, h(g x y) = g (h x) (h y).
Proof.
intros.
apply (Cancel) with (g x y).
rewrite Asoc.
rewrite <- Asoc.
rewrite Inv with ((h (g x y))).
Qed.



(*Theorem UnicInv: forall z x:G, g z x = e -> x = h z.
Proof.*)

Require Import Classical.

Section LogicaProposicional.

Theorem ModusTollens: forall p q: Prop,  (~p -> ~q) /\ q -> p.
Proof.
intros p q H.
destruct H.
destruct (classic p).
- assumption.
- absurd q.
  --  apply H. assumption.
  -- assumption.
Qed.


Theorem ModusPonens: forall P Q: Prop,  (P -> Q) /\ P -> Q.
Proof.
intros P Q H.
destruct H.
apply H.
apply H0.
Qed.