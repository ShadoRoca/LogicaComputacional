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

(*-------2-------*)
Theorem inverso_de_inverso: forall x:G, h(h(x)) = x.
Proof.
intros.
apply (Cancel) with (h(x)).
rewrite InvDer.
rewrite Inv.
reflexivity.
Qed.



(*-------3-------*)
Theorem inverso_operacion_binaria: forall x y:G, h(g x y) = g (h y) (h x).
Proof.
intros.
apply (Cancel) with (g x y).
rewrite InvDer with (g x y).
rewrite Asoc.
rewrite <- Asoc with x y (h y).
rewrite InvDer.
rewrite NeutroDer with x.
rewrite InvDer.
reflexivity.
Qed.

Require Import Classical.

Section LogicaProposicional.

(*Auxiliares equivalencia de la doble negacion*)
Hypothesis DobleNeg : forall p:Prop, (¬ ¬ p) = p.
Hypothesis DobleNegInv : forall p: Prop, (¬ ¬ p) -> p.

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

(*Auxiliar Modus Tollens sin la negacion de p y q en las hipotesis *)
Theorem ModusTollensInv: forall p q: Prop,  (p -> q) /\ ¬q -> ¬p.
Proof.
intros p q H.
destruct H.
destruct (classic (¬p)).
assumption.
absurd (¬q).
rewrite DobleNeg.
apply H.
apply DobleNegInv.
assumption.
assumption.
Qed.

Theorem ModusPonens: forall P Q: Prop,  (P -> Q) /\ P -> Q.
Proof.
intros P Q H.
destruct H.
apply H.
apply H0.
Qed.

Lemma sd: forall a b: Prop, (a \/ b) /\ ~b -> a.
Proof.
  intros a b H.
  induction H.
  induction H.
  exact H. 
  contradiction H0.
Qed.

(*Lemma transitividad_implicacion: forall p q r: Prop, (p -> q -> r) -> (p -> r).
Proof.
    intros.
    assert (H1 := conj H H0).
    apply ModusPonens in H1.
    apply H.
    assumption.
    *)

Lemma MT: forall P Q: Prop, (P -> Q) /\ ~Q -> ~P.
Proof.
intuition.
Qed.

(*-------4-------*)
Theorem ejercicio4: forall p q r s t u: Prop, (p -> q /\ r) -> (r \/ ¬q -> s /\ t) -> (t <-> u) -> (p -> u).
Proof.
intros.
apply H1.
apply H0.
left.
apply H.
assumption.
Qed.

(*-------5-------*)
Theorem ejercicio5: forall P Q R S T: Prop, (P -> Q -> R) /\ (P \/ S) /\ (T -> Q) /\ (~S) -> ~R -> ~T.
Proof.
intros.
destruct H.
destruct H1.
destruct H2.
assert (H4 := conj H1 H3).
apply sd in H4.
assert (H5 := conj H H4).
apply ModusPonens in H5.
assert (H6 := conj H5 H0).
apply MT in H6.
assert (H7 := conj H2 H6).
apply MT in H7.
assumption. 
Qed.


Theorem puntoExtra1: forall P Q: Prop, ((P /\ Q) <-> P) -> (Q <-> P \/ Q).
Proof.
intros.
destruct H.
intuition.
Qed.
