(** * An environment (noted ε) is a set of propositions that
    * hold true (hypotheses) for the current scope.
    *
    * Among the possible types of propositions, one stands out
    * by being our object of interest for this experiment: the
    * Defined proposition. It simply states that if [Defined x]
    * is true, then the variable [x] exists in the environment
    * context (noted Γ).
    *
    * Γ is a function that takes an environment and returns the
    * set of defined variables ; that is to say, each [name] for
    * which the proposition [Defined name] is true.
    * NOTE: To avoid the lookup overhead, one can implement Γ as
    *       a set that gets updated hand-to-hand with its
    *       associated ε. That's what we do here.
    *
    * Let's introduce a bit of syntax and explain it as we go.
    *
    *   let x := 3
    *
    * What this statement does is pretty obvious: it defines a
    * variable called [x] in the environment with the value 3.
    *
    * ...Right? Well, this would be true in a classical mapping-
    * style environment, where binding names are mapped to their
    * associated values. Instead, this assignment becomes sugar
    * for the following lines:
    *
    *   let x
    *   let (forall a, a = 3) x
    *
    * The first line introduces [x] to the environment by
    * establishing that the proposition [Defined x] is true.
    *
    * The second line presents an anonymous proposition that we
    * will call [P] for clarity, which states for all a, a is
    * equal to 3. Obviously, this is not a tautology. However,
    * since we want [x] to be equal to 3, we can simply define
    * [P x] as true, which we do by applying [P] to [x].
    *
    * Could we have defined the proposition without applying [x]
    * to it? Theoretically, yes! The unification engine will
    * complain, though, as there are many objects that are
    * already in the environment for which their constraints
    * would contradict [P].
    *
    * Our environment now contains two propositions: first, that
    * a variable [x] exists ; second, that [x] satisfies the
    * anonymous proposition that we mentioned as [P]. This is
    * strictly equivalent as having one entry [x] mapped to 3.
    * Nevertheless, it introduces a separation of concern for
    * two things that you might have thought as being only one
    * -- and to be fair, there are languages where it makes
    * sense to have existence and definition tied together.
    *
    * For now, this experiment does not implement anything but
    * the [Defined] proposition, so we are stuck with introducing
    * meaningless variables to the environment at the moment.
    * Hopefully this becomes false in the future!
    *
    * ---
    *
    * If you thought that having an environment as a set of
    * hypotheses is a bit odd, thanks to the Curry-Howard
    * correspondence we know that programs are proofs ;)
    * *)

open Orion_prop_oriented

let env = Environment.define_variables Environment.empty [ "a"; "b" ]
let () = print_endline (Environment.show env)
