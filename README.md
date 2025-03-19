# Element Model Type Theory

This is a type theory for Lex^op, the opposite of the category of finite limit
theories. The basic judgements of this type theory are the following.

We write a morphism in Lex^op from `C` to `D` as `C <-- D`, as it corresponds to
a morphism `D --> C` in Lex.

```
Γ ctx
# Γ is a context
# [Γ] is an object of Lex^op

Γ ⊢ T theory
# Γ derives that Τ is a theory
# a display morphism [Γ; M:T] <-- [Γ]

Γ ⊢ M : T
# Γ derives that M is a model of T
# a section [Γ] <-M-- [Γ,M:T] of the display morphism

Γ ⊢ A type
# Γ derives that A is a type
# An object of [Γ]
# Identified with its display morphism [Γ; x:A] = [Γ]/[A] <-- [Γ] given by X ↦ X x A.

Γ ⊢ a : A
# Γ derives that a is an element of A
# A global element a: 1 --> A in [Γ]
# Identified with a section of [Γ; x:A] <-- [Γ] which sends X --> A its fiber X_a
```

Check out [examples](./examples) to see how it's used.
