# Fun with Curry-Howard 

## Introduction

We are going to implement a type-checker for an **extended Church-style simply typed lambda calculus**. By 'simply typed' here, we mean to be drawing a contrast with more complex typing systems such as systems with type polymorphism and dependant types. By 'Church-style' we mean that we will use type annotations on lambda variables and that every term will be have a unique type. By 'extended' we mean that we will have type constructors other than the functional type constructor. 

Basic simply typed lambda calculus only has a functional type constructor. We will have constructors for sum types, product types, and the bottom type. Why just these? Because we want a typed lambda calculus that will correspond to the intuitionist fragment of propositional logic. Just to foreshadow that correspondence, we are going to have a type constructor `->` that corresponds to implication `->`, a type constructor `+` that corresponds to disjunction, and a type constructor `*` that corresponds to conjunction. We will also be able to express negation using `->` and `Bot`.

When I say what we are going to implement a type-checker, I mean that we are going to implement a function that can not only tell us in a 'yes/no' manner whether a term is well-typed. It will be a function that returns a derivation proving that the term is well-typed if it is. This derivation will look a lot like a natural deduction proof. This is another aspect of our correspondence. A well-typed term can be thought of as a proof of a proposition corresponding to the type. A derivation showing that `\x:A.x` is well-typed, for instance, will be a proof of the tautology `(A->A)`. 

These are two parts of what is known as the Curry-Howard correspondence. We have a correspondence here between types and propositions and between terms and proofs. The third part has to do with proof simplification, but set that aside for now. In a sense, in writing a type-checker we are doing something similar to what we would do if were writing a function for checking a natural deduction proof. A proof checker would check whether each part of a proof corresponded to a correct application of a rule for constructing a proof. In a natural deduction proof the tree structure is explicit and we often annotate the tree with rules justifying each step. When we have a term in a simply typed lambda calculus we don't have an explicit tree structure or rule annotations. The terms nonetheless carry such information. The cool thing about exploiting the correspondence to write proofs is that at the end of the day we can run or execute our proofs. 

In the following we will mainly be concerned with an abstract syntactic representation of the terms of our extended Church-style simply typed lambda calculus. We nonetheless have a concrete syntax in mind, and, a little later, we will take up the issue of how to parse our concrete syntax and how to print to out concrete syntax. Let me briefly explain the concrete syntax, then the abstract syntax, and then relate these in a table. 

## Types

First, let me describe the syntax for our types. These annotations are used to annotate variables. (They have another use we will come to below). We have the following 'base types' `E`, `T`. My use of `E` and `T` for base types is a nod to philosophers' use of `e` for entities and `t` for things with truth values. We also have `Bot` as the bottom or void type. It is a type that has no elements, that cannot be instantiated. We will print this using the unicode character `⊥` but will typically write it as `Bot` at the keyboard. The first clause of a recursive definition of our types might go like this then:

* `E` and `T` and `⊥` are types. 

We have the following 'type constructors' that will allow us to recursively generate our remaining types. These constructors are `->`, `+`, and `*` for constructing function types, sum types, and product types, respectively. Where we can, we print these as `→`, `+`, and `×` respectively. The second, third, and fourth,  clauses of our recursive definition of our types might go like this: 

* if `φ` and `ψ` are types, then `(φ→ψ)` is a type.
* if `φ` and `ψ` are types, then `(φ+ψ)` is a type.
* if `φ` and `ψ` are types, then `(φ×ψ)` is a type.
* nothing else is a type. 

In Haskell, we define our types as follows:

```haskell
data BaseType = E | T
    deriving (Show, Eq)

data Type = Base BaseType 
          | Bot 
          | Func Type Type 
          | Sum  Type Type 
          | Prod Type Type 
          deriving (Show, Eq)
```

We treat base types and derived types differently so that we can add to our base types later if we like. Our typechecking function doesn't need to know what the base types are. It is agnostic about base types. 

## Terms

Now for the terms of our extended Church-style simply typed lambda calculus. First of all, we have **variables**. We write these as `x`, `y`, `z`. Variables are our most basic terms. Next week have **abstractions**. We write an abstraction like this `\x:(Type).Term`. That is, we first write our 'lambda' symbol as a backslash `\` followed by a variable, followed by a colon `:` to introduce a type annotation, followed by a dot `.`, followed by a term. Concretely we might write `\x:E.x`. Where we can we use the unicode lambda character `λ` and write `λx:E.x`. Next we have **applications** which we write like this `(Term1 Term2)` or more concretely, `(λx:(E→T).x y)`. 

Now we get to the terms which make up our 'extended' calculus. We have **pairs** of the product type which we write like this `{Term1,Term2}`. We also have the two destructors related to product types `fst Term` and `snd Term`. We have two constructors for sum types `inl Term as Type` and `inr Term as Type`. And we have a single destructor for the sum type: `case Term of Term | Term`. This may need some explaining. For reasons we will go into below, we need to annotate our constructors for sum types with the constructed type. These constructors are a bit like the role for disjunction introduction. If you already have a term of type E, then you can introduce a term of sum type. But you need to say what that type is. So we write, for instance, `inl x as (E+T)` when we want to introduce a term of type `(E+T)` given a term `x` of type `E`. We will see how the sum type destructor works later. But basically, it is going to take a term of sum type and two terms of function type appropriately related to the sum type, and get a term of the type of the return type of those functions. Finally, we have one more term that we write `abort Type Term`, or, for instance `abort A x`. If we have a term `x` of the bottom type we can derive a term of type `A`, namely `abort A x`. 

Here is a recursive specification of our terms:

* `x`, `y`, `z`, `x1`, `x2`, ... are terms. 
* if `v` is a variable and `t` is a term, and `φ` is a type, then `λv:φ.t` is a term. 
* if `t1` and `t2` are terms, then `(t1 t2)` is a term. 
* if `t1` and `t2` are terms, then `{t1,t2}` is a term. 
* if `t` is a term, then `fst t` and `snd t` are terms. 
* if `t` is a term, and `φ` is a type, then `inl t as φ` and `inr t as φ` are terms. 
* if `t1`, `t2`, `t3`, are terms, then `case t1 of t2 | t2` is a term. 
* if `t` is a term and `φ` is a type, then `abort φ t` is a term. 
* nothing else is a term.

In Haskell, we define our terms as follows. 

```haskell
data VarSym = VarSym Char (Maybe Int)
   deriving (Show, Eq)

data Term = 
            Var VarSym 
          | Abs VarSym Type Term 
          | App Term Term 
          | Pair Term Term 
          | Fst Term 
          | Snd Term 
          | Inl Term Type 
          | Inr Term Type 
          | Case Term Term Term 
          | Abort Type Term
          deriving (Show, Eq)

``` 

This is pretty self-explanatory. A variable symbol will just be a character like `x` optionally followed by an integer like `1`, e.g. `x1`. For the most part, our type-checker won't care what variable symbols are. All that matters is that they are something that can be compared for equality. Hence the clause `deriving (Show, Eq)` which ensures both that variable symbols can be compared for equality and that they can be printed. It may be worth remarking at this stage that it will be assumed that fresh variables will be used wherever possible in our terms so that we don't have worry about variable name clashes. 

## Typing Rules 

Given this syntax, we can write terms that are not **well-typed**. A well-typed term is a term whose type can be derived according to our typing rules. Now, we do not say that a term is well typed or not in absolute terms. We say that it is well-typed or not given a context. We are concerned with **judgements** of the form `Γ ⊢ t`, where this says that term `t` is well-typed in context `Γ`.

### Variables 

    Γ, x:φ ⊢ x:φ

### Abstractions 

    Γ, x:φ ⊢ x:φ
    -----------------------
    Γ ⊢ λx:φ M : (φ → ψ)

### Applications 

    Γ ⊢ M : (φ → ψ)  Γ ⊢ N :φ
    --------------------------
    Γ ⊢ (M N) : ψ

### Products

    Γ ⊢ M : ψ  Γ ⊢ N :φ
    --------------------------
    Γ ⊢ {M,N} : (ψ × φ)

    Γ ⊢ M : (ψ × φ)
    --------------------------
    Γ ⊢ fst M : ψ

    Γ ⊢ M : (ψ × φ)
    --------------------------
    Γ ⊢ fst M : φ

### Sums 

    Γ ⊢ M : φ
    -------------------------------
    Γ ⊢ inl M as (ψ + φ) : (ψ + φ)


    Γ ⊢ M : φ
    -------------------------------
    Γ ⊢ inr M as (ψ + φ) : (ψ + φ)


    Γ ⊢ L : (ψ + φ)   Γ ⊢ M : (ψ → ρ)  Γ ⊢ M : (φ → ρ)
    ----------------------------------------------------
    Γ ⊢ case L of M | N : p

### Bottom 

    Γ ⊢ M : ⊥
    --------------------
    Γ ⊢ abort φ M : φ 

## Type Checking 

Now we want to write a type-checking algorithim to check if some arbitrary term is well-typed. Let's think through the algorithim informally. Suppose we are given some term M. We know that it can either be an abstraction, an application, a variable, and so on.

In Haskell, that means we will be writing something like this:

```haskell 
typecheck :: Judgement -> Maybe Type 
typecheck ctx intrm = case term of 
   Var vsymb ->  
   Abs vsymb typ trm ->
   App trm1 trm2 ->
   Pair trm1 trm2 ->
   Fst trm -> 
   Snd trm ->  
   Inl trm typ ->
   Inr trm typ -> 
   Case trm1 trm2 trm3 ->
   Abort typ trm ->
```


What do we do if it is a variable? Well, we just check that the variable is assigned a type in the context. If it is, that's great, it is well-typed in the context. Rather than just return a boolean 'yes' or 'no'. Let's assume that our function returns the type of the variable if it succeeds. This will be helpful if we are trying to work out the type of a term the variable occurrs free in. 

```haskell 
typecheck :: Judgement -> Maybe Type 
typecheck ctx intrm = case term of 
   Var vsymb -> Map.lookup vsymb ctx 
```

What do we do if the term is an abstraction? Well, if it is an abstraction, we know it is a functional type. And we know that the 'input' type for the function is the type given in the annotation of the variable. The return type will be φ on the assumption that the body term M has type φ in the context Γ extended by the variable assignment corresponding to the annotation of the lambda variable. The abstraction is well-typed assuming that the body term M is well-typed. 

```haskell 
typecheck :: Judgement -> Maybe Type 
typecheck ctx intrm = case term of 
   Abs vsymb typ trm -> let newctx = Map.insert vsymb typ ctx 
                            bdytrmtyp = typecheck newctx trm in 
                        Func <$> Just typ <*> bdytrmtyp 
```

What do we do if the term is an application? Well, if it is an application, it is well-typed if both M and N are well typed and M is a functional type and N is the type of the 'input' type of M. (M N) will have the return type of M. 

```haskell 
typecheck :: Judgement -> Maybe Type 
typecheck ctx intrm = case term of 
   App trm1 trm2 ->
      let trm1typ = typecheck ctx trm1 
          trm2typ = typecheck ctx trm2 in 
      case trm1typ of 
         Just (Func ityp otyp) -> if Just ityp == trm2typ then Just otyp else Nothing 
         _ -> Nothing
```

What do we do if the term is a pair? Well, if it is a pair it a product type. It is well-typed if both M and N are well typed. And its type is the type (ψ × φ) where ψ is the type of M and φ the type of N.



## Printing 


## Parsing 




