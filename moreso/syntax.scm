
## L0

define-expr := (define Ty
                 ((fname pat_1,1 ... pat_1,n) expr_1)
                 ...
                 ((fname pat_n,1 ... pat_n,n) expr_n))

type-expr := (type TypeCtor Ty
               (DataCtor_1 Ty_1)
               ...
               (DataCtor_2 Ty_n))

expr := Type
     |  (forall var Ty expr)
     |  define-expr
     |  (expr_1 .. expr_n)

pat := var
    |  _
    |  TypeCtor
    |  (TypeCtor e_1 ... e_n)
    |  DataCtor
    |  (DataCtor e_1 ... e_n)

toplevel : (type-expr | define-expr)*

## L1

type-expr := (type (TypeCtor e_1 ... e_n Ty)
               (DataCtor_1 Ty_1,1 ... Ty_1,n)
               ...
               (DataCtor_2 Ty_n,1 ... Ty_n,n))
