
lambda-expr := (lambda Ty
                 ((fname pat_1,1 ... pat_1,n) expr_1)
                 ...
                 ((fname pat_n,1 ... pat_n,n) expr_n))

type-expr := (type (TypeCtor e_1 ... e_n Ty)
               (DataCtor_1 Ty_1,1 ... Ty_1,n)
               ...
               (DataCtor_2 Ty_n,1 ... Ty_n,n))

expr := Type
     |  (forall var Ty expr)
     |  lambda-expr
     |  (expr_1 ... expr_n)

pat := var
    |  TypeCtor
    |  (TypeCtor e_1 ... e_n)
    |  DataCtor
    |  (DataCtor e_1 ... e_n)

toplevel : (type-expr | lambda-expr)*
