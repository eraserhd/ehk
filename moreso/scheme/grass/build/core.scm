(define *grass-version* 17)
(begin
  (define id (lambda (_x_43) _x_43))
  (define read-forms
    (lambda (_file-or-port_43 . _reader_44)
      ((if (string? _file-or-port_43)
         call-with-input-file
         (lambda (_fp_45 _p_46) (_p_46 _fp_45)))
       _file-or-port_43
       (let ((_rd_45 (if (pair? _reader_44) (car _reader_44) read)))
         (lambda (_port_46)
           (let _loop_47 ((_xs_48 '()))
             (let ((_x_49 (_rd_45 _port_46)))
               (if (eof-object? _x_49)
                 (cons 'begin (reverse _xs_48))
                 (_loop_47 (cons _x_49 _xs_48))))))))))
  (define emit (lambda _xs_43 (for-each display _xs_43)))
  (define stringify
    (lambda (_x_43)
      (if (symbol? _x_43)
        (symbol->string _x_43)
        (if (string? _x_43)
          _x_43
          (if (number? _x_43)
            (number->string _x_43)
            (if (char? _x_43)
              (string _x_43)
              (error "can't stringify" _x_43)))))))
  (define symbolify
    (lambda (_x_43)
      (if (symbol? _x_43)
        _x_43
        (if (string? _x_43)
          (string->symbol _x_43)
          (if (char? _x_43)
            (string->symbol (string _x_43))
            (error "can't symbolify" _x_43))))))
  (define listify (lambda (_x_43) (if (list? _x_43) _x_43 (list _x_43))))
  (define join
    (lambda (_xs_43 . _sep_44)
      (let ((_sep_45 (if (pair? _sep_44) (car _sep_44) "")))
        (apply string-append
               (let _loop_46 ((_xs_47 _xs_43))
                 (if (null? _xs_47)
                   '()
                   (if (null? (cdr _xs_47))
                     _xs_47
                     (cons (car _xs_47)
                           (cons _sep_45 (_loop_46 (cdr _xs_47)))))))))))
  (define every
    (lambda (_pred_43 _lst_44)
      (let _loop_45 ((_lst_46 _lst_44))
        (let ((_x_47 (null? _lst_46)))
          (if _x_47
            _x_47
            (if (not (_pred_43 (car _lst_46)))
              #f
              (_loop_45 (cdr _lst_46))))))))
  (define any
    (lambda (_pred_43 _lst_44)
      (let _loop_45 ((_lst_46 _lst_44))
        (if (null? _lst_46)
          #f
          (let ((_x_47 (_pred_43 (car _lst_46))))
            (if _x_47 _x_47 (_loop_45 (cdr _lst_46))))))))
  (define *procedure-tag* (list '*procedure*))
  (define %procedure
    (lambda (_proc_43 _name_44 _argc_45)
      (vector
        *procedure-tag*
        _proc_43
        (if (string? _name_44)
          _name_44
          (if (symbol? _name_44)
            (symbol->string _name_44)
            (if (not _name_44)
              #f
              (%error "internal - bad procedure name" _name_44))))
        _argc_45)))
  (define %procedure?
    (lambda (_x_43)
      (if (vector? _x_43)
        (if (= (vector-length _x_43) 4)
          (eq? (vector-ref _x_43 0) *procedure-tag*)
          #f)
        #f)))
  (define %procedure-code (lambda (_x_43) (vector-ref _x_43 1)))
  (define %procedure-name (lambda (_x_43) (vector-ref _x_43 2)))
  (define %procedure-arity (lambda (_x_43) (vector-ref _x_43 3)))
  (define disjoint-type-tag (list 'object))
  (define %disjoint-type-instance?
    (lambda (_x_43)
      (if (vector? _x_43)
        (if (= (vector-length _x_43) 4)
          (eq? disjoint-type-tag (vector-ref _x_43 0))
          #f)
        #f)))
  (define disjoint-type-instance->string
    (lambda (_x_43)
      (let ((_name_44 (vector-ref _x_43 2)))
        (if _name_44
          (string-append "#<" (symbol->string _name_44) ">")
          "#<object>"))))
  (define make-disjoint-type
    (let ((_id-counter_43 0))
      (letrec ((_dt-type-id_44 (lambda (_x_47) (vector-ref _x_47 1)))
               (_dt-type-name_45 (lambda (_x_47) (vector-ref _x_47 2)))
               (_dt-data_46 (lambda (_x_47) (vector-ref _x_47 3))))
        (lambda _name_47
          (let ((_type-id_48 _id-counter_43)
                (_name_49 (if (pair? _name_47) (car _name_47) #f)))
            (set! _name_49
              (if (not _name_49)
                #f
                (if (symbol? _name_49)
                  _name_49
                  (if (string? _name_49)
                    (string->symbol _name_49)
                    (error "invalid type name" _name_49)))))
            (set! _id-counter_43 (+ _id-counter_43 1))
            (let ((_pred_50
                    (lambda (_x_50)
                      (if (%disjoint-type-instance? _x_50)
                        (eq? (_dt-type-id_44 _x_50) _type-id_48)
                        #f))))
              (values
                (lambda _data_51
                  (vector
                    disjoint-type-tag
                    _type-id_48
                    _name_49
                    (if (pair? _data_51) (car _data_51) #f)))
                _pred_50
                (lambda (_x_51)
                  (if (_pred_50 _x_51)
                    (_dt-data_46 _x_51)
                    (apply error
                           "not an object of the correct type"
                           _x_51
                           (if _name_49 (list _name_49) '())))))))))))
  (define %%make-environment #f)
  (define %environment? #f)
  (define %environment-data #f)
  (call-with-values
    (lambda () (make-disjoint-type 'environment))
    (lambda (_make_43 _pred_44 _data_45)
      (set! %%make-environment _make_43)
      (set! %environment? _pred_44)
      (set! %environment-data _data_45)))
  (define %make-environment
    (lambda (_name_43 _oblist_44 _mstore_45 _mutable?_46)
      (%%make-environment
        (vector _name_43 _oblist_44 _mstore_45 _mutable?_46))))
  (define %environment-data-name (lambda (_x_43) (vector-ref _x_43 0)))
  (define %environment-data-oblist (lambda (_x_43) (vector-ref _x_43 1)))
  (define %environment-data-oblist-set!
    (lambda (_x_43 _x_44) (vector-set! _x_43 1 _x_44)))
  (define %environment-data-mstore (lambda (_x_43) (vector-ref _x_43 2)))
  (define %environment-data-mstore-set!
    (lambda (_x_43 _x_44) (vector-set! _x_43 2 _x_44)))
  (define %environment-data-mutable? (lambda (_x_43) (vector-ref _x_43 3)))
  (define %input-port #f)
  (define %output-port #f)
  (define %error-port #f)
  (define %initialize-ports
    (lambda (_in_43 _out_44 _err_45)
      (set! %input-port _in_43)
      (set! %output-port _out_44)
      (set! %error-port _err_45)))
  (define %call-with-input-file
    (lambda (_file_43 _proc_44)
      (let ((_in_45 (%open-input-file _file_43)))
        (call-with-values
          (lambda () (_proc_44 _in_45))
          (lambda _results_46
            (close-input-port _in_45)
            (apply values _results_46))))))
  (define %call-with-output-file
    (lambda (_file_43 _proc_44)
      (let ((_out_45 (%open-output-file _file_43)))
        (call-with-values
          (lambda () (_proc_44 _out_45))
          (lambda _results_46
            (close-output-port _out_45)
            (apply values _results_46))))))
  (define %with-input-from-file
    (lambda (_file_43 _thunk_44)
      (let ((_in_45 (%open-input-file _file_43)))
        (let ((_temp_46 _in_45))
          (dynamic-wind
            (lambda ()
              (let ((_tmp_48 _temp_46))
                (set! _temp_46 %input-port)
                (set! %input-port _tmp_48)))
            (lambda ()
              (call-with-values
                (lambda () (_thunk_44))
                (lambda _results_48
                  (close-input-port _in_45)
                  (apply values _results_48))))
            (lambda ()
              (let ((_tmp_48 _temp_46))
                (set! _temp_46 %input-port)
                (set! %input-port _tmp_48))))))))
  (define %with-output-to-file
    (lambda (_file_43 _thunk_44)
      (let ((_out_45 (%open-output-file _file_43)))
        (let ((_temp_46 _out_45))
          (dynamic-wind
            (lambda ()
              (let ((_tmp_48 _temp_46))
                (set! _temp_46 %output-port)
                (set! %output-port _tmp_48)))
            (lambda ()
              (call-with-values
                (lambda () (_thunk_44))
                (lambda _results_48
                  (close-output-port _out_45)
                  (apply values _results_48))))
            (lambda ()
              (let ((_tmp_48 _temp_46))
                (set! _temp_46 %output-port)
                (set! %output-port _tmp_48))))))))
  (define %with-ports
    (lambda (_i_43 _o_44 _e_45 _thunk_46)
      (let ((_temp_47 _i_43))
        (let ((_temp_48 _o_44))
          (let ((_temp_49 _e_45))
            (dynamic-wind
              (lambda ()
                (let ((_tmp_51 _temp_49))
                  (set! _temp_49 %error-port)
                  (set! %error-port _tmp_51))
                (let ((_tmp_51 _temp_48))
                  (set! _temp_48 %output-port)
                  (set! %output-port _tmp_51))
                (let ((_tmp_51 _temp_47))
                  (set! _temp_47 %input-port)
                  (set! %input-port _tmp_51)))
              (lambda () (_thunk_46))
              (lambda ()
                (let ((_tmp_51 _temp_49))
                  (set! _temp_49 %error-port)
                  (set! %error-port _tmp_51))
                (let ((_tmp_51 _temp_48))
                  (set! _temp_48 %output-port)
                  (set! %output-port _tmp_51))
                (let ((_tmp_51 _temp_47))
                  (set! _temp_47 %input-port)
                  (set! %input-port _tmp_51)))))))))
  (define %read-char
    (lambda _port_43
      (read-char (if (pair? _port_43) (car _port_43) %input-port))))
  (define %peek-char
    (lambda _port_43
      (peek-char (if (pair? _port_43) (car _port_43) %input-port))))
  (define %write-char
    (lambda (_c_43 . _port_44)
      (write-char _c_43 (if (pair? _port_44) (car _port_44) %output-port))))
  (define %newline
    (lambda _port_43
      (newline (if (pair? _port_43) (car _port_43) %output-port))))
  (define expand-error-hook error)
  (define debug-syntax #f)
  (define %%expand #f)
  (define %expand #f)
  (define %expand-file #f)
  (define %null-mstore #f)
  (letrec ((_expand-error_43 (lambda _args_95 (expand-error-hook _args_95)))
           (_sid?_44
             (lambda (_sexp_95)
               (let ((_x_96 (symbol? _sexp_95)))
                 (if _x_96 _x_96 (_renamed-sid?_45 _sexp_95)))))
           (_renamed-sid?_45
             (lambda (_sexp_95)
               (if (vector? _sexp_95) (< 1 (vector-length _sexp_95)) #f)))
           (_svector?_46
             (lambda (_sexp_95)
               (if (vector? _sexp_95) (= 1 (vector-length _sexp_95)) #f)))
           (_svector->list_47 (lambda (_sexp_95) (vector-ref _sexp_95 0)))
           (_list->svector_48 (lambda (_l_95) (vector _l_95)))
           (_make-sid_49
             (lambda (_name_95 _renamed-id_96 _location_97)
               (if (eq? _name_95 _location_97)
                 (vector _name_95 _renamed-id_96)
                 (vector _name_95 _renamed-id_96 _location_97))))
           (_sid-name_50
             (lambda (_sid_95)
               (if (symbol? _sid_95) _sid_95 (vector-ref _sid_95 0))))
           (_sid-id_51
             (lambda (_sid_95)
               (if (symbol? _sid_95) _sid_95 (vector-ref _sid_95 1))))
           (_sid-location_52
             (lambda (_sid_95)
               (if (symbol? _sid_95)
                 _sid_95
                 (vector-ref _sid_95 (if (= 2 (vector-length _sid_95)) 0 2)))))
           (_list1?_53
             (lambda (_x_95) (if (pair? _x_95) (null? (cdr _x_95)) #f)))
           (_list2?_54
             (lambda (_x_95) (if (pair? _x_95) (_list1?_53 (cdr _x_95)) #f)))
           (_map-vecs_55
             (lambda (_f_95 _x_96)
               (letrec ((_mv2_97
                          (lambda (_x_99)
                            (if (vector? _x_99)
                              (_f_95 _x_99)
                              (if (pair? _x_99)
                                (let ((_a_100 (car _x_99)) (_b_101 (cdr _x_99)))
                                  (let ((_a-mapped_102 (_mv2_97 _a_100)))
                                    (if _a-mapped_102
                                      (cons _a-mapped_102 (_mv_98 _b_101))
                                      (let ((_b-mapped_103 (_mv2_97 _b_101)))
                                        (if _b-mapped_103 (cons _a_100 _b-mapped_103) #f)))))
                                #f))))
                        (_mv_98
                          (lambda (_x_99)
                            (let ((_x_100 (_mv2_97 _x_99)))
                              (if _x_100 _x_100 _x_99)))))
                 (_mv_98 _x_96))))
           (_wrap-vec_56
             (lambda (_v_95)
               (_list->svector_48 (_wrap-vecs_57 (vector->list _v_95)))))
           (_wrap-vecs_57
             (lambda (_input_95) (_map-vecs_55 _wrap-vec_56 _input_95)))
           (_unwrap-vec_58
             (lambda (_v-sexp_95)
               (if (= 1 (vector-length _v-sexp_95))
                 (list->vector
                   (_unwrap-vecs_59 (_svector->list_47 _v-sexp_95)))
                 (vector-ref _v-sexp_95 0))))
           (_unwrap-vecs_59
             (lambda (_sexp_95) (_map-vecs_55 _unwrap-vec_58 _sexp_95)))
           (_make-code_60 (lambda (_output_95) (list _output_95)))
           (_make-builtin_61 (lambda (_name_95) (list 'builtin _name_95)))
           (_make-transformer_62
             (lambda (_synrules_95 _env_96) (list _synrules_95 _env_96)))
           (_variable?_63 (lambda (_val_95) (symbol? _val_95)))
           (_code?_64 (lambda (_val_95) (_list1?_53 _val_95)))
           (_code-output_65 (lambda (_code_95) (car _code_95)))
           (_syntax?_66 (lambda (_val_95) (_list2?_54 _val_95)))
           (_builtin?_67 (lambda (_syntax_95) (eq? 'builtin (car _syntax_95))))
           (_builtin-name_68 (lambda (_builtin_95) (cadr _builtin_95)))
           (_transformer?_69
             (lambda (_syntax_95) (not (_builtin?_67 _syntax_95))))
           (_transformer-synrules_70 (lambda (_trans_95) (car _trans_95)))
           (_transformer-env_71 (lambda (_trans_95) (cadr _trans_95)))
           (_acons_72
             (lambda (_key_95 _val_96 _alist_97)
               (cons (cons _key_95 _val_96) _alist_97)))
           (_empty-env_73 '())
           (_empty-store_74 '())
           (_lookup-sid_75
             (lambda (_sid_95 _env_96)
               (let ((_tmp_97 (assv (_sid-id_51 _sid_95) _env_96)))
                 (if _tmp_97 (cdr _tmp_97) (_sid-location_52 _sid_95)))))
           (_lookup-location_76
             (lambda (_location_95 _store_96)
               (let ((_tmp_97 (assv _location_95 _store_96)))
                 (if _tmp_97
                   (cdr _tmp_97)
                   (if (symbol? _location_95)
                     (_symloc->var_81 _location_95)
                     #f)))))
           (_lookup2_77
             (lambda (_sid_95 _env_96 _store_97)
               (let ((_x_98 (_lookup-location_76
                              (_lookup-sid_75 _sid_95 _env_96)
                              _store_97)))
                 (if _x_98
                   _x_98
                   (_expand-error_43
                     "Premature use of keyword bound by letrec-syntax (or an internal define-syntax): "
                     _sid_95)))))
           (_extend-env_78
             (lambda (_env_95 _id_96 _location_97)
               (_acons_72 _id_96 _location_97 _env_95)))
           (_extend-store_79
             (lambda (_store_95 _loc_96 _val_97)
               (_acons_72 _loc_96 _val_97 _store_95)))
           (_substitute-in-store_80
             (lambda (_store_95 _loc_96 _val_97)
               (let ((_store_98
                       (if (assv _loc_96 _store_95)
                         (let _loop_98 ((_store_99 _store_95))
                           (let ((_p_100 (car _store_99)))
                             (if (eqv? _loc_96 (car _p_100))
                               (cdr _store_99)
                               (cons _p_100 (_loop_98 (cdr _store_99))))))
                         _store_95)))
                 (if (if (symbol? _loc_96)
                       (eq? _val_97 (_symloc->var_81 _loc_96))
                       #f)
                   _store_98
                   (_acons_72 _loc_96 _val_97 _store_98)))))
           (_symloc->var_81
             (lambda (_sym_95)
               (letrec ((_str_96 (symbol->string _sym_95))
                        (_rename_97
                          (lambda ()
                            (string->symbol (string-append "_" _str_96 "_")))))
                 (let ((_key_98 _sym_95))
                   (if (if (eqv? _key_98 'begin)
                         #t
                         (if (eqv? _key_98 'define)
                           #t
                           (if (eqv? _key_98 'delay)
                             #t
                             (if (eqv? _key_98 'if)
                               #t
                               (if (eqv? _key_98 'lambda)
                                 #t
                                 (if (eqv? _key_98 'letrec)
                                   #t
                                   (if (eqv? _key_98 'quote)
                                     #t
                                     (if (eqv? _key_98 'set!) #t #f))))))))
                     (_rename_97)
                     (if (if (positive? (string-length _str_96))
                           (char=? #\_ (string-ref _str_96 0))
                           #f)
                       (_rename_97)
                       _sym_95))))))
           (_intloc->var_82
             (lambda (_intloc_95 _sid_96)
               (let ((_str_97 (symbol->string (_sid-name_50 _sid_96))))
                 (string->symbol
                   (string-append
                     "_"
                     _str_97
                     "_"
                     (number->string _intloc_95))))))
           (_loc->var_83
             (lambda (_loc_95 _sid_96)
               (if (symbol? _loc_95)
                 (_symloc->var_81 _loc_95)
                 (_intloc->var_82 _loc_95 _sid_96))))
           (_make-begin_84
             (lambda (_outputs_95)
               (if (_list1?_53 _outputs_95)
                 (car _outputs_95)
                 (cons 'begin _outputs_95))))
           (_expand-lambda_85
             (lambda (_formals_95
                      _expr_96
                      _id-n_97
                      _env_98
                      _store_99
                      _loc-n_100)
               (letrec ((_flatten-dotted_101
                          (lambda (_x_103)
                            (if (pair? _x_103)
                              (cons (car _x_103)
                                    (_flatten-dotted_101 (cdr _x_103)))
                              (list _x_103))))
                        (_dot-flattened_102
                          (lambda (_x_103)
                            (if (null? (cdr _x_103))
                              (car _x_103)
                              (cons (car _x_103)
                                    (_dot-flattened_102 (cdr _x_103)))))))
                 (let ((_dotted?_103 (not (list? _formals_95))))
                   (let ((_flattened_104
                           (if _dotted?_103
                             (_flatten-dotted_101 _formals_95)
                             _formals_95)))
                     (letrec ((_check_105
                                (lambda (_x_106)
                                  (let ((_x_107 (_sid?_44 _x_106)))
                                    (if _x_107
                                      _x_107
                                      (_expand-error_43
                                        "Non-identifier: "
                                        _x_106
                                        " in lambda formals: "
                                        _formals_95)))
                                  (if (member
                                        _x_106
                                        (cdr (member _x_106 _flattened_104)))
                                    (_expand-error_43
                                      "Duplicate variable: "
                                      _x_106
                                      " in lambda formals: "
                                      _formals_95)))))
                       (for-each _check_105 _flattened_104)
                       (let _loop_106 ((_formals_107 _flattened_104)
                                       (_rvars_108 '())
                                       (_env_109 _env_98)
                                       (_store_110 _store_99)
                                       (_loc-n_111 _loc-n_100))
                         (if (not (null? _formals_107))
                           (let ((_var_112
                                   (_intloc->var_82 _loc-n_111 (car _formals_107))))
                             (let ((_env_113
                                     (_extend-env_78
                                       _env_109
                                       (_sid-id_51 (car _formals_107))
                                       _loc-n_111)))
                               (let ((_store_114
                                       (_extend-store_79 _store_110 _loc-n_111 _var_112)))
                                 (_loop_106
                                   (cdr _formals_107)
                                   (cons _var_112 _rvars_108)
                                   _env_113
                                   _store_114
                                   (+ 1 _loc-n_111)))))
                           (let ((_vars_112 (reverse _rvars_108)))
                             (let ((_vars_113
                                     (if _dotted?_103
                                       (_dot-flattened_102 _vars_112)
                                       _vars_112)))
                               (list _vars_113
                                     (_expand-expr_90
                                       _expr_96
                                       _id-n_97
                                       _env_109
                                       _store_110
                                       _loc-n_111))))))))))))
           (_check-syntax-bindings_86
             (lambda (_bindings_95)
               (let ((_x_96 (list? _bindings_95)))
                 (if _x_96
                   _x_96
                   (_expand-error_43
                     "Non-list syntax bindings list: "
                     _bindings_95)))
               (for-each
                 (lambda (_b_96)
                   (let ((_x_97 (if (_list2?_54 _b_96) (_sid?_44 (car _b_96)) #f)))
                     (if _x_97
                       _x_97
                       (_expand-error_43 "Malformed syntax binding: " _b_96))))
                 _bindings_95)
               (let _loop_96 ((_bs_97 _bindings_95))
                 (if (null? _bs_97)
                   #f
                   (begin
                     (let ((_dup_98 (assoc (caar _bs_97) (cdr _bs_97))))
                       (if _dup_98
                         (_expand-error_43
                           "Duplicate bindings for a keyword: "
                           (car _bs_97)
                           " and: "
                           _dup_98)))
                     (_loop_96 (cdr _bs_97)))))))
           (_expand-syntax-bindings_87
             (lambda (_bindings_95
                      _id-n_96
                      _syntax-env_97
                      _ienv_98
                      _store_99
                      _loc-n_100
                      _k_101)
               (let _loop_102 ((_bs_103 _bindings_95)
                               (_vals_104 '())
                               (_store_105 _store_99)
                               (_loc-n_106 _loc-n_100))
                 (if (not (null? _bs_103))
                   (_expand-val_89
                     (cadar _bs_103)
                     _id-n_96
                     _syntax-env_97
                     _store_105
                     _loc-n_106
                     (lambda (_val_107 _store_108 _loc-n_109)
                       (_loop_102
                         (cdr _bs_103)
                         (cons _val_107 _vals_104)
                         _store_108
                         _loc-n_109)))
                   (let _loop_107 ((_store_108 _store_105)
                                   (_vals_109 (reverse _vals_104))
                                   (_bs_110 _bindings_95))
                     (if (not (null? _vals_109))
                       (let ((_loc_111
                               (_lookup-sid_75 (caar _bs_110) _ienv_98)))
                         (let ((_store_112
                                 (_extend-store_79
                                   _store_108
                                   _loc_111
                                   (car _vals_109))))
                           (_loop_107
                             _store_112
                             (cdr _vals_109)
                             (cdr _bs_110))))
                       (_k_101 _store_108 _loc-n_106)))))))
           (_expand-any_88
             (lambda (_sexp_95
                      _id-n_96
                      _env_97
                      _store_98
                      _loc-n_99
                      _lsd?_100
                      _ek_101
                      _sk_102
                      _dk_103
                      _bk_104)
               (letrec ((_get-k_105
                          (lambda (_k_110 _sexp_111 _name_112)
                            (let ((_x_113 _k_110))
                              (if _x_113
                                _x_113
                                (_expand-error_43
                                  (string-append
                                    _name_112
                                    " used in bad context: ")
                                  _sexp_111)))))
                        (_get-ek_106
                          (lambda (_sexp_110)
                            (_get-k_105 _ek_101 _sexp_110 "Expression")))
                        (_get-sk_107
                          (lambda (_sexp_110)
                            (_get-k_105 _sk_102 _sexp_110 "Syntax")))
                        (_get-dk_108
                          (lambda (_sexp_110)
                            (_get-k_105 _dk_103 _sexp_110 "Definition")))
                        (_get-bk_109
                          (lambda (_sexp_110)
                            (_get-k_105 _bk_104 _sexp_110 "Begin"))))
                 (let _again_110 ((_sexp_111 _sexp_95)
                                  (_id-n_112 _id-n_96)
                                  (_store_113 _store_98)
                                  (_loc-n_114 _loc-n_99))
                   (letrec ((_expand-subexpr_115
                              (lambda (_sexp_118)
                                (_expand-expr_90
                                  _sexp_118
                                  _id-n_112
                                  _env_97
                                  _store_113
                                  _loc-n_114)))
                            (_handle-syntax-use_116
                              (lambda (_syntax_118 _head_119 _store_120 _loc-n_121)
                                (let ((_tail_122 (cdr _sexp_111)))
                                  (let ((_sexp_123 (cons _head_119 _tail_122)))
                                    (if (_transformer?_69 _syntax_118)
                                      (_apply-transformer_94
                                        _syntax_118
                                        _sexp_123
                                        _id-n_112
                                        _env_97
                                        (lambda (_sexp_124 _id-n_125)
                                          (_again_110
                                            _sexp_124
                                            _id-n_125
                                            _store_120
                                            _loc-n_121)))
                                      (let ((_builtin_124 (_builtin-name_68 _syntax_118))
                                            (_len_125 (length _tail_122)))
                                        (letrec ((_handle-macro-block_126
                                                   (lambda ()
                                                     (let ((_x_128 _ek_101))
                                                       (if _x_128
                                                         _x_128
                                                         (let ((_x_129 _sk_102))
                                                           (if _x_129
                                                             _x_129
                                                             (let ((_x_130 _lsd?_100))
                                                               (if _x_130
                                                                 _x_130
                                                                 (_expand-error_43
                                                                   "Macro block used in bad context: "
                                                                   _sexp_123)))))))
                                                     (let ((_x_128 (>= _len_125 2)))
                                                       (if _x_128
                                                         _x_128
                                                         (_expand-error_43
                                                           "Malformed macro block: "
                                                           _sexp_123)))
                                                     (let ((_bindings_128 (car _tail_122))
                                                           (_body_129 (cdr _tail_122)))
                                                       (_check-syntax-bindings_86 _bindings_128)
                                                       (let _loop_130 ((_bs_131 _bindings_128)
                                                                       (_loc-n_132 _loc-n_121)
                                                                       (_ienv_133 _env_97))
                                                         (if (not (null? _bs_131))
                                                           (_loop_130
                                                             (cdr _bs_131)
                                                             (+ _loc-n_132 1)
                                                             (_extend-env_78
                                                               _ienv_133
                                                               (_sid-id_51 (caar _bs_131))
                                                               _loc-n_132))
                                                           (_expand-syntax-bindings_87
                                                             _bindings_128
                                                             _id-n_112
                                                             _env_97
                                                             _ienv_133
                                                             _store_120
                                                             _loc-n_132
                                                             (lambda (_store_134 _loc-n_135)
                                                               (_expand-body_91
                                                                 _body_129
                                                                 _id-n_112
                                                                 _ienv_133
                                                                 _store_134
                                                                 _loc-n_135
                                                                 _lsd?_100
                                                                 _ek_101
                                                                 _sk_102
                                                                 (if _lsd?_100 _dk_103 #f)
                                                                 (if _lsd?_100 _bk_104 #f)))))))))
                                                 (_handle-expr-builtin_127
                                                   (lambda ()
                                                     (letrec ((_expr-assert_128
                                                                (lambda (_test_129)
                                                                  (let ((_x_130 _test_129))
                                                                    (if _x_130
                                                                      _x_130
                                                                      (_expand-error_43
                                                                        "Malformed "
                                                                        _builtin_124
                                                                        " expression: "
                                                                        _sexp_123))))))
                                                       (cons _builtin_124
                                                             (let ((_key_129 _builtin_124))
                                                               (if (if (eqv? _key_129 'lambda) #t #f)
                                                                 (begin
                                                                   (_expr-assert_128 (= _len_125 2))
                                                                   (_expand-lambda_85
                                                                     (car _tail_122)
                                                                     (cadr _tail_122)
                                                                     _id-n_112
                                                                     _env_97
                                                                     _store_120
                                                                     _loc-n_121))
                                                                 (if (if (eqv? _key_129 'quote) #t #f)
                                                                   (begin
                                                                     (_expr-assert_128 (= _len_125 1))
                                                                     (list (_unwrap-vecs_59 (car _tail_122))))
                                                                   (if (if (eqv? _key_129 'set!) #t #f)
                                                                     (begin
                                                                       (_expr-assert_128
                                                                         (if (= _len_125 2) (_sid?_44 (car _tail_122)) #f))
                                                                       (let ((_var_130
                                                                               (_lookup2_77 (car _tail_122) _env_97 _store_120)))
                                                                         (let ((_x_131 (_variable?_63 _var_130)))
                                                                           (if _x_131
                                                                             _x_131
                                                                             (_expand-error_43
                                                                               "Attempt to set a keyword: "
                                                                               _sexp_123)))
                                                                         (list _var_130
                                                                               (_expand-subexpr_115 (cadr _tail_122)))))
                                                                     (if (if (eqv? _key_129 'delay) #t #f)
                                                                       (begin
                                                                         (_expr-assert_128 (= _len_125 1))
                                                                         (list (_expand-subexpr_115 (car _tail_122))))
                                                                       (if (if (eqv? _key_129 'if) #t #f)
                                                                         (begin
                                                                           (_expr-assert_128 (<= 2 _len_125 3))
                                                                           (map _expand-subexpr_115 _tail_122))
                                                                         #f)))))))))))
                                          (let ((_key_128 _builtin_124))
                                            (if (if (eqv? _key_128 'let-syntax) #t #f)
                                              (_handle-macro-block_126)
                                              (if (if (eqv? _key_128 'syntax-rules) #t #f)
                                                (begin
                                                  (if (< _len_125 1)
                                                    (_expand-error_43
                                                      "Empty syntax-rules form: "
                                                      _sexp_123))
                                                  (let ((_syn_129
                                                          (_compile-syntax-rules_93 _sexp_123 _env_97)))
                                                    ((_get-sk_107 _sexp_123)
                                                     _syn_129
                                                     _sexp_123
                                                     _store_120
                                                     _loc-n_121)))
                                                (if (if (eqv? _key_128 'begin) #t #f)
                                                  (begin
                                                    (let ((_x_129 _ek_101))
                                                      (if _x_129 _x_129 (_get-bk_109 _sexp_123)))
                                                    (if _bk_104
                                                      (_bk_104
                                                        _sexp_123
                                                        _id-n_112
                                                        _env_97
                                                        _store_120
                                                        _loc-n_121)
                                                      (if (null? _tail_122)
                                                        (_expand-error_43
                                                          "Empty begin expression: "
                                                          _sexp_123)
                                                        (_ek_101
                                                          (_make-begin_84
                                                            (map _expand-subexpr_115 _tail_122))))))
                                                  (if (if (eqv? _key_128 'define)
                                                        #t
                                                        (if (eqv? _key_128 'define-syntax) #t #f))
                                                    (begin
                                                      (let ((_x_129
                                                              (if (= 2 _len_125) (_sid?_44 (car _tail_122)) #f)))
                                                        (if _x_129
                                                          _x_129
                                                          (let ((_x_130
                                                                  (if (= 1 _len_125) (eq? _builtin_124 'define) #f)))
                                                            (if _x_130
                                                              _x_130
                                                              (_expand-error_43
                                                                "Malformed definition: "
                                                                _sexp_123)))))
                                                      ((_get-dk_108 _sexp_123)
                                                       _builtin_124
                                                       _sexp_123
                                                       _id-n_112
                                                       _env_97
                                                       _store_120
                                                       _loc-n_121))
                                                    (begin
                                                      (_get-ek_106 _sexp_123)
                                                      (_ek_101 (_handle-expr-builtin_127)))))))))))))))
                            (_handle-combination_117
                              (lambda (_output_118)
                                (_ek_101
                                  (if (if (pair? _output_118)
                                        (if (eq? 'lambda (car _output_118))
                                          (if (null? (cadr _output_118))
                                            (null? (cdr _sexp_111))
                                            #f)
                                          #f)
                                        #f)
                                    (caddr _output_118)
                                    (cons _output_118
                                          (map _expand-subexpr_115 (cdr _sexp_111))))))))
                     (if debug-syntax (pp _sexp_111))
                     (if (_sid?_44 _sexp_111)
                       (let ((_val_118
                               (_lookup2_77 _sexp_111 _env_97 _store_113)))
                         (if (_syntax?_66 _val_118)
                           ((_get-sk_107 _sexp_111)
                            _val_118
                            _sexp_111
                            _store_113
                            _loc-n_114)
                           ((_get-ek_106 _sexp_111)
                            (if (_code?_64 _val_118)
                              (_code-output_65 _val_118)
                              _val_118))))
                       (if (if (pair? _sexp_111) (list? _sexp_111) #f)
                         (_expand-any_88
                           (car _sexp_111)
                           _id-n_112
                           _env_97
                           _store_113
                           _loc-n_114
                           #f
                           (if _ek_101 _handle-combination_117 #f)
                           _handle-syntax-use_116
                           #f
                           #f)
                         (if (let ((_x_118 (number? _sexp_111)))
                               (if _x_118
                                 _x_118
                                 (let ((_x_119 (boolean? _sexp_111)))
                                   (if _x_119
                                     _x_119
                                     (let ((_x_120 (string? _sexp_111)))
                                       (if _x_120
                                         _x_120
                                         (let ((_x_121 (char? _sexp_111)))
                                           (if _x_121 _x_121 (eof-object? _sexp_111)))))))))
                           ((_get-ek_106 _sexp_111) _sexp_111)
                           (_expand-error_43
                             (if (pair? _sexp_111)
                               "Improper list: "
                               (if (null? _sexp_111)
                                 "Empty list: "
                                 (if (vector? _sexp_111)
                                   "Vector: "
                                   "Non-S-Expression: ")))
                             _sexp_111
                             " used as an expression, syntax, or definition.")))))))))
           (_expand-val_89
             (lambda (_sexp_95 _id-n_96 _env_97 _store_98 _loc-n_99 _k_100)
               (_expand-any_88
                 _sexp_95
                 _id-n_96
                 _env_97
                 _store_98
                 _loc-n_99
                 #f
                 (lambda (_output_101)
                   (_k_100 (_make-code_60 _output_101) _store_98 _loc-n_99))
                 (lambda (_syn_101 _error-sexp_102 _store_103 _loc-n_104)
                   (_k_100 _syn_101 _store_103 _loc-n_104))
                 #f
                 #f)))
           (_expand-expr_90
             (lambda (_sexp_95 _id-n_96 _env_97 _store_98 _loc-n_99)
               (_expand-any_88
                 _sexp_95
                 _id-n_96
                 _env_97
                 _store_98
                 _loc-n_99
                 #f
                 (lambda (_x_100) _x_100)
                 #f
                 #f
                 #f)))
           (_expand-body_91
             (lambda (_sexps_95
                      _id-n_96
                      _env_97
                      _store_98
                      _loc-n_99
                      _lsd?_100
                      _ek_101
                      _sk_102
                      _dk_103
                      _bk_104)
               (letrec ((_expand-def_105
                          (lambda (_sexp_106
                                   _vds_107
                                   _sds_108
                                   _exprs_109
                                   _id-n_110
                                   _env_111
                                   _store_112
                                   _loc-n_113
                                   _k_114
                                   _dek_115)
                            (letrec ((_dk_116
                                       (lambda (_builtin_118
                                                _sexp_119
                                                _id-n_120
                                                _env_121
                                                _store_122
                                                _loc-n_123)
                                         (let ((_x_124 _ek_101))
                                           (if _x_124
                                             _x_124
                                             (let ((_x_125 (eq? _builtin_118 'define-syntax)))
                                               (if _x_125
                                                 _x_125
                                                 (_expand-error_43
                                                   "Non-syntax definition is a syntax body: "
                                                   _sexp_119)))))
                                         (if (_list2?_54 _sexp_119)
                                           (_k_114
                                             _vds_107
                                             _sds_108
                                             (cons (cadr _sexp_119) _exprs_109)
                                             _id-n_120
                                             _env_121
                                             _store_122
                                             _loc-n_123)
                                           (let ((_sid_124 (cadr _sexp_119)))
                                             (let ((_id_125 (_sid-id_51 _sid_124)))
                                               (let ((_env_126
                                                       (_extend-env_78 _env_121 _id_125 _loc-n_123)))
                                                 (letrec ((_check_127
                                                            (lambda (_def_128)
                                                              (if (eqv? _id_125 (_sid-id_51 (cadr _def_128)))
                                                                (_expand-error_43
                                                                  "Duplicate internal definitions: "
                                                                  _def_128
                                                                  " and: "
                                                                  _sexp_119)))))
                                                   (for-each _check_127 _sds_108)
                                                   (for-each _check_127 _vds_107)
                                                   (let ((_key_128 _builtin_118))
                                                     (if (if (eqv? _key_128 'define-syntax) #t #f)
                                                       (_k_114
                                                         _vds_107
                                                         (cons _sexp_119 _sds_108)
                                                         _exprs_109
                                                         _id-n_120
                                                         _env_126
                                                         _store_122
                                                         (+ _loc-n_123 1))
                                                       (if (if (eqv? _key_128 'define) #t #f)
                                                         (let ((_var_129 (_intloc->var_82 _loc-n_123 _sid_124)))
                                                           (let ((_store_130
                                                                   (_extend-store_79 _store_122 _loc-n_123 _var_129)))
                                                             (let ((_loc-n_131 (+ _loc-n_123 1)))
                                                               (_k_114
                                                                 (cons _sexp_119 _vds_107)
                                                                 _sds_108
                                                                 _exprs_109
                                                                 _id-n_120
                                                                 _env_126
                                                                 _store_130
                                                                 _loc-n_131))))
                                                         #f))))))))))
                                     (_bk_117
                                       (lambda (_sexp_118
                                                _id-n_119
                                                _env_120
                                                _store_121
                                                _loc-n_122)
                                         (let _loop_123 ((_sexps_124 (cdr _sexp_118))
                                                         (_vds_125 _vds_107)
                                                         (_sds_126 _sds_108)
                                                         (_exprs_127 _exprs_109)
                                                         (_id-n_128 _id-n_119)
                                                         (_env_129 _env_120)
                                                         (_store_130 _store_121)
                                                         (_loc-n_131 _loc-n_122)
                                                         (_dek_132 _dek_115))
                                           (if (null? _sexps_124)
                                             (_k_114
                                               _vds_125
                                               _sds_126
                                               _exprs_127
                                               _id-n_128
                                               _env_129
                                               _store_130
                                               _loc-n_131)
                                             (_expand-def_105
                                               (car _sexps_124)
                                               _vds_125
                                               _sds_126
                                               _exprs_127
                                               _id-n_128
                                               _env_129
                                               _store_130
                                               _loc-n_131
                                               (lambda (_vds_133
                                                        _sds_134
                                                        _exprs_135
                                                        _id-n_136
                                                        _env_137
                                                        _store_138
                                                        _loc-n_139)
                                                 (_loop_123
                                                   (cdr _sexps_124)
                                                   _vds_133
                                                   _sds_134
                                                   _exprs_135
                                                   _id-n_136
                                                   _env_137
                                                   _store_138
                                                   _loc-n_139
                                                   #f))
                                               (if _dek_132
                                                 (lambda (_out_133)
                                                   (letrec ((_expand-one_134
                                                              (lambda (_sexp_135)
                                                                (_expand-expr_90
                                                                  _sexp_135
                                                                  _id-n_128
                                                                  _env_129
                                                                  _store_130
                                                                  _loc-n_131))))
                                                     (let ((_rest_135
                                                             (map _expand-one_134 (cdr _sexps_124))))
                                                       (_dek_132
                                                         (_make-begin_84 (cons _out_133 _rest_135))))))
                                                 #f)))))))
                              (_expand-any_88
                                _sexp_106
                                _id-n_110
                                _env_111
                                _store_112
                                _loc-n_113
                                #f
                                _dek_115
                                #f
                                _dk_116
                                _bk_117)))))
                 (let _loop_106 ((_first_107 (car _sexps_95))
                                 (_rest_108 (cdr _sexps_95))
                                 (_vds_109 '())
                                 (_sds_110 '())
                                 (_exprs_111 '())
                                 (_id-n_112 _id-n_96)
                                 (_env_113 _env_97)
                                 (_store_114 _store_98)
                                 (_loc-n_115 _loc-n_99))
                   (letrec ((_finish-body_116
                              (lambda (_boundary-exp-output_117)
                                (_expand-syntax-bindings_87
                                  (map cdr _sds_110)
                                  _id-n_112
                                  _env_113
                                  _env_113
                                  _store_114
                                  _loc-n_115
                                  (lambda (_store_118 _loc-n_119)
                                    (letrec ((_iexpand_120
                                               (lambda (_sexp_123)
                                                 (_expand-expr_90
                                                   _sexp_123
                                                   _id-n_112
                                                   _env_113
                                                   _store_118
                                                   _loc-n_119)))
                                             (_expand-vd_121
                                               (lambda (_vd_123)
                                                 (list (_lookup2_77 (cadr _vd_123) _env_113 _store_118)
                                                       (_iexpand_120 (caddr _vd_123)))))
                                             (_make-letrec_122
                                               (lambda (_bindings_123 _expr_124)
                                                 (if (null? _bindings_123)
                                                   _expr_124
                                                   (list 'letrec _bindings_123 _expr_124)))))
                                      (if (if (null? _rest_108)
                                            (if (null? _vds_109) (null? _exprs_111) #f)
                                            #f)
                                        (_expand-any_88
                                          _first_107
                                          _id-n_112
                                          _env_113
                                          _store_118
                                          _loc-n_119
                                          _lsd?_100
                                          _ek_101
                                          _sk_102
                                          _dk_103
                                          _bk_104)
                                        (_ek_101
                                          (_make-letrec_122
                                            (map _expand-vd_121 (reverse _vds_109))
                                            (let ((_body-exprs-output_123
                                                    (if (null? _rest_108)
                                                      (list (_iexpand_120 _first_107))
                                                      (cons _boundary-exp-output_117
                                                            (map _iexpand_120 _rest_108)))))
                                              (_make-begin_84
                                                (append
                                                  (map _iexpand_120 (reverse _exprs_111))
                                                  _body-exprs-output_123))))))))))))
                     (if (null? _rest_108)
                       (_finish-body_116 #f)
                       (_expand-def_105
                         _first_107
                         _vds_109
                         _sds_110
                         _exprs_111
                         _id-n_112
                         _env_113
                         _store_114
                         _loc-n_115
                         (lambda (_vds_117
                                  _sds_118
                                  _exprs_119
                                  _id-n_120
                                  _env_121
                                  _store_122
                                  _loc-n_123)
                           (_loop_106
                             (car _rest_108)
                             (cdr _rest_108)
                             _vds_117
                             _sds_118
                             _exprs_119
                             _id-n_120
                             _env_121
                             _store_122
                             _loc-n_123))
                         (if _ek_101 _finish-body_116 #f))))))))
           (_expand-top-level-forms_92
             (lambda (_forms_95 _store_96 _loc-n_97 _k_98)
               (letrec ((_finalize_99
                          (lambda (_store_100 _loc-n_101 _acc_102)
                            (_k_98 (reverse _acc_102) _store_100 _loc-n_101))))
                 (let _expand_100 ((_sexps_101 (_wrap-vecs_57 _forms_95))
                                   (_id-n_102 0)
                                   (_env_103 _empty-env_73)
                                   (_store_104 _store_96)
                                   (_loc-n_105 _loc-n_97)
                                   (_acc_106 '())
                                   (_k_107 _finalize_99))
                   (if (null? _sexps_101)
                     (_k_107 _store_104 _loc-n_105 _acc_106)
                     (let ((_rest_108 (cdr _sexps_101)))
                       (letrec ((_ek_109
                                  (lambda (_output_112)
                                    (_expand_100
                                      _rest_108
                                      _id-n_102
                                      _env_103
                                      _store_104
                                      _loc-n_105
                                      (cons _output_112 _acc_106)
                                      _k_107)))
                                (_dk_110
                                  (lambda (_builtin_112
                                           _sexp_113
                                           _id-n*_114
                                           _env*_115
                                           _store_116
                                           _loc-n_117)
                                    (if (_list2?_54 _sexp_113)
                                      (_ek_109
                                        (_expand-expr_90
                                          (cadr _sexp_113)
                                          _id-n*_114
                                          _env*_115
                                          _store_116
                                          _loc-n_117))
                                      (let ((_tail_118 (cdr _sexp_113)))
                                        (let ((_sid_119 (car _tail_118)))
                                          (let ((_loc_120 (_lookup-sid_75 _sid_119 _env*_115)))
                                            (let ((_init_121 (cadr _tail_118)))
                                              (if (eq? _builtin_112 'define)
                                                (let ((_expr_122
                                                        (_expand-expr_90
                                                          _init_121
                                                          _id-n*_114
                                                          _env*_115
                                                          _store_116
                                                          _loc-n_117)))
                                                  (let ((_var_123 (_loc->var_83 _loc_120 _sid_119)))
                                                    (let ((_acc_124
                                                            (cons (list 'define _var_123 _expr_122) _acc_106)))
                                                      (let ((_store_125
                                                              (_substitute-in-store_80
                                                                _store_116
                                                                _loc_120
                                                                _var_123)))
                                                        (_expand_100
                                                          _rest_108
                                                          _id-n_102
                                                          _env_103
                                                          _store_125
                                                          _loc-n_117
                                                          _acc_124
                                                          _k_107)))))
                                                (_expand-val_89
                                                  _init_121
                                                  _id-n*_114
                                                  _env*_115
                                                  _store_116
                                                  _loc-n_117
                                                  (lambda (_val_122 _store_123 _loc-n_124)
                                                    (let ((_store_125
                                                            (_substitute-in-store_80
                                                              _store_123
                                                              _loc_120
                                                              _val_122)))
                                                      (_expand_100
                                                        _rest_108
                                                        _id-n_102
                                                        _env_103
                                                        _store_125
                                                        _loc-n_124
                                                        _acc_106
                                                        _k_107))))))))))))
                                (_bk_111
                                  (lambda (_sexp_112
                                           _id-n*_113
                                           _env*_114
                                           _store_115
                                           _loc-n_116)
                                    (_expand_100
                                      (cdr _sexp_112)
                                      _id-n*_113
                                      _env*_114
                                      _store_115
                                      _loc-n_116
                                      _acc_106
                                      (lambda (_store_117 _loc-n_118 _acc_119)
                                        (_expand_100
                                          _rest_108
                                          _id-n_102
                                          _env_103
                                          _store_117
                                          _loc-n_118
                                          _acc_119
                                          _k_107))))))
                         (_expand-any_88
                           (car _sexps_101)
                           _id-n_102
                           _env_103
                           _store_104
                           _loc-n_105
                           #t
                           _ek_109
                           #f
                           _dk_110
                           _bk_111))))))))
           (_compile-syntax-rules_93
             (lambda (_synrules_95 _env_96)
               (letrec ((_ellipsis-id_97
                          (if (pair? (cddr _synrules_95))
                            (if (_sid?_44 (cadr _synrules_95))
                              (_sid-id_51 (cadr _synrules_95))
                              #f)
                            #f))
                        (_ellipsis?_98
                          (lambda (_x_100)
                            (if (_sid?_44 _x_100)
                              (if _ellipsis-id_97
                                (eqv? _ellipsis-id_97 (_sid-id_51 _x_100))
                                (eq? '... (_lookup-sid_75 _x_100 _env_96)))
                              #f)))
                        (_check-lit_99
                          (lambda (_lit_100)
                            (let ((_x_101 (_sid?_44 _lit_100)))
                              (if _x_101
                                _x_101
                                (_expand-error_43
                                  "Non-id: "
                                  _lit_100
                                  " in literals list of: "
                                  _synrules_95)))
                            (if (_ellipsis?_98 _lit_100)
                              (_expand-error_43
                                "Ellipsis "
                                _lit_100
                                " in literals list of: "
                                _synrules_95)))))
                 (let ((_rest_100
                         (if _ellipsis-id_97
                           (cddr _synrules_95)
                           (cdr _synrules_95))))
                   (let ((_pat-literal-sids_101 (car _rest_100)))
                     (let ((_rules_102 (cdr _rest_100)))
                       (let ((_pat-literals_103
                               (begin
                                 (let ((_x_103 (list? _pat-literal-sids_101)))
                                   (if _x_103
                                     _x_103
                                     (_expand-error_43
                                       "Pattern literals list is not a list: "
                                       _pat-literal-sids_101)))
                                 (for-each _check-lit_99 _pat-literal-sids_101)
                                 (map _sid-id_51 _pat-literal-sids_101))))
                         (letrec ((_ellipsis-pair?_104
                                    (lambda (_x_110)
                                      (if (pair? _x_110)
                                        (_ellipsis?_98 (car _x_110))
                                        #f)))
                                  (_check-ellipses_105
                                    (lambda (_pat/tmpl_110 _in-template?_111)
                                      (letrec ((_bad-ellipsis_112
                                                 (lambda (_x_116 _reason_117)
                                                   (_expand-error_43
                                                     (string-append _reason_117 ": ")
                                                     _x_116
                                                     (if _in-template?_111
                                                       " in template: "
                                                       " in pattern: ")
                                                     _pat/tmpl_110)))
                                               (_multi-ellipsis-error_113
                                                 (lambda (_x_116)
                                                   (_bad-ellipsis_112
                                                     _x_116
                                                     "List or vector pattern with multiple ellipses")))
                                               (_ellipsis/tail-error_114
                                                 (lambda (_x_116)
                                                   (_bad-ellipsis_112
                                                     _x_116
                                                     "Improper list pattern with an ellipsis")))
                                               (_ellipsis-follows_115
                                                 (lambda (_x_116 _thing_117)
                                                   (_bad-ellipsis_112
                                                     _x_116
                                                     (string-append "Ellipsis following " _thing_117)))))
                                        (let ((_x_116
                                                (if _in-template?_111
                                                  _pat/tmpl_110
                                                  (cdr _pat/tmpl_110))))
                                          (if _in-template?_111
                                            (if (_ellipsis?_98 _x_116)
                                              (_ellipsis-follows_115 _x_116 "nothing"))
                                            (if (_ellipsis?_98 _x_116)
                                              (_ellipsis-follows_115 _pat/tmpl_110 "a '.'")
                                              (if (_ellipsis-pair?_104 _x_116)
                                                (_ellipsis-follows_115
                                                  _pat/tmpl_110
                                                  "the pattern keyword")
                                                #f)))
                                          (let _check_117 ((_x_118 _x_116))
                                            (if (pair? _x_118)
                                              (begin
                                                (if (_ellipsis?_98 (car _x_118))
                                                  (_ellipsis-follows_115 _x_118 "a '('"))
                                                (_check_117 (car _x_118))
                                                (if (_ellipsis?_98 (cdr _x_118))
                                                  (_ellipsis-follows_115 _x_118 "a '.'"))
                                                (if (_ellipsis-pair?_104 (cdr _x_118))
                                                  (if (_ellipsis?_98 (cddr _x_118))
                                                    (_ellipsis-follows_115 (cdr _x_118) "a '.'")
                                                    (if (_ellipsis-pair?_104 (cddr _x_118))
                                                      (_ellipsis-follows_115
                                                        (cdr _x_118)
                                                        "an ellipsis")
                                                      (if _in-template?_111
                                                        (_check_117 (cddr _x_118))
                                                        (begin
                                                          (let ((_x_119 (list? _x_118)))
                                                            (if _x_119
                                                              _x_119
                                                              (_ellipsis/tail-error_114 _x_118)))
                                                          (for-each
                                                            (lambda (_y_119)
                                                              (if (_ellipsis?_98 _y_119)
                                                                (_multi-ellipsis-error_113 _x_118))
                                                              (_check_117 _y_119))
                                                            (cddr _x_118))))))
                                                  (_check_117 (cdr _x_118))))
                                              (if (_svector?_46 _x_118)
                                                (let ((_elts_119 (_svector->list_47 _x_118)))
                                                  (if (_ellipsis-pair?_104 _elts_119)
                                                    (_ellipsis-follows_115 _x_118 "a '#('")
                                                    (_check_117 _elts_119)))
                                                #f)))))))
                                  (_make-pat-env_106
                                    (lambda (_pat_110)
                                      (let _collect_111 ((_x_112 (cdr _pat_110))
                                                         (_depth_113 0)
                                                         (_l_114 '()))
                                        (if (_sid?_44 _x_112)
                                          (let ((_id_115 (_sid-id_51 _x_112)))
                                            (if (memv _id_115 _pat-literals_103)
                                              _l_114
                                              (if (assv _id_115 _l_114)
                                                (_expand-error_43
                                                  "Duplicate pattern var: "
                                                  _x_112
                                                  " in pattern: "
                                                  _pat_110)
                                                (_acons_72 _id_115 _depth_113 _l_114))))
                                          (if (vector? _x_112)
                                            (_collect_111
                                              (_svector->list_47 _x_112)
                                              _depth_113
                                              _l_114)
                                            (if (pair? _x_112)
                                              (if (_ellipsis-pair?_104 (cdr _x_112))
                                                (_collect_111
                                                  (car _x_112)
                                                  (+ 1 _depth_113)
                                                  (_collect_111 (cddr _x_112) _depth_113 _l_114))
                                                (_collect_111
                                                  (car _x_112)
                                                  _depth_113
                                                  (_collect_111 (cdr _x_112) _depth_113 _l_114)))
                                              _l_114))))))
                                  (_check-var-depths_107
                                    (lambda (_tmpl_110 _pat-env_111)
                                      (letrec ((_depth-error_112
                                                 (lambda (_x_114)
                                                   (_expand-error_43
                                                     "Pattern var used at bad depth: "
                                                     _x_114
                                                     " in template: "
                                                     _tmpl_110)))
                                               (_close-error_113
                                                 (lambda (_x_114)
                                                   (_expand-error_43
                                                     "Template ellipsis closes no variables: "
                                                     _x_114
                                                     " in template: "
                                                     _tmpl_110))))
                                        (let _collect_114 ((_x_115 _tmpl_110) (_depth_116 0))
                                          (if (_sid?_44 _x_115)
                                            (let ((_p_117 (assv (_sid-id_51 _x_115) _pat-env_111)))
                                              (if _p_117
                                                (let ((_pat-depth_118 (cdr _p_117)))
                                                  (let ((_same-depth?_119 (= _depth_116 _pat-depth_118)))
                                                    (if (if (positive? _pat-depth_118)
                                                          (not _same-depth?_119)
                                                          #f)
                                                      (_depth-error_112 _x_115))
                                                    _same-depth?_119))
                                                #f))
                                            (if (vector? _x_115)
                                              (_collect_114
                                                (_svector->list_47 _x_115)
                                                _depth_116)
                                              (if (pair? _x_115)
                                                (let ((_ellip?_117 (_ellipsis-pair?_104 (cdr _x_115))))
                                                  (let ((_car-closed?_118
                                                          (_collect_114
                                                            (car _x_115)
                                                            (if _ellip?_117 (+ 1 _depth_116) _depth_116))))
                                                    (let ((_cdr-closed?_119
                                                            (_collect_114
                                                              ((if _ellip?_117 cddr cdr) _x_115)
                                                              _depth_116)))
                                                      (if _ellip?_117
                                                        (if (not _car-closed?_118)
                                                          (_close-error_113 _x_115)
                                                          #f)
                                                        #f)
                                                      (let ((_x_120 _car-closed?_118))
                                                        (if _x_120 _x_120 _cdr-closed?_119)))))
                                                #f)))))))
                                  (_check-rule_108
                                    (lambda (_rule_110)
                                      (let ((_x_111 (_list2?_54 _rule_110)))
                                        (if _x_111
                                          _x_111
                                          (_expand-error_43
                                            "Malformed syntax rule: "
                                            _rule_110)))
                                      (let ((_pat_111 (car _rule_110))
                                            (_tmpl_112 (cadr _rule_110)))
                                        (let ((_x_113
                                                (if (pair? _pat_111)
                                                  (_sid?_44 (car _pat_111))
                                                  #f)))
                                          (if _x_113
                                            _x_113
                                            (_expand-error_43 "Malformed pattern: " _pat_111)))
                                        (_check-ellipses_105 _pat_111 #f)
                                        (_check-ellipses_105 _tmpl_112 #t)
                                        (let ((_pat-env_113 (_make-pat-env_106 _pat_111)))
                                          (_check-var-depths_107 _tmpl_112 _pat-env_113)
                                          (let _collect_114 ((_x_115 _tmpl_112) (_lits_116 '()))
                                            (if (_ellipsis?_98 _x_115)
                                              _lits_116
                                              (if (_sid?_44 _x_115)
                                                (if (assv (_sid-id_51 _x_115) _pat-env_113)
                                                  _lits_116
                                                  (cons (_sid-id_51 _x_115) _lits_116))
                                                (if (vector? _x_115)
                                                  (_collect_114
                                                    (_svector->list_47 _x_115)
                                                    _lits_116)
                                                  (if (pair? _x_115)
                                                    (_collect_114
                                                      (car _x_115)
                                                      (_collect_114 (cdr _x_115) _lits_116))
                                                    _lits_116)))))))))
                                  (_reduce-env_109
                                    (lambda (_lits_110)
                                      (letrec ((_list-dots-ids_111
                                                 (lambda (_x_112 _ids_113)
                                                   (if (_sid?_44 _x_112)
                                                     (if (eq? '... (_sid-location_52 _x_112))
                                                       (cons (_sid-id_51 _x_112) _ids_113)
                                                       _ids_113)
                                                     (if (vector? _x_112)
                                                       (_list-dots-ids_111
                                                         (_svector->list_47 _x_112)
                                                         _ids_113)
                                                       (if (pair? _x_112)
                                                         (_list-dots-ids_111
                                                           (car _x_112)
                                                           (_list-dots-ids_111 (cdr _x_112) _ids_113))
                                                         _ids_113))))))
                                        (let _loop_112 ((_ids_113
                                                          (if _ellipsis-id_97
                                                            _lits_110
                                                            (_list-dots-ids_111 _rules_102 _lits_110)))
                                                        (_reduced-env_114 _empty-env_73))
                                          (if (null? _ids_113)
                                            _reduced-env_114
                                            (_loop_112
                                              (cdr _ids_113)
                                              (let ((_id_115 (car _ids_113)))
                                                (let ((_tmp_116
                                                        (if (not (assv _id_115 _reduced-env_114))
                                                          (assv _id_115 _env_96)
                                                          #f)))
                                                  (if _tmp_116
                                                    (let ((_binding_117 _tmp_116))
                                                      (cons _binding_117 _reduced-env_114))
                                                    _reduced-env_114))))))))))
                           (let ((_lits_110
                                   (apply append
                                          _pat-literals_103
                                          (map _check-rule_108 _rules_102))))
                             (let ((_env_111 (_reduce-env_109 _lits_110)))
                               (_make-transformer_62 _synrules_95 _env_111)))))))))))
           (_apply-transformer_94
             (lambda (_transformer_95 _sexp_96 _id-n_97 _env_98 _k_99)
               (let ((_synrules_100
                       (_transformer-synrules_70 _transformer_95)))
                 (let ((_mac-env_101 (_transformer-env_71 _transformer_95)))
                   (let ((_ellipsis-id_102
                           (if (_sid?_44 (cadr _synrules_100))
                             (_sid-id_51 (cadr _synrules_100))
                             #f)))
                     (let ((_rest_103
                             (if _ellipsis-id_102
                               (cddr _synrules_100)
                               (cdr _synrules_100))))
                       (let ((_pat-literals_104
                               (map _sid-id_51 (car _rest_103))))
                         (let ((_rules_105 (cdr _rest_103)))
                           (letrec ((_pat-literal?_106
                                      (lambda (_id_115)
                                        (memv _id_115 _pat-literals_104)))
                                    (_not-pat-literal?_107
                                      (lambda (_id_115)
                                        (not (_pat-literal?_106 _id_115))))
                                    (_ellipsis-pair?_108
                                      (lambda (_x_115)
                                        (if (pair? _x_115)
                                          (_ellipsis?_109 (car _x_115))
                                          #f)))
                                    (_ellipsis?_109
                                      (lambda (_x_115)
                                        (if (_sid?_44 _x_115)
                                          (if _ellipsis-id_102
                                            (eqv? _ellipsis-id_102 (_sid-id_51 _x_115))
                                            (eq? '... (_lookup-sid_75 _x_115 _mac-env_101)))
                                          #f)))
                                    (_list-ids_110
                                      (lambda (_x_115 _include-scalars_116 _pred?_117)
                                        (let _collect_118 ((_x_119 _x_115)
                                                           (_inc_120 _include-scalars_116)
                                                           (_l_121 '()))
                                          (if (_sid?_44 _x_119)
                                            (let ((_id_122 (_sid-id_51 _x_119)))
                                              (if (if _inc_120 (_pred?_117 _id_122) #f)
                                                (cons _id_122 _l_121)
                                                _l_121))
                                            (if (vector? _x_119)
                                              (_collect_118
                                                (_svector->list_47 _x_119)
                                                _inc_120
                                                _l_121)
                                              (if (pair? _x_119)
                                                (if (_ellipsis-pair?_108 (cdr _x_119))
                                                  (_collect_118
                                                    (car _x_119)
                                                    #t
                                                    (_collect_118 (cddr _x_119) _inc_120 _l_121))
                                                  (_collect_118
                                                    (car _x_119)
                                                    _inc_120
                                                    (_collect_118 (cdr _x_119) _inc_120 _l_121)))
                                                _l_121))))))
                                    (_matches?_111
                                      (lambda (_pat_115)
                                        (let _match_116 ((_pat_117 _pat_115) (_sexp_118 (cdr _sexp_96)))
                                          (if (_sid?_44 _pat_117)
                                            (let ((_x_119
                                                    (not (_pat-literal?_106 (_sid-id_51 _pat_117)))))
                                              (if _x_119
                                                _x_119
                                                (if (_sid?_44 _sexp_118)
                                                  (eqv? (_lookup-sid_75 _pat_117 _mac-env_101)
                                                        (_lookup-sid_75 _sexp_118 _env_98))
                                                  #f)))
                                            (if (_svector?_46 _pat_117)
                                              (if (_svector?_46 _sexp_118)
                                                (_match_116
                                                  (_svector->list_47 _pat_117)
                                                  (_svector->list_47 _sexp_118))
                                                #f)
                                              (if (not (pair? _pat_117))
                                                (equal? _pat_117 _sexp_118)
                                                (if (_ellipsis-pair?_108 (cdr _pat_117))
                                                  (let _skip_119 ((_p_120 (cddr _pat_117)) (_s_121 _sexp_118))
                                                    (if (pair? _p_120)
                                                      (if (pair? _s_121)
                                                        (_skip_119 (cdr _p_120) (cdr _s_121))
                                                        #f)
                                                      (let _match-cars_122 ((_sexp_123 _sexp_118) (_s_124 _s_121))
                                                        (if (pair? _s_124)
                                                          (if (_match_116 (car _pat_117) (car _sexp_123))
                                                            (_match-cars_122 (cdr _sexp_123) (cdr _s_124))
                                                            #f)
                                                          (_match_116 (cddr _pat_117) _sexp_123)))))
                                                  (if (pair? _sexp_118)
                                                    (if (_match_116 (car _pat_117) (car _sexp_118))
                                                      (_match_116 (cdr _pat_117) (cdr _sexp_118))
                                                      #f)
                                                    #f))))))))
                                    (_make-bindings_112
                                      (lambda (_pat_115)
                                        (let _collect_116 ((_pat_117 _pat_115)
                                                           (_sexp_118 (cdr _sexp_96))
                                                           (_bindings_119 '()))
                                          (if (if (_sid?_44 _pat_117)
                                                (not (_pat-literal?_106 (_sid-id_51 _pat_117)))
                                                #f)
                                            (_acons_72
                                              (_sid-id_51 _pat_117)
                                              _sexp_118
                                              _bindings_119)
                                            (if (_svector?_46 _pat_117)
                                              (_collect_116
                                                (_svector->list_47 _pat_117)
                                                (_svector->list_47 _sexp_118)
                                                _bindings_119)
                                              (if (not (pair? _pat_117))
                                                _bindings_119
                                                (if (_ellipsis-pair?_108 (cdr _pat_117))
                                                  (let ((_tail-len_120 (length (cddr _pat_117))))
                                                    (let ((_tail_121
                                                            (list-tail
                                                              _sexp_118
                                                              (- (length _sexp_118) _tail-len_120))))
                                                      (let ((_matches_122
                                                              (reverse
                                                                (list-tail (reverse _sexp_118) _tail-len_120))))
                                                        (let ((_vars_123
                                                                (_list-ids_110
                                                                  (car _pat_117)
                                                                  #t
                                                                  _not-pat-literal?_107)))
                                                          (letrec ((_collect1_124
                                                                     (lambda (_match_125)
                                                                       (map cdr
                                                                            (_collect_116 (car _pat_117) _match_125 '())))))
                                                            (append
                                                              (apply map
                                                                     list
                                                                     _vars_123
                                                                     (map _collect1_124 _matches_122))
                                                              (_collect_116
                                                                (cddr _pat_117)
                                                                _tail_121
                                                                _bindings_119)))))))
                                                  (_collect_116
                                                    (car _pat_117)
                                                    (car _sexp_118)
                                                    (_collect_116
                                                      (cdr _pat_117)
                                                      (cdr _sexp_118)
                                                      _bindings_119)))))))))
                                    (_remove-dups_113
                                      (lambda (_l_115)
                                        (let _loop_116 ((_l_117 _l_115) (_result_118 '()))
                                          (if (null? _l_117)
                                            _result_118
                                            (_loop_116
                                              (cdr _l_117)
                                              (let ((_elt_119 (car _l_117)))
                                                (if (memv _elt_119 _result_118)
                                                  _result_118
                                                  (cons _elt_119 _result_118))))))))
                                    (_expand-template_114
                                      (lambda (_pat_115 _tmpl_116 _top-bindings_117)
                                        (letrec ((_tmpl-literals_118
                                                   (_remove-dups_113
                                                     (_list-ids_110
                                                       _tmpl_116
                                                       #t
                                                       (lambda (_id_122)
                                                         (not (assv _id_122 _top-bindings_117))))))
                                                 (_ellipsis-vars_119
                                                   (_list-ids_110 _pat_115 #f _not-pat-literal?_107))
                                                 (_list-ellipsis-vars_120
                                                   (lambda (_subtmpl_122)
                                                     (_list-ids_110
                                                       _subtmpl_122
                                                       #t
                                                       (lambda (_id_123)
                                                         (memv _id_123 _ellipsis-vars_119)))))
                                                 (_expand_121
                                                   (lambda (_tmpl_122 _bindings_123)
                                                     (let _expand-part_124 ((_tmpl_125 _tmpl_122))
                                                       (if (_sid?_44 _tmpl_125)
                                                         (let ((_id_126 (_sid-id_51 _tmpl_125)))
                                                           (let ((_tmp_127 (assv _id_126 _bindings_123)))
                                                             (if _tmp_127
                                                               (cdr _tmp_127)
                                                               (let ((_tmp_128 (assv _id_126 _top-bindings_117)))
                                                                 (if _tmp_128
                                                                   (cdr _tmp_128)
                                                                   (let ((_index_129
                                                                           (+ -1 (length (memv _id_126 _tmpl-literals_118))))
                                                                         (_location_130
                                                                           (_lookup-sid_75 _tmpl_125 _mac-env_101)))
                                                                     (_make-sid_49
                                                                       (_sid-name_50 _tmpl_125)
                                                                       (+ _id-n_97 _index_129)
                                                                       _location_130)))))))
                                                         (if (vector? _tmpl_125)
                                                           (_list->svector_48
                                                             (_expand-part_124 (_svector->list_47 _tmpl_125)))
                                                           (if (pair? _tmpl_125)
                                                             (if (_ellipsis-pair?_108 (cdr _tmpl_125))
                                                               (let ((_vars-to-iterate_126
                                                                       (_list-ellipsis-vars_120 (car _tmpl_125))))
                                                                 (letrec ((_lookup_127
                                                                            (lambda (_var_129)
                                                                              (cdr (assv _var_129 _bindings_123))))
                                                                          (_expand-using-vals_128
                                                                            (lambda _vals_129
                                                                              (_expand_121
                                                                                (car _tmpl_125)
                                                                                (map cons _vars-to-iterate_126 _vals_129)))))
                                                                   (let ((_val-lists_129
                                                                           (map _lookup_127 _vars-to-iterate_126)))
                                                                     (if (let ((_x_130 (null? (cdr _val-lists_129))))
                                                                           (if _x_130
                                                                             _x_130
                                                                             (apply = (map length _val-lists_129))))
                                                                       (append
                                                                         (apply map _expand-using-vals_128 _val-lists_129)
                                                                         (_expand-part_124 (cddr _tmpl_125)))
                                                                       (_expand-error_43
                                                                         "Unequal sequence lengths for pattern vars: "
                                                                         _vars-to-iterate_126
                                                                         " in macro call: "
                                                                         _sexp_96)))))
                                                               (cons (_expand-part_124 (car _tmpl_125))
                                                                     (_expand-part_124 (cdr _tmpl_125))))
                                                             _tmpl_125)))))))
                                          (_k_99 (_expand_121 _tmpl_116 _top-bindings_117)
                                                 (+ _id-n_97 (length _tmpl-literals_118)))))))
                             (let _loop_115 ((_rules_116 _rules_105))
                               (if (null? _rules_116)
                                 (_expand-error_43
                                   "No matching rule for macro use: "
                                   _sexp_96)
                                 (let ((_rule_117 (car _rules_116)))
                                   (let ((_pat_118 (cdar _rule_117)))
                                     (let ((_tmpl_119 (cadr _rule_117)))
                                       (if (_matches?_111 _pat_118)
                                         (_expand-template_114
                                           _pat_118
                                           _tmpl_119
                                           (_make-bindings_112 _pat_118))
                                         (_loop_115 (cdr _rules_116))))))))))))))))))
    (letrec ((_builtins-store_95
               (let _loop_97 ((_bs_98
                                '(begin
                                   define
                                   define-syntax
                                   if
                                   lambda
                                   quote
                                   set!
                                   delay
                                   let-syntax
                                   syntax-rules))
                              (_store_99 _empty-store_74))
                 (if (null? _bs_98)
                   _store_99
                   (_loop_97
                     (cdr _bs_98)
                     (_extend-store_79
                       _store_99
                       (car _bs_98)
                       (_make-builtin_61 (car _bs_98)))))))
             (_null-prog_96
               '((define-syntax
                   letrec-syntax
                   (let-syntax
                     ((let-syntax let-syntax) (define-syntax define-syntax))
                     (syntax-rules
                       ()
                       ((_ ((kw init) ...) . body)
                        (let-syntax
                          ()
                          (define-syntax kw init)
                          ...
                          (let-syntax () . body))))))
                 (let-syntax
                   ()
                   (define-syntax
                     multi-define
                     (syntax-rules
                       ()
                       ((_ definer (id ...) (init ...))
                        (begin (definer id init) ...))))
                   (define-syntax dummy (syntax-rules ()))
                   (define-syntax
                     define-protected-macros
                     (syntax-rules
                       (define-syntax)
                       ((_ let/letrec-syntax
                           (saved-kw ...)
                           (saved-var ...)
                           (define-syntax kw syntax)
                           ...)
                        ((let-syntax
                           ((saved-kw saved-kw) ... (saved-var dummy) ...)
                           (let/letrec-syntax
                             ((kw syntax) ...)
                             (syntax-rules
                               ()
                               ((_ top-level-kws top-level-vars)
                                (begin
                                  (multi-define
                                    define
                                    (saved-var ...)
                                    top-level-vars)
                                  (multi-define
                                    define-syntax
                                    top-level-kws
                                    (kw ...)))))))
                         (kw ...)
                         (saved-var ...)))))
                   (begin
                     (define-protected-macros
                       let-syntax
                       (lambda define let-syntax)
                       ()
                       (define-syntax
                         lambda
                         (syntax-rules
                           ()
                           ((lambda args . body)
                            (lambda args (let-syntax () . body)))))
                       (define-syntax
                         define
                         (syntax-rules
                           ()
                           ((_ expr) (define expr))
                           ((_ (var . args) . body)
                            (define var (lambda args (let-syntax () . body))))
                           ((_ var init) (define var init))))
                       (define-syntax
                         letrec
                         (syntax-rules
                           ()
                           ((_ ((var init) ...) . body)
                            (let () (define var init) ... (let () . body))))))
                     (define-protected-macros
                       letrec-syntax
                       (if lambda quote begin define letrec)
                       ()
                       (define-syntax
                         let
                         (syntax-rules
                           ()
                           ((_ ((var init) ...) . body)
                            ((lambda (var ...) . body) init ...))
                           ((_ name ((var init) ...) . body)
                            ((letrec ((name (lambda (var ...) . body))) name)
                             init
                             ...))))
                       (define-syntax
                         let*
                         (syntax-rules
                           ()
                           ((_ () . body) (let () . body))
                           ((let* ((var init) . bindings) . body)
                            (let ((var init)) (let* bindings . body)))))
                       (define-syntax
                         do
                         (let-syntax
                           ((do-step (syntax-rules () ((_ x) x) ((_ x y) y))))
                           (syntax-rules
                             ()
                             ((_ ((var init step ...) ...)
                                 (test expr ...)
                                 command
                                 ...)
                              (let loop ((var init) ...)
                                (if test
                                  (begin #f expr ...)
                                  (let ()
                                    command
                                    ...
                                    (loop (do-step var step ...) ...))))))))
                       (define-syntax
                         case
                         (letrec-syntax
                           ((compare
                              (syntax-rules
                                ()
                                ((_ key ()) #f)
                                ((_ key (datum . data))
                                 (if (eqv? key 'datum) #t (compare key data)))))
                            (case (syntax-rules
                                    (else)
                                    ((case key) #f)
                                    ((case key (else result1 . results))
                                     (begin result1 . results))
                                    ((case key
                                       ((datum ...) result1 . results)
                                       .
                                       clauses)
                                     (if (compare key (datum ...))
                                       (begin result1 . results)
                                       (case key . clauses))))))
                           (syntax-rules
                             ()
                             ((_ expr clause1 clause ...)
                              (let ((key expr)) (case key clause1 clause ...))))))
                       (define-syntax
                         cond
                         (syntax-rules
                           (else =>)
                           ((_) #f)
                           ((_ (else . exps)) (let () (begin . exps)))
                           ((_ (x) . rest) (or x (cond . rest)))
                           ((_ (x => proc) . rest)
                            (let ((tmp x)) (cond (tmp (proc tmp)) . rest)))
                           ((_ (generator guard => receiver) . rest)
                            (let ((tmp generator))
                              (cond ((guard tmp) (receiver tmp)) . rest)))
                           ((_ (x . exps) . rest)
                            (if x (begin . exps) (cond . rest)))))
                       (define-syntax
                         and
                         (syntax-rules
                           ()
                           ((_) #t)
                           ((_ test) (let () test))
                           ((_ test . tests) (if test (and . tests) #f))))
                       (define-syntax
                         or
                         (syntax-rules
                           ()
                           ((_) #f)
                           ((_ test) (let () test))
                           ((_ test . tests)
                            (let ((x test)) (if x x (or . tests))))))
                       (define-syntax
                         delay
                         (syntax-rules
                           ()
                           ((_ expr) (%make-promise (lambda () expr))))))
                     (define-protected-macros
                       let-syntax
                       (lambda quote let)
                       ()
                       (define-syntax
                         quasiquote
                         (let-syntax
                           ((tail-preserving-syntax-rules
                              (syntax-rules
                                ()
                                ((_ literals
                                    ((subpattern ...) (subtemplate ...))
                                    ...)
                                 (syntax-rules
                                   literals
                                   ((subpattern ... . tail)
                                    (subtemplate ... . tail))
                                   ...)))))
                           (define-syntax
                             qq
                             (tail-preserving-syntax-rules
                               (unquote unquote-splicing quasiquote)
                               ((_ ,x ()) (do-next x))
                               ((_ (,@x . y) ()) (qq y () make-splice x))
                               ((_ `x depth)
                                (qq x (depth) make-list 'quasiquote))
                               ((_ ,x (depth)) (qq x depth make-list 'unquote))
                               ((_ (,x . y) (depth))
                                (qq-nested-unquote (,x . y) (depth)))
                               ((_ (,@x . y) (depth))
                                (qq-nested-unquote (,@x . y) (depth)))
                               ((_ ,@x depth) (unquote-splicing-error ,@x))
                               ((_ (x . y) depth)
                                (qq x depth qq-cdr y depth make-pair))
                               ((_ #(x y ...) depth)
                                (qq (x)
                                    depth
                                    qq-cdr
                                    #(y ...)
                                    depth
                                    make-vector-splice))
                               ((_ x depth) (do-next 'x))))
                           (define-syntax
                             do-next
                             (syntax-rules
                               ()
                               ((_ expr original-template) expr)
                               ((_ expr next-macro . tail)
                                (next-macro expr . tail))))
                           (define-syntax
                             unquote-splicing-error
                             (syntax-rules
                               ()
                               ((_ ,@x stack ... original-template)
                                (unquote-splicing-error
                                  (,@x in original-template)))))
                           (define-syntax
                             qq-cdr
                             (tail-preserving-syntax-rules
                               ()
                               ((_ car cdr depth combiner)
                                (qq cdr depth combiner car))))
                           (define-syntax
                             qq-nested-unquote
                             (tail-preserving-syntax-rules
                               ()
                               ((_ ((sym x) . y) (depth))
                                (qq (x)
                                    depth
                                    make-map
                                    sym
                                    qq-cdr
                                    y
                                    (depth)
                                    make-splice))))
                           (define-syntax
                             make-map
                             (tail-preserving-syntax-rules
                               (quote list map lambda)
                               ((_ '(x) sym) (do-next '((sym x))))
                               ((_ (list x) sym) (do-next (list (list 'sym x))))
                               ((_ (map (lambda (x) y) z) sym)
                                (do-next (map (lambda (x) (list 'sym y)) z)))
                               ((_ expr sym)
                                (do-next (map (lambda (x) (list 'sym x)) expr)))))
                           (define-syntax
                             make-pair
                             (tail-preserving-syntax-rules
                               'list
                               ((_ 'y 'x) (do-next '(x . y)))
                               ((_ '() x) (do-next (list x)))
                               ((_ (list . elts) x) (do-next (list x . elts)))
                               ((_ y x) (do-next (cons x y)))))
                           (define-syntax
                             make-list
                             (tail-preserving-syntax-rules
                               (quote)
                               ((_ y x) (make-pair '() y make-pair x))))
                           (define-syntax
                             make-splice
                             (tail-preserving-syntax-rules
                               ()
                               ((_ '() x) (do-next x))
                               ((_ y x) (do-next (append x y)))))
                           (define-syntax
                             make-vector-splice
                             (tail-preserving-syntax-rules
                               (quote list vector list->vector)
                               ((_ '#(y ...) '(x)) (do-next '#(x y ...)))
                               ((_ '#(y ...) (list x))
                                (do-next (vector x 'y ...)))
                               ((_ '#() x) (do-next (list->vector x)))
                               ((_ '#(y ...) x)
                                (do-next (list->vector (append x '(y ...)))))
                               ((_ y '(x)) (make-vector-splice y (list 'x)))
                               ((_ (vector y ...) (list x))
                                (do-next (vector x y ...)))
                               ((_ (vector y ...) x)
                                (do-next (list->vector (append x (list y ...)))))
                               ((_ (list->vector y) (list x))
                                (do-next (list->vector (cons x y))))
                               ((_ (list->vector y) x)
                                (do-next (list->vector (append x y))))))
                           (syntax-rules
                             ()
                             ((_ template) (let () (qq template () template))))))))))))
      (let ((_null-stuff_97
              (_expand-top-level-forms_92
                _null-prog_96
                _builtins-store_95
                0
                list)))
        (let ((_null-store_98 (cadr _null-stuff_97)))
          (let ((_null-loc-n_99 (caddr _null-stuff_97)))
            (letrec ((_null-mstore_100
                       (lambda () (cons _null-store_98 _null-loc-n_99)))
                     (_expand-top-level-forms!_101
                       (lambda (_forms_102 _mstore_103 _mutable?_104)
                         (_expand-top-level-forms_92
                           _forms_102
                           (car _mstore_103)
                           (cdr _mstore_103)
                           (lambda (_outputs_105 _store_106 _loc-n_107)
                             (if _mutable?_104
                               (begin
                                 (set-car! _mstore_103 _store_106)
                                 (set-cdr! _mstore_103 _loc-n_107)))
                             _outputs_105)))))
              (set! %null-mstore _null-mstore_100)
              (set! %%expand
                (let ((_store_102 (_null-mstore_100)))
                  (lambda (_form_103)
                    (cons 'begin
                          (_expand-top-level-forms!_101
                            (list _form_103)
                            _store_102
                            #t)))))
              (set! %expand
                (lambda (_form_102 . _env_103)
                  (let ((_env_104
                          (if (pair? _env_103)
                            (car _env_103)
                            %interaction-environment)))
                    (let ((_data_105 (%environment-data _env_104)))
                      (let ((_forms_106
                              (_expand-top-level-forms!_101
                                (list _form_102)
                                (vector-ref _data_105 2)
                                (vector-ref _data_105 3))))
                        (cons 'begin _forms_106))))))
              (set! %expand-file
                (lambda (_filename_102 . _env_103)
                  (let ((_env_104
                          (if (pair? _env_103)
                            (car _env_103)
                            %interaction-environment)))
                    (cons 'begin
                          (map (lambda (_x_105) (%expand _x_105 _env_104))
                               (read-forms _filename_102 %read))))))))))))
  (define make-promise-object #f)
  (define %promise? #f)
  (define %promise-thunk #f)
  (call-with-values
    (lambda () (make-disjoint-type 'promise))
    (lambda (_tmp_43 _tmp_44 _tmp_45)
      (set! make-promise-object _tmp_43)
      (set! %promise? _tmp_44)
      (set! %promise-thunk _tmp_45)))
  (define %make-promise
    (lambda (_proc_43)
      (let ((_result-ready_44 #f) (_results_45 #f))
        (make-promise-object
          (lambda ()
            (if _result-ready_44
              (apply values _results_45)
              (call-with-values
                _proc_43
                (lambda _xs_46
                  (if _result-ready_44
                    (apply values _results_45)
                    (begin
                      (set! _result-ready_44 #t)
                      (set! _results_45 _xs_46)
                      (apply values _results_45)))))))))))
  (define %force
    (lambda (_x_43) (if (%promise? _x_43) ((%promise-thunk _x_43)) _x_43)))
  (define %case-sensitive
    (let ((_f_43 #t))
      (lambda _arg_44 (if (pair? _arg_44) (set! _f_43 (car _arg_44)) _f_43))))
  (define %read
    (let ((_read-char_43 read-char)
          (_reverse_44 reverse)
          (_peek-char_45 peek-char)
          (_list->vector_46 list->vector)
          (_list->string_47 list->string)
          (_case-sensitive_48 %case-sensitive)
          (_string->number_49 string->number))
      (lambda _port_50
        (let ((_port_51 (if (pair? _port_50) (car _port_50) %input-port))
              (_cs_52 (_case-sensitive_48))
              (_eol_53 (lambda (_c_51) (error "unexpected delimiter" _c_51))))
          (letrec ((_parse-token_54
                     (lambda (_t_63)
                       (let ((_x_64 (_string->number_49 _t_63)))
                         (if _x_64 _x_64 (string->symbol _t_63)))))
                   (_read1_55
                     (lambda ()
                       (let ((_c_63 (_read-char_43 _port_51)))
                         (if (eof-object? _c_63)
                           _c_63
                           (let ((_key_64 _c_63))
                             (if (if (eqv? _key_64 #\#) #t #f)
                               (_read-sharp_58)
                               (if (if (eqv? _key_64 #\() #t #f)
                                 (_read-list_59 #\))
                                 (if (if (eqv? _key_64 #\[) #t #f)
                                   (_read-list_59 #\])
                                   (if (if (eqv? _key_64 #\{) #t #f)
                                     (_read-list_59 #\})
                                     (if (if (eqv? _key_64 #\,) #t #f)
                                       (if (eqv? (_peek-char_45 _port_51) #\@)
                                         (begin
                                           (_read-char_43 _port_51)
                                           (list 'unquote-splicing (_read1_55)))
                                         (list 'unquote (_read1_55)))
                                       (if (if (eqv? _key_64 #\`) #t #f)
                                         (list 'quasiquote (_read1_55))
                                         (if (if (eqv? _key_64 #\') #t #f)
                                           (list 'quote (_read1_55))
                                           (if (if (eqv? _key_64 #\;) #t #f)
                                             (begin (_skip-line_56) (_read1_55))
                                             (if (if (eqv? _key_64 #\") #t #f)
                                               (_read-string_60)
                                               (if (if (eqv? _key_64 #\))
                                                     #t
                                                     (if (eqv? _key_64 #\])
                                                       #t
                                                       (if (eqv? _key_64 #\}) #t #f)))
                                                 (_eol_53 _c_63)
                                                 (if (char-whitespace? _c_63)
                                                   (_read1_55)
                                                   (_parse-token_54
                                                     (_read-token_62 (list (_docase_61 _c_63)) _cs_52))))))))))))))))))
                   (_skip-line_56
                     (lambda ()
                       (let ((_c_63 (_read-char_43 _port_51)))
                         (if (not (let ((_x_64 (eof-object? _c_63)))
                                    (if _x_64 _x_64 (char=? #\newline _c_63))))
                           (_skip-line_56)))))
                   (_skip-whitespace_57
                     (lambda ()
                       (let ((_c_63 (_peek-char_45 _port_51)))
                         (if (eof-object? _c_63)
                           _c_63
                           (if (char-whitespace? _c_63)
                             (begin
                               (_read-char_43 _port_51)
                               (_skip-whitespace_57))
                             _c_63)))))
                   (_read-sharp_58
                     (lambda ()
                       (let ((_c_63 (_read-char_43 _port_51)))
                         (if (eof-object? _c_63)
                           (error "unexpected EOF after `#'")
                           (let ((_key_64 _c_63))
                             (if (if (eqv? _key_64 #\f)
                                   #t
                                   (if (eqv? _key_64 #\F) #t #f))
                               #f
                               (if (if (eqv? _key_64 #\t)
                                     #t
                                     (if (eqv? _key_64 #\T) #t #f))
                                 #t
                                 (if (if (eqv? _key_64 #\x)
                                       #t
                                       (if (eqv? _key_64 #\X) #t #f))
                                   (_string->number_49 (_read-token_62 '() #f) 16)
                                   (if (if (eqv? _key_64 #\o)
                                         #t
                                         (if (eqv? _key_64 #\O) #t #f))
                                     (_string->number_49 (_read-token_62 '() #f) 8)
                                     (if (if (eqv? _key_64 #\b)
                                           #t
                                           (if (eqv? _key_64 #\B) #t #f))
                                       (_string->number_49 (_read-token_62 '() #f) 2)
                                       (if (if (eqv? _key_64 #\i)
                                             #t
                                             (if (eqv? _key_64 #\I) #t #f))
                                         (let ((_tok_65 (_read-token_62 '() #f)))
                                           (let ((_n_66 (_string->number_49 _tok_65)))
                                             (if (not (number? _n_66))
                                               (error "invalid number syntax" _tok_65)
                                               (if (inexact? _n_66)
                                                 _n_66
                                                 (exact->inexact _n_66)))))
                                         (if (if (eqv? _key_64 #\e)
                                               #t
                                               (if (eqv? _key_64 #\E) #t #f))
                                           (let ((_tok_65 (_read-token_62 '() #f)))
                                             (let ((_n_66 (_string->number_49 _tok_65)))
                                               (if (not (number? _n_66))
                                                 (error "invalid number syntax" _tok_65)
                                                 (if (exact? _n_66) _n_66 (inexact->exact _n_66)))))
                                           (if (if (eqv? _key_64 #\() #t #f)
                                             (_list->vector_46 (_read-list_59 #\)))
                                             (if (if (eqv? _key_64 #\;) #t #f)
                                               (begin (_read1_55) (_read1_55))
                                               (if (if (eqv? _key_64 #\%) #t #f)
                                                 (string->symbol
                                                   (_read-token_62
                                                     (list (_docase_61 _c_63) #\#)
                                                     _cs_52))
                                                 (if (if (eqv? _key_64 #\!) #t #f)
                                                   (begin (_skip-line_56) (_read1_55))
                                                   (if (if (eqv? _key_64 #\\) #t #f)
                                                     (let ((_t_65 (_read-token_62 '() #t)))
                                                       (if (string-ci=? "newline" _t_65)
                                                         #\newline
                                                         (if (string-ci=? "tab" _t_65)
                                                           #\tab
                                                           (if (string-ci=? "space" _t_65)
                                                             #\space
                                                             (if (string-ci=? "return" _t_65)
                                                               #\return
                                                               (if (zero? (string-length _t_65))
                                                                 (_read-char_43 _port_51)
                                                                 (string-ref _t_65 0)))))))
                                                     (if (if (eqv? _key_64 #\') #t #f)
                                                       (list 'syntax (_read1_55))
                                                       (error "invalid `#' syntax" _c_63)))))))))))))))))))
                   (_read-list_59
                     (lambda (_delim_63)
                       (%call-with-exit-continuation
                         (lambda (_return_64)
                           (let ((_lst_65 '()) (_old_66 _eol_53))
                             (set! _eol_53
                               (lambda (_c_67)
                                 (set! _eol_53 _old_66)
                                 (if (eqv? _c_67 _delim_63)
                                   (_return_64 (_reverse_44 _lst_65))
                                   (error "missing closing delimiter" _delim_63))))
                             (let _loop_67 ()
                               (let ((_c_68 (_skip-whitespace_57)))
                                 (if (eof-object? _c_68)
                                   (error "unexpected EOF while reading list")
                                   (if (char=? _c_68 _delim_63)
                                     (begin
                                       (_read-char_43 _port_51)
                                       (set! _eol_53 _old_66)
                                       (_return_64 (_reverse_44 _lst_65)))
                                     (begin
                                       (if (eqv? #\. _c_68)
                                         (let ((_t_69 (_read-token_62 '() _cs_52)))
                                           (if (string=? "." _t_69)
                                             (let ((_rest_70 (_read1_55)))
                                               (_skip-whitespace_57)
                                               (set! _eol_53 _old_66)
                                               (if (eqv? (_read-char_43 _port_51) _delim_63)
                                                 (_return_64
                                                   (append (_reverse_44 _lst_65) _rest_70))
                                                 (error "missing closing delimiter" _delim_63)))
                                             (set! _lst_65
                                               (cons (_parse-token_54 _t_69) _lst_65))))
                                         (set! _lst_65 (cons (_read1_55) _lst_65)))
                                       (_loop_67)))))))))))
                   (_read-string_60
                     (lambda ()
                       (let _loop_63 ((_lst_64 '()))
                         (let ((_c_65 (_read-char_43 _port_51)))
                           (if (eof-object? _c_65)
                             (error "unexpected EOF while reading string")
                             (if (char=? #\" _c_65)
                               (_list->string_47 (_reverse_44 _lst_64))
                               (if (char=? #\\ _c_65)
                                 (let ((_c_66 (_read-char_43 _port_51)))
                                   (if (eof-object? _c_66)
                                     (error "unexpected EOF while reading string")
                                     (let ((_key_67 _c_66))
                                       (if (if (eqv? _key_67 #\n) #t #f)
                                         (_loop_63 (cons #\newline _lst_64))
                                         (if (if (eqv? _key_67 #\r) #t #f)
                                           (_loop_63 (cons #\return _lst_64))
                                           (if (if (eqv? _key_67 #\t) #t #f)
                                             (_loop_63 (cons #\tab _lst_64))
                                             (_loop_63 (cons _c_66 _lst_64))))))))
                                 (_loop_63 (cons _c_65 _lst_64)))))))))
                   (_docase_61
                     (lambda (_c_63) (if _cs_52 _c_63 (char-downcase _c_63))))
                   (_read-token_62
                     (lambda (_prefix_63 _cs_64)
                       (let _loop_65 ((_lst_66 _prefix_63))
                         (let ((_c_67 (_peek-char_45 _port_51)))
                           (if (let ((_x_68 (eof-object? _c_67)))
                                 (if _x_68
                                   _x_68
                                   (let ((_x_69 (memv _c_67 '(#\{ #\} #\( #\) #\[ #\] #\; #\"))))
                                     (if _x_69 _x_69 (char-whitespace? _c_67)))))
                             (_list->string_47 (_reverse_44 _lst_66))
                             (_loop_65
                               (cons ((if _cs_64 id _docase_61)
                                      (_read-char_43 _port_51))
                                     _lst_66))))))))
            (_read1_55))))))
  (define %write-hook (lambda (_x_43 _port_44) #f))
  (define %output-to-port
    (lambda (_x_43 _rd_44 . _port_45)
      (let ((_port_46 (if (pair? _port_45) (car _port_45) %output-port)))
        (letrec ((_wr_47 (if _rd_44 write display))
                 (_out_48 (lambda (_x_50) (_wr_47 _x_50 _port_46)))
                 (_out1_49 (lambda (_x_50) (display _x_50 _port_46))))
          (let _show_50 ((_x_51 _x_43))
            (let ((_x_52 (%write-hook _x_51 _port_46)))
              (if _x_52
                _x_52
                (if (vector? _x_51)
                  (let ((_len_53 (vector-length _x_51)))
                    (_out1_49 "#")
                    (_show_50 (vector->list _x_51)))
                  (if (pair? _x_51)
                    (begin
                      (_out1_49 "(")
                      (_show_50 (car _x_51))
                      (let _loop_53 ((_x_54 (cdr _x_51)))
                        (if (null? _x_54)
                          (_out1_49 ")")
                          (if (pair? _x_54)
                            (begin
                              (_out1_49 " ")
                              (_show_50 (car _x_54))
                              (_loop_53 (cdr _x_54)))
                            (begin
                              (_out1_49 " . ")
                              (_show_50 _x_54)
                              (_out1_49 ")"))))))
                    (_out_48 _x_51))))))
          (%void)))))
  (define %display
    (lambda (_x_43 . _port_44)
      (%output-to-port
        _x_43
        #f
        (if (pair? _port_44) (car _port_44) %output-port))))
  (define %write
    (lambda (_x_43 . _port_44)
      (%output-to-port
        _x_43
        #t
        (if (pair? _port_44) (car _port_44) %output-port))))
  (define *pretty-print-width* 79)
  (define pretty-print-hook (lambda (_x_43 _out_44) #f))
  (define pretty-print
    (lambda (_obj_43 . _opt_44)
      (let ((_port_45 (if (pair? _opt_44) (car _opt_44) %output-port)))
        (letrec ((_generic-write_46
                   (lambda (_obj_48 _display?_49 _width_50 _output_51)
                     (letrec ((_read-macro?_52
                                (lambda (_l_59)
                                  (letrec ((_length1?_60
                                             (lambda (_l_61)
                                               (if (pair? _l_61) (null? (cdr _l_61)) #f))))
                                    (let ((_head_61 (car _l_59)) (_tail_62 (cdr _l_59)))
                                      (let ((_key_63 _head_61))
                                        (if (if (eqv? _key_63 'quote)
                                              #t
                                              (if (eqv? _key_63 'quasiquote)
                                                #t
                                                (if (eqv? _key_63 'unquote)
                                                  #t
                                                  (if (eqv? _key_63 'unquote-splicing) #t #f))))
                                          (_length1?_60 _tail_62)
                                          #f))))))
                              (_read-macro-body_53
                                (lambda (_l_59) (cadr _l_59)))
                              (_read-macro-prefix_54
                                (lambda (_l_59)
                                  (let ((_head_60 (car _l_59)) (_tail_61 (cdr _l_59)))
                                    (let ((_key_62 _head_60))
                                      (if (if (eqv? _key_62 'quote) #t #f)
                                        "'"
                                        (if (if (eqv? _key_62 'quasiquote) #t #f)
                                          "`"
                                          (if (if (eqv? _key_62 'unquote) #t #f)
                                            ","
                                            (if (if (eqv? _key_62 'unquote-splicing) #t #f)
                                              ",@"
                                              #f))))))))
                              (_out_55
                                (lambda (_str_59 _col_60)
                                  (if _col_60
                                    (if (_output_51 _str_59)
                                      (+ _col_60 (string-length _str_59))
                                      #f)
                                    #f)))
                              (_char-name_56
                                (lambda (_c_59)
                                  (let ((_key_60 _c_59))
                                    (if (if (eqv? _key_60 #\space) #t #f)
                                      "space"
                                      (if (if (eqv? _key_60 #\newline) #t #f)
                                        "newline"
                                        (if (if (eqv? _key_60 #\return) #t #f)
                                          "return"
                                          (if (if (eqv? _key_60 #\tab) #t #f) "tab" #f)))))))
                              (_wr_57
                                (lambda (_obj_59 _col_60)
                                  (letrec ((_out/col_61
                                             (lambda (_col_64)
                                               (lambda (_x_65) (_out_55 _x_65 _col_64))))
                                           (_wr-expr_62
                                             (lambda (_expr_64 _col_65)
                                               (if (_read-macro?_52 _expr_64)
                                                 (_wr_57
                                                   (_read-macro-body_53 _expr_64)
                                                   (_out_55
                                                     (_read-macro-prefix_54 _expr_64)
                                                     _col_65))
                                                 (_wr-lst_63 _expr_64 _col_65))))
                                           (_wr-lst_63
                                             (lambda (_l_64 _col_65)
                                               (if (pair? _l_64)
                                                 (let _loop_66 ((_l_67 (cdr _l_64))
                                                                (_col_68
                                                                  (if _col_65
                                                                    (_wr_57 (car _l_64) (_out_55 "(" _col_65))
                                                                    #f)))
                                                   (if (not _col_68)
                                                     _col_68
                                                     (if (pair? _l_67)
                                                       (_loop_66
                                                         (cdr _l_67)
                                                         (_wr_57 (car _l_67) (_out_55 " " _col_68)))
                                                       (if (null? _l_67)
                                                         (_out_55 ")" _col_68)
                                                         (_out_55
                                                           ")"
                                                           (_wr_57 _l_67 (_out_55 " . " _col_68)))))))
                                                 (_out_55 "()" _col_65)))))
                                    (let ((_x_64 (pretty-print-hook _obj_59 (_out/col_61 _col_60))))
                                      (if _x_64
                                        _x_64
                                        (if (pair? _obj_59)
                                          (_wr-expr_62 _obj_59 _col_60)
                                          (if (null? _obj_59)
                                            (_wr-lst_63 _obj_59 _col_60)
                                            (if (boolean? _obj_59)
                                              (_out_55 (if _obj_59 "#t" "#f") _col_60)
                                              (if (number? _obj_59)
                                                (_out_55 (number->string _obj_59) _col_60)
                                                (if (symbol? _obj_59)
                                                  (_out_55 (symbol->string _obj_59) _col_60)
                                                  (if (vector? _obj_59)
                                                    (_wr-lst_63
                                                      (vector->list _obj_59)
                                                      (_out_55 "#" _col_60))
                                                    (if (string? _obj_59)
                                                      (if _display?_49
                                                        (_out_55 _obj_59 _col_60)
                                                        (let _loop_65 ((_i_66 0)
                                                                       (_j_67 0)
                                                                       (_col_68 (_out_55 "\"" _col_60)))
                                                          (if (if _col_68 (< _j_67 (string-length _obj_59)) #f)
                                                            (let ((_c_69 (string-ref _obj_59 _j_67)))
                                                              (if (let ((_x_70 (char=? _c_69 #\\)))
                                                                    (if _x_70 _x_70 (char=? _c_69 #\")))
                                                                (_loop_65
                                                                  _j_67
                                                                  (+ _j_67 1)
                                                                  (_out_55
                                                                    "\\"
                                                                    (_out_55 (substring _obj_59 _i_66 _j_67) _col_68)))
                                                                (_loop_65 _i_66 (+ _j_67 1) _col_68)))
                                                            (_out_55
                                                              "\""
                                                              (_out_55 (substring _obj_59 _i_66 _j_67) _col_68)))))
                                                      (if (procedure? _obj_59)
                                                        (_out_55 "#<procedure>" _col_60)
                                                        (if (char? _obj_59)
                                                          (if _display?_49
                                                            (_out_55 (make-string 1 _obj_59) _col_60)
                                                            (let ((_code_65 (char->integer _obj_59)))
                                                              (_out_55 "#\\" _col_60)
                                                              (let ((_tmp_66 (_char-name_56 _obj_59)))
                                                                (if _tmp_66
                                                                  (let ((_cn_67 _tmp_66)) (_out_55 _cn_67 _col_60))
                                                                  (if (< _code_65 32)
                                                                    (begin
                                                                      (_out_55 "x" _col_60)
                                                                      (_out_55 (number->string _code_65 16) _col_60))
                                                                    (if (> _code_65 255)
                                                                      (begin
                                                                        (_out_55 (if (> _code_65 65535) "U" "u") _col_60)
                                                                        (_out_55 (number->string _code_65 16) _col_60))
                                                                      (_out_55 (make-string 1 _obj_59) _col_60)))))))
                                                          (if (eof-object? _obj_59)
                                                            (_out_55 "#<eof>" _col_60)
                                                            (if (input-port? _obj_59)
                                                              (_out_55 "#<input port>" _col_60)
                                                              (if (output-port? _obj_59)
                                                                (_out_55 "#<output port>" _col_60)
                                                                (if (eq? (%void) _obj_59)
                                                                  (_out_55 "#<undefined>" _col_60)
                                                                  (_out_55 "#<unprintable object>" _col_60)))))))))))))))))))
                              (_pp_58
                                (lambda (_obj_59 _col_60)
                                  (letrec ((_spaces_61
                                             (lambda (_n_82 _col_83)
                                               (if (> _n_82 0)
                                                 (if (> _n_82 7)
                                                   (_spaces_61
                                                     (- _n_82 8)
                                                     (_out_55 "        " _col_83))
                                                   (_out_55 (substring "        " 0 _n_82) _col_83))
                                                 _col_83)))
                                           (_indent_62
                                             (lambda (_to_82 _col_83)
                                               (if _col_83
                                                 (if (< _to_82 _col_83)
                                                   (if (_out_55 (make-string 1 #\newline) _col_83)
                                                     (_spaces_61 _to_82 0)
                                                     #f)
                                                   (_spaces_61 (- _to_82 _col_83) _col_83))
                                                 #f)))
                                           (_pr_63
                                             (lambda (_obj_82 _col_83 _extra_84 _pp-pair_85)
                                               (if (let ((_x_86 (pair? _obj_82)))
                                                     (if _x_86 _x_86 (vector? _obj_82)))
                                                 (let ((_result_86 '())
                                                       (_left_87
                                                         (max (+ (- (- _width_50 _col_83) _extra_84) 1)
                                                              _max-expr-width_80)))
                                                   (_generic-write_46
                                                     _obj_82
                                                     _display?_49
                                                     #f
                                                     (lambda (_str_88)
                                                       (set! _result_86 (cons _str_88 _result_86))
                                                       (set! _left_87
                                                         (- _left_87 (string-length _str_88)))
                                                       (> _left_87 0)))
                                                   (if (> _left_87 0)
                                                     (_out_55
                                                       (_reverse-string-append_47 _result_86)
                                                       _col_83)
                                                     (if (pair? _obj_82)
                                                       (_pp-pair_85 _obj_82 _col_83 _extra_84)
                                                       (_pp-list_66
                                                         (vector->list _obj_82)
                                                         (_out_55 "#" _col_83)
                                                         _extra_84
                                                         _pp-expr_64))))
                                                 (_wr_57 _obj_82 _col_83))))
                                           (_pp-expr_64
                                             (lambda (_expr_82 _col_83 _extra_84)
                                               (if (_read-macro?_52 _expr_82)
                                                 (_pr_63
                                                   (_read-macro-body_53 _expr_82)
                                                   (_out_55
                                                     (_read-macro-prefix_54 _expr_82)
                                                     _col_83)
                                                   _extra_84
                                                   _pp-expr_64)
                                                 (let ((_head_85 (car _expr_82)))
                                                   (if (symbol? _head_85)
                                                     (let ((_proc_86 (_style_81 _head_85)))
                                                       (if _proc_86
                                                         (_proc_86 _expr_82 _col_83 _extra_84)
                                                         (if (> (string-length (symbol->string _head_85))
                                                                _max-call-head-width_79)
                                                           (_pp-general_68
                                                             _expr_82
                                                             _col_83
                                                             _extra_84
                                                             #f
                                                             #f
                                                             #f
                                                             _pp-expr_64)
                                                           (_pp-call_65
                                                             _expr_82
                                                             _col_83
                                                             _extra_84
                                                             _pp-expr_64))))
                                                     (_pp-list_66
                                                       _expr_82
                                                       _col_83
                                                       _extra_84
                                                       _pp-expr_64))))))
                                           (_pp-call_65
                                             (lambda (_expr_82 _col_83 _extra_84 _pp-item_85)
                                               (let ((_col*_86
                                                       (_wr_57 (car _expr_82) (_out_55 "(" _col_83))))
                                                 (if _col_83
                                                   (_pp-down_67
                                                     (cdr _expr_82)
                                                     _col*_86
                                                     (+ _col*_86 1)
                                                     _extra_84
                                                     _pp-item_85)
                                                   #f))))
                                           (_pp-list_66
                                             (lambda (_l_82 _col_83 _extra_84 _pp-item_85)
                                               (let ((_col_86 (_out_55 "(" _col_83)))
                                                 (_pp-down_67
                                                   _l_82
                                                   _col_86
                                                   _col_86
                                                   _extra_84
                                                   _pp-item_85))))
                                           (_pp-down_67
                                             (lambda (_l_82 _col1_83 _col2_84 _extra_85 _pp-item_86)
                                               (let _loop_87 ((_l_88 _l_82) (_col_89 _col1_83))
                                                 (if _col_89
                                                   (if (pair? _l_88)
                                                     (let ((_rest_90 (cdr _l_88)))
                                                       (let ((_extra_91
                                                               (if (null? _rest_90) (+ _extra_85 1) 0)))
                                                         (_loop_87
                                                           _rest_90
                                                           (_pr_63
                                                             (car _l_88)
                                                             (_indent_62 _col2_84 _col_89)
                                                             _extra_91
                                                             _pp-item_86))))
                                                     (if (null? _l_88)
                                                       (_out_55 ")" _col_89)
                                                       (_out_55
                                                         ")"
                                                         (_pr_63
                                                           _l_88
                                                           (_indent_62
                                                             _col2_84
                                                             (_out_55 "." (_indent_62 _col2_84 _col_89)))
                                                           (+ _extra_85 1)
                                                           _pp-item_86))))
                                                   #f))))
                                           (_pp-general_68
                                             (lambda (_expr_82
                                                      _col_83
                                                      _extra_84
                                                      _named?_85
                                                      _pp-1_86
                                                      _pp-2_87
                                                      _pp-3_88)
                                               (letrec ((_tail1_89
                                                          (lambda (_rest_92 _col1_93 _col2_94 _col3_95)
                                                            (if (if _pp-1_86 (pair? _rest_92) #f)
                                                              (let ((_val1_96 (car _rest_92)))
                                                                (let ((_rest_97 (cdr _rest_92)))
                                                                  (let ((_extra_98
                                                                          (if (null? _rest_97) (+ _extra_84 1) 0)))
                                                                    (_tail2_90
                                                                      _rest_97
                                                                      _col1_93
                                                                      (_pr_63
                                                                        _val1_96
                                                                        (_indent_62 _col3_95 _col2_94)
                                                                        _extra_98
                                                                        _pp-1_86)
                                                                      _col3_95))))
                                                              (_tail2_90 _rest_92 _col1_93 _col2_94 _col3_95))))
                                                        (_tail2_90
                                                          (lambda (_rest_92 _col1_93 _col2_94 _col3_95)
                                                            (if (if _pp-2_87 (pair? _rest_92) #f)
                                                              (let ((_val1_96 (car _rest_92)))
                                                                (let ((_rest_97 (cdr _rest_92)))
                                                                  (let ((_extra_98
                                                                          (if (null? _rest_97) (+ _extra_84 1) 0)))
                                                                    (_tail3_91
                                                                      _rest_97
                                                                      _col1_93
                                                                      (_pr_63
                                                                        _val1_96
                                                                        (_indent_62 _col3_95 _col2_94)
                                                                        _extra_98
                                                                        _pp-2_87)))))
                                                              (_tail3_91 _rest_92 _col1_93 _col2_94))))
                                                        (_tail3_91
                                                          (lambda (_rest_92 _col1_93 _col2_94)
                                                            (_pp-down_67
                                                              _rest_92
                                                              _col2_94
                                                              _col1_93
                                                              _extra_84
                                                              _pp-3_88))))
                                                 (let ((_head_92 (car _expr_82)))
                                                   (let ((_rest_93 (cdr _expr_82)))
                                                     (let ((_col*_94
                                                             (_wr_57 _head_92 (_out_55 "(" _col_83))))
                                                       (if (if _named?_85 (pair? _rest_93) #f)
                                                         (let ((_name_95 (car _rest_93)))
                                                           (let ((_rest_96 (cdr _rest_93)))
                                                             (let ((_col**_97
                                                                     (_wr_57 _name_95 (_out_55 " " _col*_94))))
                                                               (_tail1_89
                                                                 _rest_96
                                                                 (+ _col_83 _indent-general_78)
                                                                 _col**_97
                                                                 (+ _col**_97 1)))))
                                                         (_tail1_89
                                                           _rest_93
                                                           (+ _col_83 _indent-general_78)
                                                           _col*_94
                                                           (+ _col*_94 1)))))))))
                                           (_pp-expr-list_69
                                             (lambda (_l_82 _col_83 _extra_84)
                                               (_pp-list_66 _l_82 _col_83 _extra_84 _pp-expr_64)))
                                           (_pp-lambda_70
                                             (lambda (_expr_82 _col_83 _extra_84)
                                               (_pp-general_68
                                                 _expr_82
                                                 _col_83
                                                 _extra_84
                                                 #f
                                                 _pp-expr-list_69
                                                 #f
                                                 _pp-expr_64)))
                                           (_pp-if_71
                                             (lambda (_expr_82 _col_83 _extra_84)
                                               (_pp-general_68
                                                 _expr_82
                                                 _col_83
                                                 _extra_84
                                                 #f
                                                 _pp-expr_64
                                                 #f
                                                 _pp-expr_64)))
                                           (_pp-cond_72
                                             (lambda (_expr_82 _col_83 _extra_84)
                                               (_pp-call_65
                                                 _expr_82
                                                 _col_83
                                                 _extra_84
                                                 _pp-expr-list_69)))
                                           (_pp-case_73
                                             (lambda (_expr_82 _col_83 _extra_84)
                                               (_pp-general_68
                                                 _expr_82
                                                 _col_83
                                                 _extra_84
                                                 #f
                                                 _pp-expr_64
                                                 #f
                                                 _pp-expr-list_69)))
                                           (_pp-and_74
                                             (lambda (_expr_82 _col_83 _extra_84)
                                               (_pp-call_65
                                                 _expr_82
                                                 _col_83
                                                 _extra_84
                                                 _pp-expr_64)))
                                           (_pp-let_75
                                             (lambda (_expr_82 _col_83 _extra_84)
                                               (let ((_rest_85 (cdr _expr_82)))
                                                 (let ((_named?_86
                                                         (if (pair? _rest_85) (symbol? (car _rest_85)) #f)))
                                                   (_pp-general_68
                                                     _expr_82
                                                     _col_83
                                                     _extra_84
                                                     _named?_86
                                                     _pp-expr-list_69
                                                     #f
                                                     _pp-expr_64)))))
                                           (_pp-begin_76
                                             (lambda (_expr_82 _col_83 _extra_84)
                                               (_pp-general_68
                                                 _expr_82
                                                 _col_83
                                                 _extra_84
                                                 #f
                                                 #f
                                                 #f
                                                 _pp-expr_64)))
                                           (_pp-do_77
                                             (lambda (_expr_82 _col_83 _extra_84)
                                               (_pp-general_68
                                                 _expr_82
                                                 _col_83
                                                 _extra_84
                                                 #f
                                                 _pp-expr-list_69
                                                 _pp-expr-list_69
                                                 _pp-expr_64)))
                                           (_indent-general_78 2)
                                           (_max-call-head-width_79 5)
                                           (_max-expr-width_80 50)
                                           (_style_81
                                             (lambda (_head_82)
                                               (let ((_key_83 _head_82))
                                                 (if (if (eqv? _key_83 'lambda)
                                                       #t
                                                       (if (eqv? _key_83 'let*)
                                                         #t
                                                         (if (eqv? _key_83 'letrec)
                                                           #t
                                                           (if (eqv? _key_83 'define) #t #f))))
                                                   _pp-lambda_70
                                                   (if (if (eqv? _key_83 'if)
                                                         #t
                                                         (if (eqv? _key_83 'set!) #t #f))
                                                     _pp-if_71
                                                     (if (if (eqv? _key_83 'cond) #t #f)
                                                       _pp-cond_72
                                                       (if (if (eqv? _key_83 'case) #t #f)
                                                         _pp-case_73
                                                         (if (if (eqv? _key_83 'and)
                                                               #t
                                                               (if (eqv? _key_83 'or) #t #f))
                                                           _pp-and_74
                                                           (if (if (eqv? _key_83 'let) #t #f)
                                                             _pp-let_75
                                                             (if (if (eqv? _key_83 'begin) #t #f)
                                                               _pp-begin_76
                                                               (if (if (eqv? _key_83 'do) #t #f) _pp-do_77 #f))))))))))))
                                    (_pr_63 _obj_59 _col_60 0 _pp-expr_64)))))
                       (if _width_50
                         (_out_55 (make-string 1 #\newline) (_pp_58 _obj_48 0))
                         (_wr_57 _obj_48 0)))))
                 (_reverse-string-append_47
                   (lambda (_l_48)
                     (letrec ((_rev-string-append_49
                                (lambda (_l_50 _i_51)
                                  (if (pair? _l_50)
                                    (let ((_str_52 (car _l_50)))
                                      (let ((_len_53 (string-length _str_52)))
                                        (let ((_result_54
                                                (_rev-string-append_49
                                                  (cdr _l_50)
                                                  (+ _i_51 _len_53))))
                                          (let _loop_55 ((_j_56 0)
                                                         (_k_57 (- (- (string-length _result_54) _i_51) _len_53)))
                                            (if (< _j_56 _len_53)
                                              (begin
                                                (string-set!
                                                  _result_54
                                                  _k_57
                                                  (string-ref _str_52 _j_56))
                                                (_loop_55 (+ _j_56 1) (+ _k_57 1)))
                                              _result_54)))))
                                    (make-string _i_51)))))
                       (_rev-string-append_49 _l_48 0)))))
          (_generic-write_46
            _obj_43
            #f
            *pretty-print-width*
            (lambda (_s_48) (display _s_48 _port_45) #t))
          (%void)))))
  (define pp pretty-print)
  (define library-path
    (let ((_path_43
            (list (let ((_x_43 (%get-environment-variable "SCHEME_LIBRARY_PATH")))
                    (if _x_43
                      _x_43
                      (string-append
                        (let ((_x_44 (%get-environment-variable "HOME")))
                          (if _x_44 _x_44 "."))
                        "/.scheme/lib")))
                  ".")))
      (lambda _val_44
        (if (null? _val_44) _path_43 (set! _path_43 (car _val_44))))))
  (define program-file-filename
    (lambda (_x_43)
      (if (string? _x_43)
        _x_43
        (if (symbol? _x_43)
          (symbol->string _x_43)
          (if (list? _x_43)
            (join (map program-file-filename _x_43) "/")
            (if (not (pair? _x_43))
              (error "invalid filename" _x_43)
              (stringify _x_43)))))))
  (define locate-library
    (lambda (_fn_43)
      (let ((_x_44 (let ((_fn_44 (program-file-filename _fn_43)))
                     (any (lambda (_ip_45)
                            (let ((_x_46 (%file-exists?
                                           (string-append _ip_45 "/" _fn_44 ".scm"))))
                              (if _x_46
                                _x_46
                                (%file-exists? (string-append _ip_45 "/" _fn_44)))))
                          (library-path)))))
        (if _x_44
          _x_44
          (let ((_x_45 (%file-exists? (string-append _fn_43 ".scm"))))
            (if _x_45
              _x_45
              (let ((_x_46 (%file-exists? _fn_43)))
                (if _x_46 _x_46 (error "library not found" _fn_43)))))))))
  (define expand-program
    (lambda (_prg_43 . _src_44)
      (let ((_src_45 (if (pair? _src_44) (car _src_44) #f)))
        (letrec ((_localize_46
                   (lambda (_path_50 _dir_51)
                     (let ((_path_52 (program-file-filename _path_50)))
                       (if (let ((_x_53 (not _dir_51)))
                             (if _x_53
                               _x_53
                               (if (positive? (string-length _path_52))
                                 (memq (string-ref _path_52 0) '(#\\ #\/))
                                 #f)))
                         _path_52
                         (string-append _dir_51 "/" _path_52)))))
                 (_dirname_47
                   (lambda (_path_50)
                     (let ((_len_51 (string-length _path_50)))
                       (let _loop_52 ((_i_53 (- _len_51 1)))
                         (if (negative? _i_53)
                           "."
                           (if (memq (string-ref _path_50 _i_53) '(#\\ #\/))
                             (substring _path_50 0 _i_53)
                             (_loop_52 (- _i_53 1))))))))
                 (_expand-req_48
                   (lambda (_expr_50)
                     (let ((_v_51 _expr_50))
                       (let ((_failure_52
                               (lambda ()
                                 (let ((_failure_52
                                         (lambda ()
                                           (let ((_failure_52
                                                   (lambda ()
                                                     (let ((_failure_52
                                                             (lambda ()
                                                               (let ((_failure_52
                                                                       (lambda () (error 'match "no matching pattern"))))
                                                                 (let ((_r_54 _v_51))
                                                                   (error "invalid feature requirement" _r_54))))))
                                                       (if (symbol? _v_51)
                                                         (let ((_r_56 _v_51)) (memq _r_56 %features))
                                                         (_failure_52))))))
                                             (if (pair? _v_51)
                                               (let ((_w_54 (car _v_51)) (_x_55 (cdr _v_51)))
                                                 (if (equal? _w_54 'not)
                                                   (if (if (pair? _x_55) (null? (cdr _x_55)) #f)
                                                     (let ((_w_57 (car _x_55)))
                                                       (let ((_req_59 _w_57))
                                                         (not (_expand-req_48 (list _req_59)))))
                                                     (_failure_52))
                                                   (_failure_52)))
                                               (_failure_52))))))
                                   (if (pair? _v_51)
                                     (let ((_w_54 (car _v_51)) (_x_55 (cdr _v_51)))
                                       (if (equal? _w_54 'or)
                                         (let ((_reqs_60 _x_55))
                                           (if (list? _reqs_60)
                                             (any _expand-req_48 _reqs_60)
                                             (_failure_52)))
                                         (_failure_52)))
                                     (_failure_52))))))
                         (if (pair? _v_51)
                           (let ((_w_54 (car _v_51)) (_x_55 (cdr _v_51)))
                             (if (equal? _w_54 'and)
                               (let ((_reqs_60 _x_55))
                                 (if (list? _reqs_60)
                                   (every _expand-req_48 _reqs_60)
                                   (_failure_52)))
                               (_failure_52)))
                           (_failure_52))))))
                 (_expand-clause_49
                   (lambda (_clause_50)
                     (let ((_v_51 _clause_50))
                       (let ((_failure_52
                               (lambda ()
                                 (let ((_failure_52
                                         (lambda ()
                                           (let ((_failure_52
                                                   (lambda ()
                                                     (let ((_failure_52
                                                             (lambda ()
                                                               (let ((_failure_52
                                                                       (lambda ()
                                                                         (let ((_failure_52
                                                                                 (lambda ()
                                                                                   (let ((_failure_52
                                                                                           (lambda ()
                                                                                             (let ((_failure_52
                                                                                                     (lambda () (error 'match "no matching pattern"))))
                                                                                               (error "invalid program clause" _clause_50)))))
                                                                                     (if (pair? _v_51)
                                                                                       (let ((_w_54 (car _v_51)) (_x_55 (cdr _v_51)))
                                                                                         (if (equal? _w_54 'unless)
                                                                                           (if (pair? _x_55)
                                                                                             (let ((_w_58 (car _x_55)) (_x_59 (cdr _x_55)))
                                                                                               (let ((_req_61 _w_58))
                                                                                                 (let ((_clauses_65 _x_59))
                                                                                                   (if (list? _clauses_65)
                                                                                                     (_expand-clause_49
                                                                                                       (cons 'when
                                                                                                             (cons (list 'not _req_61) _clauses_65)))
                                                                                                     (_failure_52)))))
                                                                                             (_failure_52))
                                                                                           (_failure_52)))
                                                                                       (_failure_52))))))
                                                                           (if (pair? _v_51)
                                                                             (let ((_w_54 (car _v_51)) (_x_55 (cdr _v_51)))
                                                                               (if (equal? _w_54 'when)
                                                                                 (if (pair? _x_55)
                                                                                   (let ((_w_58 (car _x_55)) (_x_59 (cdr _x_55)))
                                                                                     (let ((_req_61 _w_58))
                                                                                       (let ((_clauses_65 _x_59))
                                                                                         (if (list? _clauses_65)
                                                                                           (_expand-clause_49
                                                                                             (cons 'cond
                                                                                                   (cons (cons _req_61 _clauses_65)
                                                                                                         '((else (code (%void)))))))
                                                                                           (_failure_52)))))
                                                                                   (_failure_52))
                                                                                 (_failure_52)))
                                                                             (_failure_52))))))
                                                                 (if (pair? _v_51)
                                                                   (let ((_w_54 (car _v_51)) (_x_55 (cdr _v_51)))
                                                                     (let ((_sk2_57
                                                                             (lambda ()
                                                                               (let ((_clauses_60 _x_55))
                                                                                 (if (list? _clauses_60)
                                                                                   (let _loop_61 ((_cs_62 _clauses_60))
                                                                                     (let ((_v_63 _cs_62))
                                                                                       (let ((_failure_64
                                                                                               (lambda ()
                                                                                                 (let ((_failure_64
                                                                                                         (lambda ()
                                                                                                           (let ((_failure_64
                                                                                                                   (lambda ()
                                                                                                                     (let ((_failure_64
                                                                                                                             (lambda () (error 'match "no matching pattern"))))
                                                                                                                       (let ((_c_66 _v_63))
                                                                                                                         (error "invalid clause syntax" _c_66))))))
                                                                                                             (if (pair? _v_63)
                                                                                                               (let ((_w_65 (car _v_63)) (_x_66 (cdr _v_63)))
                                                                                                                 (if (pair? _w_65)
                                                                                                                   (let ((_w_68 (car _w_65)) (_x_69 (cdr _w_65)))
                                                                                                                     (let ((_req_71 _w_68))
                                                                                                                       (let ((_clauses_75 _x_69))
                                                                                                                         (if (list? _clauses_75)
                                                                                                                           (let ((_more_77 _x_66))
                                                                                                                             (if (_expand-req_48 _req_71)
                                                                                                                               (cons 'begin (map _expand-clause_49 _clauses_75))
                                                                                                                               (_loop_61 _more_77)))
                                                                                                                           (_failure_64)))))
                                                                                                                   (_failure_64)))
                                                                                                               (_failure_64))))))
                                                                                                   (if (if (pair? _v_63) (null? (cdr _v_63)) #f)
                                                                                                     (let ((_w_65 (car _v_63)))
                                                                                                       (if (pair? _w_65)
                                                                                                         (let ((_w_67 (car _w_65)) (_x_68 (cdr _w_65)))
                                                                                                           (if (equal? _w_67 'else)
                                                                                                             (let ((_clauses_73 _x_68))
                                                                                                               (if (list? _clauses_73)
                                                                                                                 (cons 'begin (map _expand-clause_49 _clauses_73))
                                                                                                                 (_failure_64)))
                                                                                                             (_failure_64)))
                                                                                                         (_failure_64)))
                                                                                                     (_failure_64))))))
                                                                                         (if (null? _v_63)
                                                                                           (error "no requirement satisfied"
                                                                                                  (map car _clauses_60))
                                                                                           (_failure_64)))))
                                                                                   (_failure_52))))))
                                                                       (let ((_fk2_58
                                                                               (lambda ()
                                                                                 (if (equal? _w_54 'cond) (_sk2_57) (_failure_52)))))
                                                                         (if (equal? _w_54 'feature-cond)
                                                                           (_sk2_57)
                                                                           (_fk2_58)))))
                                                                   (_failure_52))))))
                                                       (if (pair? _v_51)
                                                         (let ((_w_54 (car _v_51)) (_x_55 (cdr _v_51)))
                                                           (if (equal? _w_54 'code)
                                                             (let ((_exps_60 _x_55))
                                                               (if (list? _exps_60)
                                                                 (cons 'begin _exps_60)
                                                                 (_failure_52)))
                                                             (_failure_52)))
                                                         (_failure_52))))))
                                             (if (pair? _v_51)
                                               (let ((_w_54 (car _v_51)) (_x_55 (cdr _v_51)))
                                                 (if (equal? _w_54 'files)
                                                   (let ((_fns_60 _x_55))
                                                     (if (list? _fns_60)
                                                       (cons 'begin
                                                             (map (lambda (_fn_61)
                                                                    (read-forms (_localize_46 _fn_61 _src_45) %read))
                                                                  _fns_60))
                                                       (_failure_52)))
                                                   (_failure_52)))
                                               (_failure_52))))))
                                   (if (pair? _v_51)
                                     (let ((_w_54 (car _v_51)) (_x_55 (cdr _v_51)))
                                       (if (equal? _w_54 'libraries)
                                         (let ((_names_60 _x_55))
                                           (if (list? _names_60)
                                             (cons 'begin
                                                   (map (lambda (_name_61)
                                                          (read-forms (locate-library _name_61) %read))
                                                        _names_60))
                                             (_failure_52)))
                                         (_failure_52)))
                                     (_failure_52))))))
                         (if (pair? _v_51)
                           (let ((_w_54 (car _v_51)) (_x_55 (cdr _v_51)))
                             (if (equal? _w_54 'requires)
                               (let ((_ids_60 _x_55))
                                 (if (list? _ids_60)
                                   (begin
                                     (for-each
                                       (lambda (_id_61)
                                         (if (not (memq _id_61 %features))
                                           (error "required feature not available" _id_61)))
                                       _ids_60)
                                     '(void))
                                   (_failure_52)))
                               (_failure_52)))
                           (_failure_52)))))))
          (set! _src_45
            (if _src_45 (if (string? _src_45) (_dirname_47 _src_45) #f) #f))
          (let ((_v_50 _prg_43))
            (let ((_failure_51
                    (lambda ()
                      (let ((_failure_51
                              (lambda () (error 'match "no matching pattern"))))
                        (error "invalid program form" _prg_43)))))
              (if (pair? _v_50)
                (let ((_w_53 (car _v_50)) (_x_54 (cdr _v_50)))
                  (if (equal? _w_53 'program)
                    (let ((_clauses_59 _x_54))
                      (if (list? _clauses_59)
                        (cons 'begin (map _expand-clause_49 _clauses_59))
                        (_failure_51)))
                    (_failure_51)))
                (_failure_51))))))))
  (define load-program
    (lambda (_filename_43 . _evaluator_44)
      (let ((_temp_45 _filename_43))
        (dynamic-wind
          (lambda ()
            (let ((_tmp_47 _temp_45))
              (set! _temp_45 *current-source-filename*)
              (set! *current-source-filename* _tmp_47)))
          (lambda ()
            ((if (pair? _evaluator_44) (car _evaluator_44) %eval)
             (expand-program
               (call-with-input-file
                 _filename_43
                 (lambda (_in_47) (%read _in_47)))
               _filename_43)))
          (lambda ()
            (let ((_tmp_47 _temp_45))
              (set! _temp_45 *current-source-filename*)
              (set! *current-source-filename* _tmp_47)))))))
  (define %library (lambda (_name_43) (locate-library (stringify _name_43))))
  (define compile #f)
  (define set-toplevel-variable! #f)
  (define %oblist #f)
  (define *unbound* (list '*unbound*))
  (define *uninitialized* (list '*uninitialized*))
  (define *unbound-variables* #f)
  (define *toplevel-environment* #f)
  (define abort-continuation (lambda () (%exit 1)))
  (define report-error
    (let ((_pp_43 pp))
      (lambda (_msg_44 _args_45)
        (let ((_out_46 %error-port))
          (%flush-output %output-port)
          (display "
Error: " _out_46)
          (display _msg_44 _out_46)
          (if (pair? _args_45)
            (begin
              (newline _out_46)
              (for-each
                (lambda (_x_47) (newline _out_46) (_pp_43 _x_47 _out_46))
                _args_45)))
          (newline _out_46)
          (%flush-output _out_46)))))
  (define error-handler
    (lambda (_msg_43 _args_44)
      (report-error _msg_43 _args_44)
      (abort-continuation)))
  (define %error
    (lambda (_msg_43 . _args_44) (error-handler _msg_43 _args_44)))
  (set! expand-error-hook %error)
  (define %with-exception-handler
    (lambda (_handler_43 _thunk_44)
      (let ((_handler_45
              (lambda (_msg_45 _args_46)
                (_handler_43 _msg_45 _args_46)
                (display
                  "
Error: exception-handler returned - terminating.
"
                  %error-port)
                (%exit 1))))
        (let ((_temp_46 _handler_45))
          (dynamic-wind
            (lambda ()
              (let ((_tmp_48 _temp_46))
                (set! _temp_46 error-handler)
                (set! error-handler _tmp_48)))
            (lambda () (%catch-system-errors _handler_45 _thunk_44))
            (lambda ()
              (let ((_tmp_48 _temp_46))
                (set! _temp_46 error-handler)
                (set! error-handler _tmp_48))))))))
  (letrec ((_potentially-unbound_47
             (lambda (_a_55 _set_56)
               (if (if *unbound-variables* (not _set_56) #f)
                 (set! *unbound-variables* (cons _a_55 *unbound-variables*)))))
           (_lookup_48
             (lambda (_sym_55 _env_56 _set_57 _k_58)
               (let ((_oblist_59
                       (%environment-data-oblist *toplevel-environment*)))
                 (let _findenv_60 ((_env_61 _env_56) (_i_62 0))
                   (if (null? _env_61)
                     (let ((_tmp_63 (assq _sym_55 _oblist_59)))
                       (if _tmp_63
                         (let ((_a_64 _tmp_63))
                           (_k_58 #f
                                  (lambda (_e_65) (cdr _a_64))
                                  (lambda (_e_65 _x_66) (set-cdr! _a_64 _x_66))))
                         (let ((_a_64 (cons _sym_55 *unbound*)))
                           (_potentially-unbound_47 _a_64 _set_57)
                           (if (%environment-data-mutable?
                                 *toplevel-environment*)
                             (%environment-data-oblist-set!
                               *toplevel-environment*
                               (cons _a_64 _oblist_59)))
                           (_k_58 #f
                                  (lambda (_e_65) (cdr _a_64))
                                  (lambda (_e_65 _x_66) (set-cdr! _a_64 _x_66))))))
                     (let _findpos_63 ((_b_64 (car _env_61)) (_j_65 0))
                       (if (null? _b_64)
                         (_findenv_60 (cdr _env_61) (+ _i_62 1))
                         (if (eq? _sym_55 (car _b_64))
                           (_k_58 #t
                                  (lambda (_e_66)
                                    (vector-ref (list-ref _e_66 _i_62) _j_65))
                                  (lambda (_e_66 _x_67)
                                    (vector-set! (list-ref _e_66 _i_62) _j_65 _x_67)))
                           (_findpos_63 (cdr _b_64) (+ _j_65 1))))))))))
           (_fudge-argument-list_49
             (lambda (_n_55 _alst_56)
               (let _loop_57 ((_n_58 _n_55)
                              (_c_59 0)
                              (_args_60 _alst_56)
                              (_last_61 #f))
                 (if (= _n_58 0)
                   (begin #f (set-cdr! _last_61 (list _args_60)) _alst_56)
                   (_loop_57
                     (- _n_58 1)
                     (+ _c_59 1)
                     (if (null? _args_60)
                       (%error "bad argument count" _n_58 _c_59)
                       (cdr _args_60))
                     _args_60)))))
           (_proper-list?_50
             (lambda (_x_55)
               (if (null? _x_55)
                 #t
                 (if (pair? _x_55) (_proper-list?_50 (cdr _x_55)) #f))))
           (_check_51
             (lambda (_shape_55 _form_56)
               (letrec ((_fail_57
                          (lambda ()
                            (%error "syntax error" _form_56 _shape_55))))
                 (let _walk_58 ((_x_59 _shape_55) (_y_60 _form_56))
                   (if (eq? _x_59 '_)
                     _form_56
                     (if (eq? _x_59 _y_60)
                       _form_56
                       (if (pair? _x_59)
                         (if (eq? '... (car _x_59))
                           (if (_proper-list?_50 _y_60) _form_56 (_fail_57))
                           (if (pair? _y_60)
                             (begin
                               (_walk_58 (car _x_59) (car _y_60))
                               (_walk_58 (cdr _x_59) (cdr _y_60))
                               _form_56)
                             (_fail_57)))
                         (if (if (vector? _x_59) (vector? _y_60) #f)
                           (_walk_58 (vector->list _x_59) (vector->list _y_60))
                           (_fail_57)))))))))
           (_comp-call_52
             (lambda (_form_55 _env_56 _name_57 _here_58 _tail_59)
               (_comp-call1_53 _form_55 _env_56 _name_57 _here_58 _tail_59)))
           (_comp-call1_53
             (lambda (_form_55 _env_56 _name_57 _here_58 _tail_59)
               (_check_51 '(_ ...) _form_55)
               (let ((_op_60 (car _form_55)))
                 (let ((_op_61
                         (if (%procedure? _op_60)
                           _op_60
                           (_comp_54 (car _form_55) _env_56 #f _here_58 #f))))
                   (let ((_args_62 (cdr _form_55)))
                     (let ((_argc_63 (length _args_62)))
                       (letrec ((_checkf_64
                                  (lambda (_e_65)
                                    (let ((_p_66 (_op_61 _e_65)))
                                      (if (not (%procedure? _p_66))
                                        (%error "call of non-procedure" _p_66))
                                      (let ((_arity_67 (%procedure-arity _p_66)))
                                        (let ((_x_68 (not _arity_67)))
                                          (if _x_68
                                            _x_68
                                            (if (= _argc_63 _arity_67)
                                              (%procedure-code _p_66)
                                              (if (negative? _arity_67)
                                                (let ((_arity_69 (- (abs _arity_67) 1)))
                                                  (if (>= _argc_63 _arity_69)
                                                    (%procedure-code _p_66)
                                                    (%error
                                                      (string-append
                                                        "procedure expected "
                                                        (number->string _arity_69)
                                                        " or more arguments but was called with "
                                                        (number->string _argc_63))
                                                      _p_66)))
                                                (%error
                                                  (string-append
                                                    "procedure expected "
                                                    (number->string _arity_67)
                                                    " argument"
                                                    (if (= _arity_67 1) "" "s")
                                                    " but was called with "
                                                    (number->string _argc_63))
                                                  _p_66))))))))))
                         (let ((_key_65 _argc_63))
                           (if (if (eqv? _key_65 0) #t #f)
                             (lambda (_e_66) ((_checkf_64 _e_66)))
                             (if (if (eqv? _key_65 1) #t #f)
                               (let ((_a1_66
                                       (_comp_54 (car _args_62) _env_56 #f _here_58 #f)))
                                 (lambda (_e_67)
                                   ((_checkf_64 _e_67) (_a1_66 _e_67))))
                               (if (if (eqv? _key_65 2) #t #f)
                                 (let ((_a1_66
                                         (_comp_54 (car _args_62) _env_56 #f _here_58 #f))
                                       (_a2_67
                                         (_comp_54 (cadr _args_62) _env_56 #f _here_58 #f)))
                                   (lambda (_e_68)
                                     ((_checkf_64 _e_68)
                                      (_a1_66 _e_68)
                                      (_a2_67 _e_68))))
                                 (if (if (eqv? _key_65 3) #t #f)
                                   (let ((_a1_66
                                           (_comp_54 (car _args_62) _env_56 #f _here_58 #f))
                                         (_a2_67
                                           (_comp_54 (cadr _args_62) _env_56 #f _here_58 #f))
                                         (_a3_68
                                           (_comp_54
                                             (caddr _args_62)
                                             _env_56
                                             #f
                                             _here_58
                                             #f)))
                                     (lambda (_e_69)
                                       ((_checkf_64 _e_69)
                                        (_a1_66 _e_69)
                                        (_a2_67 _e_69)
                                        (_a3_68 _e_69))))
                                   (if (if (eqv? _key_65 4) #t #f)
                                     (let ((_a1_66
                                             (_comp_54 (car _args_62) _env_56 #f _here_58 #f))
                                           (_a2_67
                                             (_comp_54 (cadr _args_62) _env_56 #f _here_58 #f))
                                           (_a3_68
                                             (_comp_54
                                               (caddr _args_62)
                                               _env_56
                                               #f
                                               _here_58
                                               #f))
                                           (_a4_69
                                             (_comp_54
                                               (cadddr _args_62)
                                               _env_56
                                               #f
                                               _here_58
                                               #f)))
                                       (lambda (_e_70)
                                         ((_checkf_64 _e_70)
                                          (_a1_66 _e_70)
                                          (_a2_67 _e_70)
                                          (_a3_68 _e_70)
                                          (_a4_69 _e_70))))
                                     (let ((_as_66
                                             (map (lambda (_a_66)
                                                    (_comp_54 _a_66 _env_56 #f _here_58 #f))
                                                  _args_62)))
                                       (lambda (_e_67)
                                         (let ((_fn_68 (_checkf_64 _e_67)))
                                           (apply _fn_68
                                                  (map (lambda (_a_69) (_a_69 _e_67)) _as_66))))))))))))))))))
           (_comp_54
             (lambda (_form_55 _env_56 _name_57 _here_58 _tail_59)
               (if (let ((_x_60 (number? _form_55)))
                     (if _x_60
                       _x_60
                       (let ((_x_61 (string? _form_55)))
                         (if _x_61
                           _x_61
                           (let ((_x_62 (char? _form_55)))
                             (if _x_62 _x_62 (boolean? _form_55)))))))
                 (lambda (_e_60) _form_55)
                 (if (symbol? _form_55)
                   (_lookup_48
                     _form_55
                     _env_56
                     #f
                     (lambda (_found_60 _ref_61 _set_62)
                       (lambda (_e_63)
                         (let ((_x_64 (_ref_61 _e_63)))
                           (if _found_60
                             (if (eq? *uninitialized* _x_64)
                               (%error
                                 "reference to uninitialized \"letrec\" variable"
                                 _form_55)
                               _x_64)
                             (if (eq? *unbound* _x_64)
                               (%error "unbound variable" _form_55)
                               _x_64))))))
                   (if (pair? _form_55)
                     (let ((_op_60 (car _form_55)) (_args_61 (cdr _form_55)))
                       (if (symbol? _op_60)
                         (let ((_key_62 _op_60))
                           (if (if (eqv? _key_62 'begin) #t #f)
                             (begin
                               (_check_51 '(begin ...) _form_55)
                               (let ((_n_63 (length _args_61)))
                                 (let ((_key_64 _n_63))
                                   (if (if (eqv? _key_64 0) #t #f)
                                     (lambda (_e_65) (%void))
                                     (if (if (eqv? _key_64 1) #t #f)
                                       (_comp_54
                                         (car _args_61)
                                         _env_56
                                         _name_57
                                         _here_58
                                         _tail_59)
                                       (if (if (eqv? _key_64 2) #t #f)
                                         (let ((_x1_65
                                                 (_comp_54 (car _args_61) _env_56 #f _here_58 #f)))
                                           (let ((_x2_66
                                                   (_comp_54
                                                     (cadr _args_61)
                                                     _env_56
                                                     _name_57
                                                     _here_58
                                                     _tail_59)))
                                             (lambda (_e_67) (_x1_65 _e_67) (_x2_66 _e_67))))
                                         (if (if (eqv? _key_64 3) #t #f)
                                           (let ((_x1_65
                                                   (_comp_54 (car _args_61) _env_56 #f _here_58 #f)))
                                             (let ((_x2_66
                                                     (_comp_54 (cadr _args_61) _env_56 #f _here_58 #f)))
                                               (let ((_x3_67
                                                       (_comp_54
                                                         (caddr _args_61)
                                                         _env_56
                                                         _name_57
                                                         _here_58
                                                         _tail_59)))
                                                 (lambda (_e_68)
                                                   (_x1_65 _e_68)
                                                   (_x2_66 _e_68)
                                                   (_x3_67 _e_68)))))
                                           (if (if (eqv? _key_64 4) #t #f)
                                             (let ((_x1_65
                                                     (_comp_54 (car _args_61) _env_56 #f _here_58 #f)))
                                               (let ((_x2_66
                                                       (_comp_54 (cadr _args_61) _env_56 #f _here_58 #f)))
                                                 (let ((_x3_67
                                                         (_comp_54
                                                           (caddr _args_61)
                                                           _env_56
                                                           #f
                                                           _here_58
                                                           #f)))
                                                   (let ((_x4_68
                                                           (_comp_54
                                                             (cadddr _args_61)
                                                             _env_56
                                                             _name_57
                                                             _here_58
                                                             _tail_59)))
                                                     (lambda (_e_69)
                                                       (_x1_65 _e_69)
                                                       (_x2_66 _e_69)
                                                       (_x3_67 _e_69)
                                                       (_x4_68 _e_69))))))
                                             (let ((_x1_65
                                                     (_comp_54 (car _args_61) _env_56 #f _here_58 #f)))
                                               (let ((_x2_66
                                                       (_comp_54
                                                         (cons 'begin (cdr _args_61))
                                                         _env_56
                                                         _name_57
                                                         _here_58
                                                         _tail_59)))
                                                 (lambda (_e_67) (_x1_65 _e_67) (_x2_66 _e_67))))))))))))
                             (if (if (eqv? _key_62 'quote) #t #f)
                               (begin
                                 (_check_51 ''_ _form_55)
                                 (let ((_c_63 (car _args_61)))
                                   (lambda (_e_64) _c_63)))
                               (if (if (eqv? _key_62 'if) #t #f)
                                 (begin
                                   (_check_51 '(if _ _ ...) _form_55)
                                   (let ((_x1_63
                                           (_comp_54 (car _args_61) _env_56 #f _here_58 #f)))
                                     (let ((_x2_64
                                             (_comp_54
                                               (cadr _args_61)
                                               _env_56
                                               _name_57
                                               _here_58
                                               _tail_59)))
                                       (let ((_x3_65
                                               (if (pair? (cddr _args_61))
                                                 (_comp_54
                                                   (caddr _args_61)
                                                   _env_56
                                                   _name_57
                                                   _here_58
                                                   _tail_59)
                                                 (lambda (_e_65) (%void)))))
                                         (lambda (_e_66)
                                           (if (_x1_63 _e_66) (_x2_64 _e_66) (_x3_65 _e_66)))))))
                                 (if (if (eqv? _key_62 'set!)
                                       #t
                                       (if (eqv? _key_62 'define) #t #f))
                                   (begin
                                     (_check_51 (cons (car _form_55) '(_ _)) _form_55)
                                     (let ((_var_63 (car _args_61)))
                                       (let ((_val_64
                                               (_comp_54
                                                 (cadr _args_61)
                                                 _env_56
                                                 _var_63
                                                 _here_58
                                                 #f)))
                                         (if (%environment-data-mutable?
                                               *toplevel-environment*)
                                           (_lookup_48
                                             _var_63
                                             _env_56
                                             #t
                                             (lambda (_found_65 _ref_66 _set_67)
                                               (lambda (_e_68)
                                                 (_set_67 _e_68 (_val_64 _e_68))
                                                 (%void))))
                                           (let ((_name_65
                                                   (%environment-data-name *toplevel-environment*)))
                                             (lambda (_e_66)
                                               (%error "environment is not mutable" _name_65)))))))
                                   (if (if (eqv? _key_62 'let) #t #f)
                                     (begin
                                       (_check_51 '(let _ _ ...) _form_55)
                                       (let ((_bindings_63 (car _args_61)))
                                         (let ((_n_64 (length _bindings_63)))
                                           (if (zero? _n_64)
                                             (_comp_54
                                               (cons 'begin (cdr _args_61))
                                               _env_56
                                               _name_57
                                               _here_58
                                               _tail_59)
                                             (let ((_vars_65 (map car _bindings_63)))
                                               (let ((_env2_66 (cons _vars_65 _env_56)))
                                                 (let ((_vals_67
                                                         (map (lambda (_b_67)
                                                                (_comp_54
                                                                  (cadr _b_67)
                                                                  _env_56
                                                                  (car _b_67)
                                                                  _here_58
                                                                  #f))
                                                              _bindings_63)))
                                                   (let ((_body_68
                                                           (_comp_54
                                                             (cons 'begin (cdr _args_61))
                                                             _env2_66
                                                             _name_57
                                                             _here_58
                                                             _tail_59)))
                                                     (lambda (_e_69)
                                                       (let ((_v_70 (make-vector _n_64 (%void))))
                                                         (let ((_e2_71 (cons _v_70 _e_69)))
                                                           (let _loop_72 ((_i_73 0) (_vals_74 _vals_67))
                                                             (if (>= _i_73 _n_64)
                                                               #f
                                                               (begin
                                                                 (vector-set! _v_70 _i_73 ((car _vals_74) _e2_71))
                                                                 (_loop_72 (+ _i_73 1) (cdr _vals_74)))))
                                                           (_body_68 _e2_71))))))))))))
                                     (if (if (eqv? _key_62 'letrec) #t #f)
                                       (begin
                                         (_check_51 '(letrec _ _ ...) _form_55)
                                         (let ((_bindings_63 (car _args_61)))
                                           (let ((_n_64 (length _bindings_63)))
                                             (if (zero? _n_64)
                                               (_comp_54
                                                 (cons 'begin (cdr _args_61))
                                                 _env_56
                                                 _name_57
                                                 _here_58
                                                 _tail_59)
                                               (let ((_vars_65 (map car _bindings_63)))
                                                 (let ((_env2_66 (cons _vars_65 _env_56)))
                                                   (let ((_vals_67
                                                           (map (lambda (_b_67)
                                                                  (_comp_54
                                                                    (cadr _b_67)
                                                                    _env2_66
                                                                    (car _b_67)
                                                                    _here_58
                                                                    #f))
                                                                _bindings_63)))
                                                     (let ((_body_68
                                                             (_comp_54
                                                               (cons 'begin (cdr _args_61))
                                                               _env2_66
                                                               _name_57
                                                               _here_58
                                                               _tail_59)))
                                                       (lambda (_e_69)
                                                         (let ((_v_70 (make-vector _n_64 *uninitialized*)))
                                                           (let ((_tv_71 (make-vector _n_64)))
                                                             (let ((_e2_72 (cons _v_70 _e_69)))
                                                               (let _loop_73 ((_i_74 0) (_vals_75 _vals_67))
                                                                 (if (>= _i_74 _n_64)
                                                                   #f
                                                                   (begin
                                                                     (vector-set!
                                                                       _tv_71
                                                                       _i_74
                                                                       ((car _vals_75) _e2_72))
                                                                     (_loop_73 (+ _i_74 1) (cdr _vals_75)))))
                                                               (let _loop_73 ((_i_74 0))
                                                                 (if (>= _i_74 _n_64)
                                                                   #f
                                                                   (begin
                                                                     (vector-set!
                                                                       _v_70
                                                                       _i_74
                                                                       (vector-ref _tv_71 _i_74))
                                                                     (_loop_73 (+ _i_74 1)))))
                                                               (_body_68 _e2_72)))))))))))))
                                       (if (if (eqv? _key_62 'lambda) #t #f)
                                         (begin
                                           (_check_51 '(lambda _ _ ...) _form_55)
                                           (let ((_llist_63 (car _args_61)))
                                             (letrec ((_comp-lambda_64
                                                        (lambda (_vars_65 _argc_66 _rest_67)
                                                          (if (null? _vars_65)
                                                            (let ((_body_68
                                                                    (_comp_54
                                                                      (cons 'begin (cdr _args_61))
                                                                      _env_56
                                                                      #f
                                                                      _name_57
                                                                      #t)))
                                                              (lambda (_e_69)
                                                                (%procedure
                                                                  (lambda () (_body_68 _e_69))
                                                                  _name_57
                                                                  0)))
                                                            (let ((_env2_68 (cons _vars_65 _env_56)))
                                                              (let ((_body_69
                                                                      (_comp_54
                                                                        (cons 'begin (cdr _args_61))
                                                                        _env2_68
                                                                        #f
                                                                        _name_57
                                                                        #t)))
                                                                (let ((_key_70 _argc_66))
                                                                  (if (if (eqv? _key_70 0) #t #f)
                                                                    (lambda (_e_71)
                                                                      (%procedure
                                                                        (lambda _r_72
                                                                          (_body_69 (cons (vector _r_72) _e_71)))
                                                                        _name_57
                                                                        -1))
                                                                    (if (if (eqv? _key_70 1) #t #f)
                                                                      (if _rest_67
                                                                        (lambda (_e_71)
                                                                          (%procedure
                                                                            (lambda (_a1_72 . _r_73)
                                                                              (_body_69 (cons (vector _a1_72 _r_73) _e_71)))
                                                                            _name_57
                                                                            -2))
                                                                        (lambda (_e_71)
                                                                          (%procedure
                                                                            (lambda (_a1_72)
                                                                              (_body_69 (cons (vector _a1_72) _e_71)))
                                                                            _name_57
                                                                            1)))
                                                                      (if (if (eqv? _key_70 2) #t #f)
                                                                        (if _rest_67
                                                                          (lambda (_e_71)
                                                                            (%procedure
                                                                              (lambda (_a1_72 _a2_73 . _r_74)
                                                                                (_body_69
                                                                                  (cons (vector _a1_72 _a2_73 _r_74) _e_71)))
                                                                              _name_57
                                                                              -3))
                                                                          (lambda (_e_71)
                                                                            (%procedure
                                                                              (lambda (_a1_72 _a2_73)
                                                                                (_body_69 (cons (vector _a1_72 _a2_73) _e_71)))
                                                                              _name_57
                                                                              2)))
                                                                        (if (if (eqv? _key_70 3) #t #f)
                                                                          (if _rest_67
                                                                            (lambda (_e_71)
                                                                              (%procedure
                                                                                (lambda (_a1_72 _a2_73 _a3_74 . _r_75)
                                                                                  (_body_69
                                                                                    (cons (vector _a1_72 _a2_73 _a3_74 _r_75) _e_71)))
                                                                                _name_57
                                                                                -4))
                                                                            (lambda (_e_71)
                                                                              (%procedure
                                                                                (lambda (_a1_72 _a2_73 _a3_74)
                                                                                  (_body_69
                                                                                    (cons (vector _a1_72 _a2_73 _a3_74) _e_71)))
                                                                                _name_57
                                                                                3)))
                                                                          (if (if (eqv? _key_70 4) #t #f)
                                                                            (if _rest_67
                                                                              (lambda (_e_71)
                                                                                (%procedure
                                                                                  (lambda (_a1_72 _a2_73 _a3_74 _a4_75 . _r_76)
                                                                                    (_body_69
                                                                                      (cons (vector _a1_72 _a2_73 _a3_74 _a4_75 _r_76)
                                                                                            _e_71)))
                                                                                  _name_57
                                                                                  -5))
                                                                              (lambda (_e_71)
                                                                                (%procedure
                                                                                  (lambda (_a1_72 _a2_73 _a3_74 _a4_75)
                                                                                    (_body_69
                                                                                      (cons (vector _a1_72 _a2_73 _a3_74 _a4_75) _e_71)))
                                                                                  _name_57
                                                                                  4)))
                                                                            (if _rest_67
                                                                              (lambda (_e_71)
                                                                                (%procedure
                                                                                  (lambda _as_72
                                                                                    (_body_69
                                                                                      (cons (apply vector
                                                                                                   (_fudge-argument-list_49 _argc_66 _as_72))
                                                                                            _e_71)))
                                                                                  _name_57
                                                                                  (- (+ _argc_66 1))))
                                                                              (lambda (_e_71)
                                                                                (%procedure
                                                                                  (lambda _as_72
                                                                                    (_body_69 (cons (list->vector _as_72) _e_71)))
                                                                                  _name_57
                                                                                  _argc_66)))))))))))))))
                                               (let _loop_65 ((_ll_66 _llist_63) (_vars_67 '()) (_argc_68 0))
                                                 (if (null? _ll_66)
                                                   (_comp-lambda_64 (reverse _vars_67) _argc_68 #f)
                                                   (if (symbol? _ll_66)
                                                     (_comp-lambda_64
                                                       (reverse (cons _ll_66 _vars_67))
                                                       _argc_68
                                                       _ll_66)
                                                     (if (pair? _ll_66)
                                                       (if (symbol? (car _ll_66))
                                                         (_loop_65
                                                           (cdr _ll_66)
                                                           (cons (car _ll_66) _vars_67)
                                                           (+ _argc_68 1))
                                                         (%error "invalid lambda-list syntax" _llist_63))
                                                       (%error "invalid lambda-list syntax" _llist_63))))))))
                                         (_comp-call_52
                                           _form_55
                                           _env_56
                                           _name_57
                                           _here_58
                                           _tail_59)))))))))
                         (_comp-call_52
                           _form_55
                           _env_56
                           _name_57
                           _here_58
                           _tail_59)))
                     (%error "invalid form" _form_55)))))))
    (set! compile
      (lambda (_form_55 _env_56)
        (let ((_data_57 (%environment-data _env_56)))
          (let ((_temp_58 _data_57))
            (dynamic-wind
              (lambda ()
                (let ((_tmp_60 _temp_58))
                  (set! _temp_58 *toplevel-environment*)
                  (set! *toplevel-environment* _tmp_60)))
              (lambda () (_comp_54 _form_55 '() #f '<toplevel> #t))
              (lambda ()
                (let ((_tmp_60 _temp_58))
                  (set! _temp_58 *toplevel-environment*)
                  (set! *toplevel-environment* _tmp_60))))))))
    (set! set-toplevel-variable!
      (lambda (_name_55 _val_56 _env_57)
        (let ((_data_58 (%environment-data _env_57)))
          (let ((_oblist_59 (%environment-data-oblist _data_58)))
            (let ((_tmp_60 (assq _name_55 _oblist_59)))
              (if _tmp_60
                (let ((_a_61 _tmp_60)) (set-cdr! _a_61 _val_56))
                (vector-set!
                  _data_58
                  1
                  (cons (cons _name_55 _val_56) _oblist_59))))))))
    (set! %oblist
      (lambda _env_55
        (let ((_env_56
                (if (pair? _env_55) (car _env_55) %interaction-environment)))
          (let ((_data_57 (%environment-data _env_56)))
            (append
              (let _loop_58 ((_lst_59
                               (car (%environment-data-mstore _data_57))))
                (let ((_v_60 _lst_59))
                  (let ((_failure_61
                          (lambda ()
                            (let ((_failure_61
                                    (lambda ()
                                      (let ((_failure_61
                                              (lambda () (error 'match "no matching pattern"))))
                                        (if (pair? _v_60)
                                          (let ((_w_62 (car _v_60)) (_x_63 (cdr _v_60)))
                                            (let ((_x_65 _w_62))
                                              (let ((_more_67 _x_63))
                                                (cons _x_65 (_loop_58 _more_67)))))
                                          (_failure_61))))))
                              (if (pair? _v_60)
                                (let ((_w_62 (car _v_60)) (_x_63 (cdr _v_60)))
                                  (if (pair? _w_62)
                                    (let ((_w_64 (car _w_62)) (_x_65 (cdr _w_62)))
                                      (if (number? _w_64)
                                        (let ((_more_68 _x_63)) (_loop_58 _more_68))
                                        (_failure_61)))
                                    (_failure_61)))
                                (_failure_61))))))
                    (if (null? _v_60)
                      (%environment-data-oblist _data_57)
                      (_failure_61)))))))))))
  (define %eval
    (let ((_compile_43 compile))
      (lambda (_x_44 . _env_45)
        (let ((_env_46
                (if (pair? _env_45) (car _env_45) %interaction-environment)))
          ((_compile_43 (%expand _x_44 _env_46) _env_46) '())))))
  (define *current-source-filename* #f)
  (define %load
    (lambda (_filename_43 . _evaluator_44)
      (let ((_eval_45 (if (pair? _evaluator_44) (car _evaluator_44) %eval))
            (_in_46 (open-input-file _filename_43))
            (_ac_47 abort-continuation))
        (let ((_temp_48 _filename_43))
          (let ((_temp_49 (lambda () (close-input-port _in_46) (_ac_47))))
            (dynamic-wind
              (lambda ()
                (let ((_tmp_51 _temp_49))
                  (set! _temp_49 abort-continuation)
                  (set! abort-continuation _tmp_51))
                (let ((_tmp_51 _temp_48))
                  (set! _temp_48 *current-source-filename*)
                  (set! *current-source-filename* _tmp_51)))
              (lambda ()
                (let _loop_51 ()
                  (let ((_x_52 (%read _in_46)))
                    (if (not (eof-object? _x_52))
                      (begin (_eval_45 _x_52) (_loop_51)))))
                (close-input-port _in_46))
              (lambda ()
                (let ((_tmp_51 _temp_49))
                  (set! _temp_49 abort-continuation)
                  (set! abort-continuation _tmp_51))
                (let ((_tmp_51 _temp_48))
                  (set! _temp_48 *current-source-filename*)
                  (set! *current-source-filename* _tmp_51)))))))))
  (define *repl-level* 0)
  (define repl-exit (lambda (_result_43) (%exit)))
  (define prompt
    (lambda (_level_43)
      (display (string-append (make-string _level_43 #\>) " ") %output-port)
      (%flush-output %output-port)))
  (define %repl-prompt prompt)
  (define %quit
    (lambda _result_43
      (repl-exit (if (pair? _result_43) (car _result_43) (%void)))))
  (define %repl
    (lambda _evaluator_43
      (%call-with-exit-continuation
        (lambda (_quit_44)
          (let ((_temp_45 (+ *repl-level* 1)))
            (let ((_temp_46 _quit_44))
              (let ((_temp_47 '()))
                (dynamic-wind
                  (lambda ()
                    (let ((_tmp_49 _temp_47))
                      (set! _temp_47 *unbound-variables*)
                      (set! *unbound-variables* _tmp_49))
                    (let ((_tmp_49 _temp_46))
                      (set! _temp_46 repl-exit)
                      (set! repl-exit _tmp_49))
                    (let ((_tmp_49 _temp_45))
                      (set! _temp_45 *repl-level*)
                      (set! *repl-level* _tmp_49)))
                  (lambda ()
                    (letrec ((_report-unbound_49
                               (lambda ()
                                 (let _loop_50 ((_vars_51 *unbound-variables*) (_ub_52 '()))
                                   (let ((_v_53 _vars_51))
                                     (let ((_failure_54
                                             (lambda ()
                                               (let ((_failure_54
                                                       (lambda () (error 'match "no matching pattern"))))
                                                 (if (pair? _v_53)
                                                   (let ((_w_55 (car _v_53)) (_x_56 (cdr _v_53)))
                                                     (let ((_a_59 _w_55))
                                                       (if (pair? _w_55)
                                                         (let ((_w_60 (car _w_55)) (_x_61 (cdr _w_55)))
                                                           (let ((_val_63 _x_61))
                                                             (let ((_more_65 _x_56))
                                                               (_loop_50
                                                                 _more_65
                                                                 (if (eq? *unbound* _val_63)
                                                                   (cons _a_59 _ub_52)
                                                                   _ub_52)))))
                                                         (_failure_54))))
                                                   (_failure_54))))))
                                       (if (null? _v_53)
                                         (if (pair? _ub_52)
                                           (let ((_out_55 %error-port))
                                             (display
                                               "Warning: the following global variables are currently unbound:

"
                                               _out_55)
                                             (for-each
                                               (lambda (_a_56)
                                                 (display "  " _out_55)
                                                 (display (car _a_56) _out_55)
                                                 (newline _out_55))
                                               _ub_52)
                                             (newline _out_55)))
                                         (_failure_54))))))))
                      (let ((_in_50 %input-port))
                        (let ((_out_51 %output-port))
                          (let ((_err_52 %error-port))
                            (let ((_eval_53
                                    (if (pair? _evaluator_43)
                                      (car _evaluator_43)
                                      %eval)))
                              (if (%environment-data-mutable?
                                    (%environment-data %interaction-environment))
                                (set-toplevel-variable!
                                  'it
                                  (%void)
                                  %interaction-environment))
                              (%call-with-exit-continuation
                                (lambda (_return_54)
                                  (let _loop_55 ()
                                    (let ((_prompt-returned_56 #f))
                                      (%call-with-exit-continuation
                                        (lambda (_k_57)
                                          (let ((_temp_58 _in_50))
                                            (let ((_temp_59 _out_51))
                                              (let ((_temp_60 _err_52))
                                                (let ((_temp_61 (lambda () (_k_57 #f))))
                                                  (dynamic-wind
                                                    (lambda ()
                                                      (let ((_tmp_63 _temp_61))
                                                        (set! _temp_61 abort-continuation)
                                                        (set! abort-continuation _tmp_63))
                                                      (let ((_tmp_63 _temp_60))
                                                        (set! _temp_60 %error-port)
                                                        (set! %error-port _tmp_63))
                                                      (let ((_tmp_63 _temp_59))
                                                        (set! _temp_59 %output-port)
                                                        (set! %output-port _tmp_63))
                                                      (let ((_tmp_63 _temp_58))
                                                        (set! _temp_58 %input-port)
                                                        (set! %input-port _tmp_63)))
                                                    (lambda ()
                                                      (set! *unbound-variables* '())
                                                      (%repl-prompt *repl-level*)
                                                      (set! _prompt-returned_56 #t)
                                                      (%with-exception-handler
                                                        (lambda (_msg_63 _args_64)
                                                          (report-error _msg_63 _args_64)
                                                          (_k_57 #f))
                                                        (lambda ()
                                                          (let ((_x_63 (%read)))
                                                            (if (eof-object? _x_63)
                                                              (_return_54 _x_63)
                                                              (begin
                                                                (if (eqv? (%peek-char) #\newline) (%read-char))
                                                                (call-with-values
                                                                  (lambda () (_eval_53 _x_63))
                                                                  (lambda _rs_64
                                                                    (_report-unbound_49)
                                                                    (let ((_v_65 _rs_64))
                                                                      (let ((_failure_66
                                                                              (lambda ()
                                                                                (let ((_failure_66
                                                                                        (lambda ()
                                                                                          (let ((_failure_66
                                                                                                  (lambda () (error 'match "no matching pattern"))))
                                                                                            (if (pair? _v_65)
                                                                                              (let ((_w_67 (car _v_65)) (_x_68 (cdr _v_65)))
                                                                                                (let ((_r1_70 _w_67))
                                                                                                  (let ((_rs_72 _x_68))
                                                                                                    (if (%environment-data-mutable?
                                                                                                          (%environment-data %interaction-environment))
                                                                                                      (set-toplevel-variable!
                                                                                                        'it
                                                                                                        _r1_70
                                                                                                        %interaction-environment))
                                                                                                    (for-each pp (cons _r1_70 _rs_72))
                                                                                                    (%flush-output %output-port))))
                                                                                              (_failure_66))))))
                                                                                  (if (null? _v_65) #f (_failure_66))))))
                                                                        (if (if (pair? _v_65) (null? (cdr _v_65)) #f)
                                                                          (let ((_w_67 (car _v_65)))
                                                                            (if (let ((_x_68 _w_67)) (eq? _x_68 (%void)))
                                                                              #f
                                                                              (_failure_66)))
                                                                          (_failure_66))))))))))))
                                                    (lambda ()
                                                      (let ((_tmp_63 _temp_61))
                                                        (set! _temp_61 abort-continuation)
                                                        (set! abort-continuation _tmp_63))
                                                      (let ((_tmp_63 _temp_60))
                                                        (set! _temp_60 %error-port)
                                                        (set! %error-port _tmp_63))
                                                      (let ((_tmp_63 _temp_59))
                                                        (set! _temp_59 %output-port)
                                                        (set! %output-port _tmp_63))
                                                      (let ((_tmp_63 _temp_58))
                                                        (set! _temp_58 %input-port)
                                                        (set! %input-port _tmp_63))))))))))
                                      (if (not _prompt-returned_56)
                                        (begin
                                          (display
                                            "Error in \"repl-prompt\" procedure - restoring default prompt
"
                                            %error-port)
                                          (set! %repl-prompt prompt))))
                                    (_loop_55))))
                              (newline _out_51)))))))
                  (lambda ()
                    (let ((_tmp_49 _temp_47))
                      (set! _temp_47 *unbound-variables*)
                      (set! *unbound-variables* _tmp_49))
                    (let ((_tmp_49 _temp_46))
                      (set! _temp_46 repl-exit)
                      (set! repl-exit _tmp_49))
                    (let ((_tmp_49 _temp_45))
                      (set! _temp_45 *repl-level*)
                      (set! *repl-level* _tmp_49)))))))))))
  (define %interaction-environment
    (%make-environment "interaction-environment" '() (%null-mstore) #t))
  (define %copy-environment
    (lambda (_env_43 _name_44 _mutable_45)
      (letrec ((_copy-alist_46
                 (lambda (_lst_47)
                   (let _loop_48 ((_lst_49 _lst_47) (_acc_50 '()))
                     (if (null? _lst_49)
                       (reverse _acc_50)
                       (_loop_48
                         (cdr _lst_49)
                         (cons (cons (caar _lst_49) (cdar _lst_49))
                               _acc_50)))))))
        (let ((_v_47 (%environment-data _env_43)))
          (let ((_failure_48 (lambda () (error 'match "no matching pattern"))))
            (if (vector? _v_47)
              (let ((_len_50 (vector-length _v_47)))
                (if (= _len_50 (+ (+ (+ (+ 0 1) 1) 1) 1))
                  (let ((_w_51 (vector-ref _v_47 0)))
                    (let ((_w_52 (vector-ref _v_47 (+ 0 1))))
                      (let ((_oblist_54 _w_52))
                        (let ((_w_55 (vector-ref _v_47 (+ (+ 0 1) 1))))
                          (let ((_mstore_57 _w_55))
                            (let ((_w_58 (vector-ref _v_47 (+ (+ (+ 0 1) 1) 1))))
                              (let ((_omutable_60 _w_58))
                                (%make-environment
                                  (if (string? _name_44)
                                    _name_44
                                    (if (symbol? _name_44)
                                      (symbol->string _name_44)
                                      (error "bad environment name" _name_44)))
                                  (if (let ((_x_61 _omutable_60))
                                        (if _x_61 _x_61 _mutable_45))
                                    (_copy-alist_46 _oblist_54)
                                    _oblist_54)
                                  (if (let ((_x_61 _omutable_60))
                                        (if _x_61 _x_61 _mutable_45))
                                    (cons (_copy-alist_46 (car _mstore_57))
                                          (cdr _mstore_57))
                                    _mstore_57)
                                  _mutable_45))))))))
                  (_failure_48)))
              (_failure_48)))))))
  (define %null-environment/5
    (%make-environment "null-environment" '() (%null-mstore) #f))
  (letrec ((_def_43
             (lambda (_proc_52 _names_53 _argc_54)
               (for-each
                 (lambda (_name_55)
                   (let ((_p_56 (%procedure _proc_52 _name_55 _argc_54)))
                     (set-toplevel-variable!
                       (string->symbol _name_55)
                       _p_56
                       %interaction-environment)))
                 (if (list? _names_53) _names_53 (list _names_53)))))
           (_def*_44
             (lambda (_proc_52 _names_53 _argc_54)
               (for-each
                 (lambda (_name_55)
                   (let ((_p_56 (%procedure _proc_52 _name_55 (- (+ _argc_54 1)))))
                     (set-toplevel-variable!
                       (string->symbol _name_55)
                       _p_56
                       %interaction-environment)))
                 (if (list? _names_53) _names_53 (list _names_53)))))
           (_check_45
             (lambda (_x_52 _pred_53 _name_54 _loc_55)
               (if (_pred_53 _x_52)
                 _x_52
                 (%error
                   (string-append
                     "argument to `"
                     _loc_55
                     "' is not of correct type ("
                     _name_54
                     ")")
                   _x_52))))
           (_check-procedure_46
             (lambda (_x_52 _loc_53)
               (%procedure-code
                 (_check_45 _x_52 %procedure? "procedure" _loc_53))))
           (_check-port_47
             (lambda (_x_52 _loc_53)
               (_check_45
                 _x_52
                 (lambda (_x_54)
                   (let ((_x_55 (input-port? _x_54)))
                     (if _x_55 _x_55 (output-port? _x_54))))
                 "port"
                 _loc_53)))
           (_check-string_48
             (lambda (_x_52 _loc_53)
               (_check_45
                 _x_52
                 (lambda (_x_54) (string? _x_54))
                 "string"
                 _loc_53)))
           (_check-environment_49
             (lambda (_x_52 _loc_53)
               (_check_45 _x_52 %environment? "environment" _loc_53)))
           (_check-report_50
             (lambda (_r_52 _loc_53)
               (if (eqv? _r_52 5)
                 _r_52
                 (%error "unsupported Scheme report" _r_52))))
           (_undefd_51
             (lambda (_p_52)
               (lambda _args_53 (apply _p_52 _args_53) (%void)))))
    (_def_43 not "not" 1)
    (_def_43 boolean? "boolean?" 1)
    (_def_43 eq? "eq?" 2)
    (_def_43 eqv? "eqv?" 2)
    (_def_43 equal? "equal?" 2)
    (_def_43 pair? "pair?" 1)
    (_def_43 cons '("cons" "%%cons") 2)
    (_def_43 car "car" 1)
    (_def_43 cdr "cdr" 1)
    (_def_43 caar "caar" 1)
    (_def_43 cadr "cadr" 1)
    (_def_43 cdar "cdar" 1)
    (_def_43 cddr "cddr" 1)
    (_def_43 caaar "caaar" 1)
    (_def_43 caadr "caadr" 1)
    (_def_43 cadar "cadar" 1)
    (_def_43 caddr "caddr" 1)
    (_def_43 cdaar "cdaar" 1)
    (_def_43 cdadr "cdadr" 1)
    (_def_43 cddar "cddar" 1)
    (_def_43 cdddr "cdddr" 1)
    (_def_43 caaaar "caaaar" 1)
    (_def_43 caaadr "caaadr" 1)
    (_def_43 caadar "caadar" 1)
    (_def_43 caaddr "caaddr" 1)
    (_def_43 cadaar "cadaar" 1)
    (_def_43 cadadr "cadadr" 1)
    (_def_43 caddar "caddar" 1)
    (_def_43 cadddr "cadddr" 1)
    (_def_43 cdaaar "cdaaar" 1)
    (_def_43 cdaadr "cdaadr" 1)
    (_def_43 cdadar "cdadar" 1)
    (_def_43 cdaddr "cdaddr" 1)
    (_def_43 cddaar "cddaar" 1)
    (_def_43 cddadr "cddadr" 1)
    (_def_43 cdddar "cdddar" 1)
    (_def_43 cddddr "cddddr" 1)
    (_def_43
      (lambda (_x_53 _y_54) (set-car! _x_53 _y_54) (%void))
      "set-car!"
      2)
    (_def_43
      (lambda (_x_53 _y_54) (set-cdr! _x_53 _y_54) (%void))
      "set-cdr!"
      2)
    (_def_43 null? "null?" 1)
    (_def_43 list? "list?" 1)
    (_def*_44 list '("list" "%%list") 0)
    (_def_43 length "length" 1)
    (_def_43 list-tail "list-tail" 2)
    (_def_43 list-ref "list-ref" 2)
    (_def*_44 append '("append" "%%append") 1)
    (_def_43 reverse "reverse" 1)
    (_def_43 memq "memq" 2)
    (_def_43 memv "memv" 2)
    (_def_43 member "member" 2)
    (_def_43 assq "assq" 2)
    (_def_43 assv "assv" 2)
    (_def_43 assoc "assoc" 2)
    (_def_43 symbol? "symbol?" 1)
    (_def_43 symbol->string "symbol->string" 1)
    (_def_43 string->symbol "string->symbol" 1)
    (_def_43 number? "number?" 1)
    (_def_43 integer? "integer?" 1)
    (_def_43 exact? "exact?" 1)
    (_def_43 real? "real?" 1)
    (_def_43 complex? "complex?" 1)
    (_def_43 inexact? "inexact?" 1)
    (_def_43 rational? "rational?" 1)
    (_def_43 zero? "zero?" 1)
    (_def_43 odd? "odd?" 1)
    (_def_43 even? "even?" 1)
    (_def_43 positive? "positive?" 1)
    (_def_43 negative? "negative?" 1)
    (_def*_44 max "max" 1)
    (_def*_44 min "min" 1)
    (_def*_44 + "+" 0)
    (_def*_44 - "-" 1)
    (_def*_44 * "*" 0)
    (_def*_44 / "/" 1)
    (_def*_44 = "=" 2)
    (_def*_44 > ">" 2)
    (_def*_44 < "<" 2)
    (_def*_44 >= ">=" 2)
    (_def*_44 <= "<=" 2)
    (_def_43 quotient "quotient" 2)
    (_def_43 remainder "remainder" 2)
    (_def_43 modulo "modulo" 2)
    (_def_43 gcd "gcd" 2)
    (_def_43 lcm "lcm" 2)
    (_def_43 abs "abs" 1)
    (_def_43 floor "floor" 1)
    (_def_43 ceiling "ceiling" 1)
    (_def_43 truncate "truncate" 1)
    (_def_43 round "round" 1)
    (_def_43 exact->inexact "exact->inexact" 1)
    (_def_43 inexact->exact "inexact->exact" 1)
    (_def_43 exp "exp" 1)
    (_def_43 log "log" 1)
    (_def_43 expt "expt" 2)
    (_def_43 sqrt "sqrt" 1)
    (_def_43 sin "sin" 1)
    (_def_43 cos "cos" 1)
    (_def_43 tan "tan" 1)
    (_def_43 asin "asin" 1)
    (_def_43 acos "acos" 1)
    (_def*_44 atan "atan" 1)
    (_def_43 numerator "numerator" 1)
    (_def_43 denominator "denominator" 1)
    (_def_43 magnitude "magnitude" 1)
    (_def_43 angle "angle" 1)
    (_def_43 make-rectangular "make-rectangular" 2)
    (_def_43 make-polar "make-polar" 2)
    (_def_43 real-part "real-part" 1)
    (_def_43 imag-part "imag-part" 1)
    (_def_43 rationalize "rationalize" 2)
    (_def*_44 number->string "number->string" 1)
    (_def*_44 string->number "string->number" 1)
    (_def_43 char? "char?" 1)
    (_def_43 char=? "char=?" 2)
    (_def_43 char>? "char>?" 2)
    (_def_43 char<? "char<?" 2)
    (_def_43 char>=? "char>=?" 2)
    (_def_43 char<=? "char<=?" 2)
    (_def_43 char-ci=? "char-ci=?" 1)
    (_def_43 char-ci<? "char-ci<?" 1)
    (_def_43 char-ci>? "char-ci>?" 1)
    (_def_43 char-ci>=? "char-ci>=?" 1)
    (_def_43 char-ci<=? "char-ci<=?" 1)
    (_def_43 char-alphabetic? "char-alphabetic?" 1)
    (_def_43 char-whitespace? "char-whitespace?" 1)
    (_def_43 char-numeric? "char-numeric?" 1)
    (_def_43 char-upper-case? "char-upper-case?" 1)
    (_def_43 char-lower-case? "char-lower-case?" 1)
    (_def_43 char-upcase "char-upcase" 1)
    (_def_43 char-downcase "char-downcase" 1)
    (_def_43 char->integer "char->integer" 1)
    (_def_43 integer->char "integer->char" 1)
    (_def_43 string? "string?" 1)
    (_def_43 string=? "string=?" 2)
    (_def_43 string>? "string>?" 2)
    (_def_43 string<? "string<?" 2)
    (_def_43 string>=? "string>=?" 2)
    (_def_43 string<=? "string<=?" 2)
    (_def_43 string-ci=? "string-ci=?" 2)
    (_def_43 string-ci<? "string-ci<?" 2)
    (_def_43 string-ci>? "string-ci>?" 2)
    (_def_43 string-ci>=? "string-ci>=?" 2)
    (_def_43 string-ci<=? "string-ci<=?" 2)
    (_def*_44 make-string "make-string" 1)
    (_def_43 string-length "string-length" 1)
    (_def_43 string-ref "string-ref" 2)
    (_def_43 string-set! "string-set!" 3)
    (_def*_44 string-append "string-append" 1)
    (_def_43 string-copy "string-copy" 1)
    (_def_43 string->list "string->list" 1)
    (_def_43 list->string "list->string" 1)
    (_def_43 substring "substring" 3)
    (_def_43
      (lambda (_x_53 _y_54) (string-fill! _x_53 _y_54) (%void))
      "string-fill!"
      2)
    (_def_43
      (lambda (_x_53)
        (if (vector? _x_53)
          (let ((_x_54 (zero? (vector-length _x_53))))
            (if _x_54
              _x_54
              (if (not (%procedure? _x_53))
                (not (%disjoint-type-instance? _x_53))
                #f)))
          #f))
      "vector?"
      1)
    (_def*_44 make-vector "make-vector" 1)
    (_def_43 vector-ref "vector-ref" 2)
    (_def_43
      (lambda (_x_53 _y_54 _z_55) (vector-set! _x_53 _y_54 _z_55) (%void))
      "vector-set!"
      3)
    (_def*_44 string "string" 0)
    (_def*_44 vector '("vector" "%%vector") 0)
    (_def_43 vector-length "vector-length" 1)
    (_def_43 vector->list "vector->list" 1)
    (_def_43 list->vector '("list->vector" "%%list->vector") 1)
    (_def_43
      (lambda (_x_53 _y_54) (vector-fill! _x_53 _y_54) (%void))
      "vector-fill!"
      2)
    (_def_43 %procedure? "procedure?" 1)
    (_def*_44
      (lambda (_proc_53 . _args_54)
        (apply map (_check-procedure_46 _proc_53 "map") _args_54))
      '("map" "%%map")
      2)
    (_def*_44
      (lambda (_proc_53 . _args_54)
        (apply for-each (_check-procedure_46 _proc_53 "for-each") _args_54)
        (%void))
      "for-each"
      2)
    (_def*_44
      (lambda (_proc_53 _arg1_54 . _args_55)
        (apply apply
               (_check-procedure_46 _proc_53 "apply")
               (cons _arg1_54 _args_55)))
      "apply"
      2)
    (_def_43 %force "force" 1)
    (_def_43
      (lambda (_proc_53)
        (let ((_proc_54
                (_check-procedure_46
                  _proc_53
                  "call-with-current-continuation")))
          (call-with-current-continuation
            (lambda (_k_55)
              (_proc_54 (%procedure _k_55 "[continuation]" -1))))))
      "call-with-current-continuation"
      1)
    (_def_43
      (lambda (_a_53 _b_54 _c_55)
        (dynamic-wind
          (_check-procedure_46 _a_53 "dynamic-wind")
          (_check-procedure_46 _b_54 "dynamic-wind")
          (_check-procedure_46 _c_55 "dynamic-wind")))
      "dynamic-wind"
      3)
    (_def*_44 values "values" 0)
    (_def_43
      (lambda (_a_53 _b_54)
        (call-with-values
          (_check-procedure_46 _a_53 "call-with-values")
          (_check-procedure_46 _b_54 "call-with-values")))
      "call-with-values"
      2)
    (_def_43 input-port? "input-port?" 1)
    (_def_43 output-port? "output-port?" 1)
    (_def*_44
      (lambda _args_53
        (let ((_tmp_54 _args_53))
          (let ((_val_55 (if (null? _tmp_54) #f (car _tmp_54))))
            (let ((_rest2_56 (if (null? _tmp_54) '() (cdr _tmp_54))))
              (let ((_tmp_57 _rest2_56))
                (let ((_reset_58 (if (null? _tmp_57) #f (car _tmp_57))))
                  (let ((_rest2_59 (if (null? _tmp_57) '() (cdr _tmp_57))))
                    (if (null? _args_53)
                      %input-port
                      (set! %input-port
                        (if _reset_58
                          _val_55
                          (_check-port_47 _val_55 "current-input-port")))))))))))
      "current-input-port"
      0)
    (_def*_44
      (lambda _args_53
        (let ((_tmp_54 _args_53))
          (let ((_val_55 (if (null? _tmp_54) #f (car _tmp_54))))
            (let ((_rest2_56 (if (null? _tmp_54) '() (cdr _tmp_54))))
              (let ((_tmp_57 _rest2_56))
                (let ((_reset_58 (if (null? _tmp_57) #f (car _tmp_57))))
                  (let ((_rest2_59 (if (null? _tmp_57) '() (cdr _tmp_57))))
                    (if (null? _args_53)
                      %output-port
                      (set! %output-port
                        (if _reset_58
                          _val_55
                          (_check-port_47 _val_55 "current-output-port")))))))))))
      "current-output-port"
      0)
    (_def_43
      (lambda (_fname_53 _proc_54)
        (%call-with-input-file
          _fname_53
          (_check-procedure_46 _proc_54 "call-with-input-file")))
      "call-with-input-file"
      2)
    (_def_43
      (lambda (_fname_53 _proc_54)
        (%call-with-output-file
          _fname_53
          (_check-procedure_46 _proc_54 "call-with-output-file")))
      "call-with-output-file"
      2)
    (_def_43 %open-input-file "open-input-file" 1)
    (_def_43 %open-output-file "open-output-file" 1)
    (_def_43 close-input-port "close-input-port" 1)
    (_def_43 close-output-port "close-output-port" 1)
    (_def*_44 %read "read" 0)
    (_def_43 eof-object? "eof-object?" 1)
    (_def*_44 %read-char "read-char" 0)
    (_def*_44 %peek-char "peek-char" 0)
    (_def*_44 %write "write" 1)
    (_def*_44 %display "display" 1)
    (_def*_44 (_undefd_51 %write-char) "write-char" 1)
    (_def*_44 (_undefd_51 %newline) "newline" 0)
    (_def_43
      (lambda (_fname_53 _proc_54)
        (%with-input-from-file
          _fname_53
          (_check-procedure_46 _proc_54 "with-input-from-file")))
      "with-input-from-file"
      2)
    (_def_43
      (lambda (_fname_53 _proc_54)
        (%with-output-to-file
          _fname_53
          (_check-procedure_46 _proc_54 "with-output-to-file")))
      "with-output-to-file"
      2)
    (_def*_44
      (lambda (_expr_53 . _env_54)
        (%eval _expr_53
               (_check-environment_49
                 (if (pair? _env_54) (car _env_54) %interaction-environment)
                 "eval")))
      "eval"
      1)
    (_def_43
      (lambda (_r_53)
        (_check-report_50 _r_53 "null-environment")
        %null-environment/5)
      "null-environment"
      1)
    (_def_43
      (lambda (_r_53)
        (_check-report_50 _r_53 "scheme-report-environment")
        %scheme-report-environment/5)
      "scheme-report-environment"
      1)
    (_def*_44
      (lambda _args_53
        (let ((_tmp_54 _args_53))
          (let ((_val_55 (if (null? _tmp_54) #f (car _tmp_54))))
            (let ((_rest2_56 (if (null? _tmp_54) '() (cdr _tmp_54))))
              (let ((_tmp_57 _rest2_56))
                (let ((_reset_58 (if (null? _tmp_57) #f (car _tmp_57))))
                  (let ((_rest2_59 (if (null? _tmp_57) '() (cdr _tmp_57))))
                    (if (null? _args_53)
                      %interaction-environment
                      (set! %interaction-environment
                        (if _reset_58
                          _val_55
                          (_check-environment_49
                            _val_55
                            "interaction-environment")))))))))))
      "interaction-environment"
      0)
    (_def_43 %environment? "environment?" 1)
    (_def*_44
      (lambda (_env_53 . _args_54)
        (let ((_tmp_55 _args_54))
          (let ((_name_56 (if (null? _tmp_55) "UNNAMED" (car _tmp_55))))
            (let ((_rest2_57 (if (null? _tmp_55) '() (cdr _tmp_55))))
              (let ((_tmp_58 _rest2_57))
                (let ((_mutable_59 (if (null? _tmp_58) #t (car _tmp_58))))
                  (let ((_rest2_60 (if (null? _tmp_58) '() (cdr _tmp_58))))
                    (%copy-environment
                      (_check-environment_49 _env_53 "copy-environment")
                      _name_56
                      _mutable_59))))))))
      "copy-environment"
      1)
    (_def_43
      (lambda (_thunk_53)
        (%make-promise (_check-procedure_46 _thunk_53 "delay")))
      "%make-promise"
      1)
    (_def_43 %promise? "%%promise?" 1)
    (_def_43
      (lambda (_h_53 _thunk_54)
        (%with-exception-handler
          (_check-procedure_46 _h_53 "with-exception-handler")
          (_check-procedure_46 _thunk_54 "with-exception-handler")))
      "with-exception-handler"
      2)
    (_def_43 %current-process-id "current-process-id" 0)
    (_def_43
      (lambda (_x_53) (eval _x_53 (interaction-environment)))
      "%%meta-eval"
      1)
    (_def*_44
      (lambda (_fn_53 . _ev_54)
        (apply %load
               _fn_53
               (if (null? _ev_54)
                 '()
                 (list (_check-procedure_46 (car _ev_54) "load"))))
        (%void))
      "load"
      1)
    (_def*_44 %command-line-arguments "command-line-arguments" 0)
    (_def*_44
      (lambda _args_53
        (let ((_tmp_54 _args_53))
          (let ((_val_55 (if (null? _tmp_54) #f (car _tmp_54))))
            (let ((_rest2_56 (if (null? _tmp_54) '() (cdr _tmp_54))))
              (let ((_tmp_57 _rest2_56))
                (let ((_reset_58 (if (null? _tmp_57) #f (car _tmp_57))))
                  (let ((_rest2_59 (if (null? _tmp_57) '() (cdr _tmp_57))))
                    (if (null? _args_53)
                      %error-port
                      (set! %error-port
                        (if _reset_58
                          _val_55
                          (_check-port_47 _val_55 "current-error-port")))))))))))
      "current-error-port"
      0)
    (_def_43 (lambda (_x_53) (%delete-file _x_53) (%void)) "delete-file" 1)
    (_def_43 %file-exists? "file-exists?" 1)
    (_def*_44 %current-directory "current-directory" 0)
    (_def*_44 %void "void" 0)
    (_def*_44 %exit "exit" 0)
    (_def*_44
      (lambda _evaluator_53
        (let ((_eval_54
                (if (pair? _evaluator_53)
                  (%procedure-code (car _evaluator_53))
                  %eval))
              (_fs_55 %features))
          (dynamic-wind
            (lambda () (add-feature 'interactive))
            (lambda () (%repl _eval_54))
            (lambda () (remove-feature 'interactive)))))
      "repl"
      0)
    (_def*_44
      (lambda _prompt_53
        (if (null? _prompt_53)
          (%procedure %repl-prompt "[repl prompt]" 1)
          (set! %repl-prompt
            (_check-procedure_46 (car _prompt_53) "repl-prompt"))))
      "repl-prompt"
      0)
    (_def*_44 (lambda _args_53 (apply %quit _args_53)) "quit" 0)
    (_def*_44
      (lambda _port_53
        (%flush-output (if (pair? _port_53) (car _port_53) %output-port)))
      "flush-output"
      0)
    (_def*_44
      (lambda (_exp_53 . _env_54)
        (%expand
          _exp_53
          (_check-environment_49
            (if (pair? _env_54) (car _env_54) %interaction-environment)
            "expand")))
      "expand"
      1)
    (_def_43 %system "system" 1)
    (_def_43 %current-time "current-time" 0)
    (_def_43 %get-environment-variable "get-environment-variable" 1)
    (_def*_44 pp "pp" 1)
    (_def*_44
      (lambda (_fn_53 . _args_54)
        (let ((_eval_55
                (if (pair? _args_54) (%procedure-code (car _args_54)) %eval)))
          (load-program _fn_53 _eval_55)))
      "load-program"
      1)
    (_def*_44
      (lambda _args_53
        (let ((_tmp_54 _args_53))
          (let ((_val_55 (if (null? _tmp_54) #f (car _tmp_54))))
            (let ((_rest2_56 (if (null? _tmp_54) '() (cdr _tmp_54))))
              (let ((_tmp_57 _rest2_56))
                (let ((_reset_58 (if (null? _tmp_57) #f (car _tmp_57))))
                  (let ((_rest2_59 (if (null? _tmp_57) '() (cdr _tmp_57))))
                    (if (null? _args_53)
                      *current-source-filename*
                      (set! *current-source-filename*
                        (if _reset_58
                          _val_55
                          (_check-string_48
                            _val_55
                            "current-source-filename")))))))))))
      "current-source-filename"
      0)
    (_def*_44
      (lambda _env_53
        (%oblist
          (_check-environment_49
            (if (pair? _env_53) (car _env_53) %interaction-environment)
            "oblist")))
      "oblist"
      0)
    (_def*_44
      (lambda _args_53
        (call-with-values
          (lambda () (apply make-disjoint-type _args_53))
          (lambda (_make_54 _pred_55 _data_56)
            (values
              (%procedure _make_54 "[constructor]" -1)
              (%procedure _pred_55 "[predicate]" 1)
              (%procedure _data_56 "[accessor]" 1)))))
      "make-disjoint-type"
      0)
    (_def*_44 library-path "library-path" 0)
    (_def_43 %library "library" 1)
    (_def*_44 %case-sensitive "case-sensitive" 0)
    (_def_43 (lambda () *grass-version*) "grass-version" 0)
    (_def*_44 %error "error" 1)
    (_def*_44
      (lambda _arg_53
        (if (null? _arg_53)
          %features
          (begin
            (set! %features (car _arg_53))
            (regenerate-cond-expand %features))))
      "features"
      0))
  (define %scheme-report-environment/5
    (%copy-environment
      %interaction-environment
      "scheme-report-environment/5"
      #f))
  (define add-feature
    (lambda (_f_43)
      (if (not (memq _f_43 %features))
        (begin
          (set! %features (cons _f_43 %features))
          (regenerate-cond-expand %features)))))
  (define remove-feature
    (lambda (_f_43)
      (set! %features
        (let _loop_44 ((_fs_45 %features))
          (if (null? _fs_45)
            '()
            (if (eq? _f_43 (car _fs_45))
              (cdr _fs_45)
              (cons (car _fs_45) (_loop_44 (cdr _fs_45)))))))
      (regenerate-cond-expand %features)))
  (define regenerate-cond-expand
    (lambda (_features_43)
      (%expand
        (list 'define-syntax
              'cond-expand
              (cons 'syntax-rules
                    (cons (cons 'and
                                (cons 'or (cons 'not (cons 'else _features_43))))
                          (cons '((cond-expand)
                                  (error "no matching \"cond-expand\" clause"))
                                (append
                                  (map (lambda (_f_44)
                                         (cons (cons 'cond-expand
                                                     (cons (cons _f_44 '(body ...)) 'more-clauses))
                                               '((begin body ...))))
                                       _features_43)
                                  '(((cond-expand (else body ...)) (begin body ...))
                                    ((cond-expand ((and) body ...) more-clauses ...)
                                     (begin body ...))
                                    ((cond-expand
                                       ((and req1 req2 ...) body ...)
                                       more-clauses
                                       ...)
                                     (cond-expand
                                       (req1 (cond-expand
                                               ((and req2 ...) body ...)
                                               more-clauses
                                               ...))
                                       more-clauses
                                       ...))
                                    ((cond-expand ((or) body ...) more-clauses ...)
                                     (cond-expand more-clauses ...))
                                    ((cond-expand
                                       ((or req1 req2 ...) body ...)
                                       more-clauses
                                       ...)
                                     (cond-expand
                                       (req1 (begin body ...))
                                       (else (cond-expand
                                               ((or req2 ...) body ...)
                                               more-clauses
                                               ...))))
                                    ((cond-expand
                                       ((not req) body ...)
                                       more-clauses
                                       ...)
                                     (cond-expand
                                       (req (cond-expand more-clauses ...))
                                       (else body ...)))
                                    ((cond-expand
                                       (feature-id body ...)
                                       more-clauses
                                       ...)
                                     (cond-expand more-clauses ...)))))))))))
  (add-feature 'grass)
  (letrec ((_proc->string_43
             (lambda (_proc_44)
               (let ((_name_45 (%procedure-name _proc_44)))
                 (if _name_45
                   (string-append "#<procedure " _name_45 ">")
                   "#<procedure>")))))
    (set! %write-hook
      (lambda (_x_44 _port_45)
        (if (%procedure? _x_44)
          (begin (display (_proc->string_43 _x_44) _port_45) #t)
          (if (%environment? _x_44)
            (begin
              (display "#<environment " _port_45)
              (display
                (%environment-data-name (%environment-data _x_44))
                _port_45)
              (write-char #\> _port_45))
            (if (%disjoint-type-instance? _x_44)
              (begin
                (display (disjoint-type-instance->string _x_44) _port_45)
                #t)
              #f)))))
    (set! pretty-print-hook
      (lambda (_x_44 _out_45)
        (if (%procedure? _x_44)
          (_out_45 (_proc->string_43 _x_44))
          (if (%environment? _x_44)
            (begin
              (_out_45 "#<environment ")
              (_out_45 (%environment-data-name (%environment-data _x_44)))
              (_out_45 ">"))
            (if (%disjoint-type-instance? _x_44)
              (_out_45 (disjoint-type-instance->string _x_44))
              #f))))))
  (define usage
    (lambda (_code_43)
      (display
        "usage: scheme [-h] [-FEATURE ...] [FILENAME ARGUMENT ...]
"
        %error-port)
      (%exit _code_43)))
  (define home
    (let ((_x_43 (%get-environment-variable "HOME"))) (if _x_43 _x_43 ".")))
  (define *temporary-directory*
    (let ((_x_43 (%get-environment-variable "TMP")))
      (if _x_43
        _x_43
        (let ((_x_44 (%get-environment-variable "TMPDIR")))
          (if _x_44
            _x_44
            (let ((_x_45 (%get-environment-variable "TEMP")))
              (if _x_45 _x_45 "/tmp")))))))
  (define main
    (lambda (_args_43)
      (letrec ((_load-init_44
                 (lambda ()
                   (let ((_tmp_47
                           (%file-exists?
                             (string-append home "/.scheme/init.scm"))))
                     (if _tmp_47 (let ((_f_48 _tmp_47)) (%load _f_48)) #f))))
               (_eval-file_45
                 (lambda (_in_47)
                   (let ((_temp_48 _in_47))
                     (dynamic-wind
                       (lambda ()
                         (let ((_tmp_50 _temp_48))
                           (set! _temp_48 *current-source-filename*)
                           (set! *current-source-filename* _tmp_50)))
                       (lambda ()
                         (let ((_v_50 (read-forms _in_47 %read)))
                           (let ((_failure_51
                                   (lambda ()
                                     (let ((_failure_51
                                             (lambda () (error 'match "no matching pattern"))))
                                       (if (pair? _v_50)
                                         (let ((_w_53 (car _v_50)) (_x_54 (cdr _v_50)))
                                           (if (equal? _w_53 'begin)
                                             (let ((_forms_59 _x_54))
                                               (if (list? _forms_59)
                                                 (for-each %eval _forms_59)
                                                 (_failure_51)))
                                             (_failure_51)))
                                         (_failure_51))))))
                             (if (pair? _v_50)
                               (let ((_w_52 (car _v_50)) (_x_53 (cdr _v_50)))
                                 (if (equal? _w_52 'begin)
                                   (if (if (pair? _x_53) (null? (cdr _x_53)) #f)
                                     (let ((_w_55 (car _x_53)))
                                       (let ((_spec_58 _w_55))
                                         (if (pair? _w_55)
                                           (let ((_w_59 (car _w_55)) (_x_60 (cdr _w_55)))
                                             (if (equal? _w_59 'program)
                                               (%eval (expand-program _spec_58 _in_47))
                                               (_failure_51)))
                                           (_failure_51))))
                                     (_failure_51))
                                   (_failure_51)))
                               (_failure_51)))))
                       (lambda ()
                         (let ((_tmp_50 _temp_48))
                           (set! _temp_48 *current-source-filename*)
                           (set! *current-source-filename* _tmp_50)))))))
               (_eval-strings_46
                 (lambda (_args_47)
                   (let ((_tmp_48
                           (string-append
                             *temporary-directory*
                             "/scheme-startup."
                             (number->string (%current-process-id))
                             ".scm")))
                     (%with-output-to-file
                       _tmp_48
                       (lambda ()
                         (display "(begin " %output-port)
                         (for-each
                           (lambda (_str_49)
                             (display _str_49 %output-port)
                             (write-char #\space %output-port))
                           _args_47)
                         (display "
)
" %output-port)))
                     (%load _tmp_48)
                     (%delete-file _tmp_48)))))
        (%initialize-ports
          (current-input-port)
          (current-output-port)
          (%current-error-port))
        (%command-line-arguments _args_43)
        (let _loop_47 ((_args_48 _args_43))
          (let ((_v_49 _args_48))
            (let ((_failure_50
                    (lambda ()
                      (let ((_failure_50
                              (lambda ()
                                (let ((_failure_50
                                        (lambda () (error 'match "no matching pattern"))))
                                  (if (pair? _v_49)
                                    (let ((_w_51 (car _v_49)) (_x_52 (cdr _v_49)))
                                      (let ((_x_54 _w_51))
                                        (let ((_more_56 _x_52))
                                          (let ((_len_57 (string-length _x_54)))
                                            (if (if (>= _len_57 2)
                                                  (char=? #\- (string-ref _x_54 0))
                                                  #f)
                                              (begin
                                                (add-feature
                                                  (string->symbol (substring _x_54 1 _len_57)))
                                                (_loop_47 _more_56))
                                              (if (if (positive? _len_57)
                                                    (char=? #\( (string-ref _x_54 0))
                                                    #f)
                                                (begin (_eval-strings_46 _args_48) (%exit))
                                                (begin
                                                  (%command-line-arguments _more_56)
                                                  (_eval-file_45 _x_54)
                                                  (%exit))))))))
                                    (_failure_50))))))
                        (if (pair? _v_49)
                          (let ((_w_51 (car _v_49)) (_x_52 (cdr _v_49)))
                            (let ((_sk2_60 (lambda () (usage 0))))
                              (let ((_fk2_61
                                      (lambda ()
                                        (let ((_fk2_61
                                                (lambda ()
                                                  (if (equal? _w_51 "--help")
                                                    (_sk2_60)
                                                    (_failure_50)))))
                                          (if (equal? _w_51 "-help") (_sk2_60) (_fk2_61))))))
                                (if (equal? _w_51 "-h") (_sk2_60) (_fk2_61)))))
                          (_failure_50))))))
              (if (null? _v_49)
                (begin
                  (%display "[0m[42m[30mGRASS No. ")
                  (%display *grass-version*)
                  (%display "[0m
")
                  (add-feature 'interactive)
                  (_load-init_44)
                  (%repl)
                  (%exit))
                (_failure_50)))))))))
