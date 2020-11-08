Right
  (fromList
     [ ( "examples/../fixtures/IO.mad"
       , AST
           { aimports = []
           , aexps =
               [ Meta
                   Infos { nthArg = Nothing , origin = Nothing }
                   (Area (Loc 0 1 1) (Loc 69 2 56))
                   (TypedExp
                      (Meta
                         Infos { nthArg = Nothing , origin = Nothing }
                         (Area (Loc 0 1 1) (Loc 3 1 4))
                         (Export
                            (Meta
                               Infos { nthArg = Nothing , origin = Nothing }
                               (Area (Loc 4 1 5) (Loc 6 1 7))
                               (Assignment
                                  "log"
                                  (Meta
                                     Infos { nthArg = Nothing , origin = Nothing }
                                     (Area (Loc 27 2 14) (Loc 69 2 56))
                                     (Abs
                                        "a"
                                        (Meta
                                           Infos { nthArg = Nothing , origin = Nothing }
                                           (Area (Loc 34 2 21) (Loc 69 2 56))
                                           (JSExp "{ console.log(a); return a; }"))))))))
                      (Meta
                         Infos { nthArg = Nothing , origin = Nothing }
                         (Area (Loc 7 1 8) (Loc 13 1 14))
                         (TRArr
                            (Meta
                               Infos { nthArg = Nothing , origin = Nothing }
                               (Area (Loc 7 1 8) (Loc 8 1 9))
                               (TRSingle "a"))
                            (Meta
                               Infos { nthArg = Nothing , origin = Nothing }
                               (Area (Loc 12 1 13) (Loc 13 1 14))
                               (TRSingle "a")))))
               , Meta
                   Infos { nthArg = Nothing , origin = Nothing }
                   (Area (Loc 71 4 1) (Loc 143 5 58))
                   (TypedExp
                      (Meta
                         Infos { nthArg = Nothing , origin = Nothing }
                         (Area (Loc 71 4 1) (Loc 75 4 5))
                         (Export
                            (Meta
                               Infos { nthArg = Nothing , origin = Nothing }
                               (Area (Loc 76 4 6) (Loc 78 4 8))
                               (Assignment
                                  "warn"
                                  (Meta
                                     Infos { nthArg = Nothing , origin = Nothing }
                                     (Area (Loc 100 5 15) (Loc 143 5 58))
                                     (Abs
                                        "a"
                                        (Meta
                                           Infos { nthArg = Nothing , origin = Nothing }
                                           (Area (Loc 107 5 22) (Loc 143 5 58))
                                           (JSExp "{ console.warn(a); return a; }"))))))))
                      (Meta
                         Infos { nthArg = Nothing , origin = Nothing }
                         (Area (Loc 79 4 9) (Loc 85 4 15))
                         (TRArr
                            (Meta
                               Infos { nthArg = Nothing , origin = Nothing }
                               (Area (Loc 79 4 9) (Loc 80 4 10))
                               (TRSingle "a"))
                            (Meta
                               Infos { nthArg = Nothing , origin = Nothing }
                               (Area (Loc 84 4 14) (Loc 85 4 15))
                               (TRSingle "a")))))
               , Meta
                   Infos { nthArg = Nothing , origin = Nothing }
                   (Area (Loc 145 7 1) (Loc 215 8 57))
                   (TypedExp
                      (Meta
                         Infos { nthArg = Nothing , origin = Nothing }
                         (Area (Loc 145 7 1) (Loc 148 7 4))
                         (Export
                            (Meta
                               Infos { nthArg = Nothing , origin = Nothing }
                               (Area (Loc 149 7 5) (Loc 151 7 7))
                               (Assignment
                                  "err"
                                  (Meta
                                     Infos { nthArg = Nothing , origin = Nothing }
                                     (Area (Loc 172 8 14) (Loc 215 8 57))
                                     (Abs
                                        "a"
                                        (Meta
                                           Infos { nthArg = Nothing , origin = Nothing }
                                           (Area (Loc 179 8 21) (Loc 215 8 57))
                                           (JSExp "{ console.warn(a); return a; }"))))))))
                      (Meta
                         Infos { nthArg = Nothing , origin = Nothing }
                         (Area (Loc 152 7 8) (Loc 158 7 14))
                         (TRArr
                            (Meta
                               Infos { nthArg = Nothing , origin = Nothing }
                               (Area (Loc 152 7 8) (Loc 153 7 9))
                               (TRSingle "a"))
                            (Meta
                               Infos { nthArg = Nothing , origin = Nothing }
                               (Area (Loc 157 7 13) (Loc 158 7 14))
                               (TRSingle "a")))))
               ]
           , aadts = []
           , apath = Just "examples/../fixtures/IO.mad"
           }
       )
     , ( "examples/read-a-file.mad"
       , AST
           { aimports =
               [ Meta
                   Infos { nthArg = Nothing , origin = Nothing }
                   (Area (Loc 0 1 1) (Loc 31 1 32))
                   (DefaultImport "IO" "../fixtures/IO")
               ]
           , aexps =
               [ Meta
                   Infos { nthArg = Nothing , origin = Nothing }
                   (Area (Loc 33 3 1) (Loc 39 3 7))
                   (FieldAccess
                      (Meta
                         Infos { nthArg = Nothing , origin = Nothing }
                         (Area (Loc 33 3 1) (Loc 35 3 3))
                         (Var "IO"))
                      (Meta
                         Infos { nthArg = Nothing , origin = Nothing }
                         (Area (Loc 36 3 4) (Loc 39 3 7))
                         (Var ".log")))
               , Meta
                   Infos { nthArg = Nothing , origin = Nothing }
                   (Area (Loc 40 3 8) (Loc 42 3 10))
                   (Var "IO")
               ]
           , aadts = []
           , apath = Just "examples/read-a-file.mad"
           }
       )
     ])
RESOLVED:
Right
  ( fromList
      [ ( "examples/../fixtures/IO.mad"
        , AST
            { aimports = []
            , aexps =
                [ Solved
                    (TArr (TVar (TV "a")) (TVar (TV "a")))
                    (Area (Loc 0 1 1) (Loc 69 2 56))
                    (TypedExp
                       (Solved
                          (TArr (TVar (TV "a")) (TVar (TV "a")))
                          (Area (Loc 0 1 1) (Loc 3 1 4))
                          (Export
                             (Solved
                                (TArr (TVar (TV "b")) (TVar (TV "a")))
                                (Area (Loc 4 1 5) (Loc 6 1 7))
                                (Assignment
                                   "log"
                                   (Solved
                                      (TArr (TVar (TV "b")) (TVar (TV "a")))
                                      (Area (Loc 27 2 14) (Loc 69 2 56))
                                      (Abs
                                         "a"
                                         (Solved
                                            (TVar (TV "a"))
                                            (Area (Loc 34 2 21) (Loc 69 2 56))
                                            (JSExp "{ console.log(a); return a; }"))))))))
                       (TRArr (TRSingle "a") (TRSingle "a")))
                , Solved
                    (TArr (TVar (TV "a")) (TVar (TV "a")))
                    (Area (Loc 71 4 1) (Loc 143 5 58))
                    (TypedExp
                       (Solved
                          (TArr (TVar (TV "a")) (TVar (TV "a")))
                          (Area (Loc 71 4 1) (Loc 75 4 5))
                          (Export
                             (Solved
                                (TArr (TVar (TV "d")) (TVar (TV "a")))
                                (Area (Loc 76 4 6) (Loc 78 4 8))
                                (Assignment
                                   "warn"
                                   (Solved
                                      (TArr (TVar (TV "d")) (TVar (TV "a")))
                                      (Area (Loc 100 5 15) (Loc 143 5 58))
                                      (Abs
                                         "a"
                                         (Solved
                                            (TVar (TV "a"))
                                            (Area (Loc 107 5 22) (Loc 143 5 58))
                                            (JSExp "{ console.warn(a); return a; }"))))))))
                       (TRArr (TRSingle "a") (TRSingle "a")))
                , Solved
                    (TArr (TVar (TV "a")) (TVar (TV "a")))
                    (Area (Loc 145 7 1) (Loc 215 8 57))
                    (TypedExp
                       (Solved
                          (TArr (TVar (TV "a")) (TVar (TV "a")))
                          (Area (Loc 145 7 1) (Loc 148 7 4))
                          (Export
                             (Solved
                                (TArr (TVar (TV "f")) (TVar (TV "a")))
                                (Area (Loc 149 7 5) (Loc 151 7 7))
                                (Assignment
                                   "err"
                                   (Solved
                                      (TArr (TVar (TV "f")) (TVar (TV "a")))
                                      (Area (Loc 172 8 14) (Loc 215 8 57))
                                      (Abs
                                         "a"
                                         (Solved
                                            (TVar (TV "a"))
                                            (Area (Loc 179 8 21) (Loc 215 8 57))
                                            (JSExp "{ console.warn(a); return a; }"))))))))
                       (TRArr (TRSingle "a") (TRSingle "a")))
                ]
            , aadts = []
            , apath = Just "examples/../fixtures/IO.mad"
            }
        )
      , ( "examples/read-a-file.mad"
        , AST
            { aimports = [ DefaultImport "IO" "../fixtures/IO" ]
            , aexps =
                [ Solved
                    (TArr (TVar (TV "b")) (TVar (TV "a")))
                    (Area (Loc 33 3 1) (Loc 39 3 7))
                    (FieldAccess
                       (Solved
                          (TRecord
                             (fromList
                                [ ( "err" , TArr (TVar (TV "f")) (TVar (TV "a")) )
                                , ( "log" , TArr (TVar (TV "b")) (TVar (TV "a")) )
                                , ( "warn" , TArr (TVar (TV "d")) (TVar (TV "a")) )
                                ])
                             False)
                          (Area (Loc 33 3 1) (Loc 35 3 3))
                          (Var "IO"))
                       (Solved
                          (TArr
                             (TRecord (fromList [ ( "log" , TVar (TV "g") ) ]) True)
                             (TVar (TV "g")))
                          (Area (Loc 36 3 4) (Loc 39 3 7))
                          (Var ".log")))
                , Solved
                    (TRecord
                       (fromList
                          [ ( "err" , TArr (TVar (TV "f")) (TVar (TV "a")) )
                          , ( "log" , TArr (TVar (TV "b")) (TVar (TV "a")) )
                          , ( "warn" , TArr (TVar (TV "d")) (TVar (TV "a")) )
                          ])
                       False)
                    (Area (Loc 40 3 8) (Loc 42 3 10))
                    (Var "IO")
                ]
            , aadts = []
            , apath = Just "examples/read-a-file.mad"
            }
        )
      ]
  , Unique { count = 7 }
  )
compiled JS:
// file: examples/../fixtures/IO.mad
const __buildCtorParam = n => {
  if (typeof n === "string") {
    return { type: "String", value: n };
  } else {
    return { type: "", value: n };
  }
};

export const log = (a => { console.log(a); return a; });
export const warn = (a => { console.warn(a); return a; });
export const err = (a => { console.warn(a); return a; });
export default { log, warn, err };
// file: examples/read-a-file.mad
import IO from "./../fixtures/IO.mjs";

const __buildCtorParam = n => {
  if (typeof n === "string") {
    return { type: "String", value: n };
  } else {
    return { type: "", value: n };
  }
};

IO.log;
IO;

