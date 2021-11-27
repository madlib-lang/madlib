; ModuleID = './generate-llvm/tests.ll'
source_filename = "./generate-llvm/tests.ll"

@"$Functor$WriterT" = global { { i8*, i32, i32 } } { { i8*, i32, i32 } { i8* bitcast (i8* (i8*)* @"$Functor$WriterT$map" to i8*), i32 1, i32 1 } }
@"$Applicative$WriterT" = global { { i8*, i32, i32 }, { i8*, i32, i32 } } { { i8*, i32, i32 } { i8* bitcast (i8* (i8*, i8*)* @"$Applicative$WriterT$ap" to i8*), i32 2, i32 2 }, { i8*, i32, i32 } { i8* bitcast (i8* (i8*, i8*)* @"$Applicative$WriterT$pure" to i8*), i32 2, i32 2 } }
@"$Monad$WriterT" = global { { i8*, i32, i32 }, { i8*, i32, i32 } } { { i8*, i32, i32 } { i8* bitcast (i8* (i8*, i8*, i8*)* @"$Monad$WriterT$chain" to i8*), i32 3, i32 3 }, { i8*, i32, i32 } { i8* bitcast (i8* (i8*, i8*, i8*, i8*, i8*)* @"$Monad$WriterT$of" to i8*), i32 5, i32 5 } }
@"$MonadTrans$m_WriterT" = global { { i8*, i32, i32 } } { { i8*, i32, i32 } { i8* bitcast (i8* (i8*, i8*, i8*)* @"$MonadTrans$m_WriterT$lift" to i8*), i32 3, i32 3 } }
@"$Functor$Identity" = global { { i8*, i32, i32 } } { { i8*, i32, i32 } { i8* bitcast (i8* (i8*, i8*)* @"$Functor$Identity$map" to i8*), i32 2, i32 2 } }
@"$Applicative$Identity" = global { { i8*, i32, i32 }, { i8*, i32, i32 } } { { i8*, i32, i32 } { i8* bitcast (i8* (i8*, i8*)* @"$Applicative$Identity$ap" to i8*), i32 2, i32 2 }, { i8*, i32, i32 } { i8* bitcast (i8* (i8*)* @"$Applicative$Identity$pure" to i8*), i32 1, i32 1 } }
@"$Monad$Identity" = global { { i8*, i32, i32 }, { i8*, i32, i32 } } { { i8*, i32, i32 } { i8* bitcast (i8* (i8*, i8*)* @"$Monad$Identity$chain" to i8*), i32 2, i32 2 }, { i8*, i32, i32 } { i8* bitcast (i8* (i8*)* @"$Monad$Identity$of" to i8*), i32 1, i32 1 } }
@"$MonadWriter$WriterT" = global { { i8*, i32, i32 } } { { i8*, i32, i32 } { i8* bitcast (i8* (i8*, i8*, i8*)* @"$MonadWriter$WriterT$tell" to i8*), i32 3, i32 3 } }

define i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT(i8*) {
  %2 = call i8* @GC_malloc(i64 ptrtoint ({ i64, i8* }* getelementptr inbounds ({ i64, i8* }, { i64, i8* }* null, i32 1) to i64))
  %3 = bitcast i8* %2 to { i64, i8* }*
  %4 = getelementptr { i64, i8* }, { i64, i8* }* %3, i32 0, i32 1
  store i8* %0, i8** %4, align 8
  %5 = getelementptr { i64, i8* }, { i64, i8* }* %3, i32 0, i32 0
  store i64 0, i64* %5, align 8
  %6 = bitcast { i64, i8* }* %3 to i8*
  ret i8* %6
}

define i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__Identity(i8*) {
  %2 = call i8* @GC_malloc(i64 ptrtoint ({ i64, i8* }* getelementptr inbounds ({ i64, i8* }, { i64, i8* }* null, i32 1) to i64))
  %3 = bitcast i8* %2 to { i64, i8* }*
  %4 = getelementptr { i64, i8* }, { i64, i8* }* %3, i32 0, i32 1
  store i8* %0, i8** %4, align 8
  %5 = getelementptr { i64, i8* }, { i64, i8* }* %3, i32 0, i32 0
  store i64 0, i64* %5, align 8
  %6 = bitcast { i64, i8* }* %3 to i8*
  ret i8* %6
}

define i8* @"$Functor$WriterT$map"(i8* %"$Functor$t19_0") {
  %1 = bitcast i8* %"$Functor$t19_0" to i8*
  %2 = bitcast i8* (i8*, i8*, i8*, i8*)* @"anonymous$lifted$4" to i8*
  %3 = bitcast i8* (i8*)* @__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT to i8*
  %4 = call i8* @GC_malloc(i64 ptrtoint ({ i8*, i32, i32 }* getelementptr inbounds ({ i8*, i32, i32 }, { i8*, i32, i32 }* null, i32 1) to i64))
  %5 = bitcast i8* %4 to { i8*, i32, i32 }*
  %6 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %5, i32 0, i32 0
  store i8* %3, i8** %6, align 8
  %7 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %5, i32 0, i32 1
  store i32 1, i32* %7, align 8
  %8 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %5, i32 0, i32 2
  store i32 1, i32* %8, align 8
  %9 = bitcast { i8*, i32, i32 }* %5 to i8*
  %10 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %11 = bitcast i8* %10 to { i8*, i8* }*
  %12 = getelementptr { i8*, i8* }, { i8*, i8* }* %11, i32 0, i32 0
  store i8* %1, i8** %12, align 8
  %13 = getelementptr { i8*, i8* }, { i8*, i8* }* %11, i32 0, i32 1
  store i8* %9, i8** %13, align 8
  %14 = call i8* @GC_malloc(i64 ptrtoint ({ i8*, i32, i32, i8* }* getelementptr inbounds ({ i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* null, i32 1) to i64))
  %15 = bitcast i8* %14 to { i8*, i32, i32, i8* }*
  %16 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %15, i32 0, i32 0
  store i8* %2, i8** %16, align 8
  %17 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %15, i32 0, i32 1
  store i32 4, i32* %17, align 8
  %18 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %15, i32 0, i32 2
  store i32 2, i32* %18, align 8
  %19 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %15, i32 0, i32 3
  store i8* %10, i8** %19, align 8
  %20 = bitcast { i8*, i32, i32, i8* }* %15 to i8*
  ret i8* %20
}

define i8* @"$Applicative$WriterT$ap"(i8* %"$Functor$d55_0", i8* %"$Applicative$d55_0") {
  %1 = bitcast i8* %"$Functor$d55_0" to i8*
  %2 = bitcast i8* %"$Applicative$d55_0" to i8*
  %3 = bitcast i8* (i8*, i8*, i8*, i8*)* @"anonymous$lifted$6" to i8*
  %4 = bitcast i8* (i8*)* @__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT to i8*
  %5 = call i8* @GC_malloc(i64 ptrtoint ({ i8*, i32, i32 }* getelementptr inbounds ({ i8*, i32, i32 }, { i8*, i32, i32 }* null, i32 1) to i64))
  %6 = bitcast i8* %5 to { i8*, i32, i32 }*
  %7 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %6, i32 0, i32 0
  store i8* %4, i8** %7, align 8
  %8 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %6, i32 0, i32 1
  store i32 1, i32* %8, align 8
  %9 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %6, i32 0, i32 2
  store i32 1, i32* %9, align 8
  %10 = bitcast { i8*, i32, i32 }* %6 to i8*
  %11 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %12 = bitcast i8* %11 to { i8*, i8* }*
  %13 = getelementptr { i8*, i8* }, { i8*, i8* }* %12, i32 0, i32 0
  store i8* %2, i8** %13, align 8
  %14 = getelementptr { i8*, i8* }, { i8*, i8* }* %12, i32 0, i32 1
  store i8* %10, i8** %14, align 8
  %15 = call i8* @GC_malloc(i64 ptrtoint ({ i8*, i32, i32, i8* }* getelementptr inbounds ({ i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* null, i32 1) to i64))
  %16 = bitcast i8* %15 to { i8*, i32, i32, i8* }*
  %17 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %16, i32 0, i32 0
  store i8* %3, i8** %17, align 8
  %18 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %16, i32 0, i32 1
  store i32 4, i32* %18, align 8
  %19 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %16, i32 0, i32 2
  store i32 2, i32* %19, align 8
  %20 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %16, i32 0, i32 3
  store i8* %11, i8** %20, align 8
  %21 = bitcast { i8*, i32, i32, i8* }* %16 to i8*
  ret i8* %21
}

define i8* @"$Applicative$WriterT$pure"(i8* %"$Functor$p67_0", i8* %"$Applicative$p67_0") {
  %1 = bitcast i8* %"$Functor$p67_0" to i8*
  %2 = bitcast i8* %"$Applicative$p67_0" to i8*
  %3 = bitcast i8* (i8*, i8*, i8*)* @"anonymous$lifted$7" to i8*
  %4 = bitcast i8* (i8*)* @__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT to i8*
  %5 = call i8* @GC_malloc(i64 ptrtoint ({ i8*, i32, i32 }* getelementptr inbounds ({ i8*, i32, i32 }, { i8*, i32, i32 }* null, i32 1) to i64))
  %6 = bitcast i8* %5 to { i8*, i32, i32 }*
  %7 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %6, i32 0, i32 0
  store i8* %4, i8** %7, align 8
  %8 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %6, i32 0, i32 1
  store i32 1, i32* %8, align 8
  %9 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %6, i32 0, i32 2
  store i32 1, i32* %9, align 8
  %10 = bitcast { i8*, i32, i32 }* %6 to i8*
  %11 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %12 = bitcast i8* %11 to { i8*, i8* }*
  %13 = getelementptr { i8*, i8* }, { i8*, i8* }* %12, i32 0, i32 0
  store i8* %2, i8** %13, align 8
  %14 = getelementptr { i8*, i8* }, { i8*, i8* }* %12, i32 0, i32 1
  store i8* %10, i8** %14, align 8
  %15 = call i8* @GC_malloc(i64 ptrtoint ({ i8*, i32, i32, i8* }* getelementptr inbounds ({ i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* null, i32 1) to i64))
  %16 = bitcast i8* %15 to { i8*, i32, i32, i8* }*
  %17 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %16, i32 0, i32 0
  store i8* %3, i8** %17, align 8
  %18 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %16, i32 0, i32 1
  store i32 3, i32* %18, align 8
  %19 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %16, i32 0, i32 2
  store i32 1, i32* %19, align 8
  %20 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %16, i32 0, i32 3
  store i8* %11, i8** %20, align 8
  %21 = bitcast { i8*, i32, i32, i8* }* %16 to i8*
  ret i8* %21
}

define i8* @"$Monad$WriterT$chain"(i8* %"$Functor$f109_0", i8* %"$Applicative$f109_0", i8* %"$Monad$f109_0") {
  %1 = bitcast i8* %"$Functor$f109_0" to i8*
  %2 = bitcast i8* %"$Applicative$f109_0" to i8*
  %3 = bitcast i8* %"$Monad$f109_0" to i8*
  %4 = bitcast i8* (i8*, i8*, i8*, i8*)* @"anonymous$lifted$10" to i8*
  %5 = bitcast i8* (i8*)* @__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT to i8*
  %6 = call i8* @GC_malloc(i64 ptrtoint ({ i8*, i32, i32 }* getelementptr inbounds ({ i8*, i32, i32 }, { i8*, i32, i32 }* null, i32 1) to i64))
  %7 = bitcast i8* %6 to { i8*, i32, i32 }*
  %8 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %7, i32 0, i32 0
  store i8* %5, i8** %8, align 8
  %9 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %7, i32 0, i32 1
  store i32 1, i32* %9, align 8
  %10 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %7, i32 0, i32 2
  store i32 1, i32* %10, align 8
  %11 = bitcast { i8*, i32, i32 }* %7 to i8*
  %12 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %13 = bitcast i8* %12 to { i8*, i8* }*
  %14 = getelementptr { i8*, i8* }, { i8*, i8* }* %13, i32 0, i32 0
  store i8* %3, i8** %14, align 8
  %15 = getelementptr { i8*, i8* }, { i8*, i8* }* %13, i32 0, i32 1
  store i8* %11, i8** %15, align 8
  %16 = call i8* @GC_malloc(i64 ptrtoint ({ i8*, i32, i32, i8* }* getelementptr inbounds ({ i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* null, i32 1) to i64))
  %17 = bitcast i8* %16 to { i8*, i32, i32, i8* }*
  %18 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %17, i32 0, i32 0
  store i8* %4, i8** %18, align 8
  %19 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %17, i32 0, i32 1
  store i32 4, i32* %19, align 8
  %20 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %17, i32 0, i32 2
  store i32 2, i32* %20, align 8
  %21 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %17, i32 0, i32 3
  store i8* %12, i8** %21, align 8
  %22 = bitcast { i8*, i32, i32, i8* }* %17 to i8*
  ret i8* %22
}

define i8* @"$Monad$WriterT$of"(i8* %"$Functor$o118_0", i8* %"$Applicative$o118_0", i8* %"$Monad$o118_0", i8* %"$Functor$o118_1", i8* %"$Applicative$o118_1") {
  %1 = bitcast i8* %"$Functor$o118_0" to i8*
  %2 = bitcast i8* %"$Applicative$o118_0" to i8*
  %3 = bitcast i8* %"$Monad$o118_0" to i8*
  %4 = bitcast i8* %"$Functor$o118_1" to i8*
  %5 = bitcast i8* %"$Applicative$o118_1" to i8*
  %6 = bitcast i8* (i8*, i8*)* @"$Applicative$WriterT$pure" to i8*
  %7 = call i8* @GC_malloc(i64 ptrtoint ({ i8*, i32, i32 }* getelementptr inbounds ({ i8*, i32, i32 }, { i8*, i32, i32 }* null, i32 1) to i64))
  %8 = bitcast i8* %7 to { i8*, i32, i32 }*
  %9 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %8, i32 0, i32 0
  store i8* %6, i8** %9, align 8
  %10 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %8, i32 0, i32 1
  store i32 2, i32* %10, align 8
  %11 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %8, i32 0, i32 2
  store i32 2, i32* %11, align 8
  %12 = bitcast { i8*, i32, i32 }* %8 to i8*
  ret i8* %12
}

define i8* @"$MonadTrans$m_WriterT$lift"(i8* %"$Functor$g136_0", i8* %"$Applicative$g136_0", i8* %"$Monad$g136_0") {
  %1 = bitcast i8* %"$Functor$g136_0" to i8*
  %2 = bitcast i8* %"$Applicative$g136_0" to i8*
  %3 = bitcast i8* %"$Monad$g136_0" to i8*
  %4 = bitcast i8* (i8*, i8*, i8*)* @"anonymous$lifted$12" to i8*
  %5 = bitcast i8* (i8*)* @__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT to i8*
  %6 = call i8* @GC_malloc(i64 ptrtoint ({ i8*, i32, i32 }* getelementptr inbounds ({ i8*, i32, i32 }, { i8*, i32, i32 }* null, i32 1) to i64))
  %7 = bitcast i8* %6 to { i8*, i32, i32 }*
  %8 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %7, i32 0, i32 0
  store i8* %5, i8** %8, align 8
  %9 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %7, i32 0, i32 1
  store i32 1, i32* %9, align 8
  %10 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %7, i32 0, i32 2
  store i32 1, i32* %10, align 8
  %11 = bitcast { i8*, i32, i32 }* %7 to i8*
  %12 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %13 = bitcast i8* %12 to { i8*, i8* }*
  %14 = getelementptr { i8*, i8* }, { i8*, i8* }* %13, i32 0, i32 0
  store i8* %3, i8** %14, align 8
  %15 = getelementptr { i8*, i8* }, { i8*, i8* }* %13, i32 0, i32 1
  store i8* %11, i8** %15, align 8
  %16 = call i8* @GC_malloc(i64 ptrtoint ({ i8*, i32, i32, i8* }* getelementptr inbounds ({ i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* null, i32 1) to i64))
  %17 = bitcast i8* %16 to { i8*, i32, i32, i8* }*
  %18 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %17, i32 0, i32 0
  store i8* %4, i8** %18, align 8
  %19 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %17, i32 0, i32 1
  store i32 3, i32* %19, align 8
  %20 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %17, i32 0, i32 2
  store i32 1, i32* %20, align 8
  %21 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %17, i32 0, i32 3
  store i8* %12, i8** %21, align 8
  %22 = bitcast { i8*, i32, i32, i8* }* %17 to i8*
  ret i8* %22
}

define i8* @"$Functor$Identity$map"(i8* %f_0, i8* %m_0) {
  %1 = bitcast i8* %f_0 to { i8*, i32, i32, i8* }*
  %2 = bitcast i8* %m_0 to i8*
  %3 = bitcast { i8*, i32, i32, i8* }* %1 to i8*
  %4 = call i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__runIdentity(i8* %2)
  %5 = bitcast i8* %4 to i8*
  %6 = call i8* (i8*, i32, ...) @__applyPAP__(i8* %3, i32 1, i8* %5)
  %7 = bitcast i8* %6 to i8*
  %8 = call i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__Identity(i8* %7)
  %9 = bitcast i8* %8 to i8*
  ret i8* %9
}

define i8* @"$Applicative$Identity$ap"(i8* %mf_0, i8* %mm_0) {
  %1 = bitcast i8* %mf_0 to i8*
  %2 = bitcast i8* %mm_0 to i8*
  %3 = call i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__runIdentity(i8* %1)
  %4 = call i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__runIdentity(i8* %2)
  %5 = bitcast i8* %4 to i8*
  %6 = call i8* (i8*, i32, ...) @__applyPAP__(i8* %3, i32 1, i8* %5)
  %7 = bitcast i8* %6 to i8*
  %8 = call i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__Identity(i8* %7)
  %9 = bitcast i8* %8 to i8*
  ret i8* %9
}

define i8* @"$Applicative$Identity$pure"(i8*) {
  %2 = bitcast i8* (i8*)* @__eddaab7dd3adaa6b7d769751eb6c3b5e__Identity to i8*
  %3 = call i8* @GC_malloc(i64 ptrtoint ({ i8*, i32, i32 }* getelementptr inbounds ({ i8*, i32, i32 }, { i8*, i32, i32 }* null, i32 1) to i64))
  %4 = bitcast i8* %3 to { i8*, i32, i32 }*
  %5 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %4, i32 0, i32 0
  store i8* %2, i8** %5, align 8
  %6 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %4, i32 0, i32 1
  store i32 1, i32* %6, align 8
  %7 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %4, i32 0, i32 2
  store i32 1, i32* %7, align 8
  %8 = bitcast { i8*, i32, i32 }* %4 to i8*
  %9 = call i8* (i8*, i32, ...) @__applyPAP__(i8* %8, i32 1, i8* %0)
  ret i8* %9
}

define i8* @"$Monad$Identity$chain"(i8* %f_0, i8* %mm_0) {
  %1 = bitcast i8* %f_0 to { i8*, i32, i32, i8* }*
  %2 = bitcast i8* %mm_0 to i8*
  %3 = bitcast { i8*, i32, i32, i8* }* %1 to i8*
  %4 = call i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__runIdentity(i8* %2)
  %5 = bitcast i8* %4 to i8*
  %6 = call i8* (i8*, i32, ...) @__applyPAP__(i8* %3, i32 1, i8* %5)
  %7 = bitcast i8* %6 to i8*
  ret i8* %7
}

define i8* @"$Monad$Identity$of"(i8*) {
  %2 = bitcast i8* (i8*)* @"$Applicative$Identity$pure" to i8*
  %3 = call i8* @GC_malloc(i64 ptrtoint ({ i8*, i32, i32 }* getelementptr inbounds ({ i8*, i32, i32 }, { i8*, i32, i32 }* null, i32 1) to i64))
  %4 = bitcast i8* %3 to { i8*, i32, i32 }*
  %5 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %4, i32 0, i32 0
  store i8* %2, i8** %5, align 8
  %6 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %4, i32 0, i32 1
  store i32 1, i32* %6, align 8
  %7 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %4, i32 0, i32 2
  store i32 1, i32* %7, align 8
  %8 = bitcast { i8*, i32, i32 }* %4 to i8*
  %9 = call i8* (i8*, i32, ...) @__applyPAP__(i8* %8, i32 1, i8* %0)
  ret i8* %9
}

define i8* @"$MonadWriter$WriterT$tell"(i8* %"$Functor$l193_0", i8* %"$Applicative$l193_0", i8* %"$Monad$l193_0") {
  %1 = bitcast i8* %"$Functor$l193_0" to i8*
  %2 = bitcast i8* %"$Applicative$l193_0" to i8*
  %3 = bitcast i8* %"$Monad$l193_0" to i8*
  %4 = bitcast i8* (i8*, i8*, i8*)* @"anonymous$lifted$13" to i8*
  %5 = bitcast i8* (i8*)* @__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT to i8*
  %6 = call i8* @GC_malloc(i64 ptrtoint ({ i8*, i32, i32 }* getelementptr inbounds ({ i8*, i32, i32 }, { i8*, i32, i32 }* null, i32 1) to i64))
  %7 = bitcast i8* %6 to { i8*, i32, i32 }*
  %8 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %7, i32 0, i32 0
  store i8* %5, i8** %8, align 8
  %9 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %7, i32 0, i32 1
  store i32 1, i32* %9, align 8
  %10 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %7, i32 0, i32 2
  store i32 1, i32* %10, align 8
  %11 = bitcast { i8*, i32, i32 }* %7 to i8*
  %12 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %13 = bitcast i8* %12 to { i8*, i8* }*
  %14 = getelementptr { i8*, i8* }, { i8*, i8* }* %13, i32 0, i32 0
  store i8* %3, i8** %14, align 8
  %15 = getelementptr { i8*, i8* }, { i8*, i8* }* %13, i32 0, i32 1
  store i8* %11, i8** %15, align 8
  %16 = call i8* @GC_malloc(i64 ptrtoint ({ i8*, i32, i32, i8* }* getelementptr inbounds ({ i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* null, i32 1) to i64))
  %17 = bitcast i8* %16 to { i8*, i32, i32, i8* }*
  %18 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %17, i32 0, i32 0
  store i8* %4, i8** %18, align 8
  %19 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %17, i32 0, i32 1
  store i32 3, i32* %19, align 8
  %20 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %17, i32 0, i32 2
  store i32 1, i32* %20, align 8
  %21 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %17, i32 0, i32 3
  store i8* %12, i8** %21, align 8
  %22 = bitcast { i8*, i32, i32, i8* }* %17 to i8*
  ret i8* %22
}

define i8* @"anonymous$lifted$0"(i8* %"$Applicative$y206_0", i8* %"$Functor$y206_0", i8* %f_0, i8* %x1_0, i8* %x2_0) {
  %1 = bitcast i8* %"$Applicative$y206_0" to i8*
  %2 = bitcast i8* %"$Functor$y206_0" to i8*
  %3 = bitcast i8* %f_0 to { i8*, i32, i32, i8* }*
  %4 = bitcast i8* %x1_0 to i8*
  %5 = bitcast i8* %x2_0 to i8*
  %6 = bitcast i8* %1 to { { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* } }*
  %7 = getelementptr { { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* } }, { { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* } }* %6, i32 0, i32 0
  %8 = bitcast { i8*, i32, i32, i8* }* %7 to i8*
  %9 = bitcast i8* %2 to { { i8*, i32, i32, i8* } }*
  %10 = getelementptr { { i8*, i32, i32, i8* } }, { { i8*, i32, i32, i8* } }* %9, i32 0, i32 0
  %11 = bitcast { i8*, i32, i32, i8* }* %10 to i8*
  %12 = bitcast { i8*, i32, i32, i8* }* %3 to i8*
  %13 = call i8* (i8*, i32, ...) @__applyPAP__(i8* %11, i32 2, i8* %12, i8* %4)
  %14 = bitcast i8* %13 to i8*
  %15 = call i8* (i8*, i32, ...) @__applyPAP__(i8* %8, i32 2, i8* %14, i8* %5)
  %16 = bitcast i8* %15 to i8*
  ret i8* %16
}

define i8* @"anonymous$lifted$1"(i8* %__0) {
  %1 = bitcast i8* %__0 to double*
  %2 = load double, double* %1, align 8
  %3 = bitcast i8* (i8*, i8*, i8*)* @"$MonadWriter$WriterT$tell" to i8*
  %4 = call i8* @GC_malloc(i64 ptrtoint ({ i8*, i32, i32 }* getelementptr inbounds ({ i8*, i32, i32 }, { i8*, i32, i32 }* null, i32 1) to i64))
  %5 = bitcast i8* %4 to { i8*, i32, i32 }*
  %6 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %5, i32 0, i32 0
  store i8* %3, i8** %6, align 8
  %7 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %5, i32 0, i32 1
  store i32 3, i32* %7, align 8
  %8 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %5, i32 0, i32 2
  store i32 3, i32* %8, align 8
  %9 = bitcast { i8*, i32, i32 }* %5 to i8*
  %10 = bitcast { { i8*, i32, i32 } }* @"$Functor$Identity" to i8*
  %11 = bitcast { { i8*, i32, i32 }, { i8*, i32, i32 } }* @"$Applicative$Identity" to i8*
  %12 = bitcast { { i8*, i32, i32 }, { i8*, i32, i32 } }* @"$Monad$Identity" to i8*
  %13 = call i8* (i8*, i32, ...) @__applyPAP__(i8* %9, i32 3, i8* %10, i8* %11, i8* %12)
  %14 = bitcast i8* %13 to { i8*, i32, i32, i8* }*
  %15 = bitcast { i8*, i32, i32, i8* }* %14 to i8*
  %16 = call i8* @GC_malloc(i64 4)
  %17 = getelementptr i8, i8* %16, i32 0
  store i8 72, i8* %17, align 8
  %18 = getelementptr i8, i8* %16, i32 1
  store i8 79, i8* %18, align 8
  %19 = getelementptr i8, i8* %16, i32 2
  store i8 80, i8* %19, align 8
  %20 = getelementptr i8, i8* %16, i32 3
  store i8 0, i8* %20, align 8
  %21 = call { i8*, i8* }* @MadList_singleton(i8* %16)
  %22 = bitcast { i8*, i8* }* %21 to { i8*, i8* }*
  %23 = bitcast { i8*, i8* }* %22 to i8*
  %24 = call i8* (i8*, i32, ...) @__applyPAP__(i8* %15, i32 1, i8* %23)
  %25 = bitcast i8* %24 to i8*
  ret i8* %25
}

define i8* @"anonymous$lifted$2"(i8* %_P__0) {
  %1 = bitcast i8* %_P__0 to i8*
  %2 = bitcast i8* (i8*, i8*, i8*)* @"$Monad$WriterT$chain" to i8*
  %3 = call i8* @GC_malloc(i64 ptrtoint ({ i8*, i32, i32 }* getelementptr inbounds ({ i8*, i32, i32 }, { i8*, i32, i32 }* null, i32 1) to i64))
  %4 = bitcast i8* %3 to { i8*, i32, i32 }*
  %5 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %4, i32 0, i32 0
  store i8* %2, i8** %5, align 8
  %6 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %4, i32 0, i32 1
  store i32 3, i32* %6, align 8
  %7 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %4, i32 0, i32 2
  store i32 3, i32* %7, align 8
  %8 = bitcast { i8*, i32, i32 }* %4 to i8*
  %9 = bitcast { { i8*, i32, i32 } }* @"$Functor$Identity" to i8*
  %10 = bitcast { { i8*, i32, i32 }, { i8*, i32, i32 } }* @"$Applicative$Identity" to i8*
  %11 = bitcast { { i8*, i32, i32 }, { i8*, i32, i32 } }* @"$Monad$Identity" to i8*
  %12 = call i8* (i8*, i32, ...) @__applyPAP__(i8* %8, i32 3, i8* %9, i8* %10, i8* %11)
  %13 = bitcast i8* %12 to { i8*, i32, i32, i8* }*
  %14 = bitcast { i8*, i32, i32, i8* }* %13 to i8*
  %15 = bitcast i8* (i8*)* @"anonymous$lifted$1" to i8*
  %16 = call i8* @GC_malloc(i64 ptrtoint ({ i8*, i32, i32 }* getelementptr inbounds ({ i8*, i32, i32 }, { i8*, i32, i32 }* null, i32 1) to i64))
  %17 = bitcast i8* %16 to { i8*, i32, i32 }*
  %18 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %17, i32 0, i32 0
  store i8* %15, i8** %18, align 8
  %19 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %17, i32 0, i32 1
  store i32 1, i32* %19, align 8
  %20 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %17, i32 0, i32 2
  store i32 1, i32* %20, align 8
  %21 = bitcast { i8*, i32, i32 }* %17 to i8*
  %22 = call i8* (i8*, i32, ...) @__applyPAP__(i8* %14, i32 2, i8* %21, i8* %1)
  %23 = bitcast i8* %22 to i8*
  %24 = call i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__runSimpleStack(i8* %23)
  %25 = bitcast i8* %24 to { i8*, i8* }*
  %26 = bitcast { i8*, i8* }* %25 to i8*
  ret i8* %26
}

define i8* @"anonymous$lifted$3"(i8* %f_0, i8* %__x___0) {
  %1 = bitcast i8* %f_0 to { i8*, i32, i32, i8* }*
  %2 = bitcast i8* %__x___0 to { i8*, i8* }*
  %3 = getelementptr { i8*, i8* }, { i8*, i8* }* %2, i32 0, i32 0
  %4 = getelementptr { i8*, i8* }, { i8*, i8* }* %2, i32 0, i32 1
  %5 = load i8*, i8** %3, align 8
  %6 = bitcast i8* %5 to i8*
  %7 = and i1 true, true
  %8 = load i8*, i8** %4, align 8
  %9 = bitcast i8* %8 to i8*
  %10 = and i1 %7, true
  br i1 %10, label %branchExpBlock_0, label %exitBlock_0

branchExpBlock_0:                                 ; preds = %0
  %11 = getelementptr { i8*, i8* }, { i8*, i8* }* %2, i32 0, i32 0
  %12 = load i8*, i8** %11, align 8
  %13 = bitcast i8* %12 to i8*
  %14 = getelementptr { i8*, i8* }, { i8*, i8* }* %2, i32 0, i32 1
  %15 = load i8*, i8** %14, align 8
  %16 = bitcast i8* %15 to i8*
  %17 = bitcast { i8*, i32, i32, i8* }* %1 to i8*
  %18 = call i8* (i8*, i32, ...) @__applyPAP__(i8* %17, i32 1, i8* %13)
  %19 = bitcast i8* %18 to i8*
  %20 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %21 = bitcast i8* %20 to { i8*, i8* }*
  %22 = getelementptr { i8*, i8* }, { i8*, i8* }* %21, i32 0, i32 0
  store i8* %19, i8** %22, align 8
  %23 = getelementptr { i8*, i8* }, { i8*, i8* }* %21, i32 0, i32 1
  store i8* %16, i8** %23, align 8
  %24 = bitcast { i8*, i8* }* %21 to i8*
  br label %exitBlock_0

exitBlock_0:                                      ; preds = %branchExpBlock_0, %0
  %25 = phi i8* [ %24, %branchExpBlock_0 ], [ null, %0 ]
  ret i8* %25
}

define i8* @"anonymous$lifted$4"(i8* %"$Functor$t19_0", i8* %__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT_0, i8* %f_0, i8* %m_0) {
  %1 = bitcast i8* %"$Functor$t19_0" to i8*
  %2 = bitcast i8* %__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT_0 to { i8*, i32, i32, i8* }*
  %3 = bitcast i8* %f_0 to { i8*, i32, i32, i8* }*
  %4 = bitcast i8* %m_0 to i8*
  %5 = bitcast i8* %1 to { { i8*, i32, i32, i8* } }*
  %6 = getelementptr { { i8*, i32, i32, i8* } }, { { i8*, i32, i32, i8* } }* %5, i32 0, i32 0
  %7 = bitcast { i8*, i32, i32, i8* }* %6 to i8*
  %8 = bitcast i8* (i8*, i8*)* @"anonymous$lifted$3" to i8*
  %9 = bitcast { i8*, i32, i32, i8* }* %3 to i8*
  %10 = call i8* @GC_malloc(i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64))
  %11 = bitcast i8* %10 to { i8* }*
  %12 = getelementptr { i8* }, { i8* }* %11, i32 0, i32 0
  store i8* %9, i8** %12, align 8
  %13 = call i8* @GC_malloc(i64 ptrtoint ({ i8*, i32, i32, i8* }* getelementptr inbounds ({ i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* null, i32 1) to i64))
  %14 = bitcast i8* %13 to { i8*, i32, i32, i8* }*
  %15 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %14, i32 0, i32 0
  store i8* %8, i8** %15, align 8
  %16 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %14, i32 0, i32 1
  store i32 2, i32* %16, align 8
  %17 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %14, i32 0, i32 2
  store i32 1, i32* %17, align 8
  %18 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %14, i32 0, i32 3
  store i8* %10, i8** %18, align 8
  %19 = call i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__runWriterT(i8* %4)
  %20 = bitcast i8* %19 to i8*
  %21 = bitcast { i8*, i32, i32, i8* }* %14 to i8*
  %22 = call i8* (i8*, i32, ...) @__applyPAP__(i8* %7, i32 2, i8* %21, i8* %20)
  %23 = bitcast i8* %22 to i8*
  %24 = call i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT(i8* %23)
  %25 = bitcast i8* %24 to i8*
  ret i8* %25
}

define i8* @"anonymous$lifted$5"(i8* %x1_0, i8* %x2_0) {
  %1 = bitcast i8* %x1_0 to { i8*, i8* }*
  %2 = bitcast i8* %x2_0 to { i8*, i8* }*
  %3 = getelementptr { i8*, i8* }, { i8*, i8* }* %1, i32 0, i32 0
  %4 = getelementptr { i8*, i8* }, { i8*, i8* }* %1, i32 0, i32 1
  %5 = load i8*, i8** %3, align 8
  %6 = bitcast i8* %5 to { i8*, i32, i32, i8* }*
  %7 = and i1 true, true
  %8 = load i8*, i8** %4, align 8
  %9 = bitcast i8* %8 to { i8*, i8* }*
  %10 = and i1 %7, true
  br i1 %10, label %branchExpBlock_0, label %exitBlock_1

branchExpBlock_0:                                 ; preds = %0
  %11 = getelementptr { i8*, i8* }, { i8*, i8* }* %1, i32 0, i32 0
  %12 = load i8*, i8** %11, align 8
  %13 = bitcast i8* %12 to { i8*, i32, i32, i8* }*
  %14 = getelementptr { i8*, i8* }, { i8*, i8* }* %1, i32 0, i32 1
  %15 = load i8*, i8** %14, align 8
  %16 = bitcast i8* %15 to { i8*, i8* }*
  %17 = getelementptr { i8*, i8* }, { i8*, i8* }* %2, i32 0, i32 0
  %18 = getelementptr { i8*, i8* }, { i8*, i8* }* %2, i32 0, i32 1
  %19 = load i8*, i8** %17, align 8
  %20 = bitcast i8* %19 to i8*
  %21 = and i1 true, true
  %22 = load i8*, i8** %18, align 8
  %23 = bitcast i8* %22 to { i8*, i8* }*
  %24 = and i1 %21, true
  br i1 %24, label %branchExpBlock_1, label %exitBlock_0

branchExpBlock_1:                                 ; preds = %branchExpBlock_0
  %25 = getelementptr { i8*, i8* }, { i8*, i8* }* %2, i32 0, i32 0
  %26 = load i8*, i8** %25, align 8
  %27 = bitcast i8* %26 to i8*
  %28 = getelementptr { i8*, i8* }, { i8*, i8* }* %2, i32 0, i32 1
  %29 = load i8*, i8** %28, align 8
  %30 = bitcast i8* %29 to { i8*, i8* }*
  %31 = bitcast { i8*, i32, i32, i8* }* %13 to i8*
  %32 = call i8* (i8*, i32, ...) @__applyPAP__(i8* %31, i32 1, i8* %27)
  %33 = bitcast i8* %32 to i8*
  %34 = call { i8*, i8* }* @MadList_concat({ i8*, i8* }* %16, { i8*, i8* }* %30)
  %35 = bitcast { i8*, i8* }* %34 to { i8*, i8* }*
  %36 = bitcast { i8*, i8* }* %35 to i8*
  %37 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %38 = bitcast i8* %37 to { i8*, i8* }*
  %39 = getelementptr { i8*, i8* }, { i8*, i8* }* %38, i32 0, i32 0
  store i8* %33, i8** %39, align 8
  %40 = getelementptr { i8*, i8* }, { i8*, i8* }* %38, i32 0, i32 1
  store i8* %36, i8** %40, align 8
  %41 = bitcast { i8*, i8* }* %38 to i8*
  br label %exitBlock_0

exitBlock_0:                                      ; preds = %branchExpBlock_1, %branchExpBlock_0
  %42 = phi i8* [ %41, %branchExpBlock_1 ], [ null, %branchExpBlock_0 ]
  br label %exitBlock_1

exitBlock_1:                                      ; preds = %exitBlock_0, %0
  %43 = phi i8* [ %42, %exitBlock_0 ], [ null, %0 ]
  ret i8* %43
}

define i8* @"anonymous$lifted$6"(i8* %"$Applicative$d55_0", i8* %__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT_0, i8* %mf_0, i8* %mm_0) {
  %1 = bitcast i8* %"$Applicative$d55_0" to i8*
  %2 = bitcast i8* %__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT_0 to { i8*, i32, i32, i8* }*
  %3 = bitcast i8* %mf_0 to i8*
  %4 = bitcast i8* %mm_0 to i8*
  %5 = bitcast i8* (i8*, i8*)* @__eddaab7dd3adaa6b7d769751eb6c3b5e__liftA2 to i8*
  %6 = call i8* @GC_malloc(i64 ptrtoint ({ i8*, i32, i32 }* getelementptr inbounds ({ i8*, i32, i32 }, { i8*, i32, i32 }* null, i32 1) to i64))
  %7 = bitcast i8* %6 to { i8*, i32, i32 }*
  %8 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %7, i32 0, i32 0
  store i8* %5, i8** %8, align 8
  %9 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %7, i32 0, i32 1
  store i32 2, i32* %9, align 8
  %10 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %7, i32 0, i32 2
  store i32 2, i32* %10, align 8
  %11 = bitcast { i8*, i32, i32 }* %7 to i8*
  %12 = call i8* (i8*, i32, ...) @__applyPAP__(i8* %11, i32 1, i8* %1)
  %13 = bitcast i8* %12 to { i8*, i32, i32, i8* }*
  %14 = bitcast { i8*, i32, i32, i8* }* %13 to i8*
  %15 = bitcast i8* (i8*, i8*)* @"anonymous$lifted$5" to i8*
  %16 = call i8* @GC_malloc(i64 ptrtoint ({ i8*, i32, i32 }* getelementptr inbounds ({ i8*, i32, i32 }, { i8*, i32, i32 }* null, i32 1) to i64))
  %17 = bitcast i8* %16 to { i8*, i32, i32 }*
  %18 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %17, i32 0, i32 0
  store i8* %15, i8** %18, align 8
  %19 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %17, i32 0, i32 1
  store i32 2, i32* %19, align 8
  %20 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %17, i32 0, i32 2
  store i32 2, i32* %20, align 8
  %21 = call i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__runWriterT(i8* %3)
  %22 = bitcast i8* %21 to i8*
  %23 = call i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__runWriterT(i8* %4)
  %24 = bitcast i8* %23 to i8*
  %25 = bitcast { i8*, i32, i32 }* %17 to i8*
  %26 = call i8* (i8*, i32, ...) @__applyPAP__(i8* %14, i32 3, i8* %25, i8* %22, i8* %24)
  %27 = bitcast i8* %26 to i8*
  %28 = call i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT(i8* %27)
  %29 = bitcast i8* %28 to i8*
  ret i8* %29
}

define i8* @"anonymous$lifted$7"(i8* %"$Applicative$p67_0", i8* %__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT_0, i8* %x_0) {
  %1 = bitcast i8* %"$Applicative$p67_0" to i8*
  %2 = bitcast i8* %__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT_0 to { i8*, i32, i32, i8* }*
  %3 = bitcast i8* %x_0 to i8*
  %4 = bitcast i8* %1 to { { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* } }*
  %5 = getelementptr { { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* } }, { { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* } }* %4, i32 0, i32 1
  %6 = bitcast { i8*, i32, i32, i8* }* %5 to i8*
  %7 = bitcast { i8*, i8* }* null to i8*
  %8 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %9 = bitcast i8* %8 to { i8*, i8* }*
  %10 = getelementptr { i8*, i8* }, { i8*, i8* }* %9, i32 0, i32 0
  store i8* %3, i8** %10, align 8
  %11 = getelementptr { i8*, i8* }, { i8*, i8* }* %9, i32 0, i32 1
  store i8* %7, i8** %11, align 8
  %12 = bitcast { i8*, i8* }* %9 to i8*
  %13 = call i8* (i8*, i32, ...) @__applyPAP__(i8* %6, i32 1, i8* %12)
  %14 = bitcast i8* %13 to i8*
  %15 = call i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT(i8* %14)
  %16 = bitcast i8* %15 to i8*
  ret i8* %16
}

define i8* @"anonymous$lifted$8"(i8* %"$Monad$f109_0", i8* %w_0, i8* %__x___0) {
  %1 = bitcast i8* %"$Monad$f109_0" to i8*
  %2 = bitcast i8* %w_0 to { i8*, i8* }*
  %3 = bitcast i8* %__x___0 to { i8*, i8* }*
  %4 = getelementptr { i8*, i8* }, { i8*, i8* }* %3, i32 0, i32 0
  %5 = getelementptr { i8*, i8* }, { i8*, i8* }* %3, i32 0, i32 1
  %6 = load i8*, i8** %4, align 8
  %7 = bitcast i8* %6 to i8*
  %8 = and i1 true, true
  %9 = load i8*, i8** %5, align 8
  %10 = bitcast i8* %9 to { i8*, i8* }*
  %11 = and i1 %8, true
  br i1 %11, label %branchExpBlock_0, label %exitBlock_0

branchExpBlock_0:                                 ; preds = %0
  %12 = getelementptr { i8*, i8* }, { i8*, i8* }* %3, i32 0, i32 0
  %13 = load i8*, i8** %12, align 8
  %14 = bitcast i8* %13 to i8*
  %15 = getelementptr { i8*, i8* }, { i8*, i8* }* %3, i32 0, i32 1
  %16 = load i8*, i8** %15, align 8
  %17 = bitcast i8* %16 to { i8*, i8* }*
  %18 = bitcast i8* %1 to { { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* } }*
  %19 = getelementptr { { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* } }, { { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* } }* %18, i32 0, i32 1
  %20 = bitcast { i8*, i32, i32, i8* }* %19 to i8*
  %21 = call { i8*, i8* }* @MadList_concat({ i8*, i8* }* %2, { i8*, i8* }* %17)
  %22 = bitcast { i8*, i8* }* %21 to { i8*, i8* }*
  %23 = bitcast { i8*, i8* }* %22 to i8*
  %24 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %25 = bitcast i8* %24 to { i8*, i8* }*
  %26 = getelementptr { i8*, i8* }, { i8*, i8* }* %25, i32 0, i32 0
  store i8* %14, i8** %26, align 8
  %27 = getelementptr { i8*, i8* }, { i8*, i8* }* %25, i32 0, i32 1
  store i8* %23, i8** %27, align 8
  %28 = bitcast { i8*, i8* }* %25 to i8*
  %29 = call i8* (i8*, i32, ...) @__applyPAP__(i8* %20, i32 1, i8* %28)
  %30 = bitcast i8* %29 to i8*
  br label %exitBlock_0

exitBlock_0:                                      ; preds = %branchExpBlock_0, %0
  %31 = phi i8* [ %30, %branchExpBlock_0 ], [ null, %0 ]
  ret i8* %31
}

define i8* @"anonymous$lifted$9"(i8* %"$Monad$f109_0", i8* %f_0, i8* %__x___0) {
  %1 = bitcast i8* %"$Monad$f109_0" to i8*
  %2 = bitcast i8* %f_0 to { i8*, i32, i32, i8* }*
  %3 = bitcast i8* %__x___0 to { i8*, i8* }*
  %4 = getelementptr { i8*, i8* }, { i8*, i8* }* %3, i32 0, i32 0
  %5 = getelementptr { i8*, i8* }, { i8*, i8* }* %3, i32 0, i32 1
  %6 = load i8*, i8** %4, align 8
  %7 = bitcast i8* %6 to i8*
  %8 = and i1 true, true
  %9 = load i8*, i8** %5, align 8
  %10 = bitcast i8* %9 to { i8*, i8* }*
  %11 = and i1 %8, true
  br i1 %11, label %branchExpBlock_0, label %exitBlock_0

branchExpBlock_0:                                 ; preds = %0
  %12 = getelementptr { i8*, i8* }, { i8*, i8* }* %3, i32 0, i32 0
  %13 = load i8*, i8** %12, align 8
  %14 = bitcast i8* %13 to i8*
  %15 = getelementptr { i8*, i8* }, { i8*, i8* }* %3, i32 0, i32 1
  %16 = load i8*, i8** %15, align 8
  %17 = bitcast i8* %16 to { i8*, i8* }*
  %18 = bitcast i8* %1 to { { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* } }*
  %19 = getelementptr { { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* } }, { { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* } }* %18, i32 0, i32 0
  %20 = bitcast { i8*, i32, i32, i8* }* %19 to i8*
  %21 = bitcast i8* (i8*, i8*, i8*)* @"anonymous$lifted$8" to i8*
  %22 = bitcast { i8*, i8* }* %17 to i8*
  %23 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %24 = bitcast i8* %23 to { i8*, i8* }*
  %25 = getelementptr { i8*, i8* }, { i8*, i8* }* %24, i32 0, i32 0
  store i8* %1, i8** %25, align 8
  %26 = getelementptr { i8*, i8* }, { i8*, i8* }* %24, i32 0, i32 1
  store i8* %22, i8** %26, align 8
  %27 = call i8* @GC_malloc(i64 ptrtoint ({ i8*, i32, i32, i8* }* getelementptr inbounds ({ i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* null, i32 1) to i64))
  %28 = bitcast i8* %27 to { i8*, i32, i32, i8* }*
  %29 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %28, i32 0, i32 0
  store i8* %21, i8** %29, align 8
  %30 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %28, i32 0, i32 1
  store i32 3, i32* %30, align 8
  %31 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %28, i32 0, i32 2
  store i32 1, i32* %31, align 8
  %32 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %28, i32 0, i32 3
  store i8* %23, i8** %32, align 8
  %33 = bitcast { i8*, i32, i32, i8* }* %2 to i8*
  %34 = call i8* (i8*, i32, ...) @__applyPAP__(i8* %33, i32 1, i8* %14)
  %35 = bitcast i8* %34 to i8*
  %36 = call i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__runWriterT(i8* %35)
  %37 = bitcast i8* %36 to i8*
  %38 = bitcast { i8*, i32, i32, i8* }* %28 to i8*
  %39 = call i8* (i8*, i32, ...) @__applyPAP__(i8* %20, i32 2, i8* %38, i8* %37)
  %40 = bitcast i8* %39 to i8*
  br label %exitBlock_0

exitBlock_0:                                      ; preds = %branchExpBlock_0, %0
  %41 = phi i8* [ %40, %branchExpBlock_0 ], [ null, %0 ]
  ret i8* %41
}

define i8* @"anonymous$lifted$10"(i8* %"$Monad$f109_0", i8* %__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT_0, i8* %f_0, i8* %m_0) {
  %1 = bitcast i8* %"$Monad$f109_0" to i8*
  %2 = bitcast i8* %__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT_0 to { i8*, i32, i32, i8* }*
  %3 = bitcast i8* %f_0 to { i8*, i32, i32, i8* }*
  %4 = bitcast i8* %m_0 to i8*
  %5 = bitcast i8* %1 to { { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* } }*
  %6 = getelementptr { { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* } }, { { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* } }* %5, i32 0, i32 0
  %7 = bitcast { i8*, i32, i32, i8* }* %6 to i8*
  %8 = bitcast i8* (i8*, i8*, i8*)* @"anonymous$lifted$9" to i8*
  %9 = bitcast { i8*, i32, i32, i8* }* %3 to i8*
  %10 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %11 = bitcast i8* %10 to { i8*, i8* }*
  %12 = getelementptr { i8*, i8* }, { i8*, i8* }* %11, i32 0, i32 0
  store i8* %1, i8** %12, align 8
  %13 = getelementptr { i8*, i8* }, { i8*, i8* }* %11, i32 0, i32 1
  store i8* %9, i8** %13, align 8
  %14 = call i8* @GC_malloc(i64 ptrtoint ({ i8*, i32, i32, i8* }* getelementptr inbounds ({ i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* null, i32 1) to i64))
  %15 = bitcast i8* %14 to { i8*, i32, i32, i8* }*
  %16 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %15, i32 0, i32 0
  store i8* %8, i8** %16, align 8
  %17 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %15, i32 0, i32 1
  store i32 3, i32* %17, align 8
  %18 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %15, i32 0, i32 2
  store i32 1, i32* %18, align 8
  %19 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %15, i32 0, i32 3
  store i8* %10, i8** %19, align 8
  %20 = call i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__runWriterT(i8* %4)
  %21 = bitcast i8* %20 to i8*
  %22 = bitcast { i8*, i32, i32, i8* }* %15 to i8*
  %23 = call i8* (i8*, i32, ...) @__applyPAP__(i8* %7, i32 2, i8* %22, i8* %21)
  %24 = bitcast i8* %23 to i8*
  %25 = call i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT(i8* %24)
  %26 = bitcast i8* %25 to i8*
  ret i8* %26
}

define i8* @"anonymous$lifted$11"(i8* %"$Monad$g136_0", i8* %a_0) {
  %1 = bitcast i8* %"$Monad$g136_0" to i8*
  %2 = bitcast i8* %a_0 to i8*
  %3 = bitcast i8* %1 to { { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* } }*
  %4 = getelementptr { { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* } }, { { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* } }* %3, i32 0, i32 1
  %5 = bitcast { i8*, i32, i32, i8* }* %4 to i8*
  %6 = bitcast { i8*, i8* }* null to i8*
  %7 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %8 = bitcast i8* %7 to { i8*, i8* }*
  %9 = getelementptr { i8*, i8* }, { i8*, i8* }* %8, i32 0, i32 0
  store i8* %2, i8** %9, align 8
  %10 = getelementptr { i8*, i8* }, { i8*, i8* }* %8, i32 0, i32 1
  store i8* %6, i8** %10, align 8
  %11 = bitcast { i8*, i8* }* %8 to i8*
  %12 = call i8* (i8*, i32, ...) @__applyPAP__(i8* %5, i32 1, i8* %11)
  %13 = bitcast i8* %12 to i8*
  ret i8* %13
}

define i8* @"anonymous$lifted$12"(i8* %"$Monad$g136_0", i8* %__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT_0, i8* %m_0) {
  %1 = bitcast i8* %"$Monad$g136_0" to i8*
  %2 = bitcast i8* %__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT_0 to { i8*, i32, i32, i8* }*
  %3 = bitcast i8* %m_0 to i8*
  %4 = bitcast i8* %1 to { { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* } }*
  %5 = getelementptr { { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* } }, { { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* } }* %4, i32 0, i32 0
  %6 = bitcast { i8*, i32, i32, i8* }* %5 to i8*
  %7 = bitcast i8* (i8*, i8*)* @"anonymous$lifted$11" to i8*
  %8 = call i8* @GC_malloc(i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64))
  %9 = bitcast i8* %8 to { i8* }*
  %10 = getelementptr { i8* }, { i8* }* %9, i32 0, i32 0
  store i8* %1, i8** %10, align 8
  %11 = call i8* @GC_malloc(i64 ptrtoint ({ i8*, i32, i32, i8* }* getelementptr inbounds ({ i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* null, i32 1) to i64))
  %12 = bitcast i8* %11 to { i8*, i32, i32, i8* }*
  %13 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %12, i32 0, i32 0
  store i8* %7, i8** %13, align 8
  %14 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %12, i32 0, i32 1
  store i32 2, i32* %14, align 8
  %15 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %12, i32 0, i32 2
  store i32 1, i32* %15, align 8
  %16 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %12, i32 0, i32 3
  store i8* %8, i8** %16, align 8
  %17 = bitcast { i8*, i32, i32, i8* }* %12 to i8*
  %18 = call i8* (i8*, i32, ...) @__applyPAP__(i8* %6, i32 2, i8* %17, i8* %3)
  %19 = bitcast i8* %18 to i8*
  %20 = call i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT(i8* %19)
  %21 = bitcast i8* %20 to i8*
  ret i8* %21
}

define i8* @"anonymous$lifted$13"(i8* %"$Monad$l193_0", i8* %__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT_0, i8* %v_0) {
  %1 = bitcast i8* %"$Monad$l193_0" to i8*
  %2 = bitcast i8* %__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT_0 to { i8*, i32, i32, i8* }*
  %3 = bitcast i8* %v_0 to { i8*, i8* }*
  %4 = bitcast i8* %1 to { { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* } }*
  %5 = getelementptr { { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* } }, { { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* } }* %4, i32 0, i32 1
  %6 = bitcast { i8*, i32, i32, i8* }* %5 to i8*
  %7 = bitcast i1* null to i8*
  %8 = bitcast { i8*, i8* }* %3 to i8*
  %9 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %10 = bitcast i8* %9 to { i8*, i8* }*
  %11 = getelementptr { i8*, i8* }, { i8*, i8* }* %10, i32 0, i32 0
  store i8* %7, i8** %11, align 8
  %12 = getelementptr { i8*, i8* }, { i8*, i8* }* %10, i32 0, i32 1
  store i8* %8, i8** %12, align 8
  %13 = bitcast { i8*, i8* }* %10 to i8*
  %14 = call i8* (i8*, i32, ...) @__applyPAP__(i8* %6, i32 1, i8* %13)
  %15 = bitcast i8* %14 to i8*
  %16 = call i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT(i8* %15)
  %17 = bitcast i8* %16 to i8*
  ret i8* %17
}

define i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__runWriterT(i8* %__x___0) {
  %1 = bitcast i8* %__x___0 to i8*
  %2 = bitcast i8* %1 to { i64, i8* }*
  %3 = getelementptr { i64, i8* }, { i64, i8* }* %2, i32 0, i32 1
  %4 = getelementptr { i64, i8* }, { i64, i8* }* %2, i32 0, i32 0
  %5 = load i64, i64* %4, align 8
  %6 = icmp eq i64 0, %5
  %7 = load i8*, i8** %3, align 8
  %8 = bitcast i8* %7 to i8*
  %9 = and i1 true, true
  %10 = and i1 %6, %9
  br i1 %10, label %branchExpBlock_0, label %exitBlock_0

branchExpBlock_0:                                 ; preds = %0
  %11 = bitcast i8* %1 to { i64, i8* }*
  %12 = getelementptr { i64, i8* }, { i64, i8* }* %11, i32 0, i32 1
  %13 = load i8*, i8** %12, align 8
  %14 = bitcast i8* %13 to i8*
  br label %exitBlock_0

exitBlock_0:                                      ; preds = %branchExpBlock_0, %0
  %15 = phi i8* [ %14, %branchExpBlock_0 ], [ null, %0 ]
  ret i8* %15
}

define i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__liftA2(i8* %"$Functor$y206_0", i8* %"$Applicative$y206_0") {
  %1 = bitcast i8* %"$Functor$y206_0" to i8*
  %2 = bitcast i8* %"$Applicative$y206_0" to i8*
  %3 = bitcast i8* (i8*, i8*, i8*, i8*, i8*)* @"anonymous$lifted$0" to i8*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { i8*, i8* }*
  %6 = getelementptr { i8*, i8* }, { i8*, i8* }* %5, i32 0, i32 0
  store i8* %2, i8** %6, align 8
  %7 = getelementptr { i8*, i8* }, { i8*, i8* }* %5, i32 0, i32 1
  store i8* %1, i8** %7, align 8
  %8 = call i8* @GC_malloc(i64 ptrtoint ({ i8*, i32, i32, i8* }* getelementptr inbounds ({ i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* null, i32 1) to i64))
  %9 = bitcast i8* %8 to { i8*, i32, i32, i8* }*
  %10 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %9, i32 0, i32 0
  store i8* %3, i8** %10, align 8
  %11 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %9, i32 0, i32 1
  store i32 5, i32* %11, align 8
  %12 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %9, i32 0, i32 2
  store i32 3, i32* %12, align 8
  %13 = getelementptr { i8*, i32, i32, i8* }, { i8*, i32, i32, i8* }* %9, i32 0, i32 3
  store i8* %4, i8** %13, align 8
  %14 = bitcast { i8*, i32, i32, i8* }* %9 to i8*
  ret i8* %14
}

define i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__runIdentity(i8* %__x___0) {
  %1 = bitcast i8* %__x___0 to i8*
  %2 = bitcast i8* %1 to { i64, i8* }*
  %3 = getelementptr { i64, i8* }, { i64, i8* }* %2, i32 0, i32 1
  %4 = getelementptr { i64, i8* }, { i64, i8* }* %2, i32 0, i32 0
  %5 = load i64, i64* %4, align 8
  %6 = icmp eq i64 0, %5
  %7 = load i8*, i8** %3, align 8
  %8 = bitcast i8* %7 to i8*
  %9 = and i1 true, true
  %10 = and i1 %6, %9
  br i1 %10, label %branchExpBlock_0, label %exitBlock_0

branchExpBlock_0:                                 ; preds = %0
  %11 = bitcast i8* %1 to { i64, i8* }*
  %12 = getelementptr { i64, i8* }, { i64, i8* }* %11, i32 0, i32 1
  %13 = load i8*, i8** %12, align 8
  %14 = bitcast i8* %13 to i8*
  br label %exitBlock_0

exitBlock_0:                                      ; preds = %branchExpBlock_0, %0
  %15 = phi i8* [ %14, %branchExpBlock_0 ], [ null, %0 ]
  ret i8* %15
}

define i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__runSimpleStack(i8* %_P__0) {
  %1 = bitcast i8* %_P__0 to i8*
  %2 = call i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__runWriterT(i8* %1)
  %3 = bitcast i8* %2 to i8*
  %4 = call i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__runIdentity(i8* %3)
  %5 = bitcast i8* %4 to { i8*, i8* }*
  %6 = bitcast { i8*, i8* }* %5 to i8*
  ret i8* %6
}

declare i8* @__applyPAP__(i8*, i32, ...)

declare i8* @malloc(i64)

declare i8* @GC_malloc(i64)

declare i8* @calloc(i32, i32)

declare i1 @__streq__(i8*, i8*)

declare i8* @__strConcat__(i8*, i8*)

declare i1 @MadList_hasMinLength(double, { i8*, i8* }*)

declare i1 @MadList_hasLength(double, { i8*, i8* }*)

declare { i8*, i8* }* @MadList_singleton(i8*)

declare { i8*, i8* }* @__MadList_push__(i8*, { i8*, i8* }*)

declare { i8*, i8* }* @MadList_concat({ i8*, i8* }*, { i8*, i8* }*)

define void @main() {
entry_0:
  %0 = bitcast i8* (i8*, i8*, i8*, i8*, i8*)* @"$Monad$WriterT$of" to i8*
  %1 = call i8* @GC_malloc(i64 ptrtoint ({ i8*, i32, i32 }* getelementptr inbounds ({ i8*, i32, i32 }, { i8*, i32, i32 }* null, i32 1) to i64))
  %2 = bitcast i8* %1 to { i8*, i32, i32 }*
  %3 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %2, i32 0, i32 0
  store i8* %0, i8** %3, align 8
  %4 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %2, i32 0, i32 1
  store i32 5, i32* %4, align 8
  %5 = getelementptr { i8*, i32, i32 }, { i8*, i32, i32 }* %2, i32 0, i32 2
  store i32 5, i32* %5, align 8
  %6 = bitcast { i8*, i32, i32 }* %2 to i8*
  %7 = bitcast { { i8*, i32, i32 } }* @"$Functor$Identity" to i8*
  %8 = bitcast { { i8*, i32, i32 }, { i8*, i32, i32 } }* @"$Applicative$Identity" to i8*
  %9 = bitcast { { i8*, i32, i32 }, { i8*, i32, i32 } }* @"$Monad$Identity" to i8*
  %10 = call i8* (i8*, i32, ...) @__applyPAP__(i8* %6, i32 3, i8* %7, i8* %8, i8* %9)
  ret void
}
