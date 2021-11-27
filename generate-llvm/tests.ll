; ModuleID = 'main'


 


define external ccc  i8* @__920d155ad8fe38d98e9ad22cf162d40f__WriterT(i8* )    {
  %2 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64, i8*}* getelementptr inbounds ({i64, i8*}, {i64, i8*}* inttoptr (i32 0 to {i64, i8*}*), i32 1) to i64))  
  %3 = bitcast i8* %2 to {i64, i8*}* 
  %4 = getelementptr  {i64, i8*}, {i64, i8*}* %3, i32 0, i32 1 
  store  i8* %0, i8** %4, align 8 
  %5 = getelementptr  {i64, i8*}, {i64, i8*}* %3, i32 0, i32 0 
  store  i64 0, i64* %5, align 8 
  %6 = bitcast {i64, i8*}* %3 to i8* 
  ret i8* %6 
}


define external ccc  i8* @__920d155ad8fe38d98e9ad22cf162d40f__Identity(i8* )    {
  %2 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64, i8*}* getelementptr inbounds ({i64, i8*}, {i64, i8*}* inttoptr (i32 0 to {i64, i8*}*), i32 1) to i64))  
  %3 = bitcast i8* %2 to {i64, i8*}* 
  %4 = getelementptr  {i64, i8*}, {i64, i8*}* %3, i32 0, i32 1 
  store  i8* %0, i8** %4, align 8 
  %5 = getelementptr  {i64, i8*}, {i64, i8*}* %3, i32 0, i32 0 
  store  i64 0, i64* %5, align 8 
  %6 = bitcast {i64, i8*}* %3 to i8* 
  ret i8* %6 
}


define external ccc  i8* @$Semigroup$List$assoc(i8*  %xs1_0, i8*  %xs2_0)    {
  %1 = bitcast i8* %xs1_0 to {i8*, i8*}* 
  %2 = bitcast i8* %xs2_0 to {i8*, i8*}* 
  %3 =  call ccc  {i8*, i8*}*  @MadList_concat({i8*, i8*}*  %1, {i8*, i8*}*  %2)  
  %4 = bitcast {i8*, i8*}* %3 to {i8*, i8*}* 
  %5 = bitcast {i8*, i8*}* %4 to i8* 
  ret i8* %5 
}


@$Semigroup$List =    global {{i8*, i32, i32, i8*}} { {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*)* @$Semigroup$List$assoc to i8*), i32 2, i32 2, i8* undef } }


define external ccc  i8* @$Monoid$List$mappend(i8*  %xs1_0, i8*  %xs2_0)    {
  %1 = bitcast i8* %xs1_0 to {i8*, i8*}* 
  %2 = bitcast i8* %xs2_0 to {i8*, i8*}* 
  %3 = bitcast {i8*, i8*}* %1 to i8* 
  %4 = bitcast {i8*, i8*}* %2 to i8* 
  %5 =  call ccc  i8*  @__920d155ad8fe38d98e9ad22cf162d40f__conc(i8*  %3, i8*  %4)  
  %6 = bitcast i8* %5 to {i8*, i8*}* 
  %7 = bitcast {i8*, i8*}* %6 to i8* 
  ret i8* %7 
}


define external ccc  i8* @$Monoid$List$mempty()    {
  ret {i8*, i8*}* zeroinitializer 
}


@$Monoid$List =    global {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}} { {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*)* @$Monoid$List$mappend to i8*), i32 2, i32 2, i8* undef }, {i8*, i32, i32, i8*} { i8* bitcast (i8* ()* @$Monoid$List$mempty to i8*), i32 0, i32 0, i8* undef } }


define external ccc  i8* @$Functor$WriterT$map(i8*  %$Functor$m38_0)    {
  %1 = bitcast i8* %$Functor$m38_0 to i8* 
  %2 = bitcast i8* (i8*, i8*, i8*)* @anonymous$lifted$8 to i8* 
  %3 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %4 = bitcast i8* %3 to {i8*}* 
  %5 = getelementptr  {i8*}, {i8*}* %4, i32 0, i32 0 
  store  i8* %1, i8** %5, align 8 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {i8*, i32, i32, i8*}* 
  %8 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 0 
  store  i8* %2, i8** %8, align 8 
  %9 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 1 
  store  i32 3, i32* %9, align 8 
  %10 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 2 
  store  i32 2, i32* %10, align 8 
  %11 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 3 
  store  i8* %3, i8** %11, align 8 
  %12 = bitcast {i8*, i32, i32, i8*}* %7 to i8* 
  ret i8* %12 
}


@$Functor$WriterT =    global {{i8*, i32, i32, i8*}} { {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*)* @$Functor$WriterT$map to i8*), i32 1, i32 1, i8* undef } }


define external ccc  i8* @$Applicative$WriterT$ap(i8*  %$Semigroup$z77_0, i8*  %$Monoid$z77_0, i8*  %$Functor$a78_0, i8*  %$Applicative$a78_0)    {
  %1 = bitcast i8* %$Semigroup$z77_0 to i8* 
  %2 = bitcast i8* %$Monoid$z77_0 to i8* 
  %3 = bitcast i8* %$Functor$a78_0 to i8* 
  %4 = bitcast i8* %$Applicative$a78_0 to i8* 
  %5 = bitcast i8* (i8*, i8*, i8*, i8*)* @anonymous$lifted$10 to i8* 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {i8*, i8*}* 
  %8 = getelementptr  {i8*, i8*}, {i8*, i8*}* %7, i32 0, i32 0 
  store  i8* %4, i8** %8, align 8 
  %9 = getelementptr  {i8*, i8*}, {i8*, i8*}* %7, i32 0, i32 1 
  store  i8* %2, i8** %9, align 8 
  %10 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %11 = bitcast i8* %10 to {i8*, i32, i32, i8*}* 
  %12 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %11, i32 0, i32 0 
  store  i8* %5, i8** %12, align 8 
  %13 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %11, i32 0, i32 1 
  store  i32 4, i32* %13, align 8 
  %14 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %11, i32 0, i32 2 
  store  i32 2, i32* %14, align 8 
  %15 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %11, i32 0, i32 3 
  store  i8* %6, i8** %15, align 8 
  %16 = bitcast {i8*, i32, i32, i8*}* %11 to i8* 
  ret i8* %16 
}


define external ccc  i8* @$Applicative$WriterT$pure(i8*  %$Semigroup$q94_0, i8*  %$Monoid$q94_0, i8*  %$Functor$o92_0, i8*  %$Applicative$o92_0)    {
  %1 = bitcast i8* %$Semigroup$q94_0 to i8* 
  %2 = bitcast i8* %$Monoid$q94_0 to i8* 
  %3 = bitcast i8* %$Functor$o92_0 to i8* 
  %4 = bitcast i8* %$Applicative$o92_0 to i8* 
  %5 = bitcast i8* (i8*, i8*, i8*)* @anonymous$lifted$11 to i8* 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {i8*, i8*}* 
  %8 = getelementptr  {i8*, i8*}, {i8*, i8*}* %7, i32 0, i32 0 
  store  i8* %4, i8** %8, align 8 
  %9 = getelementptr  {i8*, i8*}, {i8*, i8*}* %7, i32 0, i32 1 
  store  i8* %2, i8** %9, align 8 
  %10 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %11 = bitcast i8* %10 to {i8*, i32, i32, i8*}* 
  %12 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %11, i32 0, i32 0 
  store  i8* %5, i8** %12, align 8 
  %13 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %11, i32 0, i32 1 
  store  i32 3, i32* %13, align 8 
  %14 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %11, i32 0, i32 2 
  store  i32 1, i32* %14, align 8 
  %15 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %11, i32 0, i32 3 
  store  i8* %6, i8** %15, align 8 
  %16 = bitcast {i8*, i32, i32, i8*}* %11 to i8* 
  ret i8* %16 
}


@$Applicative$WriterT =    global {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}} { {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*, i8*, i8*)* @$Applicative$WriterT$ap to i8*), i32 4, i32 4, i8* undef }, {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*, i8*, i8*)* @$Applicative$WriterT$pure to i8*), i32 4, i32 4, i8* undef } }


define external ccc  i8* @$Monad$WriterT$chain(i8*  %$Functor$i138_0, i8*  %$Applicative$i138_0, i8*  %$Monad$i138_0, i8*  %$Semigroup$h137_0, i8*  %$Monoid$h137_0)    {
  %1 = bitcast i8* %$Functor$i138_0 to i8* 
  %2 = bitcast i8* %$Applicative$i138_0 to i8* 
  %3 = bitcast i8* %$Monad$i138_0 to i8* 
  %4 = bitcast i8* %$Semigroup$h137_0 to i8* 
  %5 = bitcast i8* %$Monoid$h137_0 to i8* 
  %6 = bitcast i8* (i8*, i8*, i8*, i8*)* @anonymous$lifted$14 to i8* 
  %7 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %8 = bitcast i8* %7 to {i8*, i8*}* 
  %9 = getelementptr  {i8*, i8*}, {i8*, i8*}* %8, i32 0, i32 0 
  store  i8* %3, i8** %9, align 8 
  %10 = getelementptr  {i8*, i8*}, {i8*, i8*}* %8, i32 0, i32 1 
  store  i8* %5, i8** %10, align 8 
  %11 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %12 = bitcast i8* %11 to {i8*, i32, i32, i8*}* 
  %13 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %12, i32 0, i32 0 
  store  i8* %6, i8** %13, align 8 
  %14 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %12, i32 0, i32 1 
  store  i32 4, i32* %14, align 8 
  %15 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %12, i32 0, i32 2 
  store  i32 2, i32* %15, align 8 
  %16 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %12, i32 0, i32 3 
  store  i8* %7, i8** %16, align 8 
  %17 = bitcast {i8*, i32, i32, i8*}* %12 to i8* 
  ret i8* %17 
}


define external ccc  i8* @$Monad$WriterT$of(i8*  %$Functor$w152_0, i8*  %$Applicative$w152_0, i8*  %$Monad$w152_0, i8*  %$Semigroup$y154_0, i8*  %$Monoid$y154_0)    {
  %1 = bitcast i8* %$Functor$w152_0 to i8* 
  %2 = bitcast i8* %$Applicative$w152_0 to i8* 
  %3 = bitcast i8* %$Monad$w152_0 to i8* 
  %4 = bitcast i8* %$Semigroup$y154_0 to i8* 
  %5 = bitcast i8* %$Monoid$y154_0 to i8* 
  %6 = bitcast i8* (i8*, i8*, i8*)* @anonymous$lifted$15 to i8* 
  %7 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %8 = bitcast i8* %7 to {i8*, i8*}* 
  %9 = getelementptr  {i8*, i8*}, {i8*, i8*}* %8, i32 0, i32 0 
  store  i8* %2, i8** %9, align 8 
  %10 = getelementptr  {i8*, i8*}, {i8*, i8*}* %8, i32 0, i32 1 
  store  i8* %5, i8** %10, align 8 
  %11 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %12 = bitcast i8* %11 to {i8*, i32, i32, i8*}* 
  %13 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %12, i32 0, i32 0 
  store  i8* %6, i8** %13, align 8 
  %14 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %12, i32 0, i32 1 
  store  i32 3, i32* %14, align 8 
  %15 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %12, i32 0, i32 2 
  store  i32 1, i32* %15, align 8 
  %16 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %12, i32 0, i32 3 
  store  i8* %7, i8** %16, align 8 
  %17 = bitcast {i8*, i32, i32, i8*}* %12 to i8* 
  ret i8* %17 
}


@$Monad$WriterT =    global {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}} { {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*, i8*, i8*, i8*)* @$Monad$WriterT$chain to i8*), i32 5, i32 5, i8* undef }, {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*, i8*, i8*, i8*)* @$Monad$WriterT$of to i8*), i32 5, i32 5, i8* undef } }


define external ccc  i8* @$Functor$Identity$map(i8*  %f_0, i8*  %m_0)    {
  %1 = bitcast i8* %f_0 to {i8*, i32, i32, i8*}* 
  %2 = bitcast i8* %m_0 to i8* 
  %3 = bitcast {i8*, i32, i32, i8*}* %1 to i8* 
  %4 =  call ccc  i8*  @__920d155ad8fe38d98e9ad22cf162d40f__runIdentity(i8*  %2)  
  %5 = bitcast i8* %4 to i8* 
  %6 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %3, i32  1, i8*  %5)  
  %7 = bitcast i8* %6 to i8* 
  %8 =  call ccc  i8*  @__920d155ad8fe38d98e9ad22cf162d40f__Identity(i8*  %7)  
  %9 = bitcast i8* %8 to i8* 
  ret i8* %9 
}


@$Functor$Identity =    global {{i8*, i32, i32, i8*}} { {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*)* @$Functor$Identity$map to i8*), i32 2, i32 2, i8* undef } }


define external ccc  i8* @$Applicative$Identity$ap(i8*  %mf_0, i8*  %mm_0)    {
  %1 = bitcast i8* %mf_0 to i8* 
  %2 = bitcast i8* %mm_0 to i8* 
  %3 =  call ccc  i8*  @__920d155ad8fe38d98e9ad22cf162d40f__runIdentity(i8*  %1)  
  %4 =  call ccc  i8*  @__920d155ad8fe38d98e9ad22cf162d40f__runIdentity(i8*  %2)  
  %5 = bitcast i8* %4 to i8* 
  %6 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %3, i32  1, i8*  %5)  
  %7 = bitcast i8* %6 to i8* 
  %8 =  call ccc  i8*  @__920d155ad8fe38d98e9ad22cf162d40f__Identity(i8*  %7)  
  %9 = bitcast i8* %8 to i8* 
  ret i8* %9 
}


define external ccc  i8* @$Applicative$Identity$pure(i8* )    {
  %2 = bitcast i8* (i8*)* @__920d155ad8fe38d98e9ad22cf162d40f__Identity to i8* 
  %3 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %4 = bitcast i8* %3 to {i8*, i32, i32, i8*}* 
  %5 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %4, i32 0, i32 0 
  store  i8* %2, i8** %5, align 8 
  %6 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %4, i32 0, i32 1 
  store  i32 1, i32* %6, align 8 
  %7 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %4, i32 0, i32 2 
  store  i32 1, i32* %7, align 8 
  %8 = bitcast {i8*, i32, i32, i8*}* %4 to i8* 
  %9 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %8, i32  1, i8*  %0)  
  ret i8* %9 
}


@$Applicative$Identity =    global {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}} { {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*)* @$Applicative$Identity$ap to i8*), i32 2, i32 2, i8* undef }, {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*)* @$Applicative$Identity$pure to i8*), i32 1, i32 1, i8* undef } }


define external ccc  i8* @$Monad$Identity$chain(i8*  %f_0, i8*  %mm_0)    {
  %1 = bitcast i8* %f_0 to {i8*, i32, i32, i8*}* 
  %2 = bitcast i8* %mm_0 to i8* 
  %3 = bitcast {i8*, i32, i32, i8*}* %1 to i8* 
  %4 =  call ccc  i8*  @__920d155ad8fe38d98e9ad22cf162d40f__runIdentity(i8*  %2)  
  %5 = bitcast i8* %4 to i8* 
  %6 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %3, i32  1, i8*  %5)  
  %7 = bitcast i8* %6 to i8* 
  ret i8* %7 
}


define external ccc  i8* @$Monad$Identity$of(i8* )    {
  %2 = bitcast i8* (i8*)* @$Applicative$Identity$pure to i8* 
  %3 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %4 = bitcast i8* %3 to {i8*, i32, i32, i8*}* 
  %5 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %4, i32 0, i32 0 
  store  i8* %2, i8** %5, align 8 
  %6 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %4, i32 0, i32 1 
  store  i32 1, i32* %6, align 8 
  %7 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %4, i32 0, i32 2 
  store  i32 1, i32* %7, align 8 
  %8 = bitcast {i8*, i32, i32, i8*}* %4 to i8* 
  %9 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %8, i32  1, i8*  %0)  
  ret i8* %9 
}


@$Monad$Identity =    global {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}} { {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*)* @$Monad$Identity$chain to i8*), i32 2, i32 2, i8* undef }, {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*)* @$Monad$Identity$of to i8*), i32 1, i32 1, i8* undef } }


define external ccc  i8* @$MonadWriter$w_WriterT$tell(i8*  %$Functor$e212_0, i8*  %$Applicative$e212_0, i8*  %$Monad$e212_0, i8*  %$Semigroup$y206_0, i8*  %$Monoid$y206_0)    {
  %1 = bitcast i8* %$Functor$e212_0 to i8* 
  %2 = bitcast i8* %$Applicative$e212_0 to i8* 
  %3 = bitcast i8* %$Monad$e212_0 to i8* 
  %4 = bitcast i8* %$Semigroup$y206_0 to i8* 
  %5 = bitcast i8* %$Monoid$y206_0 to i8* 
  %6 = bitcast i8* (i8*, i8*)* @anonymous$lifted$16 to i8* 
  %7 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %8 = bitcast i8* %7 to {i8*}* 
  %9 = getelementptr  {i8*}, {i8*}* %8, i32 0, i32 0 
  store  i8* %3, i8** %9, align 8 
  %10 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %11 = bitcast i8* %10 to {i8*, i32, i32, i8*}* 
  %12 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %11, i32 0, i32 0 
  store  i8* %6, i8** %12, align 8 
  %13 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %11, i32 0, i32 1 
  store  i32 2, i32* %13, align 8 
  %14 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %11, i32 0, i32 2 
  store  i32 1, i32* %14, align 8 
  %15 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %11, i32 0, i32 3 
  store  i8* %7, i8** %15, align 8 
  %16 = bitcast {i8*, i32, i32, i8*}* %11 to i8* 
  ret i8* %16 
}


@$MonadWriter$w_WriterT =    global {{i8*, i32, i32, i8*}} { {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*, i8*, i8*, i8*, i8*)* @$MonadWriter$w_WriterT$tell to i8*), i32 5, i32 5, i8* undef } }


define external ccc  i8* @anonymous$lifted$0(i8*  %b_0, i8*  %__0)    {
  %1 = bitcast i8* %b_0 to i8* 
  %2 = bitcast i8* %__0 to i8* 
  ret i8* %1 
}


define external ccc  i8* @anonymous$lifted$1(i8*  %$Monad$s226_0, i8*  %b_0, i8*  %a_0)    {
  %1 = bitcast i8* %$Monad$s226_0 to i8* 
  %2 = bitcast i8* %b_0 to i8* 
  %3 = bitcast i8* %a_0 to i8* 
  %4 = bitcast i8* %1 to {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* 
  %5 = getelementptr  {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* %4, i32 0, i32 0 
  %6 = bitcast {i8*, i32, i32, i8*}* %5 to i8* 
  %7 = bitcast i8* (i8*, i8*)* @anonymous$lifted$0 to i8* 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {i8*}* 
  %10 = getelementptr  {i8*}, {i8*}* %9, i32 0, i32 0 
  store  i8* %2, i8** %10, align 8 
  %11 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %12 = bitcast i8* %11 to {i8*, i32, i32, i8*}* 
  %13 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %12, i32 0, i32 0 
  store  i8* %7, i8** %13, align 8 
  %14 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %12, i32 0, i32 1 
  store  i32 2, i32* %14, align 8 
  %15 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %12, i32 0, i32 2 
  store  i32 1, i32* %15, align 8 
  %16 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %12, i32 0, i32 3 
  store  i8* %8, i8** %16, align 8 
  %17 = bitcast {i8*, i32, i32, i8*}* %12 to i8* 
  %18 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %6, i32  2, i8*  %17, i8*  %3)  
  %19 = bitcast i8* %18 to i8* 
  ret i8* %19 
}


define external ccc  i8* @anonymous$lifted$2(i8*  %$Applicative$t253_0, i8*  %$Functor$t253_0, i8*  %f_0, i8*  %x1_0, i8*  %x2_0)    {
  %1 = bitcast i8* %$Applicative$t253_0 to i8* 
  %2 = bitcast i8* %$Functor$t253_0 to i8* 
  %3 = bitcast i8* %f_0 to {i8*, i32, i32, i8*}* 
  %4 = bitcast i8* %x1_0 to i8* 
  %5 = bitcast i8* %x2_0 to i8* 
  %6 = bitcast i8* %1 to {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* 
  %7 = getelementptr  {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* %6, i32 0, i32 0 
  %8 = bitcast {i8*, i32, i32, i8*}* %7 to i8* 
  %9 = bitcast i8* %2 to {{i8*, i32, i32, i8*}}* 
  %10 = getelementptr  {{i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}}* %9, i32 0, i32 0 
  %11 = bitcast {i8*, i32, i32, i8*}* %10 to i8* 
  %12 = bitcast {i8*, i32, i32, i8*}* %3 to i8* 
  %13 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %11, i32  2, i8*  %12, i8*  %4)  
  %14 = bitcast i8* %13 to i8* 
  %15 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %8, i32  2, i8*  %14, i8*  %5)  
  %16 = bitcast i8* %15 to i8* 
  ret i8* %16 
}


define external ccc  i8* @anonymous$lifted$3(i8*  %__x___0)    {
; <label>:0:
  %1 = bitcast i8* %__x___0 to {i8*, i8*}* 
  %2 = getelementptr  {i8*, i8*}, {i8*, i8*}* %1, i32 0, i32 0 
  %3 = getelementptr  {i8*, i8*}, {i8*, i8*}* %1, i32 0, i32 1 
  %4 = load  i8*, i8** %2, align 8 
  %5 = bitcast i8* %4 to i8* 
  %6 = and i1 1, 1 
  %7 = load  i8*, i8** %3, align 8 
  %8 = bitcast i8* %7 to {i8*, i8*}* 
  %9 = and i1 %6, 1 
  br i1 %9, label %branchExpBlock_0, label %exitBlock_0 
branchExpBlock_0:
  %10 = getelementptr  {i8*, i8*}, {i8*, i8*}* %1, i32 0, i32 0 
  %11 = load  i8*, i8** %10, align 8 
  %12 = bitcast i8* %11 to i8* 
  %13 = getelementptr  {i8*, i8*}, {i8*, i8*}* %1, i32 0, i32 1 
  %14 = load  i8*, i8** %13, align 8 
  %15 = bitcast i8* %14 to {i8*, i8*}* 
  %16 = bitcast i8* (i8*)* @__920d155ad8fe38d98e9ad22cf162d40f__log to i8* 
  %17 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %18 = bitcast i8* %17 to {i8*, i32, i32, i8*}* 
  %19 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %18, i32 0, i32 0 
  store  i8* %16, i8** %19, align 8 
  %20 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %18, i32 0, i32 1 
  store  i32 1, i32* %20, align 8 
  %21 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %18, i32 0, i32 2 
  store  i32 1, i32* %21, align 8 
  %22 = bitcast {i8*, i32, i32, i8*}* %18 to i8* 
  %23 = bitcast {i8*, i8*}* %15 to i8* 
  %24 =  call ccc  i8*  @__920d155ad8fe38d98e9ad22cf162d40f__map2(i8*  %22, i8*  %23)  
  %25 = bitcast i8* %24 to {i8*, i8*}* 
  %26 = bitcast i1* zeroinitializer to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %27 = phi i8* [%26, %branchExpBlock_0], [zeroinitializer, %0] 
  ret i8* %27 
}


define external ccc  i8* @anonymous$lifted$4(i8*  %__0)    {
  %1 = bitcast i8* %__0 to double* 
  %2 = load  double, double* %1, align 8 
  %3 = bitcast i8* (i8*, i8*, i8*, i8*, i8*)* @$MonadWriter$w_WriterT$tell to i8* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8*, i32, i32, i8*}* 
  %6 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %5, i32 0, i32 0 
  store  i8* %3, i8** %6, align 8 
  %7 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %5, i32 0, i32 1 
  store  i32 5, i32* %7, align 8 
  %8 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %5, i32 0, i32 2 
  store  i32 5, i32* %8, align 8 
  %9 = bitcast {i8*, i32, i32, i8*}* %5 to i8* 
  %10 = bitcast {{i8*, i32, i32, i8*}}* @$Functor$Identity to i8* 
  %11 = bitcast {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* @$Applicative$Identity to i8* 
  %12 = bitcast {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* @$Monad$Identity to i8* 
  %13 = bitcast {{i8*, i32, i32, i8*}}* @$Semigroup$List to i8* 
  %14 = bitcast {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* @$Monoid$List to i8* 
  %15 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %9, i32  5, i8*  %10, i8*  %11, i8*  %12, i8*  %13, i8*  %14)  
  %16 = bitcast i8* %15 to {i8*, i32, i32, i8*}* 
  %17 = bitcast {i8*, i32, i32, i8*}* %16 to i8* 
  %18 =  call ccc  i8*  @GC_malloc(i64  4)  
  %19 = getelementptr  i8, i8* %18, i32 0 
  store  i8 72, i8* %19, align 8 
  %20 = getelementptr  i8, i8* %18, i32 1 
  store  i8 69, i8* %20, align 8 
  %21 = getelementptr  i8, i8* %18, i32 2 
  store  i8 80, i8* %21, align 8 
  %22 = getelementptr  i8, i8* %18, i32 3 
  store  i8 0, i8* %22, align 8 
  %23 =  call ccc  {i8*, i8*}*  @MadList_singleton(i8*  %18)  
  %24 = bitcast {i8*, i8*}* %23 to {i8*, i8*}* 
  %25 = bitcast {i8*, i8*}* %24 to i8* 
  %26 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %17, i32  1, i8*  %25)  
  %27 = bitcast i8* %26 to i8* 
  ret i8* %27 
}


define external ccc  i8* @anonymous$lifted$5(i8*  %_P__0)    {
  %1 = bitcast i8* %_P__0 to double* 
  %2 = load  double, double* %1, align 8 
  %3 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %4 = bitcast i8* %3 to double* 
  store  double %2, double* %4, align 8 
  %5 = bitcast double* %4 to i8* 
  %6 =  call ccc  i8*  @anonymous$lifted$4(i8*  %5)  
  %7 = bitcast i8* %6 to i8* 
  ret i8* %7 
}


define external ccc  i8* @anonymous$lifted$6(i8*  %_P__0)    {
  %1 = bitcast i8* %_P__0 to i8* 
  %2 = bitcast i8* (i8*, i8*, i8*, i8*, i8*)* @$Monad$WriterT$chain to i8* 
  %3 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %4 = bitcast i8* %3 to {i8*, i32, i32, i8*}* 
  %5 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %4, i32 0, i32 0 
  store  i8* %2, i8** %5, align 8 
  %6 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %4, i32 0, i32 1 
  store  i32 5, i32* %6, align 8 
  %7 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %4, i32 0, i32 2 
  store  i32 5, i32* %7, align 8 
  %8 = bitcast {i8*, i32, i32, i8*}* %4 to i8* 
  %9 = bitcast {{i8*, i32, i32, i8*}}* @$Functor$Identity to i8* 
  %10 = bitcast {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* @$Applicative$Identity to i8* 
  %11 = bitcast {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* @$Monad$Identity to i8* 
  %12 = bitcast {{i8*, i32, i32, i8*}}* @$Semigroup$List to i8* 
  %13 = bitcast {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* @$Monoid$List to i8* 
  %14 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %8, i32  5, i8*  %9, i8*  %10, i8*  %11, i8*  %12, i8*  %13)  
  %15 = bitcast i8* %14 to {i8*, i32, i32, i8*}* 
  %16 = bitcast {i8*, i32, i32, i8*}* %15 to i8* 
  %17 = bitcast i8* (i8*)* @anonymous$lifted$5 to i8* 
  %18 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %19 = bitcast i8* %18 to {i8*, i32, i32, i8*}* 
  %20 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %19, i32 0, i32 0 
  store  i8* %17, i8** %20, align 8 
  %21 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %19, i32 0, i32 1 
  store  i32 1, i32* %21, align 8 
  %22 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %19, i32 0, i32 2 
  store  i32 1, i32* %22, align 8 
  %23 = bitcast {i8*, i32, i32, i8*}* %19 to i8* 
  %24 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %16, i32  2, i8*  %23, i8*  %1)  
  %25 = bitcast i8* %24 to i8* 
  %26 =  call ccc  i8*  @__920d155ad8fe38d98e9ad22cf162d40f__runSimpleStack(i8*  %25)  
  %27 = bitcast i8* %26 to {i8*, i8*}* 
  %28 = bitcast {i8*, i8*}* %27 to i8* 
  %29 =  call ccc  i8*  @anonymous$lifted$3(i8*  %28)  
  %30 = bitcast i8* %29 to i8* 
  ret i8* %30 
}


define external ccc  i8* @anonymous$lifted$7(i8*  %f_0, i8*  %__x___0)    {
; <label>:0:
  %1 = bitcast i8* %f_0 to {i8*, i32, i32, i8*}* 
  %2 = bitcast i8* %__x___0 to {i8*, i8*}* 
  %3 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 0 
  %4 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 1 
  %5 = load  i8*, i8** %3, align 8 
  %6 = bitcast i8* %5 to i8* 
  %7 = and i1 1, 1 
  %8 = load  i8*, i8** %4, align 8 
  %9 = bitcast i8* %8 to i8* 
  %10 = and i1 %7, 1 
  br i1 %10, label %branchExpBlock_0, label %exitBlock_0 
branchExpBlock_0:
  %11 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 0 
  %12 = load  i8*, i8** %11, align 8 
  %13 = bitcast i8* %12 to i8* 
  %14 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 1 
  %15 = load  i8*, i8** %14, align 8 
  %16 = bitcast i8* %15 to i8* 
  %17 = bitcast {i8*, i32, i32, i8*}* %1 to i8* 
  %18 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %17, i32  1, i8*  %13)  
  %19 = bitcast i8* %18 to i8* 
  %20 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %21 = bitcast i8* %20 to {i8*, i8*}* 
  %22 = getelementptr  {i8*, i8*}, {i8*, i8*}* %21, i32 0, i32 0 
  store  i8* %19, i8** %22, align 8 
  %23 = getelementptr  {i8*, i8*}, {i8*, i8*}* %21, i32 0, i32 1 
  store  i8* %16, i8** %23, align 8 
  %24 = bitcast {i8*, i8*}* %21 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %25 = phi i8* [%24, %branchExpBlock_0], [zeroinitializer, %0] 
  ret i8* %25 
}


define external ccc  i8* @anonymous$lifted$8(i8*  %$Functor$m38_0, i8*  %f_0, i8*  %m_0)    {
  %1 = bitcast i8* %$Functor$m38_0 to i8* 
  %2 = bitcast i8* %f_0 to {i8*, i32, i32, i8*}* 
  %3 = bitcast i8* %m_0 to i8* 
  %4 = bitcast i8* %1 to {{i8*, i32, i32, i8*}}* 
  %5 = getelementptr  {{i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}}* %4, i32 0, i32 0 
  %6 = bitcast {i8*, i32, i32, i8*}* %5 to i8* 
  %7 = bitcast i8* (i8*, i8*)* @anonymous$lifted$7 to i8* 
  %8 = bitcast {i8*, i32, i32, i8*}* %2 to i8* 
  %9 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %10 = bitcast i8* %9 to {i8*}* 
  %11 = getelementptr  {i8*}, {i8*}* %10, i32 0, i32 0 
  store  i8* %8, i8** %11, align 8 
  %12 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %13 = bitcast i8* %12 to {i8*, i32, i32, i8*}* 
  %14 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %13, i32 0, i32 0 
  store  i8* %7, i8** %14, align 8 
  %15 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %13, i32 0, i32 1 
  store  i32 2, i32* %15, align 8 
  %16 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %13, i32 0, i32 2 
  store  i32 1, i32* %16, align 8 
  %17 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %13, i32 0, i32 3 
  store  i8* %9, i8** %17, align 8 
  %18 =  call ccc  i8*  @__920d155ad8fe38d98e9ad22cf162d40f__runWriterT(i8*  %3)  
  %19 = bitcast i8* %18 to i8* 
  %20 = bitcast {i8*, i32, i32, i8*}* %13 to i8* 
  %21 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %6, i32  2, i8*  %20, i8*  %19)  
  %22 = bitcast i8* %21 to i8* 
  %23 =  call ccc  i8*  @__920d155ad8fe38d98e9ad22cf162d40f__WriterT(i8*  %22)  
  %24 = bitcast i8* %23 to i8* 
  ret i8* %24 
}


define external ccc  i8* @anonymous$lifted$9(i8*  %$Monoid$z77_0, i8*  %x1_0, i8*  %x2_0)    {
; <label>:0:
  %1 = bitcast i8* %$Monoid$z77_0 to i8* 
  %2 = bitcast i8* %x1_0 to {i8*, i8*}* 
  %3 = bitcast i8* %x2_0 to {i8*, i8*}* 
  %4 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 0 
  %5 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 1 
  %6 = load  i8*, i8** %4, align 8 
  %7 = bitcast i8* %6 to {i8*, i32, i32, i8*}* 
  %8 = and i1 1, 1 
  %9 = load  i8*, i8** %5, align 8 
  %10 = bitcast i8* %9 to i8* 
  %11 = and i1 %8, 1 
  br i1 %11, label %branchExpBlock_0, label %exitBlock_1 
branchExpBlock_0:
  %12 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 0 
  %13 = load  i8*, i8** %12, align 8 
  %14 = bitcast i8* %13 to {i8*, i32, i32, i8*}* 
  %15 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 1 
  %16 = load  i8*, i8** %15, align 8 
  %17 = bitcast i8* %16 to i8* 
  %18 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 0 
  %19 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 1 
  %20 = load  i8*, i8** %18, align 8 
  %21 = bitcast i8* %20 to i8* 
  %22 = and i1 1, 1 
  %23 = load  i8*, i8** %19, align 8 
  %24 = bitcast i8* %23 to i8* 
  %25 = and i1 %22, 1 
  br i1 %25, label %branchExpBlock_1, label %exitBlock_0 
branchExpBlock_1:
  %26 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 0 
  %27 = load  i8*, i8** %26, align 8 
  %28 = bitcast i8* %27 to i8* 
  %29 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 1 
  %30 = load  i8*, i8** %29, align 8 
  %31 = bitcast i8* %30 to i8* 
  %32 = bitcast {i8*, i32, i32, i8*}* %14 to i8* 
  %33 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %32, i32  1, i8*  %28)  
  %34 = bitcast i8* %33 to i8* 
  %35 = bitcast i8* %1 to {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* 
  %36 = getelementptr  {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* %35, i32 0, i32 0 
  %37 = bitcast {i8*, i32, i32, i8*}* %36 to i8* 
  %38 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %37, i32  2, i8*  %17, i8*  %31)  
  %39 = bitcast i8* %38 to i8* 
  %40 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %41 = bitcast i8* %40 to {i8*, i8*}* 
  %42 = getelementptr  {i8*, i8*}, {i8*, i8*}* %41, i32 0, i32 0 
  store  i8* %34, i8** %42, align 8 
  %43 = getelementptr  {i8*, i8*}, {i8*, i8*}* %41, i32 0, i32 1 
  store  i8* %39, i8** %43, align 8 
  %44 = bitcast {i8*, i8*}* %41 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %45 = phi i8* [%44, %branchExpBlock_1], [zeroinitializer, %branchExpBlock_0] 
  br label %exitBlock_1 
exitBlock_1:
  %46 = phi i8* [%45, %exitBlock_0], [zeroinitializer, %0] 
  ret i8* %46 
}


define external ccc  i8* @anonymous$lifted$10(i8*  %$Applicative$a78_0, i8*  %$Monoid$z77_0, i8*  %mf_0, i8*  %mm_0)    {
  %1 = bitcast i8* %$Applicative$a78_0 to i8* 
  %2 = bitcast i8* %$Monoid$z77_0 to i8* 
  %3 = bitcast i8* %mf_0 to i8* 
  %4 = bitcast i8* %mm_0 to i8* 
  %5 = bitcast i8* (i8*, i8*)* @__920d155ad8fe38d98e9ad22cf162d40f__liftA2 to i8* 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {i8*, i32, i32, i8*}* 
  %8 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 0 
  store  i8* %5, i8** %8, align 8 
  %9 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 1 
  store  i32 2, i32* %9, align 8 
  %10 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 2 
  store  i32 2, i32* %10, align 8 
  %11 = bitcast {i8*, i32, i32, i8*}* %7 to i8* 
  %12 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %11, i32  1, i8*  %1)  
  %13 = bitcast i8* %12 to {i8*, i32, i32, i8*}* 
  %14 = bitcast {i8*, i32, i32, i8*}* %13 to i8* 
  %15 = bitcast i8* (i8*, i8*, i8*)* @anonymous$lifted$9 to i8* 
  %16 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %17 = bitcast i8* %16 to {i8*}* 
  %18 = getelementptr  {i8*}, {i8*}* %17, i32 0, i32 0 
  store  i8* %2, i8** %18, align 8 
  %19 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %20 = bitcast i8* %19 to {i8*, i32, i32, i8*}* 
  %21 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %20, i32 0, i32 0 
  store  i8* %15, i8** %21, align 8 
  %22 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %20, i32 0, i32 1 
  store  i32 3, i32* %22, align 8 
  %23 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %20, i32 0, i32 2 
  store  i32 2, i32* %23, align 8 
  %24 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %20, i32 0, i32 3 
  store  i8* %16, i8** %24, align 8 
  %25 =  call ccc  i8*  @__920d155ad8fe38d98e9ad22cf162d40f__runWriterT(i8*  %3)  
  %26 = bitcast i8* %25 to i8* 
  %27 =  call ccc  i8*  @__920d155ad8fe38d98e9ad22cf162d40f__runWriterT(i8*  %4)  
  %28 = bitcast i8* %27 to i8* 
  %29 = bitcast {i8*, i32, i32, i8*}* %20 to i8* 
  %30 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %14, i32  3, i8*  %29, i8*  %26, i8*  %28)  
  %31 = bitcast i8* %30 to i8* 
  %32 =  call ccc  i8*  @__920d155ad8fe38d98e9ad22cf162d40f__WriterT(i8*  %31)  
  %33 = bitcast i8* %32 to i8* 
  ret i8* %33 
}


define external ccc  i8* @anonymous$lifted$11(i8*  %$Applicative$o92_0, i8*  %$Monoid$q94_0, i8*  %x_0)    {
  %1 = bitcast i8* %$Applicative$o92_0 to i8* 
  %2 = bitcast i8* %$Monoid$q94_0 to i8* 
  %3 = bitcast i8* %x_0 to i8* 
  %4 = bitcast i8* %1 to {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* 
  %5 = getelementptr  {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* %4, i32 0, i32 1 
  %6 = bitcast {i8*, i32, i32, i8*}* %5 to i8* 
  %7 = bitcast i8* %2 to {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* 
  %8 = getelementptr  {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* %7, i32 0, i32 1 
  %9 = bitcast {i8*, i32, i32, i8*}* %8 to i8* 
  %10 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %11 = bitcast i8* %10 to {i8*, i8*}* 
  %12 = getelementptr  {i8*, i8*}, {i8*, i8*}* %11, i32 0, i32 0 
  store  i8* %3, i8** %12, align 8 
  %13 = getelementptr  {i8*, i8*}, {i8*, i8*}* %11, i32 0, i32 1 
  store  i8* %9, i8** %13, align 8 
  %14 = bitcast {i8*, i8*}* %11 to i8* 
  %15 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %6, i32  1, i8*  %14)  
  %16 = bitcast i8* %15 to i8* 
  %17 =  call ccc  i8*  @__920d155ad8fe38d98e9ad22cf162d40f__WriterT(i8*  %16)  
  %18 = bitcast i8* %17 to i8* 
  ret i8* %18 
}


define external ccc  i8* @anonymous$lifted$12(i8*  %$Monad$i138_0, i8*  %$Monoid$h137_0, i8*  %w_0, i8*  %__x___0)    {
; <label>:0:
  %1 = bitcast i8* %$Monad$i138_0 to i8* 
  %2 = bitcast i8* %$Monoid$h137_0 to i8* 
  %3 = bitcast i8* %w_0 to i8* 
  %4 = bitcast i8* %__x___0 to {i8*, i8*}* 
  %5 = getelementptr  {i8*, i8*}, {i8*, i8*}* %4, i32 0, i32 0 
  %6 = getelementptr  {i8*, i8*}, {i8*, i8*}* %4, i32 0, i32 1 
  %7 = load  i8*, i8** %5, align 8 
  %8 = bitcast i8* %7 to i8* 
  %9 = and i1 1, 1 
  %10 = load  i8*, i8** %6, align 8 
  %11 = bitcast i8* %10 to i8* 
  %12 = and i1 %9, 1 
  br i1 %12, label %branchExpBlock_0, label %exitBlock_0 
branchExpBlock_0:
  %13 = getelementptr  {i8*, i8*}, {i8*, i8*}* %4, i32 0, i32 0 
  %14 = load  i8*, i8** %13, align 8 
  %15 = bitcast i8* %14 to i8* 
  %16 = getelementptr  {i8*, i8*}, {i8*, i8*}* %4, i32 0, i32 1 
  %17 = load  i8*, i8** %16, align 8 
  %18 = bitcast i8* %17 to i8* 
  %19 = bitcast i8* %1 to {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* 
  %20 = getelementptr  {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* %19, i32 0, i32 1 
  %21 = bitcast {i8*, i32, i32, i8*}* %20 to i8* 
  %22 = bitcast i8* %2 to {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* 
  %23 = getelementptr  {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* %22, i32 0, i32 0 
  %24 = bitcast {i8*, i32, i32, i8*}* %23 to i8* 
  %25 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %24, i32  2, i8*  %3, i8*  %18)  
  %26 = bitcast i8* %25 to i8* 
  %27 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %28 = bitcast i8* %27 to {i8*, i8*}* 
  %29 = getelementptr  {i8*, i8*}, {i8*, i8*}* %28, i32 0, i32 0 
  store  i8* %15, i8** %29, align 8 
  %30 = getelementptr  {i8*, i8*}, {i8*, i8*}* %28, i32 0, i32 1 
  store  i8* %26, i8** %30, align 8 
  %31 = bitcast {i8*, i8*}* %28 to i8* 
  %32 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %21, i32  1, i8*  %31)  
  %33 = bitcast i8* %32 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %34 = phi i8* [%33, %branchExpBlock_0], [zeroinitializer, %0] 
  ret i8* %34 
}


define external ccc  i8* @anonymous$lifted$13(i8*  %$Monad$i138_0, i8*  %$Monoid$h137_0, i8*  %f_0, i8*  %__x___0)    {
; <label>:0:
  %1 = bitcast i8* %$Monad$i138_0 to i8* 
  %2 = bitcast i8* %$Monoid$h137_0 to i8* 
  %3 = bitcast i8* %f_0 to {i8*, i32, i32, i8*}* 
  %4 = bitcast i8* %__x___0 to {i8*, i8*}* 
  %5 = getelementptr  {i8*, i8*}, {i8*, i8*}* %4, i32 0, i32 0 
  %6 = getelementptr  {i8*, i8*}, {i8*, i8*}* %4, i32 0, i32 1 
  %7 = load  i8*, i8** %5, align 8 
  %8 = bitcast i8* %7 to i8* 
  %9 = and i1 1, 1 
  %10 = load  i8*, i8** %6, align 8 
  %11 = bitcast i8* %10 to i8* 
  %12 = and i1 %9, 1 
  br i1 %12, label %branchExpBlock_0, label %exitBlock_0 
branchExpBlock_0:
  %13 = getelementptr  {i8*, i8*}, {i8*, i8*}* %4, i32 0, i32 0 
  %14 = load  i8*, i8** %13, align 8 
  %15 = bitcast i8* %14 to i8* 
  %16 = getelementptr  {i8*, i8*}, {i8*, i8*}* %4, i32 0, i32 1 
  %17 = load  i8*, i8** %16, align 8 
  %18 = bitcast i8* %17 to i8* 
  %19 = bitcast i8* %1 to {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* 
  %20 = getelementptr  {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* %19, i32 0, i32 0 
  %21 = bitcast {i8*, i32, i32, i8*}* %20 to i8* 
  %22 = bitcast i8* (i8*, i8*, i8*, i8*)* @anonymous$lifted$12 to i8* 
  %23 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*, i8*}* getelementptr inbounds ({i8*, i8*, i8*}, {i8*, i8*, i8*}* inttoptr (i32 0 to {i8*, i8*, i8*}*), i32 1) to i64))  
  %24 = bitcast i8* %23 to {i8*, i8*, i8*}* 
  %25 = getelementptr  {i8*, i8*, i8*}, {i8*, i8*, i8*}* %24, i32 0, i32 0 
  store  i8* %1, i8** %25, align 8 
  %26 = getelementptr  {i8*, i8*, i8*}, {i8*, i8*, i8*}* %24, i32 0, i32 1 
  store  i8* %2, i8** %26, align 8 
  %27 = getelementptr  {i8*, i8*, i8*}, {i8*, i8*, i8*}* %24, i32 0, i32 2 
  store  i8* %18, i8** %27, align 8 
  %28 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %29 = bitcast i8* %28 to {i8*, i32, i32, i8*}* 
  %30 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %29, i32 0, i32 0 
  store  i8* %22, i8** %30, align 8 
  %31 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %29, i32 0, i32 1 
  store  i32 4, i32* %31, align 8 
  %32 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %29, i32 0, i32 2 
  store  i32 1, i32* %32, align 8 
  %33 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %29, i32 0, i32 3 
  store  i8* %23, i8** %33, align 8 
  %34 = bitcast {i8*, i32, i32, i8*}* %3 to i8* 
  %35 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %34, i32  1, i8*  %15)  
  %36 = bitcast i8* %35 to i8* 
  %37 =  call ccc  i8*  @__920d155ad8fe38d98e9ad22cf162d40f__runWriterT(i8*  %36)  
  %38 = bitcast i8* %37 to i8* 
  %39 = bitcast {i8*, i32, i32, i8*}* %29 to i8* 
  %40 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %21, i32  2, i8*  %39, i8*  %38)  
  %41 = bitcast i8* %40 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %42 = phi i8* [%41, %branchExpBlock_0], [zeroinitializer, %0] 
  ret i8* %42 
}


define external ccc  i8* @anonymous$lifted$14(i8*  %$Monad$i138_0, i8*  %$Monoid$h137_0, i8*  %f_0, i8*  %m_0)    {
  %1 = bitcast i8* %$Monad$i138_0 to i8* 
  %2 = bitcast i8* %$Monoid$h137_0 to i8* 
  %3 = bitcast i8* %f_0 to {i8*, i32, i32, i8*}* 
  %4 = bitcast i8* %m_0 to i8* 
  %5 = bitcast i8* %1 to {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* 
  %6 = getelementptr  {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* %5, i32 0, i32 0 
  %7 = bitcast {i8*, i32, i32, i8*}* %6 to i8* 
  %8 = bitcast i8* (i8*, i8*, i8*, i8*)* @anonymous$lifted$13 to i8* 
  %9 = bitcast {i8*, i32, i32, i8*}* %3 to i8* 
  %10 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*, i8*}* getelementptr inbounds ({i8*, i8*, i8*}, {i8*, i8*, i8*}* inttoptr (i32 0 to {i8*, i8*, i8*}*), i32 1) to i64))  
  %11 = bitcast i8* %10 to {i8*, i8*, i8*}* 
  %12 = getelementptr  {i8*, i8*, i8*}, {i8*, i8*, i8*}* %11, i32 0, i32 0 
  store  i8* %1, i8** %12, align 8 
  %13 = getelementptr  {i8*, i8*, i8*}, {i8*, i8*, i8*}* %11, i32 0, i32 1 
  store  i8* %2, i8** %13, align 8 
  %14 = getelementptr  {i8*, i8*, i8*}, {i8*, i8*, i8*}* %11, i32 0, i32 2 
  store  i8* %9, i8** %14, align 8 
  %15 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %16 = bitcast i8* %15 to {i8*, i32, i32, i8*}* 
  %17 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %16, i32 0, i32 0 
  store  i8* %8, i8** %17, align 8 
  %18 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %16, i32 0, i32 1 
  store  i32 4, i32* %18, align 8 
  %19 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %16, i32 0, i32 2 
  store  i32 1, i32* %19, align 8 
  %20 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %16, i32 0, i32 3 
  store  i8* %10, i8** %20, align 8 
  %21 =  call ccc  i8*  @__920d155ad8fe38d98e9ad22cf162d40f__runWriterT(i8*  %4)  
  %22 = bitcast i8* %21 to i8* 
  %23 = bitcast {i8*, i32, i32, i8*}* %16 to i8* 
  %24 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %7, i32  2, i8*  %23, i8*  %22)  
  %25 = bitcast i8* %24 to i8* 
  %26 =  call ccc  i8*  @__920d155ad8fe38d98e9ad22cf162d40f__WriterT(i8*  %25)  
  %27 = bitcast i8* %26 to i8* 
  ret i8* %27 
}


define external ccc  i8* @anonymous$lifted$15(i8*  %$Applicative$w152_0, i8*  %$Monoid$y154_0, i8*  %x_0)    {
  %1 = bitcast i8* %$Applicative$w152_0 to i8* 
  %2 = bitcast i8* %$Monoid$y154_0 to i8* 
  %3 = bitcast i8* %x_0 to i8* 
  %4 = bitcast i8* %1 to {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* 
  %5 = getelementptr  {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* %4, i32 0, i32 1 
  %6 = bitcast {i8*, i32, i32, i8*}* %5 to i8* 
  %7 = bitcast i8* %2 to {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* 
  %8 = getelementptr  {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* %7, i32 0, i32 1 
  %9 = bitcast {i8*, i32, i32, i8*}* %8 to i8* 
  %10 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %11 = bitcast i8* %10 to {i8*, i8*}* 
  %12 = getelementptr  {i8*, i8*}, {i8*, i8*}* %11, i32 0, i32 0 
  store  i8* %3, i8** %12, align 8 
  %13 = getelementptr  {i8*, i8*}, {i8*, i8*}* %11, i32 0, i32 1 
  store  i8* %9, i8** %13, align 8 
  %14 = bitcast {i8*, i8*}* %11 to i8* 
  %15 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %6, i32  1, i8*  %14)  
  %16 = bitcast i8* %15 to i8* 
  %17 =  call ccc  i8*  @__920d155ad8fe38d98e9ad22cf162d40f__WriterT(i8*  %16)  
  %18 = bitcast i8* %17 to i8* 
  ret i8* %18 
}


define external ccc  i8* @anonymous$lifted$16(i8*  %$Monad$e212_0, i8*  %v_0)    {
  %1 = bitcast i8* %$Monad$e212_0 to i8* 
  %2 = bitcast i8* %v_0 to i8* 
  %3 = bitcast i8* %1 to {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* 
  %4 = getelementptr  {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* %3, i32 0, i32 1 
  %5 = bitcast {i8*, i32, i32, i8*}* %4 to i8* 
  %6 = bitcast i1* zeroinitializer to i8* 
  %7 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %8 = bitcast i8* %7 to {i8*, i8*}* 
  %9 = getelementptr  {i8*, i8*}, {i8*, i8*}* %8, i32 0, i32 0 
  store  i8* %6, i8** %9, align 8 
  %10 = getelementptr  {i8*, i8*}, {i8*, i8*}* %8, i32 0, i32 1 
  store  i8* %2, i8** %10, align 8 
  %11 = bitcast {i8*, i8*}* %8 to i8* 
  %12 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %5, i32  1, i8*  %11)  
  %13 = bitcast i8* %12 to i8* 
  %14 =  call ccc  i8*  @__920d155ad8fe38d98e9ad22cf162d40f__WriterT(i8*  %13)  
  %15 = bitcast i8* %14 to i8* 
  ret i8* %15 
}


declare external ccc  i8* @puts(i8*)    


define external ccc  i8* @__920d155ad8fe38d98e9ad22cf162d40f__log(i8* )    {
  %2 =  call ccc  i8*  @puts(i8*  %0)  
  ret i8* %2 
}


declare external ccc  i8* @__doubleToStr__(i8*)    


define external ccc  i8* @__920d155ad8fe38d98e9ad22cf162d40f__showNumber(i8* )    {
  %2 =  call ccc  i8*  @__doubleToStr__(i8*  %0)  
  ret i8* %2 
}


declare external ccc  i8* @MadList_map(i8*, i8*)    


define external ccc  i8* @__920d155ad8fe38d98e9ad22cf162d40f__map2(i8* , i8* )    {
  %3 =  call ccc  i8*  @MadList_map(i8*  %0, i8*  %1)  
  ret i8* %3 
}


declare external ccc  i8* @_MadList_concat(i8*, i8*)    


define external ccc  i8* @__920d155ad8fe38d98e9ad22cf162d40f__conc(i8* , i8* )    {
  %3 =  call ccc  i8*  @_MadList_concat(i8*  %0, i8*  %1)  
  ret i8* %3 
}


define external ccc  i8* @__920d155ad8fe38d98e9ad22cf162d40f__andDo(i8*  %$Functor$s226_0, i8*  %$Applicative$s226_0, i8*  %$Monad$s226_0)    {
  %1 = bitcast i8* %$Functor$s226_0 to i8* 
  %2 = bitcast i8* %$Applicative$s226_0 to i8* 
  %3 = bitcast i8* %$Monad$s226_0 to i8* 
  %4 = bitcast i8* (i8*, i8*, i8*)* @anonymous$lifted$1 to i8* 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {i8*}* 
  %7 = getelementptr  {i8*}, {i8*}* %6, i32 0, i32 0 
  store  i8* %3, i8** %7, align 8 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {i8*, i32, i32, i8*}* 
  %10 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 0 
  store  i8* %4, i8** %10, align 8 
  %11 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 1 
  store  i32 3, i32* %11, align 8 
  %12 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 2 
  store  i32 2, i32* %12, align 8 
  %13 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 3 
  store  i8* %5, i8** %13, align 8 
  %14 = bitcast {i8*, i32, i32, i8*}* %9 to i8* 
  ret i8* %14 
}


define external ccc  i8* @__920d155ad8fe38d98e9ad22cf162d40f__runWriterT(i8*  %__x___0)    {
; <label>:0:
  %1 = bitcast i8* %__x___0 to i8* 
  %2 = bitcast i8* %1 to {i64, i8*}* 
  %3 = getelementptr  {i64, i8*}, {i64, i8*}* %2, i32 0, i32 1 
  %4 = getelementptr  {i64, i8*}, {i64, i8*}* %2, i32 0, i32 0 
  %5 = load  i64, i64* %4, align 8 
  %6 = icmp eq i64 0, %5 
  %7 = load  i8*, i8** %3, align 8 
  %8 = bitcast i8* %7 to i8* 
  %9 = and i1 1, 1 
  %10 = and i1 %6, %9 
  br i1 %10, label %branchExpBlock_0, label %exitBlock_0 
branchExpBlock_0:
  %11 = bitcast i8* %1 to {i64, i8*}* 
  %12 = getelementptr  {i64, i8*}, {i64, i8*}* %11, i32 0, i32 1 
  %13 = load  i8*, i8** %12, align 8 
  %14 = bitcast i8* %13 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %15 = phi i8* [%14, %branchExpBlock_0], [zeroinitializer, %0] 
  ret i8* %15 
}


define external ccc  i8* @__920d155ad8fe38d98e9ad22cf162d40f__liftA2(i8*  %$Functor$t253_0, i8*  %$Applicative$t253_0)    {
  %1 = bitcast i8* %$Functor$t253_0 to i8* 
  %2 = bitcast i8* %$Applicative$t253_0 to i8* 
  %3 = bitcast i8* (i8*, i8*, i8*, i8*, i8*)* @anonymous$lifted$2 to i8* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8*, i8*}* 
  %6 = getelementptr  {i8*, i8*}, {i8*, i8*}* %5, i32 0, i32 0 
  store  i8* %2, i8** %6, align 8 
  %7 = getelementptr  {i8*, i8*}, {i8*, i8*}* %5, i32 0, i32 1 
  store  i8* %1, i8** %7, align 8 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {i8*, i32, i32, i8*}* 
  %10 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 0 
  store  i8* %3, i8** %10, align 8 
  %11 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 1 
  store  i32 5, i32* %11, align 8 
  %12 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 2 
  store  i32 3, i32* %12, align 8 
  %13 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 3 
  store  i8* %4, i8** %13, align 8 
  %14 = bitcast {i8*, i32, i32, i8*}* %9 to i8* 
  ret i8* %14 
}


define external ccc  i8* @__920d155ad8fe38d98e9ad22cf162d40f__runIdentity(i8*  %__x___0)    {
; <label>:0:
  %1 = bitcast i8* %__x___0 to i8* 
  %2 = bitcast i8* %1 to {i64, i8*}* 
  %3 = getelementptr  {i64, i8*}, {i64, i8*}* %2, i32 0, i32 1 
  %4 = getelementptr  {i64, i8*}, {i64, i8*}* %2, i32 0, i32 0 
  %5 = load  i64, i64* %4, align 8 
  %6 = icmp eq i64 0, %5 
  %7 = load  i8*, i8** %3, align 8 
  %8 = bitcast i8* %7 to i8* 
  %9 = and i1 1, 1 
  %10 = and i1 %6, %9 
  br i1 %10, label %branchExpBlock_0, label %exitBlock_0 
branchExpBlock_0:
  %11 = bitcast i8* %1 to {i64, i8*}* 
  %12 = getelementptr  {i64, i8*}, {i64, i8*}* %11, i32 0, i32 1 
  %13 = load  i8*, i8** %12, align 8 
  %14 = bitcast i8* %13 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %15 = phi i8* [%14, %branchExpBlock_0], [zeroinitializer, %0] 
  ret i8* %15 
}


define external ccc  i8* @__920d155ad8fe38d98e9ad22cf162d40f__hep(i8*  %$Functor$w282_0, i8*  %$Applicative$w282_0, i8*  %$Semigroup$v281_0, i8*  %$Monad$w282_0, i8*  %$Monoid$v281_0, i8*  %$MonadWriter$v281_w282_0)    {
  %1 = bitcast i8* %$Functor$w282_0 to i8* 
  %2 = bitcast i8* %$Applicative$w282_0 to i8* 
  %3 = bitcast i8* %$Semigroup$v281_0 to i8* 
  %4 = bitcast i8* %$Monad$w282_0 to i8* 
  %5 = bitcast i8* %$Monoid$v281_0 to i8* 
  %6 = bitcast i8* %$MonadWriter$v281_w282_0 to i8* 
  %7 = bitcast i8* %6 to {{i8*, i32, i32, i8*}}* 
  %8 = getelementptr  {{i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}}* %7, i32 0, i32 0 
  %9 = bitcast {i8*, i32, i32, i8*}* %8 to i8* 
  ret i8* %9 
}


define external ccc  i8* @__920d155ad8fe38d98e9ad22cf162d40f__runSimpleStack(i8*  %_P__0)    {
  %1 = bitcast i8* %_P__0 to i8* 
  %2 =  call ccc  i8*  @__920d155ad8fe38d98e9ad22cf162d40f__runWriterT(i8*  %1)  
  %3 = bitcast i8* %2 to i8* 
  %4 =  call ccc  i8*  @__920d155ad8fe38d98e9ad22cf162d40f__runIdentity(i8*  %3)  
  %5 = bitcast i8* %4 to {i8*, i8*}* 
  %6 = bitcast {i8*, i8*}* %5 to i8* 
  ret i8* %6 
}


declare external ccc  i8* @__applyPAP__(i8*, i32, ...)    


declare external ccc  i8* @malloc(i64)    


declare external ccc  i8* @GC_malloc(i64)    


declare external ccc  i8* @calloc(i32, i32)    


declare external ccc  i1 @__streq__(i8*, i8*)    


declare external ccc  i8* @__strConcat__(i8*, i8*)    


declare external ccc  i1 @MadList_hasMinLength(double, {i8*, i8*}*)    


declare external ccc  i1 @MadList_hasLength(double, {i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_singleton(i8*)    


declare external ccc  {i8*, i8*}* @__MadList_push__(i8*, {i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_concat({i8*, i8*}*, {i8*, i8*}*)    


define external ccc  void @main()    {
entry_0:
  %0 = bitcast i8* (i8*, i8*, i8*, i8*, i8*)* @$Monad$WriterT$of to i8* 
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {i8*, i32, i32, i8*}* 
  %3 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %2, i32 0, i32 0 
  store  i8* %0, i8** %3, align 8 
  %4 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %2, i32 0, i32 1 
  store  i32 5, i32* %4, align 8 
  %5 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %2, i32 0, i32 2 
  store  i32 5, i32* %5, align 8 
  %6 = bitcast {i8*, i32, i32, i8*}* %2 to i8* 
  %7 = bitcast {{i8*, i32, i32, i8*}}* @$Functor$Identity to i8* 
  %8 = bitcast {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* @$Applicative$Identity to i8* 
  %9 = bitcast {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* @$Monad$Identity to i8* 
  %10 = bitcast {{i8*, i32, i32, i8*}}* @$Semigroup$List to i8* 
  %11 = bitcast {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* @$Monoid$List to i8* 
  %12 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %6, i32  5, i8*  %7, i8*  %8, i8*  %9, i8*  %10, i8*  %11)  
  %13 = bitcast i8* %12 to {i8*, i32, i32, i8*}* 
  %14 = bitcast {i8*, i32, i32, i8*}* %13 to i8* 
  %15 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %16 = bitcast i8* %15 to double* 
  store  double 3.000000e0, double* %16, align 8 
  %17 = bitcast double* %16 to i8* 
  %18 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %14, i32  1, i8*  %17)  
  %19 = bitcast i8* %18 to i8* 
  %20 =  call ccc  i8*  @anonymous$lifted$6(i8*  %19)  
  %21 = bitcast i8* %20 to i8* 
  ret void 
}