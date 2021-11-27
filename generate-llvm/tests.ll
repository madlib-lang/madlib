; ModuleID = 'main'


 


define external ccc  i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT(i8* )    {
  %2 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64, i8*}* getelementptr inbounds ({i64, i8*}, {i64, i8*}* inttoptr (i32 0 to {i64, i8*}*), i32 1) to i64))  
  %3 = bitcast i8* %2 to {i64, i8*}* 
  %4 = getelementptr  {i64, i8*}, {i64, i8*}* %3, i32 0, i32 1 
  store  i8* %0, i8** %4, align 8 
  %5 = getelementptr  {i64, i8*}, {i64, i8*}* %3, i32 0, i32 0 
  store  i64 0, i64* %5, align 8 
  %6 = bitcast {i64, i8*}* %3 to i8* 
  ret i8* %6 
}


define external ccc  i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__Identity(i8* )    {
  %2 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64, i8*}* getelementptr inbounds ({i64, i8*}, {i64, i8*}* inttoptr (i32 0 to {i64, i8*}*), i32 1) to i64))  
  %3 = bitcast i8* %2 to {i64, i8*}* 
  %4 = getelementptr  {i64, i8*}, {i64, i8*}* %3, i32 0, i32 1 
  store  i8* %0, i8** %4, align 8 
  %5 = getelementptr  {i64, i8*}, {i64, i8*}* %3, i32 0, i32 0 
  store  i64 0, i64* %5, align 8 
  %6 = bitcast {i64, i8*}* %3 to i8* 
  ret i8* %6 
}


define external ccc  i8* @$Functor$WriterT$map(i8*  %$Functor$t19_0)    {
  %1 = bitcast i8* %$Functor$t19_0 to i8* 
  %2 = bitcast i8* (i8*, i8*, i8*, i8*)* @anonymous$lifted$2 to i8* 
  %3 = bitcast i8* (i8*)* @__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT to i8* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8*, i32, i32}* 
  %6 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %5, i32 0, i32 0 
  store  i8* %3, i8** %6, align 8 
  %7 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %5, i32 0, i32 1 
  store  i32 1, i32* %7, align 8 
  %8 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %5, i32 0, i32 2 
  store  i32 1, i32* %8, align 8 
  %9 = bitcast {i8*, i32, i32}* %5 to i8* 
  %10 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %11 = bitcast i8* %10 to {i8*, i8*}* 
  %12 = getelementptr  {i8*, i8*}, {i8*, i8*}* %11, i32 0, i32 0 
  store  i8* %1, i8** %12, align 8 
  %13 = getelementptr  {i8*, i8*}, {i8*, i8*}* %11, i32 0, i32 1 
  store  i8* %9, i8** %13, align 8 
  %14 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %15 = bitcast i8* %14 to {i8*, i32, i32, i8*}* 
  %16 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %15, i32 0, i32 0 
  store  i8* %2, i8** %16, align 8 
  %17 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %15, i32 0, i32 1 
  store  i32 4, i32* %17, align 8 
  %18 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %15, i32 0, i32 2 
  store  i32 2, i32* %18, align 8 
  %19 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %15, i32 0, i32 3 
  store  i8* %10, i8** %19, align 8 
  %20 = bitcast {i8*, i32, i32, i8*}* %15 to i8* 
  ret i8* %20 
}


@$Functor$WriterT =    global {{i8*, i32, i32}} { {i8*, i32, i32} { i8* bitcast (i8* (i8*)* @$Functor$WriterT$map to i8*), i32 1, i32 1 } }


define external ccc  i8* @$Applicative$WriterT$ap(i8*  %$Functor$d55_0, i8*  %$Applicative$d55_0)    {
  %1 = bitcast i8* %$Functor$d55_0 to i8* 
  %2 = bitcast i8* %$Applicative$d55_0 to i8* 
  %3 = bitcast i8* (i8*, i8*, i8*, i8*)* @anonymous$lifted$4 to i8* 
  %4 = bitcast i8* (i8*)* @__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT to i8* 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {i8*, i32, i32}* 
  %7 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %6, i32 0, i32 0 
  store  i8* %4, i8** %7, align 8 
  %8 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %6, i32 0, i32 1 
  store  i32 1, i32* %8, align 8 
  %9 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %6, i32 0, i32 2 
  store  i32 1, i32* %9, align 8 
  %10 = bitcast {i8*, i32, i32}* %6 to i8* 
  %11 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %12 = bitcast i8* %11 to {i8*, i8*}* 
  %13 = getelementptr  {i8*, i8*}, {i8*, i8*}* %12, i32 0, i32 0 
  store  i8* %2, i8** %13, align 8 
  %14 = getelementptr  {i8*, i8*}, {i8*, i8*}* %12, i32 0, i32 1 
  store  i8* %10, i8** %14, align 8 
  %15 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %16 = bitcast i8* %15 to {i8*, i32, i32, i8*}* 
  %17 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %16, i32 0, i32 0 
  store  i8* %3, i8** %17, align 8 
  %18 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %16, i32 0, i32 1 
  store  i32 4, i32* %18, align 8 
  %19 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %16, i32 0, i32 2 
  store  i32 2, i32* %19, align 8 
  %20 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %16, i32 0, i32 3 
  store  i8* %11, i8** %20, align 8 
  %21 = bitcast {i8*, i32, i32, i8*}* %16 to i8* 
  ret i8* %21 
}


define external ccc  i8* @$Applicative$WriterT$pure(i8*  %$Functor$p67_0, i8*  %$Applicative$p67_0)    {
  %1 = bitcast i8* %$Functor$p67_0 to i8* 
  %2 = bitcast i8* %$Applicative$p67_0 to i8* 
  %3 = bitcast i8* (i8*, i8*, i8*)* @anonymous$lifted$5 to i8* 
  %4 = bitcast i8* (i8*)* @__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT to i8* 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {i8*, i32, i32}* 
  %7 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %6, i32 0, i32 0 
  store  i8* %4, i8** %7, align 8 
  %8 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %6, i32 0, i32 1 
  store  i32 1, i32* %8, align 8 
  %9 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %6, i32 0, i32 2 
  store  i32 1, i32* %9, align 8 
  %10 = bitcast {i8*, i32, i32}* %6 to i8* 
  %11 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %12 = bitcast i8* %11 to {i8*, i8*}* 
  %13 = getelementptr  {i8*, i8*}, {i8*, i8*}* %12, i32 0, i32 0 
  store  i8* %2, i8** %13, align 8 
  %14 = getelementptr  {i8*, i8*}, {i8*, i8*}* %12, i32 0, i32 1 
  store  i8* %10, i8** %14, align 8 
  %15 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %16 = bitcast i8* %15 to {i8*, i32, i32, i8*}* 
  %17 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %16, i32 0, i32 0 
  store  i8* %3, i8** %17, align 8 
  %18 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %16, i32 0, i32 1 
  store  i32 3, i32* %18, align 8 
  %19 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %16, i32 0, i32 2 
  store  i32 1, i32* %19, align 8 
  %20 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %16, i32 0, i32 3 
  store  i8* %11, i8** %20, align 8 
  %21 = bitcast {i8*, i32, i32, i8*}* %16 to i8* 
  ret i8* %21 
}


@$Applicative$WriterT =    global {{i8*, i32, i32}, {i8*, i32, i32}} { {i8*, i32, i32} { i8* bitcast (i8* (i8*, i8*)* @$Applicative$WriterT$ap to i8*), i32 2, i32 2 }, {i8*, i32, i32} { i8* bitcast (i8* (i8*, i8*)* @$Applicative$WriterT$pure to i8*), i32 2, i32 2 } }


define external ccc  i8* @$Functor$Identity$map(i8*  %f_0, i8*  %m_0)    {
  %1 = bitcast i8* %f_0 to {i8*, i32, i32, i8*}* 
  %2 = bitcast i8* %m_0 to i8* 
  %3 = bitcast {i8*, i32, i32, i8*}* %1 to i8* 
  %4 =  call ccc  i8*  @__eddaab7dd3adaa6b7d769751eb6c3b5e__runIdentity(i8*  %2)  
  %5 = bitcast i8* %4 to i8* 
  %6 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %3, i32  1, i8*  %5)  
  %7 = bitcast i8* %6 to i8* 
  %8 =  call ccc  i8*  @__eddaab7dd3adaa6b7d769751eb6c3b5e__Identity(i8*  %7)  
  %9 = bitcast i8* %8 to i8* 
  ret i8* %9 
}


@$Functor$Identity =    global {{i8*, i32, i32}} { {i8*, i32, i32} { i8* bitcast (i8* (i8*, i8*)* @$Functor$Identity$map to i8*), i32 2, i32 2 } }


define external ccc  i8* @$Applicative$Identity$ap(i8*  %mf_0, i8*  %mm_0)    {
  %1 = bitcast i8* %mf_0 to i8* 
  %2 = bitcast i8* %mm_0 to i8* 
  %3 =  call ccc  i8*  @__eddaab7dd3adaa6b7d769751eb6c3b5e__runIdentity(i8*  %1)  
  %4 =  call ccc  i8*  @__eddaab7dd3adaa6b7d769751eb6c3b5e__runIdentity(i8*  %2)  
  %5 = bitcast i8* %4 to i8* 
  %6 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %3, i32  1, i8*  %5)  
  %7 = bitcast i8* %6 to i8* 
  %8 =  call ccc  i8*  @__eddaab7dd3adaa6b7d769751eb6c3b5e__Identity(i8*  %7)  
  %9 = bitcast i8* %8 to i8* 
  ret i8* %9 
}


define external ccc  i8* @$Applicative$Identity$pure(i8* )    {
  %2 = bitcast i8* (i8*)* @__eddaab7dd3adaa6b7d769751eb6c3b5e__Identity to i8* 
  %3 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %4 = bitcast i8* %3 to {i8*, i32, i32}* 
  %5 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %4, i32 0, i32 0 
  store  i8* %2, i8** %5, align 8 
  %6 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %4, i32 0, i32 1 
  store  i32 1, i32* %6, align 8 
  %7 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %4, i32 0, i32 2 
  store  i32 1, i32* %7, align 8 
  %8 = bitcast {i8*, i32, i32}* %4 to i8* 
  %9 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %8, i32  1, i8*  %0)  
  ret i8* %9 
}


@$Applicative$Identity =    global {{i8*, i32, i32}, {i8*, i32, i32}} { {i8*, i32, i32} { i8* bitcast (i8* (i8*, i8*)* @$Applicative$Identity$ap to i8*), i32 2, i32 2 }, {i8*, i32, i32} { i8* bitcast (i8* (i8*)* @$Applicative$Identity$pure to i8*), i32 1, i32 1 } }


define external ccc  i8* @anonymous$lifted$0(i8*  %$Applicative$i112_0, i8*  %$Functor$i112_0, i8*  %f_0, i8*  %x1_0, i8*  %x2_0)    {
  %1 = bitcast i8* %$Applicative$i112_0 to i8* 
  %2 = bitcast i8* %$Functor$i112_0 to i8* 
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


define external ccc  i8* @anonymous$lifted$1(i8*  %f_0, i8*  %__x___0)    {
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


define external ccc  i8* @anonymous$lifted$2(i8*  %$Functor$t19_0, i8*  %__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT_0, i8*  %f_0, i8*  %m_0)    {
  %1 = bitcast i8* %$Functor$t19_0 to i8* 
  %2 = bitcast i8* %__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT_0 to {i8*, i32, i32, i8*}* 
  %3 = bitcast i8* %f_0 to {i8*, i32, i32, i8*}* 
  %4 = bitcast i8* %m_0 to i8* 
  %5 = bitcast i8* %1 to {{i8*, i32, i32, i8*}}* 
  %6 = getelementptr  {{i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}}* %5, i32 0, i32 0 
  %7 = bitcast {i8*, i32, i32, i8*}* %6 to i8* 
  %8 = bitcast i8* (i8*, i8*)* @anonymous$lifted$1 to i8* 
  %9 = bitcast {i8*, i32, i32, i8*}* %3 to i8* 
  %10 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %11 = bitcast i8* %10 to {i8*}* 
  %12 = getelementptr  {i8*}, {i8*}* %11, i32 0, i32 0 
  store  i8* %9, i8** %12, align 8 
  %13 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %14 = bitcast i8* %13 to {i8*, i32, i32, i8*}* 
  %15 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %14, i32 0, i32 0 
  store  i8* %8, i8** %15, align 8 
  %16 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %14, i32 0, i32 1 
  store  i32 2, i32* %16, align 8 
  %17 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %14, i32 0, i32 2 
  store  i32 1, i32* %17, align 8 
  %18 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %14, i32 0, i32 3 
  store  i8* %10, i8** %18, align 8 
  %19 =  call ccc  i8*  @__eddaab7dd3adaa6b7d769751eb6c3b5e__runWriterT(i8*  %4)  
  %20 = bitcast i8* %19 to i8* 
  %21 = bitcast {i8*, i32, i32, i8*}* %14 to i8* 
  %22 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %7, i32  2, i8*  %21, i8*  %20)  
  %23 = bitcast i8* %22 to i8* 
  %24 =  call ccc  i8*  @__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT(i8*  %23)  
  %25 = bitcast i8* %24 to i8* 
  ret i8* %25 
}


define external ccc  i8* @anonymous$lifted$3(i8*  %x1_0, i8*  %x2_0)    {
; <label>:0:
  %1 = bitcast i8* %x1_0 to {i8*, i8*}* 
  %2 = bitcast i8* %x2_0 to {i8*, i8*}* 
  %3 = getelementptr  {i8*, i8*}, {i8*, i8*}* %1, i32 0, i32 0 
  %4 = getelementptr  {i8*, i8*}, {i8*, i8*}* %1, i32 0, i32 1 
  %5 = load  i8*, i8** %3, align 8 
  %6 = bitcast i8* %5 to {i8*, i32, i32, i8*}* 
  %7 = and i1 1, 1 
  %8 = load  i8*, i8** %4, align 8 
  %9 = bitcast i8* %8 to {i8*, i8*}* 
  %10 = and i1 %7, 1 
  br i1 %10, label %branchExpBlock_0, label %exitBlock_1 
branchExpBlock_0:
  %11 = getelementptr  {i8*, i8*}, {i8*, i8*}* %1, i32 0, i32 0 
  %12 = load  i8*, i8** %11, align 8 
  %13 = bitcast i8* %12 to {i8*, i32, i32, i8*}* 
  %14 = getelementptr  {i8*, i8*}, {i8*, i8*}* %1, i32 0, i32 1 
  %15 = load  i8*, i8** %14, align 8 
  %16 = bitcast i8* %15 to {i8*, i8*}* 
  %17 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 0 
  %18 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 1 
  %19 = load  i8*, i8** %17, align 8 
  %20 = bitcast i8* %19 to i8* 
  %21 = and i1 1, 1 
  %22 = load  i8*, i8** %18, align 8 
  %23 = bitcast i8* %22 to {i8*, i8*}* 
  %24 = and i1 %21, 1 
  br i1 %24, label %branchExpBlock_1, label %exitBlock_0 
branchExpBlock_1:
  %25 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 0 
  %26 = load  i8*, i8** %25, align 8 
  %27 = bitcast i8* %26 to i8* 
  %28 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 1 
  %29 = load  i8*, i8** %28, align 8 
  %30 = bitcast i8* %29 to {i8*, i8*}* 
  %31 = bitcast {i8*, i32, i32, i8*}* %13 to i8* 
  %32 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %31, i32  1, i8*  %27)  
  %33 = bitcast i8* %32 to i8* 
  %34 =  call ccc  {i8*, i8*}*  @MadList_concat({i8*, i8*}*  %16, {i8*, i8*}*  %30)  
  %35 = bitcast {i8*, i8*}* %34 to {i8*, i8*}* 
  %36 = bitcast {i8*, i8*}* %35 to i8* 
  %37 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %38 = bitcast i8* %37 to {i8*, i8*}* 
  %39 = getelementptr  {i8*, i8*}, {i8*, i8*}* %38, i32 0, i32 0 
  store  i8* %33, i8** %39, align 8 
  %40 = getelementptr  {i8*, i8*}, {i8*, i8*}* %38, i32 0, i32 1 
  store  i8* %36, i8** %40, align 8 
  %41 = bitcast {i8*, i8*}* %38 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %42 = phi i8* [%41, %branchExpBlock_1], [zeroinitializer, %branchExpBlock_0] 
  br label %exitBlock_1 
exitBlock_1:
  %43 = phi i8* [%42, %exitBlock_0], [zeroinitializer, %0] 
  ret i8* %43 
}


define external ccc  i8* @anonymous$lifted$4(i8*  %$Applicative$d55_0, i8*  %__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT_0, i8*  %mf_0, i8*  %mm_0)    {
  %1 = bitcast i8* %$Applicative$d55_0 to i8* 
  %2 = bitcast i8* %__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT_0 to {i8*, i32, i32, i8*}* 
  %3 = bitcast i8* %mf_0 to i8* 
  %4 = bitcast i8* %mm_0 to i8* 
  %5 = bitcast i8* (i8*, i8*)* @__eddaab7dd3adaa6b7d769751eb6c3b5e__liftA2 to i8* 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {i8*, i32, i32}* 
  %8 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %7, i32 0, i32 0 
  store  i8* %5, i8** %8, align 8 
  %9 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %7, i32 0, i32 1 
  store  i32 2, i32* %9, align 8 
  %10 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %7, i32 0, i32 2 
  store  i32 2, i32* %10, align 8 
  %11 = bitcast {i8*, i32, i32}* %7 to i8* 
  %12 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %11, i32  1, i8*  %1)  
  %13 = bitcast i8* %12 to {i8*, i32, i32, i8*}* 
  %14 = bitcast {i8*, i32, i32, i8*}* %13 to i8* 
  %15 = bitcast i8* (i8*, i8*)* @anonymous$lifted$3 to i8* 
  %16 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %17 = bitcast i8* %16 to {i8*, i32, i32}* 
  %18 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %17, i32 0, i32 0 
  store  i8* %15, i8** %18, align 8 
  %19 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %17, i32 0, i32 1 
  store  i32 2, i32* %19, align 8 
  %20 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %17, i32 0, i32 2 
  store  i32 2, i32* %20, align 8 
  %21 =  call ccc  i8*  @__eddaab7dd3adaa6b7d769751eb6c3b5e__runWriterT(i8*  %3)  
  %22 = bitcast i8* %21 to i8* 
  %23 =  call ccc  i8*  @__eddaab7dd3adaa6b7d769751eb6c3b5e__runWriterT(i8*  %4)  
  %24 = bitcast i8* %23 to i8* 
  %25 = bitcast {i8*, i32, i32}* %17 to i8* 
  %26 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %14, i32  3, i8*  %25, i8*  %22, i8*  %24)  
  %27 = bitcast i8* %26 to i8* 
  %28 =  call ccc  i8*  @__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT(i8*  %27)  
  %29 = bitcast i8* %28 to i8* 
  ret i8* %29 
}


define external ccc  i8* @anonymous$lifted$5(i8*  %$Applicative$p67_0, i8*  %__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT_0, i8*  %x_0)    {
  %1 = bitcast i8* %$Applicative$p67_0 to i8* 
  %2 = bitcast i8* %__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT_0 to {i8*, i32, i32, i8*}* 
  %3 = bitcast i8* %x_0 to i8* 
  %4 = bitcast i8* %1 to {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* 
  %5 = getelementptr  {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* %4, i32 0, i32 1 
  %6 = bitcast {i8*, i32, i32, i8*}* %5 to i8* 
  %7 = bitcast {i8*, i8*}* zeroinitializer to i8* 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {i8*, i8*}* 
  %10 = getelementptr  {i8*, i8*}, {i8*, i8*}* %9, i32 0, i32 0 
  store  i8* %3, i8** %10, align 8 
  %11 = getelementptr  {i8*, i8*}, {i8*, i8*}* %9, i32 0, i32 1 
  store  i8* %7, i8** %11, align 8 
  %12 = bitcast {i8*, i8*}* %9 to i8* 
  %13 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %6, i32  1, i8*  %12)  
  %14 = bitcast i8* %13 to i8* 
  %15 =  call ccc  i8*  @__eddaab7dd3adaa6b7d769751eb6c3b5e__WriterT(i8*  %14)  
  %16 = bitcast i8* %15 to i8* 
  ret i8* %16 
}


define external ccc  i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__runWriterT(i8*  %__x___0)    {
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


define external ccc  i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__liftA2(i8*  %$Functor$i112_0, i8*  %$Applicative$i112_0)    {
  %1 = bitcast i8* %$Functor$i112_0 to i8* 
  %2 = bitcast i8* %$Applicative$i112_0 to i8* 
  %3 = bitcast i8* (i8*, i8*, i8*, i8*, i8*)* @anonymous$lifted$0 to i8* 
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


define external ccc  i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__runIdentity(i8*  %__x___0)    {
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


define external ccc  i8* @__eddaab7dd3adaa6b7d769751eb6c3b5e__runSimpleStack(i8*  %_P__0)    {
  %1 = bitcast i8* %_P__0 to i8* 
  %2 =  call ccc  i8*  @__eddaab7dd3adaa6b7d769751eb6c3b5e__runWriterT(i8*  %1)  
  %3 = bitcast i8* %2 to i8* 
  %4 =  call ccc  i8*  @__eddaab7dd3adaa6b7d769751eb6c3b5e__runIdentity(i8*  %3)  
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


@__eddaab7dd3adaa6b7d769751eb6c3b5e__tchouk =    global i8* undef


define external ccc  void @main()    {
entry_0:
  %0 = bitcast i8* (i8*, i8*)* @$Applicative$WriterT$pure to i8* 
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {i8*, i32, i32}* 
  %3 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %2, i32 0, i32 0 
  store  i8* %0, i8** %3, align 8 
  %4 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %2, i32 0, i32 1 
  store  i32 2, i32* %4, align 8 
  %5 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %2, i32 0, i32 2 
  store  i32 2, i32* %5, align 8 
  %6 = bitcast {i8*, i32, i32}* %2 to i8* 
  %7 = bitcast {{i8*, i32, i32}}* @$Functor$Identity to i8* 
  %8 = bitcast {{i8*, i32, i32}, {i8*, i32, i32}}* @$Applicative$Identity to i8* 
  %9 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %6, i32  2, i8*  %7, i8*  %8)  
  %10 = bitcast i8* %9 to {i8*, i32, i32, i8*}* 
  %11 = bitcast {i8*, i32, i32, i8*}* %10 to i8* 
  %12 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %13 = bitcast i8* %12 to double* 
  store  double 3.000000e0, double* %13, align 8 
  %14 = bitcast double* %13 to i8* 
ASSIGNMENT: LocalReference
  PointerType
    { pointerReferent = IntegerType { typeBits = 8 }
    , pointerAddrSpace = AddrSpace 0
    }
  (UnName 16)
Name: __eddaab7dd3adaa6b7d769751eb6c3b5e__tchouk
  %15 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %11, i32  1, i8*  %14)  
  %16 = bitcast i8* %15 to i8* 
  store  i8* %16, i8** @__eddaab7dd3adaa6b7d769751eb6c3b5e__tchouk, align 8 
  %17 = load  i8*, i8** @__eddaab7dd3adaa6b7d769751eb6c3b5e__tchouk, align 8 
  %18 = bitcast i8* %17 to {i64, i8*}* 
  %19 = getelementptr  {i64, i8*}, {i64, i8*}* %18, i32 0, i32 1 
  %20 = getelementptr  {i64, i8*}, {i64, i8*}* %18, i32 0, i32 0 
  %21 = load  i64, i64* %20, align 8 
  %22 = icmp eq i64 0, %21 
  %23 = load  i8*, i8** %19, align 8 
  %24 = bitcast i8* %23 to i8* 
  %25 = bitcast i8* %24 to {i64, i8*}* 
  %26 = getelementptr  {i64, i8*}, {i64, i8*}* %25, i32 0, i32 1 
  %27 = getelementptr  {i64, i8*}, {i64, i8*}* %25, i32 0, i32 0 
  %28 = load  i64, i64* %27, align 8 
  %29 = icmp eq i64 0, %28 
  %30 = load  i8*, i8** %26, align 8 
  %31 = bitcast i8* %30 to {i8*, i8*}* 
  %32 = getelementptr  {i8*, i8*}, {i8*, i8*}* %31, i32 0, i32 0 
  %33 = getelementptr  {i8*, i8*}, {i8*, i8*}* %31, i32 0, i32 1 
  %34 = load  i8*, i8** %32, align 8 
  %35 = bitcast i8* %34 to double* 
  %36 = load  double, double* %35, align 8 
  %37 = and i1 1, 1 
  %38 = load  i8*, i8** %33, align 8 
  %39 = bitcast i8* %38 to i8* 
  %40 = and i1 %37, 1 
  %41 = and i1 1, %40 
  %42 = and i1 %29, %41 
  %43 = and i1 1, %42 
  %44 = and i1 %22, %43 
  br i1 %44, label %branchExpBlock_0, label %exitBlock_0 
branchExpBlock_0:
  %45 = bitcast i8* %17 to {i64, i8*}* 
  %46 = getelementptr  {i64, i8*}, {i64, i8*}* %45, i32 0, i32 1 
  %47 = load  i8*, i8** %46, align 8 
  %48 = bitcast i8* %47 to i8* 
  %49 = bitcast i8* %48 to {i64, i8*}* 
  %50 = getelementptr  {i64, i8*}, {i64, i8*}* %49, i32 0, i32 1 
  %51 = load  i8*, i8** %50, align 8 
  %52 = bitcast i8* %51 to {i8*, i8*}* 
  %53 = getelementptr  {i8*, i8*}, {i8*, i8*}* %52, i32 0, i32 0 
  %54 = load  i8*, i8** %53, align 8 
  %55 = bitcast i8* %54 to double* 
  %56 = load  double, double* %55, align 8 
  %57 = getelementptr  {i8*, i8*}, {i8*, i8*}* %52, i32 0, i32 1 
  %58 = load  i8*, i8** %57, align 8 
  %59 = bitcast i8* %58 to i8* 
  %60 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %61 = bitcast i8* %60 to double* 
  store  double %56, double* %61, align 8 
  %62 = bitcast double* %61 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %63 = phi i8* [%62, %branchExpBlock_0], [zeroinitializer, %entry_0] 
  ret void 
}