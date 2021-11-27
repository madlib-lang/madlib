; ModuleID = 'main'


 


define external ccc  i8* @__4282899fd67a43a38f13e2be62dba7a9__Just(i8* )    {
  %2 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64, i8*}* getelementptr inbounds ({i64, i8*}, {i64, i8*}* inttoptr (i32 0 to {i64, i8*}*), i32 1) to i64))  
  %3 = bitcast i8* %2 to {i64, i8*}* 
  %4 = getelementptr  {i64, i8*}, {i64, i8*}* %3, i32 0, i32 1 
  store  i8* %0, i8** %4, align 8 
  %5 = getelementptr  {i64, i8*}, {i64, i8*}* %3, i32 0, i32 0 
  store  i64 0, i64* %5, align 8 
  %6 = bitcast {i64, i8*}* %3 to i8* 
  ret i8* %6 
}


define external ccc  i8* @__4282899fd67a43a38f13e2be62dba7a9__Nothing()    {
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64}* getelementptr inbounds ({i64}, {i64}* inttoptr (i32 0 to {i64}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {i64}* 
  %3 = getelementptr  {i64}, {i64}* %2, i32 0, i32 0 
  store  i64 1, i64* %3, align 8 
  %4 = bitcast {i64}* %2 to i8* 
  ret i8* %4 
}


define external ccc  i8* @$Functor$List$map(i8*  %f_0, i8*  %m_0)    {
  %1 = bitcast i8* %f_0 to {i8*, i32, i32, i8*}* 
  %2 = bitcast i8* %m_0 to {i8*, i8*}* 
  %3 = bitcast i8* (i8*, i8*, i8*)* @helper$lifted$11 to i8* 
  %4 = bitcast {i8*, i32, i32, i8*}* %1 to i8* 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {i8*}* 
  %7 = getelementptr  {i8*}, {i8*}* %6, i32 0, i32 0 
  store  i8* %4, i8** %7, align 8 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {i8*, i32, i32, i8*}* 
  %10 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 0 
  store  i8* %3, i8** %10, align 8 
  %11 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 1 
  store  i32 3, i32* %11, align 8 
  %12 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 2 
  store  i32 2, i32* %12, align 8 
  %13 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 3 
  store  i8* %5, i8** %13, align 8 
  %14 = bitcast {i8*, i32, i32, i8*}* %9 to i8* 
  %15 = bitcast {i8*, i8*}* zeroinitializer to i8* 
  %16 = bitcast {i8*, i8*}* %2 to i8* 
  %17 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %14, i32  2, i8*  %15, i8*  %16)  
  %18 = bitcast i8* %17 to {i8*, i8*}* 
  %19 = bitcast {i8*, i8*}* %18 to i8* 
  ret i8* %19 
}


@$Functor$List =    global {i8* (i8*, i8*)*} { i8* (i8*, i8*)* @$Functor$List$map }


define external ccc  i8* @$Functor$Maybe$map(i8*  %f_0, i8*  %maybe_0)    {
; <label>:0:
  %1 = bitcast i8* %f_0 to {i8*, i32, i32, i8*}* 
  %2 = bitcast i8* %maybe_0 to i8* 
  %3 = bitcast i8* %2 to {i64, i8*}* 
  %4 = getelementptr  {i64, i8*}, {i64, i8*}* %3, i32 0, i32 1 
  %5 = getelementptr  {i64, i8*}, {i64, i8*}* %3, i32 0, i32 0 
  %6 = load  i64, i64* %5, align 8 
  %7 = icmp eq i64 0, %6 
  %8 = load  i8*, i8** %4, align 8 
  %9 = bitcast i8* %8 to i8* 
  %10 = and i1 1, 1 
  %11 = and i1 %7, %10 
  br i1 %11, label %branchExpBlock_0, label %nextBlock_0 
branchExpBlock_0:
  %12 = bitcast i8* %2 to {i64, i8*}* 
  %13 = getelementptr  {i64, i8*}, {i64, i8*}* %12, i32 0, i32 1 
  %14 = load  i8*, i8** %13, align 8 
  %15 = bitcast i8* %14 to i8* 
  %16 = bitcast {i8*, i32, i32, i8*}* %1 to i8* 
  %17 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %16, i32  1, i8*  %15)  
  %18 = bitcast i8* %17 to i8* 
  %19 =  call ccc  i8*  @__4282899fd67a43a38f13e2be62dba7a9__Just(i8*  %18)  
  %20 = bitcast i8* %19 to i8* 
  br label %exitBlock_0 
nextBlock_0:
  %21 = bitcast i8* %2 to {i64}* 
  %22 = getelementptr  {i64}, {i64}* %21, i32 0, i32 0 
  %23 = load  i64, i64* %22, align 8 
  %24 = icmp eq i64 1, %23 
  %25 = and i1 %24, 1 
  br i1 %25, label %branchExpBlock_1, label %exitBlock_0 
branchExpBlock_1:
  %26 = bitcast i8* %2 to {i64}* 
  %27 =  call ccc  i8*  @__4282899fd67a43a38f13e2be62dba7a9__Nothing()  
  br label %exitBlock_0 
exitBlock_0:
  %28 = phi i8* [%20, %branchExpBlock_0], [%27, %branchExpBlock_1], [zeroinitializer, %nextBlock_0] 
  ret i8* %28 
}


@$Functor$Maybe =    global {i8* (i8*, i8*)*} { i8* (i8*, i8*)* @$Functor$Maybe$map }


define external ccc  i8* @$Applicative$List$ap(i8*  %mf_0, i8*  %ma_0)    {
  %1 = bitcast i8* %mf_0 to {i8*, i8*}* 
  %2 = bitcast i8* %ma_0 to {i8*, i8*}* 
  %3 = bitcast i8* (i8*, i8*)* @anonymous$lifted$13 to i8* 
  %4 = bitcast {i8*, i8*}* %2 to i8* 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {i8*}* 
  %7 = getelementptr  {i8*}, {i8*}* %6, i32 0, i32 0 
  store  i8* %4, i8** %7, align 8 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {i8*, i32, i32, i8*}* 
  %10 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 0 
  store  i8* %3, i8** %10, align 8 
  %11 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 1 
  store  i32 2, i32* %11, align 8 
  %12 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 2 
  store  i32 1, i32* %12, align 8 
  %13 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 3 
  store  i8* %5, i8** %13, align 8 
  %14 = bitcast {i8*, i32, i32, i8*}* %9 to i8* 
  %15 = bitcast {i8*, i8*}* %1 to i8* 
  %16 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %14, i32  1, i8*  %15)  
  %17 = bitcast i8* %16 to {i8*, i8*}* 
  %18 = bitcast {i8*, i8*}* %17 to i8* 
  ret i8* %18 
}


define external ccc  i8* @$Applicative$List$pure(i8*  %a_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 =  call ccc  {i8*, i8*}*  @MadList_singleton(i8*  %1)  
  %3 = bitcast {i8*, i8*}* %2 to {i8*, i8*}* 
  %4 = bitcast {i8*, i8*}* %3 to i8* 
  ret i8* %4 
}


@$Applicative$List =    global {i8* (i8*, i8*)*, i8* (i8*)*} { i8* (i8*, i8*)* @$Applicative$List$ap, i8* (i8*)* @$Applicative$List$pure }


define external ccc  i8* @helper$lifted$0(i8*  %acc_0, i8*  %l_0)    {
; <label>:0:
  %1 = bitcast i8* %acc_0 to {i8*, i8*}* 
  %2 = bitcast i8* %l_0 to {i8*, i8*}* 
  %3 =  call ccc  i1  @MadList_hasMinLength(double  1.000000e0, {i8*, i8*}*  %2)  
  %4 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 0 
  %5 = load  i8*, i8** %4, align 8 
  %6 = bitcast i8* %5 to i8* 
  %7 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 1 
  %8 = load  i8*, i8** %7, align 8 
  %9 = bitcast i8* %8 to {i8*, i8*}* 
  %10 = and i1 1, 1 
  %11 = and i1 %3, %10 
  br i1 %11, label %branchExpBlock_0, label %nextBlock_0 
branchExpBlock_0:
  %12 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 0 
  %13 = load  i8*, i8** %12, align 8 
  %14 = bitcast i8* %13 to i8* 
  %15 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 1 
  %16 = load  i8*, i8** %15, align 8 
  %17 = bitcast i8* %16 to {i8*, i8*}* 
  %18 = bitcast {i8*, i8*}* %1 to i8* 
  %19 =  call ccc  i8*  @push(i8*  %14, i8*  %18)  
  %20 = bitcast i8* %19 to {i8*, i8*}* 
  %21 = bitcast {i8*, i8*}* %20 to i8* 
  %22 = bitcast {i8*, i8*}* %17 to i8* 
  %23 =  call ccc  i8*  @helper$lifted$0(i8*  %21, i8*  %22)  
  %24 = bitcast i8* %23 to {i8*, i8*}* 
  %25 = bitcast {i8*, i8*}* %24 to i8* 
  br label %exitBlock_0 
nextBlock_0:
  %26 =  call ccc  i1  @MadList_hasLength(double  0.000000e0, {i8*, i8*}*  %2)  
  %27 = and i1 %26, 1 
  br i1 %27, label %branchExpBlock_1, label %exitBlock_0 
branchExpBlock_1:
  %28 = bitcast {i8*, i8*}* %1 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %29 = phi i8* [%25, %branchExpBlock_0], [%28, %branchExpBlock_1], [zeroinitializer, %nextBlock_0] 
  ret i8* %29 
}


define external ccc  i8* @anonymous$lifted$1(i8*  %x_0)    {
  %1 = bitcast i8* %x_0 to double* 
  %2 = load  double, double* %1, align 8 
  %3 = fadd double %2, 1.000000e0 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %5 = bitcast i8* %4 to double* 
  store  double %3, double* %5, align 8 
  %6 = bitcast double* %5 to i8* 
  ret i8* %6 
}


define external ccc  i8* @anonymous$lifted$2(i8*  %x_0)    {
  %1 = bitcast i8* %x_0 to double* 
  %2 = load  double, double* %1, align 8 
  %3 = fmul double %2, 2.000000e0 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %5 = bitcast i8* %4 to double* 
  store  double %3, double* %5, align 8 
  %6 = bitcast double* %5 to i8* 
  ret i8* %6 
}


define external ccc  i8* @anonymous$lifted$3(i8*  %_P__0)    {
  %1 = bitcast i8* %_P__0 to {i8*, i8*}* 
  %2 = bitcast i8* (i8*)* @__4282899fd67a43a38f13e2be62dba7a9__logNum to i8* 
  %3 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %4 = bitcast i8* %3 to {i8*, i32, i32}* 
  %5 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %4, i32 0, i32 0 
  store  i8* %2, i8** %5, align 8 
  %6 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %4, i32 0, i32 1 
  store  i32 1, i32* %6, align 8 
  %7 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %4, i32 0, i32 2 
  store  i32 1, i32* %7, align 8 
  %8 = bitcast i8* (i8*)* @anonymous$lifted$2 to i8* 
  %9 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %10 = bitcast i8* %9 to {i8*, i32, i32}* 
  %11 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %10, i32 0, i32 0 
  store  i8* %8, i8** %11, align 8 
  %12 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %10, i32 0, i32 1 
  store  i32 1, i32* %12, align 8 
  %13 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %10, i32 0, i32 2 
  store  i32 1, i32* %13, align 8 
  %14 = bitcast {i8*, i32, i32}* %10 to i8* 
  %15 =  call ccc  {i8*, i8*}*  @MadList_singleton(i8*  %14)  
  %16 = bitcast i8* (i8*)* @anonymous$lifted$1 to i8* 
  %17 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %18 = bitcast i8* %17 to {i8*, i32, i32}* 
  %19 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %18, i32 0, i32 0 
  store  i8* %16, i8** %19, align 8 
  %20 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %18, i32 0, i32 1 
  store  i32 1, i32* %20, align 8 
  %21 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %18, i32 0, i32 2 
  store  i32 1, i32* %21, align 8 
  %22 = bitcast {i8*, i32, i32}* %18 to i8* 
  %23 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %22, {i8*, i8*}*  %15)  
  %24 = bitcast {i8*, i8*}* %23 to {i8*, i8*}* 
  %25 = bitcast {i8*, i8*}* %24 to i8* 
  %26 = bitcast {i8*, i8*}* %1 to i8* 
  %27 =  call ccc  i8*  @$Applicative$List$ap(i8*  %25, i8*  %26)  
  %28 = bitcast i8* %27 to {i8*, i8*}* 
  %29 = bitcast {i8*, i32, i32}* %4 to i8* 
  %30 = bitcast {i8*, i8*}* %28 to i8* 
  %31 =  call ccc  i8*  @$Functor$List$map(i8*  %29, i8*  %30)  
  %32 = bitcast i8* %31 to {i8*, i8*}* 
  %33 = bitcast {i8*, i8*}* %32 to i8* 
  ret i8* %33 
}


define external ccc  i8* @anonymous$lifted$4(i8*  %x_0)    {
  %1 = bitcast i8* %x_0 to double* 
  %2 = load  double, double* %1, align 8 
  %3 = fadd double %2, 1.000000e0 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %5 = bitcast i8* %4 to double* 
  store  double %3, double* %5, align 8 
  %6 = bitcast double* %5 to i8* 
  ret i8* %6 
}


define external ccc  i8* @anonymous$lifted$5(i8*  %x_0)    {
  %1 = bitcast i8* %x_0 to double* 
  %2 = load  double, double* %1, align 8 
  %3 = fmul double %2, 2.000000e0 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %5 = bitcast i8* %4 to double* 
  store  double %3, double* %5, align 8 
  %6 = bitcast double* %5 to i8* 
  ret i8* %6 
}


define external ccc  i8* @anonymous$lifted$6(i8*  %x_0)    {
  %1 = bitcast i8* %x_0 to double* 
  %2 = load  double, double* %1, align 8 
  %3 = fadd double %2, 1.000000e0 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %5 = bitcast i8* %4 to double* 
  store  double %3, double* %5, align 8 
  %6 = bitcast double* %5 to i8* 
  ret i8* %6 
}


define external ccc  i8* @anonymous$lifted$7(i8*  %x_0)    {
  %1 = bitcast i8* %x_0 to double* 
  %2 = load  double, double* %1, align 8 
  %3 = fadd double %2, 1.000000e0 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %5 = bitcast i8* %4 to double* 
  store  double %3, double* %5, align 8 
  %6 = bitcast double* %5 to i8* 
  ret i8* %6 
}


define external ccc  i8* @anonymous$lifted$8(i8*  %$Functor$s304_0, i8*  %functor_0)    {
  %1 = bitcast i8* %$Functor$s304_0 to i8* 
  %2 = bitcast i8* %functor_0 to i8* 
  %3 = bitcast i8* (i8*)* @__4282899fd67a43a38f13e2be62dba7a9__mapAlias to i8* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8*, i32, i32}* 
  %6 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %5, i32 0, i32 0 
  store  i8* %3, i8** %6, align 8 
  %7 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %5, i32 0, i32 1 
  store  i32 1, i32* %7, align 8 
  %8 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %5, i32 0, i32 2 
  store  i32 1, i32* %8, align 8 
  %9 = bitcast {i8*, i32, i32}* %5 to i8* 
  %10 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %9, i32  1, i8*  %1)  
  %11 = bitcast i8* %10 to {i8*, i32, i32, i8*}* 
  %12 = bitcast {i8*, i32, i32, i8*}* %11 to i8* 
  %13 = bitcast i8* (i8*)* @anonymous$lifted$7 to i8* 
  %14 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %15 = bitcast i8* %14 to {i8*, i32, i32}* 
  %16 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %15, i32 0, i32 0 
  store  i8* %13, i8** %16, align 8 
  %17 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %15, i32 0, i32 1 
  store  i32 1, i32* %17, align 8 
  %18 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %15, i32 0, i32 2 
  store  i32 1, i32* %18, align 8 
  %19 = bitcast {i8*, i32, i32}* %15 to i8* 
  %20 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %12, i32  2, i8*  %19, i8*  %2)  
  %21 = bitcast i8* %20 to i8* 
  ret i8* %21 
}


define external ccc  i8* @anonymous$lifted$9(i8*  %x_0)    {
  %1 = bitcast i8* %x_0 to double* 
  %2 = load  double, double* %1, align 8 
  %3 = fmul double %2, 7.000000e0 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %5 = bitcast i8* %4 to double* 
  store  double %3, double* %5, align 8 
  %6 = bitcast double* %5 to i8* 
  ret i8* %6 
}


define external ccc  i8* @anonymous$lifted$10(i8*  %x_0)    {
  %1 = bitcast i8* %x_0 to double* 
  %2 = load  double, double* %1, align 8 
  %3 = fadd double %2, 1.000000e0 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %5 = bitcast i8* %4 to double* 
  store  double %3, double* %5, align 8 
  %6 = bitcast double* %5 to i8* 
  ret i8* %6 
}


define external ccc  i8* @helper$lifted$11(i8*  %f_0, i8*  %acc_0, i8*  %n_0)    {
; <label>:0:
  %1 = bitcast i8* %f_0 to {i8*, i32, i32, i8*}* 
  %2 = bitcast i8* %acc_0 to {i8*, i8*}* 
  %3 = bitcast i8* %n_0 to {i8*, i8*}* 
  %4 =  call ccc  i1  @MadList_hasMinLength(double  1.000000e0, {i8*, i8*}*  %3)  
  %5 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 0 
  %6 = load  i8*, i8** %5, align 8 
  %7 = bitcast i8* %6 to i8* 
  %8 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 1 
  %9 = load  i8*, i8** %8, align 8 
  %10 = bitcast i8* %9 to {i8*, i8*}* 
  %11 = and i1 1, 1 
  %12 = and i1 %4, %11 
  br i1 %12, label %branchExpBlock_0, label %nextBlock_0 
branchExpBlock_0:
  %13 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 0 
  %14 = load  i8*, i8** %13, align 8 
  %15 = bitcast i8* %14 to i8* 
  %16 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 1 
  %17 = load  i8*, i8** %16, align 8 
  %18 = bitcast i8* %17 to {i8*, i8*}* 
  %19 = bitcast {i8*, i32, i32, i8*}* %1 to i8* 
  %20 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %19, i32  1, i8*  %15)  
  %21 = bitcast i8* %20 to i8* 
  %22 = bitcast {i8*, i8*}* %2 to i8* 
  %23 =  call ccc  i8*  @push(i8*  %21, i8*  %22)  
  %24 = bitcast i8* %23 to {i8*, i8*}* 
  %25 = bitcast {i8*, i32, i32, i8*}* %1 to i8* 
  %26 = bitcast {i8*, i8*}* %24 to i8* 
  %27 = bitcast {i8*, i8*}* %18 to i8* 
  %28 =  call ccc  i8*  @helper$lifted$11(i8*  %25, i8*  %26, i8*  %27)  
  %29 = bitcast i8* %28 to {i8*, i8*}* 
  %30 = bitcast {i8*, i8*}* %29 to i8* 
  br label %exitBlock_0 
nextBlock_0:
  %31 =  call ccc  i1  @MadList_hasLength(double  0.000000e0, {i8*, i8*}*  %3)  
  %32 = and i1 %31, 1 
  br i1 %32, label %branchExpBlock_1, label %exitBlock_0 
branchExpBlock_1:
  %33 = bitcast {i8*, i8*}* %2 to i8* 
  %34 =  call ccc  i8*  @__4282899fd67a43a38f13e2be62dba7a9__reverse(i8*  %33)  
  %35 = bitcast i8* %34 to {i8*, i8*}* 
  %36 = bitcast {i8*, i8*}* %35 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %37 = phi i8* [%30, %branchExpBlock_0], [%36, %branchExpBlock_1], [zeroinitializer, %nextBlock_0] 
  ret i8* %37 
}


define external ccc  i8* @anonymous$lifted$12(i8*  %ma_0, i8*  %f_0)    {
  %1 = bitcast i8* %ma_0 to {i8*, i8*}* 
  %2 = bitcast i8* %f_0 to {i8*, i32, i32, i8*}* 
  %3 = bitcast {i8*, i32, i32, i8*}* %2 to i8* 
  %4 = bitcast {i8*, i8*}* %1 to i8* 
  %5 =  call ccc  i8*  @$Functor$List$map(i8*  %3, i8*  %4)  
  %6 = bitcast i8* %5 to {i8*, i8*}* 
  %7 = bitcast {i8*, i8*}* %6 to i8* 
  ret i8* %7 
}


define external ccc  i8* @anonymous$lifted$13(i8*  %ma_0, i8*  %_P__0)    {
  %1 = bitcast i8* %ma_0 to {i8*, i8*}* 
  %2 = bitcast i8* %_P__0 to {i8*, i8*}* 
  %3 = bitcast i8* (i8*, i8*)* @anonymous$lifted$12 to i8* 
  %4 = bitcast {i8*, i8*}* %1 to i8* 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {i8*}* 
  %7 = getelementptr  {i8*}, {i8*}* %6, i32 0, i32 0 
  store  i8* %4, i8** %7, align 8 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {i8*, i32, i32, i8*}* 
  %10 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 0 
  store  i8* %3, i8** %10, align 8 
  %11 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 1 
  store  i32 2, i32* %11, align 8 
  %12 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 2 
  store  i32 1, i32* %12, align 8 
  %13 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 3 
  store  i8* %5, i8** %13, align 8 
  %14 = bitcast {i8*, i32, i32, i8*}* %9 to i8* 
  %15 = bitcast {i8*, i8*}* %2 to i8* 
  %16 =  call ccc  i8*  @$Functor$List$map(i8*  %14, i8*  %15)  
  %17 = bitcast i8* %16 to {i8*, i8*}* 
  %18 = bitcast {i8*, i8*}* %17 to i8* 
  %19 =  call ccc  i8*  @__4282899fd67a43a38f13e2be62dba7a9__flatten(i8*  %18)  
  %20 = bitcast i8* %19 to {i8*, i8*}* 
  %21 = bitcast {i8*, i8*}* %20 to i8* 
  ret i8* %21 
}


declare external ccc  i8* @MadList_push(i8*, i8*)    


define external ccc  i8* @push(i8* , i8* )    {
  %3 =  call ccc  i8*  @MadList_push(i8*  %0, i8*  %1)  
  ret i8* %3 
}


declare external ccc  i8* @puts(i8*)    


define external ccc  i8* @log(i8* )    {
  %2 =  call ccc  i8*  @puts(i8*  %0)  
  ret i8* %2 
}


declare external ccc  i8* @__doubleToStr__(i8*)    


define external ccc  i8* @showNumber(i8* )    {
  %2 =  call ccc  i8*  @__doubleToStr__(i8*  %0)  
  ret i8* %2 
}


define external ccc  i8* @__4282899fd67a43a38f13e2be62dba7a9__logNum(i8*  %_P__0)    {
  %1 = bitcast i8* %_P__0 to double* 
  %2 = load  double, double* %1, align 8 
  %3 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %4 = bitcast i8* %3 to double* 
  store  double %2, double* %4, align 8 
  %5 = bitcast double* %4 to i8* 
  %6 =  call ccc  i8*  @showNumber(i8*  %5)  
  %7 = bitcast i8* %6 to i8* 
  %8 =  call ccc  i8*  @log(i8*  %7)  
  %9 = bitcast i8* %8 to i8* 
  ret i8* %9 
}


define external ccc  i8* @__4282899fd67a43a38f13e2be62dba7a9__reverse(i8*  %xs_0)    {
  %1 = bitcast i8* %xs_0 to {i8*, i8*}* 
  %2 = bitcast i8* (i8*, i8*)* @helper$lifted$0 to i8* 
  %3 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %4 = bitcast i8* %3 to {i8*, i32, i32}* 
  %5 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %4, i32 0, i32 0 
  store  i8* %2, i8** %5, align 8 
  %6 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %4, i32 0, i32 1 
  store  i32 2, i32* %6, align 8 
  %7 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %4, i32 0, i32 2 
  store  i32 2, i32* %7, align 8 
  %8 = bitcast {i8*, i32, i32}* %4 to i8* 
  %9 = bitcast {i8*, i8*}* zeroinitializer to i8* 
  %10 = bitcast {i8*, i8*}* %1 to i8* 
  %11 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %8, i32  2, i8*  %9, i8*  %10)  
  %12 = bitcast i8* %11 to {i8*, i8*}* 
  %13 = bitcast {i8*, i8*}* %12 to i8* 
  ret i8* %13 
}


define external ccc  i8* @__4282899fd67a43a38f13e2be62dba7a9__flatten(i8*  %xs_0)    {
; <label>:0:
  %1 = bitcast i8* %xs_0 to {i8*, i8*}* 
  %2 =  call ccc  i1  @MadList_hasMinLength(double  2.000000e0, {i8*, i8*}*  %1)  
  %3 = getelementptr  {i8*, i8*}, {i8*, i8*}* %1, i32 0, i32 0 
  %4 = load  i8*, i8** %3, align 8 
  %5 = bitcast i8* %4 to i8* 
  %6 = getelementptr  {i8*, i8*}, {i8*, i8*}* %1, i32 0, i32 1 
  %7 = load  i8*, i8** %6, align 8 
  %8 = bitcast i8* %7 to {i8*, i8*}* 
  %9 = getelementptr  {i8*, i8*}, {i8*, i8*}* %8, i32 0, i32 0 
  %10 = load  i8*, i8** %9, align 8 
  %11 = bitcast i8* %10 to i8* 
  %12 = getelementptr  {i8*, i8*}, {i8*, i8*}* %8, i32 0, i32 1 
  %13 = load  i8*, i8** %12, align 8 
  %14 = bitcast i8* %13 to {i8*, i8*}* 
  %15 = and i1 1, 1 
  %16 = and i1 1, %15 
  %17 = and i1 %2, %16 
  br i1 %17, label %branchExpBlock_0, label %nextBlock_0 
branchExpBlock_0:
  %18 = getelementptr  {i8*, i8*}, {i8*, i8*}* %1, i32 0, i32 0 
  %19 = load  i8*, i8** %18, align 8 
  %20 = bitcast i8* %19 to i8* 
  %21 = getelementptr  {i8*, i8*}, {i8*, i8*}* %1, i32 0, i32 1 
  %22 = load  i8*, i8** %21, align 8 
  %23 = bitcast i8* %22 to {i8*, i8*}* 
  %24 = getelementptr  {i8*, i8*}, {i8*, i8*}* %23, i32 0, i32 0 
  %25 = load  i8*, i8** %24, align 8 
  %26 = bitcast i8* %25 to i8* 
  %27 = getelementptr  {i8*, i8*}, {i8*, i8*}* %23, i32 0, i32 1 
  %28 = load  i8*, i8** %27, align 8 
  %29 = bitcast i8* %28 to {i8*, i8*}* 
  %30 = bitcast {i8*, i8*}* %29 to i8* 
  %31 =  call ccc  i8*  @__4282899fd67a43a38f13e2be62dba7a9__flatten(i8*  %30)  
  %32 = bitcast i8* %31 to {i8*, i8*}* 
  %33 =  call ccc  {i8*, i8*}*  @MadList_concat(i8*  %26, {i8*, i8*}*  %32)  
  %34 = bitcast {i8*, i8*}* %33 to {i8*, i8*}* 
  %35 = bitcast {i8*, i8*}* %34 to i8* 
  br label %exitBlock_0 
nextBlock_0:
  %36 =  call ccc  i1  @MadList_hasLength(double  0.000000e0, {i8*, i8*}*  %1)  
  %37 = and i1 %36, 1 
  br i1 %37, label %branchExpBlock_1, label %exitBlock_0 
branchExpBlock_1:
  %38 = bitcast {i8*, i8*}* zeroinitializer to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %39 = phi i8* [%35, %branchExpBlock_0], [%38, %branchExpBlock_1], [zeroinitializer, %nextBlock_0] 
  ret i8* %39 
}


define external ccc  i8* @__4282899fd67a43a38f13e2be62dba7a9__myAp(i8*  %$Applicative$t201_0)    {
  %1 = bitcast i8* %$Applicative$t201_0 to i8* 
  %2 = bitcast i8* %1 to {i8*, i8*}* 
  %3 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 0 
  %4 = load  i8*, i8** %3, align 8 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {i8*, i32, i32}* 
  %7 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %6, i32 0, i32 0 
  store  i8* %4, i8** %7, align 8 
  %8 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %6, i32 0, i32 1 
  store  i32 2, i32* %8, align 8 
  %9 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %6, i32 0, i32 2 
  store  i32 2, i32* %9, align 8 
  %10 = bitcast {i8*, i32, i32}* %6 to i8* 
  ret i8* %10 
}


define external ccc  i8* @__4282899fd67a43a38f13e2be62dba7a9__myPure(i8*  %$Applicative$z207_0)    {
  %1 = bitcast i8* %$Applicative$z207_0 to i8* 
  %2 = bitcast i8* %1 to {i8*, i8*}* 
  %3 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 1 
  %4 = load  i8*, i8** %3, align 8 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {i8*, i32, i32}* 
  %7 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %6, i32 0, i32 0 
  store  i8* %4, i8** %7, align 8 
  %8 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %6, i32 0, i32 1 
  store  i32 1, i32* %8, align 8 
  %9 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %6, i32 0, i32 2 
  store  i32 1, i32* %9, align 8 
  %10 = bitcast {i8*, i32, i32}* %6 to i8* 
  ret i8* %10 
}


define external ccc  i8* @__4282899fd67a43a38f13e2be62dba7a9__mapAlias(i8*  %$Functor$t253_0)    {
  %1 = bitcast i8* %$Functor$t253_0 to i8* 
  %2 = bitcast i8* %1 to {i8*}* 
  %3 = getelementptr  {i8*}, {i8*}* %2, i32 0, i32 0 
  %4 = load  i8*, i8** %3, align 8 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {i8*, i32, i32}* 
  %7 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %6, i32 0, i32 0 
  store  i8* %4, i8** %7, align 8 
  %8 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %6, i32 0, i32 1 
  store  i32 2, i32* %8, align 8 
  %9 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %6, i32 0, i32 2 
  store  i32 2, i32* %9, align 8 
  %10 = bitcast {i8*, i32, i32}* %6 to i8* 
  ret i8* %10 
}


define external ccc  i8* @__4282899fd67a43a38f13e2be62dba7a9__fAdd(i8*  %$Functor$l271_0)    {
  %1 = bitcast i8* %$Functor$l271_0 to i8* 
  %2 = bitcast i8* (i8*)* @__4282899fd67a43a38f13e2be62dba7a9__mapAlias to i8* 
  %3 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %4 = bitcast i8* %3 to {i8*, i32, i32}* 
  %5 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %4, i32 0, i32 0 
  store  i8* %2, i8** %5, align 8 
  %6 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %4, i32 0, i32 1 
  store  i32 1, i32* %6, align 8 
  %7 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %4, i32 0, i32 2 
  store  i32 1, i32* %7, align 8 
  %8 = bitcast {i8*, i32, i32}* %4 to i8* 
  %9 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %8, i32  1, i8*  %1)  
  %10 = bitcast i8* %9 to {i8*, i32, i32, i8*}* 
  %11 = bitcast {i8*, i32, i32, i8*}* %10 to i8* 
  %12 = bitcast i8* (i8*)* @anonymous$lifted$6 to i8* 
  %13 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %14 = bitcast i8* %13 to {i8*, i32, i32}* 
  %15 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %14, i32 0, i32 0 
  store  i8* %12, i8** %15, align 8 
  %16 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %14, i32 0, i32 1 
  store  i32 1, i32* %16, align 8 
  %17 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %14, i32 0, i32 2 
  store  i32 1, i32* %17, align 8 
  %18 = bitcast {i8*, i32, i32}* %14 to i8* 
  %19 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %11, i32  1, i8*  %18)  
  %20 = bitcast i8* %19 to {i8*, i32, i32, i8*}* 
  %21 = bitcast {i8*, i32, i32, i8*}* %20 to i8* 
  ret i8* %21 
}


define external ccc  i8* @__4282899fd67a43a38f13e2be62dba7a9__fAdd2(i8*  %$Functor$s304_0)    {
  %1 = bitcast i8* %$Functor$s304_0 to i8* 
  %2 = bitcast i8* (i8*, i8*)* @anonymous$lifted$8 to i8* 
  %3 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %4 = bitcast i8* %3 to {i8*}* 
  %5 = getelementptr  {i8*}, {i8*}* %4, i32 0, i32 0 
  store  i8* %1, i8** %5, align 8 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {i8*, i32, i32, i8*}* 
  %8 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 0 
  store  i8* %2, i8** %8, align 8 
  %9 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 1 
  store  i32 2, i32* %9, align 8 
  %10 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 2 
  store  i32 1, i32* %10, align 8 
  %11 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 3 
  store  i8* %3, i8** %11, align 8 
  %12 = bitcast {i8*, i32, i32, i8*}* %7 to i8* 
  ret i8* %12 
}


define external ccc  i8* @__4282899fd67a43a38f13e2be62dba7a9__fAdd3(i8*  %mapper_0)    {
  %1 = bitcast i8* %mapper_0 to {i8*, i32, i32, i8*}* 
  %2 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %3 = bitcast i8* %2 to double* 
  store  double 2.000000e0, double* %3, align 8 
  %4 = bitcast double* %3 to i8* 
  %5 =  call ccc  {i8*, i8*}*  @MadList_singleton(i8*  %4)  
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %7 = bitcast i8* %6 to double* 
  store  double 1.000000e0, double* %7, align 8 
  %8 = bitcast double* %7 to i8* 
  %9 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %8, {i8*, i8*}*  %5)  
  %10 = bitcast {i8*, i8*}* %9 to {i8*, i8*}* 
  %11 = bitcast {i8*, i32, i32, i8*}* %1 to i8* 
  %12 = bitcast {i8*, i8*}* %10 to i8* 
  %13 =  call ccc  i8*  @$Functor$List$map(i8*  %11, i8*  %12)  
  %14 = bitcast i8* %13 to {i8*, i8*}* 
  %15 = bitcast {i8*, i8*}* %14 to i8* 
  ret i8* %15 
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


@__4282899fd67a43a38f13e2be62dba7a9__apped =    global {i8*, i8*}* undef


@__4282899fd67a43a38f13e2be62dba7a9__r =    global {i8*, i8*}* undef


@__4282899fd67a43a38f13e2be62dba7a9__r2 =    global {i8*, i8*}* undef


@__4282899fd67a43a38f13e2be62dba7a9__r3 =    global {i8*, i8*}* undef


define external ccc  void @main()    {
entry_0:
  %0 = bitcast i8* (i8*)* @__4282899fd67a43a38f13e2be62dba7a9__logNum to i8* 
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {i8*, i32, i32}* 
  %3 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %2, i32 0, i32 0 
  store  i8* %0, i8** %3, align 8 
  %4 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %2, i32 0, i32 1 
  store  i32 1, i32* %4, align 8 
  %5 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %2, i32 0, i32 2 
  store  i32 1, i32* %5, align 8 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %7 = bitcast i8* %6 to double* 
  store  double 3.000000e0, double* %7, align 8 
  %8 = bitcast double* %7 to i8* 
  %9 =  call ccc  {i8*, i8*}*  @MadList_singleton(i8*  %8)  
  %10 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %11 = bitcast i8* %10 to double* 
  store  double 2.000000e0, double* %11, align 8 
  %12 = bitcast double* %11 to i8* 
  %13 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %12, {i8*, i8*}*  %9)  
  %14 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %15 = bitcast i8* %14 to double* 
  store  double 1.000000e0, double* %15, align 8 
  %16 = bitcast double* %15 to i8* 
  %17 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %16, {i8*, i8*}*  %13)  
  %18 = bitcast {i8*, i8*}* %17 to {i8*, i8*}* 
  %19 = bitcast {i8*, i32, i32}* %2 to i8* 
  %20 = bitcast {i8*, i8*}* %18 to i8* 
  %21 =  call ccc  i8*  @$Functor$List$map(i8*  %19, i8*  %20)  
  %22 = bitcast i8* %21 to {i8*, i8*}* 
  %23 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %24 = bitcast i8* %23 to double* 
  store  double 9.000000e0, double* %24, align 8 
  %25 = bitcast double* %24 to i8* 
  %26 =  call ccc  i8*  @$Applicative$List$pure(i8*  %25)  
  %27 = bitcast i8* %26 to {i8*, i8*}* 
  %28 = bitcast {i8*, i8*}* %27 to i8* 
  %29 =  call ccc  i8*  @anonymous$lifted$3(i8*  %28)  
  %30 = bitcast i8* %29 to {i8*, i8*}* 
  %31 = bitcast i8* (i8*)* @__4282899fd67a43a38f13e2be62dba7a9__myAp to i8* 
  %32 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %33 = bitcast i8* %32 to {i8*, i32, i32}* 
  %34 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %33, i32 0, i32 0 
  store  i8* %31, i8** %34, align 8 
  %35 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %33, i32 0, i32 1 
  store  i32 1, i32* %35, align 8 
  %36 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %33, i32 0, i32 2 
  store  i32 1, i32* %36, align 8 
  %37 = bitcast {i8*, i32, i32}* %33 to i8* 
  %38 = bitcast {i8* (i8*, i8*)*, i8* (i8*)*}* @$Applicative$List to i8* 
  %39 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %37, i32  1, i8*  %38)  
  %40 = bitcast i8* %39 to {i8*, i32, i32, i8*}* 
  %41 = bitcast {i8*, i32, i32, i8*}* %40 to i8* 
  %42 = bitcast i8* (i8*)* @anonymous$lifted$5 to i8* 
  %43 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %44 = bitcast i8* %43 to {i8*, i32, i32}* 
  %45 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %44, i32 0, i32 0 
  store  i8* %42, i8** %45, align 8 
  %46 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %44, i32 0, i32 1 
  store  i32 1, i32* %46, align 8 
  %47 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %44, i32 0, i32 2 
  store  i32 1, i32* %47, align 8 
  %48 = bitcast {i8*, i32, i32}* %44 to i8* 
  %49 =  call ccc  {i8*, i8*}*  @MadList_singleton(i8*  %48)  
  %50 = bitcast i8* (i8*)* @anonymous$lifted$4 to i8* 
  %51 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %52 = bitcast i8* %51 to {i8*, i32, i32}* 
  %53 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %52, i32 0, i32 0 
  store  i8* %50, i8** %53, align 8 
  %54 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %52, i32 0, i32 1 
  store  i32 1, i32* %54, align 8 
  %55 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %52, i32 0, i32 2 
  store  i32 1, i32* %55, align 8 
  %56 = bitcast {i8*, i32, i32}* %52 to i8* 
  %57 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %56, {i8*, i8*}*  %49)  
  %58 = bitcast {i8*, i8*}* %57 to {i8*, i8*}* 
  %59 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %60 = bitcast i8* %59 to double* 
  store  double 9.870000e2, double* %60, align 8 
  %61 = bitcast double* %60 to i8* 
  %62 =  call ccc  i8*  @$Applicative$List$pure(i8*  %61)  
  %63 = bitcast i8* %62 to {i8*, i8*}* 
  %64 = bitcast {i8*, i8*}* %58 to i8* 
  %65 = bitcast {i8*, i8*}* %63 to i8* 
  %66 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %41, i32  2, i8*  %64, i8*  %65)  
  %67 = bitcast i8* %66 to {i8*, i8*}* 
  store  {i8*, i8*}* %67, {i8*, i8*}** @__4282899fd67a43a38f13e2be62dba7a9__apped, align 8 
  %68 = bitcast i8* (i8*)* @__4282899fd67a43a38f13e2be62dba7a9__logNum to i8* 
  %69 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %70 = bitcast i8* %69 to {i8*, i32, i32}* 
  %71 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %70, i32 0, i32 0 
  store  i8* %68, i8** %71, align 8 
  %72 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %70, i32 0, i32 1 
  store  i32 1, i32* %72, align 8 
  %73 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %70, i32 0, i32 2 
  store  i32 1, i32* %73, align 8 
  %74 = bitcast {i8*, i32, i32}* %70 to i8* 
  %75 = bitcast {i8*, i8*}* %67 to i8* 
  %76 =  call ccc  i8*  @$Functor$List$map(i8*  %74, i8*  %75)  
  %77 = bitcast i8* %76 to {i8*, i8*}* 
  %78 = bitcast i8* (i8*)* @__4282899fd67a43a38f13e2be62dba7a9__mapAlias to i8* 
  %79 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %80 = bitcast i8* %79 to {i8*, i32, i32}* 
  %81 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %80, i32 0, i32 0 
  store  i8* %78, i8** %81, align 8 
  %82 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %80, i32 0, i32 1 
  store  i32 1, i32* %82, align 8 
  %83 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %80, i32 0, i32 2 
  store  i32 1, i32* %83, align 8 
  %84 = bitcast {i8*, i32, i32}* %80 to i8* 
  %85 = bitcast {i8* (i8*, i8*)*}* @$Functor$List to i8* 
  %86 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %84, i32  1, i8*  %85)  
  %87 = bitcast i8* %86 to {i8*, i32, i32, i8*}* 
  %88 = bitcast {i8*, i32, i32, i8*}* %87 to i8* 
  %89 = bitcast i8* (i8*)* @__4282899fd67a43a38f13e2be62dba7a9__logNum to i8* 
  %90 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %91 = bitcast i8* %90 to {i8*, i32, i32}* 
  %92 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %91, i32 0, i32 0 
  store  i8* %89, i8** %92, align 8 
  %93 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %91, i32 0, i32 1 
  store  i32 1, i32* %93, align 8 
  %94 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %91, i32 0, i32 2 
  store  i32 1, i32* %94, align 8 
  %95 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %96 = bitcast i8* %95 to double* 
  store  double 1.010000e2, double* %96, align 8 
  %97 = bitcast double* %96 to i8* 
  %98 =  call ccc  {i8*, i8*}*  @MadList_singleton(i8*  %97)  
  %99 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %100 = bitcast i8* %99 to double* 
  store  double 1.000000e2, double* %100, align 8 
  %101 = bitcast double* %100 to i8* 
  %102 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %101, {i8*, i8*}*  %98)  
  %103 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %104 = bitcast i8* %103 to double* 
  store  double 9.900000e1, double* %104, align 8 
  %105 = bitcast double* %104 to i8* 
  %106 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %105, {i8*, i8*}*  %102)  
  %107 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %108 = bitcast i8* %107 to double* 
  store  double 9.800000e1, double* %108, align 8 
  %109 = bitcast double* %108 to i8* 
  %110 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %109, {i8*, i8*}*  %106)  
  %111 = bitcast {i8*, i8*}* %110 to {i8*, i8*}* 
  %112 = bitcast {i8*, i32, i32}* %91 to i8* 
  %113 = bitcast {i8*, i8*}* %111 to i8* 
  %114 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %88, i32  2, i8*  %112, i8*  %113)  
  %115 = bitcast i8* %114 to {i8*, i8*}* 
  %116 = bitcast i8* (i8*)* @__4282899fd67a43a38f13e2be62dba7a9__fAdd to i8* 
  %117 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %118 = bitcast i8* %117 to {i8*, i32, i32}* 
  %119 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %118, i32 0, i32 0 
  store  i8* %116, i8** %119, align 8 
  %120 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %118, i32 0, i32 1 
  store  i32 1, i32* %120, align 8 
  %121 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %118, i32 0, i32 2 
  store  i32 1, i32* %121, align 8 
  %122 = bitcast {i8*, i32, i32}* %118 to i8* 
  %123 = bitcast {i8* (i8*, i8*)*}* @$Functor$List to i8* 
  %124 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %122, i32  1, i8*  %123)  
  %125 = bitcast i8* %124 to {i8*, i32, i32, i8*}* 
  %126 = bitcast {i8*, i32, i32, i8*}* %125 to i8* 
  %127 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %128 = bitcast i8* %127 to double* 
  store  double 3.000000e0, double* %128, align 8 
  %129 = bitcast double* %128 to i8* 
  %130 =  call ccc  {i8*, i8*}*  @MadList_singleton(i8*  %129)  
  %131 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %132 = bitcast i8* %131 to double* 
  store  double 2.000000e0, double* %132, align 8 
  %133 = bitcast double* %132 to i8* 
  %134 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %133, {i8*, i8*}*  %130)  
  %135 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %136 = bitcast i8* %135 to double* 
  store  double 1.000000e0, double* %136, align 8 
  %137 = bitcast double* %136 to i8* 
  %138 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %137, {i8*, i8*}*  %134)  
  %139 = bitcast {i8*, i8*}* %138 to {i8*, i8*}* 
  %140 = bitcast {i8*, i8*}* %139 to i8* 
  %141 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %126, i32  1, i8*  %140)  
  %142 = bitcast i8* %141 to {i8*, i8*}* 
  store  {i8*, i8*}* %142, {i8*, i8*}** @__4282899fd67a43a38f13e2be62dba7a9__r, align 8 
  %143 = bitcast i8* (i8*)* @__4282899fd67a43a38f13e2be62dba7a9__logNum to i8* 
  %144 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %145 = bitcast i8* %144 to {i8*, i32, i32}* 
  %146 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %145, i32 0, i32 0 
  store  i8* %143, i8** %146, align 8 
  %147 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %145, i32 0, i32 1 
  store  i32 1, i32* %147, align 8 
  %148 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %145, i32 0, i32 2 
  store  i32 1, i32* %148, align 8 
  %149 = bitcast {i8*, i32, i32}* %145 to i8* 
  %150 = bitcast {i8*, i8*}* %142 to i8* 
  %151 =  call ccc  i8*  @$Functor$List$map(i8*  %149, i8*  %150)  
  %152 = bitcast i8* %151 to {i8*, i8*}* 
  %153 = bitcast i8* (i8*)* @__4282899fd67a43a38f13e2be62dba7a9__fAdd2 to i8* 
  %154 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %155 = bitcast i8* %154 to {i8*, i32, i32}* 
  %156 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %155, i32 0, i32 0 
  store  i8* %153, i8** %156, align 8 
  %157 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %155, i32 0, i32 1 
  store  i32 1, i32* %157, align 8 
  %158 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %155, i32 0, i32 2 
  store  i32 1, i32* %158, align 8 
  %159 = bitcast {i8*, i32, i32}* %155 to i8* 
  %160 = bitcast {i8* (i8*, i8*)*}* @$Functor$List to i8* 
  %161 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %159, i32  1, i8*  %160)  
  %162 = bitcast i8* %161 to {i8*, i32, i32, i8*}* 
  %163 = bitcast {i8*, i32, i32, i8*}* %162 to i8* 
  %164 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %165 = bitcast i8* %164 to double* 
  store  double 6.000000e0, double* %165, align 8 
  %166 = bitcast double* %165 to i8* 
  %167 =  call ccc  {i8*, i8*}*  @MadList_singleton(i8*  %166)  
  %168 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %169 = bitcast i8* %168 to double* 
  store  double 5.000000e0, double* %169, align 8 
  %170 = bitcast double* %169 to i8* 
  %171 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %170, {i8*, i8*}*  %167)  
  %172 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %173 = bitcast i8* %172 to double* 
  store  double 4.000000e0, double* %173, align 8 
  %174 = bitcast double* %173 to i8* 
  %175 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %174, {i8*, i8*}*  %171)  
  %176 = bitcast {i8*, i8*}* %175 to {i8*, i8*}* 
  %177 = bitcast {i8*, i8*}* %176 to i8* 
  %178 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %163, i32  1, i8*  %177)  
  %179 = bitcast i8* %178 to {i8*, i8*}* 
  store  {i8*, i8*}* %179, {i8*, i8*}** @__4282899fd67a43a38f13e2be62dba7a9__r2, align 8 
  %180 = bitcast i8* (i8*)* @__4282899fd67a43a38f13e2be62dba7a9__logNum to i8* 
  %181 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %182 = bitcast i8* %181 to {i8*, i32, i32}* 
  %183 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %182, i32 0, i32 0 
  store  i8* %180, i8** %183, align 8 
  %184 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %182, i32 0, i32 1 
  store  i32 1, i32* %184, align 8 
  %185 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %182, i32 0, i32 2 
  store  i32 1, i32* %185, align 8 
  %186 = bitcast {i8*, i32, i32}* %182 to i8* 
  %187 = bitcast {i8*, i8*}* %179 to i8* 
  %188 =  call ccc  i8*  @$Functor$List$map(i8*  %186, i8*  %187)  
  %189 = bitcast i8* %188 to {i8*, i8*}* 
  %190 = bitcast i8* (i8*)* @anonymous$lifted$9 to i8* 
  %191 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %192 = bitcast i8* %191 to {i8*, i32, i32}* 
  %193 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %192, i32 0, i32 0 
  store  i8* %190, i8** %193, align 8 
  %194 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %192, i32 0, i32 1 
  store  i32 1, i32* %194, align 8 
  %195 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %192, i32 0, i32 2 
  store  i32 1, i32* %195, align 8 
  %196 = bitcast {i8*, i32, i32}* %192 to i8* 
  %197 =  call ccc  i8*  @__4282899fd67a43a38f13e2be62dba7a9__fAdd3(i8*  %196)  
  %198 = bitcast i8* %197 to {i8*, i8*}* 
  store  {i8*, i8*}* %198, {i8*, i8*}** @__4282899fd67a43a38f13e2be62dba7a9__r3, align 8 
  %199 = bitcast i8* (i8*)* @__4282899fd67a43a38f13e2be62dba7a9__logNum to i8* 
  %200 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %201 = bitcast i8* %200 to {i8*, i32, i32}* 
  %202 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %201, i32 0, i32 0 
  store  i8* %199, i8** %202, align 8 
  %203 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %201, i32 0, i32 1 
  store  i32 1, i32* %203, align 8 
  %204 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %201, i32 0, i32 2 
  store  i32 1, i32* %204, align 8 
  %205 = bitcast {i8*, i32, i32}* %201 to i8* 
  %206 = bitcast {i8*, i8*}* %198 to i8* 
  %207 =  call ccc  i8*  @$Functor$List$map(i8*  %205, i8*  %206)  
  %208 = bitcast i8* %207 to {i8*, i8*}* 
  %209 = bitcast i8* (i8*)* @anonymous$lifted$10 to i8* 
  %210 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %211 = bitcast i8* %210 to {i8*, i32, i32}* 
  %212 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %211, i32 0, i32 0 
  store  i8* %209, i8** %212, align 8 
  %213 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %211, i32 0, i32 1 
  store  i32 1, i32* %213, align 8 
  %214 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %211, i32 0, i32 2 
  store  i32 1, i32* %214, align 8 
  %215 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %216 = bitcast i8* %215 to double* 
  store  double 4.000000e0, double* %216, align 8 
  %217 = bitcast double* %216 to i8* 
  %218 =  call ccc  i8*  @__4282899fd67a43a38f13e2be62dba7a9__Just(i8*  %217)  
  %219 = bitcast i8* %218 to i8* 
  %220 = bitcast {i8*, i32, i32}* %211 to i8* 
  %221 =  call ccc  i8*  @$Functor$Maybe$map(i8*  %220, i8*  %219)  
  %222 = bitcast i8* %221 to i8* 
  %223 = bitcast i8* %222 to {i64, i8*}* 
  %224 = getelementptr  {i64, i8*}, {i64, i8*}* %223, i32 0, i32 1 
  %225 = getelementptr  {i64, i8*}, {i64, i8*}* %223, i32 0, i32 0 
  %226 = load  i64, i64* %225, align 8 
  %227 = icmp eq i64 0, %226 
  %228 = load  i8*, i8** %224, align 8 
  %229 = bitcast i8* %228 to double* 
  %230 = load  double, double* %229, align 8 
  %231 = and i1 1, 1 
  %232 = and i1 %227, %231 
  br i1 %232, label %branchExpBlock_0, label %exitBlock_0 
branchExpBlock_0:
  %233 = bitcast i8* %222 to {i64, i8*}* 
  %234 = getelementptr  {i64, i8*}, {i64, i8*}* %233, i32 0, i32 1 
  %235 = load  i8*, i8** %234, align 8 
  %236 = bitcast i8* %235 to double* 
  %237 = load  double, double* %236, align 8 
  %238 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %239 = bitcast i8* %238 to double* 
  store  double %237, double* %239, align 8 
  %240 = bitcast double* %239 to i8* 
  %241 =  call ccc  i8*  @__4282899fd67a43a38f13e2be62dba7a9__logNum(i8*  %240)  
  %242 = bitcast i8* %241 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %243 = phi i8* [%242, %branchExpBlock_0], [zeroinitializer, %entry_0] 
  ret void 
}