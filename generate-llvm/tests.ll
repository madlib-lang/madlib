; ModuleID = 'main'


 


define external ccc  i8* @$Functor$List$map(i8*  %f_0, i8*  %m_0)    {
  %1 = bitcast i8* %f_0 to {i8*, i32, i32, i8*}* 
  %2 = bitcast i8* %m_0 to {i8*, i8*}* 
  %3 = bitcast i8* (i8*, i8*, i8*)* @helper$lifted$5 to i8* 
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


define external ccc  i8* @$Applicative$List$ap(i8*  %mf_0, i8*  %ma_0)    {
  %1 = bitcast i8* %mf_0 to {i8*, i8*}* 
  %2 = bitcast i8* %ma_0 to {i8*, i8*}* 
  %3 = bitcast i8* (i8*, i8*)* @anonymous$lifted$7 to i8* 
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
  %6 = bitcast i8* %5 to {i8*, i8*}* 
  %7 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 1 
  %8 = load  i8*, i8** %7, align 8 
  %9 = bitcast i8* %8 to {i8*, i8*}* 
  %10 = and i1 1, 1 
  %11 = and i1 %3, %10 
  br i1 %11, label %branchExpBlock_0, label %nextBlock_0 
branchExpBlock_0:
  %12 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 0 
  %13 = load  i8*, i8** %12, align 8 
  %14 = bitcast i8* %13 to {i8*, i8*}* 
  %15 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 1 
  %16 = load  i8*, i8** %15, align 8 
  %17 = bitcast i8* %16 to {i8*, i8*}* 
  %18 = bitcast {i8*, i8*}* %14 to i8* 
  %19 = bitcast {i8*, i8*}* %1 to i8* 
  %20 =  call ccc  i8*  @push(i8*  %18, i8*  %19)  
  %21 = bitcast i8* %20 to {i8*, i8*}* 
  %22 = bitcast {i8*, i8*}* %21 to i8* 
  %23 = bitcast {i8*, i8*}* %17 to i8* 
  %24 =  call ccc  i8*  @helper$lifted$0(i8*  %22, i8*  %23)  
  %25 = bitcast i8* %24 to {i8*, i8*}* 
  %26 = bitcast {i8*, i8*}* %25 to i8* 
  br label %exitBlock_0 
nextBlock_0:
  %27 =  call ccc  i1  @MadList_hasLength(double  0.000000e0, {i8*, i8*}*  %2)  
  %28 = and i1 %27, 1 
  br i1 %28, label %branchExpBlock_1, label %exitBlock_0 
branchExpBlock_1:
  %29 = bitcast {i8*, i8*}* %1 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %30 = phi i8* [%26, %branchExpBlock_0], [%29, %branchExpBlock_1], [zeroinitializer, %nextBlock_0] 
  ret i8* %30 
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
  %2 = bitcast i8* (i8*)* @logNum to i8* 
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
  %28 = bitcast i8* %27 to {i8*, i32, i32, i8*}* 
  %29 = bitcast {i8*, i32, i32}* %4 to i8* 
  %30 = bitcast {i8*, i32, i32, i8*}* %28 to i8* 
  %31 =  call ccc  i8*  @$Functor$List$map(i8*  %29, i8*  %30)  
  %32 = bitcast i8* %31 to {i8*, i32, i32, i8*}* 
  %33 = bitcast {i8*, i32, i32, i8*}* %32 to i8* 
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


define external ccc  i8* @helper$lifted$5(i8*  %f_0, i8*  %acc_0, i8*  %n_0)    {
; <label>:0:
  %1 = bitcast i8* %f_0 to {i8*, i32, i32, i8*}* 
  %2 = bitcast i8* %acc_0 to {i8*, i8*}* 
  %3 = bitcast i8* %n_0 to {i8*, i8*}* 
  %4 =  call ccc  i1  @MadList_hasMinLength(double  1.000000e0, {i8*, i8*}*  %3)  
  %5 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 0 
  %6 = load  i8*, i8** %5, align 8 
  %7 = bitcast i8* %6 to {i8*, i8*}* 
  %8 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 1 
  %9 = load  i8*, i8** %8, align 8 
  %10 = bitcast i8* %9 to {i8*, i8*}* 
  %11 = and i1 1, 1 
  %12 = and i1 %4, %11 
  br i1 %12, label %branchExpBlock_0, label %nextBlock_0 
branchExpBlock_0:
  %13 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 0 
  %14 = load  i8*, i8** %13, align 8 
  %15 = bitcast i8* %14 to {i8*, i8*}* 
  %16 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 1 
  %17 = load  i8*, i8** %16, align 8 
  %18 = bitcast i8* %17 to {i8*, i8*}* 
  %19 = bitcast {i8*, i32, i32, i8*}* %1 to i8* 
  %20 = bitcast {i8*, i8*}* %15 to i8* 
  %21 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %19, i32  1, i8*  %20)  
  %22 = bitcast i8* %21 to i8* 
  %23 = bitcast {i8*, i8*}* %2 to i8* 
  %24 =  call ccc  i8*  @push(i8*  %22, i8*  %23)  
  %25 = bitcast i8* %24 to {i8*, i8*}* 
  %26 = bitcast {i8*, i32, i32, i8*}* %1 to i8* 
  %27 = bitcast {i8*, i8*}* %25 to i8* 
  %28 = bitcast {i8*, i8*}* %18 to i8* 
  %29 =  call ccc  i8*  @helper$lifted$5(i8*  %26, i8*  %27, i8*  %28)  
  %30 = bitcast i8* %29 to {i8*, i8*}* 
  %31 = bitcast {i8*, i8*}* %30 to i8* 
  br label %exitBlock_0 
nextBlock_0:
  %32 =  call ccc  i1  @MadList_hasLength(double  0.000000e0, {i8*, i8*}*  %3)  
  %33 = and i1 %32, 1 
  br i1 %33, label %branchExpBlock_1, label %exitBlock_0 
branchExpBlock_1:
  %34 = bitcast {i8*, i8*}* %2 to i8* 
  %35 =  call ccc  i8*  @reverse(i8*  %34)  
  %36 = bitcast i8* %35 to {i8*, i8*}* 
  %37 = bitcast {i8*, i8*}* %36 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %38 = phi i8* [%31, %branchExpBlock_0], [%37, %branchExpBlock_1], [zeroinitializer, %nextBlock_0] 
  ret i8* %38 
}


define external ccc  i8* @anonymous$lifted$6(i8*  %ma_0, i8*  %f_0)    {
  %1 = bitcast i8* %ma_0 to {i8*, i8*}* 
  %2 = bitcast i8* %f_0 to {i8*, i32, i32, i8*}* 
  %3 = bitcast {i8*, i32, i32, i8*}* %2 to i8* 
  %4 = bitcast {i8*, i8*}* %1 to i8* 
  %5 =  call ccc  i8*  @$Functor$List$map(i8*  %3, i8*  %4)  
  %6 = bitcast i8* %5 to {i8*, i32, i32, i8*}* 
  %7 = bitcast {i8*, i32, i32, i8*}* %6 to i8* 
  ret i8* %7 
}


define external ccc  i8* @anonymous$lifted$7(i8*  %ma_0, i8*  %_P__0)    {
  %1 = bitcast i8* %ma_0 to {i8*, i8*}* 
  %2 = bitcast i8* %_P__0 to {i8*, i8*}* 
  %3 = bitcast i8* (i8*, i8*)* @anonymous$lifted$6 to i8* 
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
  %17 = bitcast i8* %16 to {i8*, i32, i32, i8*}* 
  %18 = bitcast {i8*, i32, i32, i8*}* %17 to i8* 
  %19 =  call ccc  i8*  @flatten(i8*  %18)  
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


define external ccc  i8* @logNum(i8*  %_P__0)    {
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


define external ccc  i8* @reverse(i8*  %xs_0)    {
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


define external ccc  i8* @flatten(i8*  %xs_0)    {
; <label>:0:
  %1 = bitcast i8* %xs_0 to {i8*, i8*}* 
  %2 =  call ccc  i1  @MadList_hasMinLength(double  1.000000e0, {i8*, i8*}*  %1)  
  %3 = getelementptr  {i8*, i8*}, {i8*, i8*}* %1, i32 0, i32 0 
  %4 = load  i8*, i8** %3, align 8 
  %5 = bitcast i8* %4 to {i8*, i8*}* 
  %6 = getelementptr  {i8*, i8*}, {i8*, i8*}* %1, i32 0, i32 1 
  %7 = load  i8*, i8** %6, align 8 
  %8 = bitcast i8* %7 to {i8*, i8*}* 
  %9 = and i1 1, 1 
  %10 = and i1 %2, %9 
  br i1 %10, label %branchExpBlock_0, label %nextBlock_0 
branchExpBlock_0:
  %11 = getelementptr  {i8*, i8*}, {i8*, i8*}* %1, i32 0, i32 0 
  %12 = load  i8*, i8** %11, align 8 
  %13 = bitcast i8* %12 to {i8*, i8*}* 
  %14 = getelementptr  {i8*, i8*}, {i8*, i8*}* %1, i32 0, i32 1 
  %15 = load  i8*, i8** %14, align 8 
  %16 = bitcast i8* %15 to {i8*, i8*}* 
  %17 = bitcast {i8*, i8*}* %16 to i8* 
  %18 =  call ccc  i8*  @flatten(i8*  %17)  
  %19 = bitcast i8* %18 to {i8*, i8*}* 
  %20 =  call ccc  {i8*, i8*}*  @MadList_concat({i8*, i8*}*  %13, {i8*, i8*}*  %19)  
  %21 = bitcast {i8*, i8*}* %20 to {i8*, i8*}* 
  %22 = bitcast {i8*, i8*}* %21 to i8* 
  br label %exitBlock_0 
nextBlock_0:
  %23 =  call ccc  i1  @MadList_hasLength(double  0.000000e0, {i8*, i8*}*  %1)  
  %24 = and i1 %23, 1 
  br i1 %24, label %branchExpBlock_1, label %exitBlock_0 
branchExpBlock_1:
  %25 = bitcast {i8*, i8*}* zeroinitializer to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %26 = phi i8* [%22, %branchExpBlock_0], [%25, %branchExpBlock_1], [zeroinitializer, %nextBlock_0] 
  ret i8* %26 
}


declare external ccc  i8* @__applyPAP__(i8*, i32, ...)    


declare external ccc  i8* @malloc(i64)    


declare external ccc  i8* @GC_malloc(i64)    


declare external ccc  i8* @calloc(i32, i32)    


declare external ccc  i1 @__streq__(i8*, i8*)    


declare external ccc  i1 @MadList_hasMinLength(double, {i8*, i8*}*)    


declare external ccc  i1 @MadList_hasLength(double, {i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_singleton(i8*)    


declare external ccc  {i8*, i8*}* @__MadList_push__(i8*, {i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_concat({i8*, i8*}*, {i8*, i8*}*)    


@$EMPTY_ENV =    global {} {  }


define external ccc  i8* @whateva(i8*  %$Functor$b183_0)    {
  %1 = bitcast i8* %$Functor$b183_0 to i8* 
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
  %11 = bitcast i8* (i8*)* @anonymous$lifted$4 to i8* 
  %12 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %13 = bitcast i8* %12 to {i8*, i32, i32}* 
  %14 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %13, i32 0, i32 0 
  store  i8* %11, i8** %14, align 8 
  %15 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %13, i32 0, i32 1 
  store  i32 1, i32* %15, align 8 
  %16 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %13, i32 0, i32 2 
  store  i32 1, i32* %16, align 8 
  %17 = bitcast {i8*, i32, i32}* %13 to i8* 
  %18 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %10, i32  1, i8*  %17)  
  %19 = bitcast i8* %18 to {i8*, i32, i32, i8*}* 
  %20 = bitcast {i8*, i32, i32, i8*}* %19 to i8* 
  ret i8* %20 
}


@fAdd =    global i8* (i8*)* undef


define external ccc  void @main()    {
entry_0:
  %0 = bitcast i8* (i8*)* @logNum to i8* 
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
  %22 = bitcast i8* %21 to {i8*, i32, i32, i8*}* 
  %23 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %24 = bitcast i8* %23 to double* 
  store  double 9.000000e0, double* %24, align 8 
  %25 = bitcast double* %24 to i8* 
  %26 =  call ccc  i8*  @$Applicative$List$pure(i8*  %25)  
  %27 = bitcast i8* %26 to {i8*, i32, i32, i8*}* 
  %28 = bitcast {i8*, i32, i32, i8*}* %27 to i8* 
  %29 =  call ccc  i8*  @anonymous$lifted$3(i8*  %28)  
  %30 = bitcast i8* %29 to {i8*, i8*}* 
  store  i8* (i8*)* @whateva, i8* (i8*)** @fAdd, align 8 
  %31 = bitcast {i8* (i8*, i8*)*}* @$Functor$List to i8* 
  %32 =  call ccc  i8*  @whateva(i8*  %31)  
  %33 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %34 = bitcast i8* %33 to {i8*, i32, i32}* 
  %35 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %34, i32 0, i32 0 
  store  i8* %32, i8** %35, align 8 
  %36 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %34, i32 0, i32 1 
  store  i32 1, i32* %36, align 8 
  %37 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %34, i32 0, i32 2 
  store  i32 1, i32* %37, align 8 
  %38 = bitcast {i8*, i32, i32}* %34 to i8* 
  %39 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %40 = bitcast i8* %39 to double* 
  store  double 1.000000e0, double* %40, align 8 
  %41 = bitcast double* %40 to i8* 
  %42 =  call ccc  {i8*, i8*}*  @MadList_singleton(i8*  %41)  
  %43 = bitcast {i8*, i8*}* %42 to {i8*, i8*}* 
  %44 = bitcast {i8*, i8*}* %43 to i8* 
  %45 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %38, i32  1, i8*  %44)  
  %46 = bitcast i8* %45 to {i8*, i8*}* 
  ret void 
}