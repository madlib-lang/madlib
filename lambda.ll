; ModuleID = 'main'


 


define external ccc  i8* @$closureFn$0(i8*  %env_0, i8*  %b_0)    {
; <label>:0:
  %1 = bitcast i8* %env_0 to {i8*}* 
  %2 = getelementptr  {i8*}, {i8*}* %1, i32 0, i32 0 
  %3 = load  i8*, i8** %2, align 8 
  %4 = bitcast i8* %3 to double* 
  %5 = load  double, double* %4, align 8 
  %6 = bitcast i8* %b_0 to double* 
  %7 = load  double, double* %6, align 8 
  %8 = fcmp oeq double %5, 0.000000e0 
  %9 = icmp eq i1 %8, 1 
  br i1 %9, label %truthyBlock_0, label %falsyBlock_0 
truthyBlock_0:
  %10 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %11 = bitcast i8* %10 to double* 
  store  double %7, double* %11, align 8 
  %12 = bitcast double* %11 to i8* 
  br label %condBlock_0 
falsyBlock_0:
  %13 = fadd double %7, 1.000000e0 
  %14 = fsub double %5, 1.000000e0 
  %15 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %16 = bitcast i8* %15 to double* 
  store  double %13, double* %16, align 8 
  %17 = bitcast double* %16 to i8* 
  %18 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %19 = bitcast i8* %18 to double* 
  store  double %14, double* %19, align 8 
  %20 = bitcast double* %19 to i8* 
  %21 =  call ccc  i8*  @incTwiceInner$uncurried(i8*  %17, i8*  %20)  
  %22 = bitcast i8* %21 to double* 
  %23 = load  double, double* %22, align 8 
  %24 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %25 = bitcast i8* %24 to double* 
  store  double %23, double* %25, align 8 
  %26 = bitcast double* %25 to i8* 
  br label %condBlock_0 
condBlock_0:
  %27 = phi i8* [%12, %truthyBlock_0], [%26, %falsyBlock_0] 
  %28 = bitcast i8* %27 to double* 
  %29 = load  double, double* %28, align 8 
  %30 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %31 = bitcast i8* %30 to double* 
  store  double %29, double* %31, align 8 
  %32 = bitcast double* %31 to i8* 
  ret i8* %32 
}


declare external ccc  i8* @puts(i8*)    


define external ccc  i8* @log$0(i8*  %env_0, i8*  %arg_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %arg_0 to i8* 
  %3 =  call ccc  i8*  @puts(i8*  %2)  
  ret i8* %3 
}


@log =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @log$0, {}* zeroinitializer }


@incTwiceInner =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @incTwiceInner$fn, {}* @$EMPTY_ENV }


define external ccc  i8* @incTwiceInner$fn(i8*  %env_0, i8*  %a_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %a_0 to double* 
  %3 = load  double, double* %2, align 8 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %5 = bitcast i8* %4 to double* 
  store  double %3, double* %5, align 8 
  %6 = bitcast double* %5 to i8* 
  %7 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %8 = bitcast i8* %7 to {i8*}* 
  %9 = getelementptr  {i8*}, {i8*}* %8, i32 0, i32 0 
  store  i8* %6, i8** %9, align 8 
  %10 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {i8*}*}* getelementptr inbounds ({i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {i8*}*}*), i32 1) to i64))  
  %11 = bitcast i8* %10 to {i8* (i8*, i8*)*, {i8*}*}* 
  %12 = getelementptr  {i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* %11, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$closureFn$0, i8* (i8*, i8*)** %12, align 8 
  %13 = getelementptr  {i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* %11, i32 0, i32 1 
  store  {i8*}* %8, {i8*}** %13, align 8 
  %14 = bitcast {i8* (i8*, i8*)*, {i8*}*}* %11 to i8* 
  ret i8* %14 
}


define external ccc  i8* @incTwiceInner$uncurried(i8*  %a_0, i8*  %b_0)    {
; <label>:0:
  %1 = bitcast i8* %a_0 to double* 
  %2 = load  double, double* %1, align 8 
  %3 = bitcast i8* %b_0 to double* 
  %4 = load  double, double* %3, align 8 
  %5 = fcmp oeq double %2, 0.000000e0 
  %6 = icmp eq i1 %5, 1 
  br i1 %6, label %truthyBlock_0, label %falsyBlock_0 
truthyBlock_0:
  %7 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %8 = bitcast i8* %7 to double* 
  store  double %4, double* %8, align 8 
  %9 = bitcast double* %8 to i8* 
  br label %condBlock_0 
falsyBlock_0:
  %10 = fsub double %2, 1.000000e0 
  %11 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %12 = bitcast i8* %11 to double* 
  store  double %10, double* %12, align 8 
  %13 = bitcast double* %12 to i8* 
  %14 = bitcast {i8* (i8*, i8*)*, {}*}* @incTwiceInner to {i8* (i8*, i8*)*, i8*}* 
  %15 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %14, i32 0, i32 0 
  %16 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %15, align 8 
  %17 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %14, i32 0, i32 1 
  %18 = load  i8*, i8** %17, align 8 
  %19 =  call ccc  i8*  %16(i8*  %18, i8*  %13)  
  %20 = bitcast i8* %19 to double* 
  %21 = load  double, double* %20, align 8 
  %22 = fadd double %4, 1.000000e0 
  %23 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %24 = bitcast i8* %23 to double* 
  store  double %22, double* %24, align 8 
  %25 = bitcast double* %24 to i8* 
  %26 = bitcast double %21 to {i8* (i8*, i8*)*, i8*}* 
  %27 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %26, i32 0, i32 0 
  %28 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %27, align 8 
  %29 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %26, i32 0, i32 1 
  %30 = load  i8*, i8** %29, align 8 
  %31 =  call ccc  i8*  %28(i8*  %30, i8*  %25)  
  %32 = bitcast i8* %31 to i8* 
  br label %condBlock_0 
condBlock_0:
  %33 = phi i8* [%9, %truthyBlock_0], [%32, %falsyBlock_0] 
  %34 = bitcast i8* %33 to double* 
  %35 = load  double, double* %34, align 8 
  %36 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %37 = bitcast i8* %36 to double* 
  store  double %35, double* %37, align 8 
  %38 = bitcast double* %37 to i8* 
  ret i8* %38 
}


@incTwice =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @incTwice$fn, {}* @$EMPTY_ENV }


define external ccc  i8* @incTwice$fn(i8*  %env_0, i8*  %a_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %a_0 to double* 
  %3 = load  double, double* %2, align 8 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %5 = bitcast i8* %4 to double* 
  store  double %3, double* %5, align 8 
  %6 = bitcast double* %5 to i8* 
  %7 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %8 = bitcast i8* %7 to double* 
  store  double 2.000000e0, double* %8, align 8 
  %9 = bitcast double* %8 to i8* 
  %10 =  call ccc  i8*  @incTwiceInner$uncurried(i8*  %6, i8*  %9)  
  %11 = bitcast i8* %10 to double* 
  %12 = load  double, double* %11, align 8 
  %13 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %14 = bitcast i8* %13 to double* 
  store  double %12, double* %14, align 8 
  %15 = bitcast double* %14 to i8* 
  ret i8* %15 
}


define external ccc  i8* @incTwice$uncurried(i8*  %a_0)    {
  %1 = bitcast i8* %a_0 to double* 
  %2 = load  double, double* %1, align 8 
  %3 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %4 = bitcast i8* %3 to double* 
  store  double %2, double* %4, align 8 
  %5 = bitcast double* %4 to i8* 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %7 = bitcast i8* %6 to double* 
  store  double 2.000000e0, double* %7, align 8 
  %8 = bitcast double* %7 to i8* 
  %9 =  call ccc  i8*  @incTwiceInner$uncurried(i8*  %5, i8*  %8)  
  %10 = bitcast i8* %9 to double* 
  %11 = load  double, double* %10, align 8 
  %12 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %13 = bitcast i8* %12 to double* 
  store  double %11, double* %13, align 8 
  %14 = bitcast double* %13 to i8* 
  ret i8* %14 
}


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


define external ccc  void @main()    {
entry_0:
  %0 =  call ccc  i8*  @GC_malloc(i64  6)  
  %1 = getelementptr  i8, i8* %0, i32 0 
  store  i8 115, i8* %1, align 8 
  %2 = getelementptr  i8, i8* %0, i32 1 
  store  i8 116, i8* %2, align 8 
  %3 = getelementptr  i8, i8* %0, i32 2 
  store  i8 97, i8* %3, align 8 
  %4 = getelementptr  i8, i8* %0, i32 3 
  store  i8 114, i8* %4, align 8 
  %5 = getelementptr  i8, i8* %0, i32 4 
  store  i8 116, i8* %5, align 8 
  %6 = getelementptr  i8, i8* %0, i32 5 
  store  i8 0, i8* %6, align 8 
  %7 = bitcast {i8* (i8*, i8*)*, {}*}* @log to {i8* (i8*, i8*)*, i8*}* 
  %8 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %7, i32 0, i32 0 
  %9 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %8, align 8 
  %10 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %7, i32 0, i32 1 
  %11 = load  i8*, i8** %10, align 8 
  %12 =  call ccc  i8*  %9(i8*  %11, i8*  %0)  
  %13 = bitcast i8* %12 to i8* 
  ret void 
}