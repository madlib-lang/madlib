; ModuleID = 'main'


 


declare external ccc  i8* @puts(i8*)    


define external ccc  i8* @log(i8* )    {
  %2 = bitcast i8* %0 to i8* 
  %3 =  call ccc  i8*  @puts(i8*  %2)  
  ret i8* %3 
}


declare external ccc  i8* @__doubleToStr__(i8*)    


define external ccc  i8* @showNumber(i8* )    {
  %2 = bitcast i8* %0 to double* 
  %3 = load  double, double* %2, align 8 
  %4 =  call ccc  i8*  @__doubleToStr__(double  %3)  
  ret i8* %4 
}


define external ccc  i8* @add3(i8*  %a_0, i8*  %b_0, i8*  %c_0)    {
  %1 = bitcast i8* %a_0 to double* 
  %2 = load  double, double* %1, align 8 
  %3 = bitcast i8* %b_0 to double* 
  %4 = load  double, double* %3, align 8 
  %5 = bitcast i8* %c_0 to double* 
  %6 = load  double, double* %5, align 8 
  %7 = fadd double %2, %4 
  %8 = fadd double %7, %6 
  %9 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %10 = bitcast i8* %9 to double* 
  store  double %8, double* %10, align 8 
  %11 = bitcast double* %10 to i8* 
  ret i8* %11 
}


declare external ccc  i8* @malloc(i64)    


declare external ccc  i8* @GC_malloc(i64)    


declare external ccc  i8* @calloc(i32, i32)    


declare external ccc  i1 @__streq__(i8*, i8*)    


declare external ccc  i8* @__applyPAP__(i8*, i32, i8*)    


declare external ccc  i1 @MadList_hasMinLength(double, {i8*, i8*}*)    


declare external ccc  i1 @MadList_hasLength(double, {i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_singleton(i8*)    


declare external ccc  {i8*, i8*}* @__MadList_push__(i8*, {i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_concat({i8*, i8*}*, {i8*, i8*}*)    


@$EMPTY_ENV =    global {} {  }


@add2 =    global {i8*, i32, i32, i8*}* undef


@add1 =    global i8* undef


@x =    global double undef


define external ccc  void @main()    {
entry_0:
  %0 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %1 = bitcast i8* %0 to double* 
  store  double 3.000000e0, double* %1, align 8 
  %2 = bitcast double* %1 to i8* 
  %3 =  call ccc  i8*  @showNumber(i8*  %2)  
  %4 = bitcast i8* %3 to i8* 
  %5 =  call ccc  i8*  @log(i8*  %4)  
  %6 = bitcast i8* %5 to i8* 
  %7 = bitcast i8* (i8*, i8*, i8*)* @add3 to i8* 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %9 = bitcast i8* %8 to double* 
  store  double 1.000000e0, double* %9, align 8 
  %10 = bitcast double* %9 to i8* 
  %11 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %12 = bitcast i8* %11 to {i8*}* 
  %13 = getelementptr  {i8*}, {i8*}* %12, i32 0, i32 0 
  store  i8* %10, i8** %13, align 8 
  %14 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %15 = bitcast i8* %14 to {i8*, i32, i32, i8*}* 
  %16 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %15, i32 0, i32 0 
  store  i8* %7, i8** %16, align 8 
  %17 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %15, i32 0, i32 1 
  store  i32 3, i32* %17, align 8 
  %18 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %15, i32 0, i32 2 
  store  i32 2, i32* %18, align 8 
  %19 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %15, i32 0, i32 3 
  store  i8* %11, i8** %19, align 8 
  store  {i8*, i32, i32, i8*}* %15, {i8*, i32, i32, i8*}** @add2, align 8 
  %20 = bitcast {i8*, i32, i32, i8*}* %15 to i8* 
  %21 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %22 = bitcast i8* %21 to double* 
  store  double 1.000000e0, double* %22, align 8 
  %23 = bitcast double* %22 to i8* 
  %24 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %25 = bitcast i8* %24 to {i8*}* 
  %26 = getelementptr  {i8*}, {i8*}* %25, i32 0, i32 0 
  store  i8* %23, i8** %26, align 8 
  %27 =  call ccc  i8*  @__applyPAP__(i8*  %20, i32  1, i8*  %24)  
  store  i8* %27, i8** @add1, align 8 
  %28 = bitcast i8* %27 to i8* 
  %29 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %30 = bitcast i8* %29 to double* 
  store  double 7.000000e0, double* %30, align 8 
  %31 = bitcast double* %30 to i8* 
  %32 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %33 = bitcast i8* %32 to {i8*}* 
  %34 = getelementptr  {i8*}, {i8*}* %33, i32 0, i32 0 
  store  i8* %31, i8** %34, align 8 
  %35 =  call ccc  i8*  @__applyPAP__(i8*  %28, i32  1, i8*  %32)  
  %36 = bitcast i8* %35 to double* 
  %37 = load  double, double* %36, align 8 
  store  double %37, double* @x, align 8 
  %38 = fadd double %37, 1.000000e0 
  ret void 
}