; ModuleID = 'main'


 


define external ccc  i8* @add(i8*  %a_0, i8*  %b_0)    {
  %1 = bitcast i8* %a_0 to double* 
  %2 = load  double, double* %1, align 8 
  %3 = bitcast i8* %b_0 to double* 
  %4 = load  double, double* %3, align 8 
  %5 = fadd double %2, %4 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %7 = bitcast i8* %6 to double* 
  store  double %5, double* %7, align 8 
  %8 = bitcast double* %7 to i8* 
  ret i8* %8 
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


@inc =    global {i8*, i32, i32, i8*}* undef


define external ccc  void @main()    {
entry_0:
  %0 = bitcast i8* (i8*, i8*)* @add to i8* 
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {i8*, i32, i32}* 
  %3 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %2, i32 0, i32 0 
  store  i8* %0, i8** %3, align 8 
  %4 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %2, i32 0, i32 1 
  store  i32 2, i32* %4, align 8 
  %5 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %2, i32 0, i32 2 
  store  i32 2, i32* %5, align 8 
  %6 = bitcast {i8*, i32, i32}* %2 to i8* 
  %7 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %8 = bitcast i8* %7 to double* 
  store  double 1.000000e0, double* %8, align 8 
  %9 = bitcast double* %8 to i8* 
  %10 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %11 = bitcast i8* %10 to {i8*}* 
  %12 = getelementptr  {i8*}, {i8*}* %11, i32 0, i32 0 
  store  i8* %9, i8** %12, align 8 
  %13 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %14 = bitcast i8* %13 to {i8*, i32, i32, i8*}* 
  %15 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %14, i32 0, i32 0 
  store  i8* %6, i8** %15, align 8 
  %16 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %14, i32 0, i32 1 
  store  i32 2, i32* %16, align 8 
  %17 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %14, i32 0, i32 2 
  store  i32 1, i32* %17, align 8 
  %18 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %14, i32 0, i32 3 
  store  i8* %10, i8** %18, align 8 
  store  {i8*, i32, i32, i8*}* %14, {i8*, i32, i32, i8*}** @inc, align 8 
  %19 = bitcast {i8*, i32, i32, i8*}* %14 to i8* 
  %20 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %21 = bitcast i8* %20 to double* 
  store  double 7.000000e0, double* %21, align 8 
  %22 = bitcast double* %21 to i8* 
  %23 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %19, i32  1, i8*  %22)  
  %24 = bitcast i8* %23 to double* 
  %25 = load  double, double* %24, align 8 
  ret void 
}