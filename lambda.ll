; ModuleID = 'main'


 


define external ccc  i8* @inner(i8*  %env_0, i8*  %x_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %x_0 to double* 
  %3 = load  double, double* %2, align 8 
  %4 = fadd double %3, 1.000000e0 
  %5 =  call ccc  i8*  @inner(double  %4)  
  ret i8* %5 
}


@inc =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @inc$fn, {}* @$EMPTY_ENV }


define external ccc  i8* @inc$fn(i8*  %env_0, i8*  %a_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %a_0 to double* 
  %3 = load  double, double* %2, align 8 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {}* 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {}*}* getelementptr inbounds ({i8* (i8*, i8*)*, {}*}, {i8* (i8*, i8*)*, {}*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {}*}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {i8* (i8*, i8*)*, {}*}* 
  %8 = getelementptr  {i8* (i8*, i8*)*, {}*}, {i8* (i8*, i8*)*, {}*}* %7, i32 0, i32 0 
  store  i8* (i8*, i8*)* @inner, i8* (i8*, i8*)** %8, align 8 
  %9 = getelementptr  {i8* (i8*, i8*)*, {}*}, {i8* (i8*, i8*)*, {}*}* %7, i32 0, i32 1 
  store  {}* %5, {}** %9, align 8 
  %10 =  call ccc  i8*  @inner(double  %3)  
  ret i8* %10 
}


declare external ccc  i8* @malloc(i64)    


declare external ccc  i8* @GC_malloc(i64)    


declare external ccc  i8* @calloc(i32, i32)    


declare external ccc  i1 @__streq__(i8*, i8*)    


declare external ccc  i1 @MadList_hasMinLength(double, {i8*, i8*}*)    


declare external ccc  i1 @MadList_hasLength(double, {i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_singleton(i8*)    


declare external ccc  {i8*, i8*}* @MadList_push(i8*, {i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_concat({i8*, i8*}*, {i8*, i8*}*)    


@$EMPTY_ENV =    global {} {  }


define external ccc  void @main()    {
entry_0:
  ret void 
}