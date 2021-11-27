; ModuleID = 'main'


 


define external ccc  i8* @__closure__0(i8*  %a_0, i8*  %b_0)    {
entry_0:
  %0 = alloca i8*, align 4 
  %1 = alloca i8*, align 4 
  store  i8* %a_0, i8** %0, align 4 
  store  i8* %b_0, i8** %1, align 4 
  %2 = bitcast i8** %0 to i8* 
  %3 = bitcast i8** %1 to i8* 
  ret i8* %2 
}


define external ccc  i8* @__closure__1(i8*  %a_0, i8*  %b_0)    {
entry_0:
  %0 = alloca i8*, align 4 
  %1 = alloca i8*, align 4 
  store  i8* %a_0, i8** %0, align 4 
  store  i8* %b_0, i8** %1, align 4 
  %2 = ptrtoint i8** %0 to i64 
  %3 = bitcast i64 %2 to double 
  %4 = ptrtoint i8** %1 to i64 
  %5 = bitcast i64 %4 to double 
  %6 = alloca double, align 8 
  store  double %3, double* %6, align 8 
  %7 = bitcast double* %6 to i8* 
  ret i8* %7 
}


define external ccc  {i8* (i8*, i8*)*, {i8*}} @fnStr(i8*  %a_0)    {
entry_0:
  %0 = bitcast i8* %a_0 to i8* 
  %1 = alloca {i8*}, align 8 
  %2 = getelementptr  {i8*}, {i8*}* %1, i32 0, i32 0 
  store  i8* %0, i8** %2, align 8 
  %3 = load  {i8*}, {i8*}* %1, align 8 
  %4 = alloca {i8* (i8*, i8*)*, {i8*}}, align 8 
  %5 = getelementptr  {i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* %4, i32 0, i32 0 
  store  i8* (i8*, i8*)* @__closure__0, i8* (i8*, i8*)** %5, align 8 
  %6 = getelementptr  {i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* %4, i32 0, i32 1 
  store  {i8*} %3, {i8*}* %6, align 8 
  %7 = load  {i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* %4, align 8 
  ret {i8* (i8*, i8*)*, {i8*}} %7 
}


define external ccc  {i8* (i8*, i8*)*, {i8*}} @fnNum(i8*  %a_0)    {
entry_0:
  %0 = ptrtoint i8* %a_0 to i64 
  %1 = bitcast i64 %0 to double 
  %2 = alloca double, align 8 
  store  double %1, double* %2, align 8 
  %3 = bitcast double* %2 to i8* 
  %4 = alloca {i8*}, align 8 
  %5 = getelementptr  {i8*}, {i8*}* %4, i32 0, i32 0 
  store  i8* %3, i8** %5, align 8 
  %6 = load  {i8*}, {i8*}* %4, align 8 
  %7 = alloca {i8* (i8*, i8*)*, {i8*}}, align 8 
  %8 = getelementptr  {i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* %7, i32 0, i32 0 
  store  i8* (i8*, i8*)* @__closure__1, i8* (i8*, i8*)** %8, align 8 
  %9 = getelementptr  {i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* %7, i32 0, i32 1 
  store  {i8*} %6, {i8*}* %9, align 8 
  %10 = load  {i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* %7, align 8 
  ret {i8* (i8*, i8*)*, {i8*}} %10 
}


declare external ccc  i32 @puts(i8*)    


declare external ccc  i8* @malloc(i64)    


declare external ccc  i8* @GC_malloc(i64)    


declare external ccc  i8* @calloc(i32, i32)    


declare external ccc  i1 @__streq__(i8*, i8*)    


define external ccc  void @main()    {
entry_0:
  %0 =  call ccc  i8*  @GC_malloc(i64  2)  
  %1 = getelementptr  i8, i8* %0, i32 0 
  store  i8 51, i8* %1, align 8 
  %2 = getelementptr  i8, i8* %0, i32 1 
  store  i8 0, i8* %2, align 8 
  %3 =  call ccc  {i8* (i8*, i8*)*, {i8*}}  @fnStr(i8*  %0)  
  %4 = alloca {i8* (i8*, i8*)*, {i8*}}, align 4 
  store  {i8* (i8*, i8*)*, {i8*}} %3, {i8* (i8*, i8*)*, {i8*}}* %4, align 4 
  %5 =  call ccc  i8*  @GC_malloc(i64  2)  
  %6 = getelementptr  i8, i8* %5, i32 0 
  store  i8 52, i8* %6, align 8 
  %7 = getelementptr  i8, i8* %5, i32 1 
  store  i8 0, i8* %7, align 8 
  %8 = alloca {i8* (i8*, i8*)*, {i8*}}, align 8 
  store  {i8* (i8*, i8*)*, {i8*}} %3, {i8* (i8*, i8*)*, {i8*}}* %8, align 8 
  %9 = getelementptr  {i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* %8, i32 0, i32 0 
  %10 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %9, align 8 
  %11 = getelementptr  {i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* %8, i32 0, i32 1 
  %12 = load  {i8*}, {i8*}* %11, align 8 
  %13 = getelementptr  {i8*}, {i8*}* %11, i32 0, i32 0 
  %14 = load  i8*, i8** %13, align 8 
  %15 =  call ccc  i8*  %10(i8*  %14, i8*  %5)  
  %16 = alloca double, align 8 
  store  double 1.000000e0, double* %16, align 8 
  %17 = bitcast double* %16 to i8* 
  %18 =  call ccc  {i8* (i8*, i8*)*, {i8*}}  @fnNum(i8*  %17)  
  %19 = alloca double, align 8 
  store  double 5.000000e0, double* %19, align 8 
  %20 = bitcast double* %19 to i8* 
  %21 = alloca {i8* (i8*, i8*)*, {i8*}}, align 8 
  store  {i8* (i8*, i8*)*, {i8*}} %18, {i8* (i8*, i8*)*, {i8*}}* %21, align 8 
  %22 = getelementptr  {i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* %21, i32 0, i32 0 
  %23 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %22, align 8 
  %24 = getelementptr  {i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* %21, i32 0, i32 1 
  %25 = load  {i8*}, {i8*}* %24, align 8 
  %26 = getelementptr  {i8*}, {i8*}* %24, i32 0, i32 0 
  %27 = load  i8*, i8** %26, align 8 
  %28 =  call ccc  i8*  %23(i8*  %27, double  5.000000e0)  
  ret void 
}