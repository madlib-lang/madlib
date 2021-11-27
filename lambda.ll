; ModuleID = 'main'


 


define external ccc  i8* @__closure__0(i8*  %a_0, i8*  %b_0)    {
entry_0:
  %0 = alloca i8*, align 4 
  %1 = alloca i8*, align 4 
  store  i8* %a_0, i8** %0, align 4 
  store  i8* %b_0, i8** %1, align 4 
  %2 = load  i8*, i8** %0, align 4 
  ret i8* %2 
}


define external ccc  double @__closure__1(double  %a_0, double  %b_0)    {
entry_0:
  %0 = alloca double, align 4 
  %1 = alloca double, align 4 
  store  double %a_0, double* %0, align 4 
  store  double %b_0, double* %1, align 4 
  %2 = load  double, double* %0, align 4 
  ret double %2 
}


define external ccc  {i8* (i8*, i8*)*, {i8*}} @fn2(i8*  %a_0)    {
entry_0:
  %0 = alloca i8*, align 4 
  store  i8* %a_0, i8** %0, align 4 
  %1 = load  i8*, i8** %0, align 4 
  %2 = alloca {i8*}, align 8 
  %3 = getelementptr  {i8*}, {i8*}* %2, i32 0, i32 0 
  store  i8* %1, i8** %3, align 8 
  %4 = load  {i8*}, {i8*}* %2, align 8 
  %5 = alloca {i8* (i8*, i8*)*, {i8*}}, align 8 
  %6 = getelementptr  {i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* %5, i32 0, i32 0 
  store  i8* (i8*, i8*)* @__closure__0, i8* (i8*, i8*)** %6, align 8 
  %7 = getelementptr  {i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* %5, i32 0, i32 1 
  store  {i8*} %4, {i8*}* %7, align 8 
  %8 = load  {i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* %5, align 8 
  ret {i8* (i8*, i8*)*, {i8*}} %8 
}


define external ccc  {double (double, double)*, {double}} @fn(double  %a_0)    {
entry_0:
  %0 = alloca double, align 4 
  store  double %a_0, double* %0, align 4 
  %1 = load  double, double* %0, align 4 
  %2 = alloca {double}, align 8 
  %3 = getelementptr  {double}, {double}* %2, i32 0, i32 0 
  store  double %1, double* %3, align 8 
  %4 = load  {double}, {double}* %2, align 8 
  %5 = alloca {double (double, double)*, {double}}, align 8 
  %6 = getelementptr  {double (double, double)*, {double}}, {double (double, double)*, {double}}* %5, i32 0, i32 0 
  store  double (double, double)* @__closure__1, double (double, double)** %6, align 8 
  %7 = getelementptr  {double (double, double)*, {double}}, {double (double, double)*, {double}}* %5, i32 0, i32 1 
  store  {double} %4, {double}* %7, align 8 
  %8 = load  {double (double, double)*, {double}}, {double (double, double)*, {double}}* %5, align 8 
  ret {double (double, double)*, {double}} %8 
}


declare external ccc  i32 @puts(i8*)    


declare external ccc  i8* @malloc(i64)    


declare external ccc  i8* @calloc(i32, i32)    


declare external ccc  i1 @__streq__(i8*, i8*)    


define external ccc  void @main()    {
entry_0:
  %0 =  call ccc  i8*  @malloc(i64  2)  
  %1 = getelementptr  i8, i8* %0, i32 0 
  store  i8 49, i8* %1, align 8 
  %2 = getelementptr  i8, i8* %0, i32 1 
  store  i8 0, i8* %2, align 8 
  %3 =  call ccc  {i8* (i8*, i8*)*, {i8*}}  @fn2(i8*  %0)  
  %4 =  call ccc  i8*  @malloc(i64  2)  
  %5 = getelementptr  i8, i8* %4, i32 0 
  store  i8 50, i8* %5, align 8 
  %6 = getelementptr  i8, i8* %4, i32 1 
  store  i8 0, i8* %6, align 8 
  %7 = alloca {i8* (i8*, i8*)*, {i8*}}, align 8 
  store  {i8* (i8*, i8*)*, {i8*}} %3, {i8* (i8*, i8*)*, {i8*}}* %7, align 8 
  %8 = getelementptr  {i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* %7, i32 0, i32 0 
  %9 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %8, align 8 
  %10 = getelementptr  {i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* %7, i32 0, i32 1 
  %11 =  call ccc  i8*  %9(i8*  %4)  
  ret void 
}