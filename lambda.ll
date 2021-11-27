; ModuleID = 'main'


 


define external ccc  double @__closure__0(double  %a_0, double  %b_0)    {
entry_0:
  %0 = alloca double, align 4 
  %1 = alloca double, align 4 
  store  double %a_0, double* %0, align 4 
  store  double %b_0, double* %1, align 4 
  %2 = load  double, double* %0, align 4 
  ret double %2 
}


define external ccc  {double (double, double)*, {double}*}* @fn(double  %a_0)    {
entry_0:
  %0 = alloca double, align 4 
  store  double %a_0, double* %0, align 4 
  %1 = load  double, double* %0, align 4 
  %2 = alloca {double}, align 8 
  %3 = getelementptr  {double}, {double}* %2, i32 0, i32 0 
  store  double %1, double* %3, align 8 
  %4 = alloca {double (double, double)*, {double}*}, align 8 
  %5 = getelementptr  {double (double, double)*, {double}*}, {double (double, double)*, {double}*}* %4, i32 0, i32 0 
  store  double (double, double)* @__closure__0, double (double, double)** %5, align 8 
  %6 = getelementptr  {double (double, double)*, {double}*}, {double (double, double)*, {double}*}* %4, i32 0, i32 1 
  store  {double}* %2, {double}** %6, align 8 
  ret {double (double, double)*, {double}*}* %4 
}


declare external ccc  i32 @puts(i8*)    


declare external ccc  i8* @malloc(i64)    


declare external ccc  i1 @__streq__(i8*, i8*)    


define external ccc  void @main()    {
entry_0:
  %0 =  call ccc  {double (double, double)*, {double}*}*  @fn(double  1.000000e0)  
  %1 = getelementptr  {double (double, double)*, {double}*}, {double (double, double)*, {double}*}* %0, i32 0, i32 0
  %2 = load double (double, double)*, double (double, double)** %1, align 8
  %3 =  call ccc  double  %2(double  2.000000e0, double  2.000000e0)  
  ret void 
}