; ModuleID = 'main'


 


define external ccc  i8* @__closure__0(i8*  %f_0, i8*  %v_0)    {
entry_0:
  %0 = bitcast i8* %f_0 to {i8* (i8*)*, {}}* 
  %1 = load  {i8* (i8*)*, {}}, {i8* (i8*)*, {}}* %0, align 8 
  %2 = bitcast i8* %v_0 to i8* 
  %3 = alloca {i8* (i8*)*, {}}, align 8 
  store  {i8* (i8*)*, {}} %1, {i8* (i8*)*, {}}* %3, align 8 
  %4 = getelementptr  {i8* (i8*)*, {}}, {i8* (i8*)*, {}}* %3, i32 0, i32 0 
  %5 = load  i8* (i8*)*, i8* (i8*)** %4, align 8 
  %6 = getelementptr  {i8* (i8*)*, {}}, {i8* (i8*)*, {}}* %3, i32 0, i32 1 
  %7 = load  {}, {}* %6, align 8 
  %8 =  call ccc  i8*  %5(i8*  %2)  
  ret i8* %8 
}


define external ccc  i8* @__closure__1(i8*  %a_0)    {
entry_0:
  %0 = ptrtoint i8* %a_0 to i64 
  %1 = bitcast i64 %0 to double 
  %2 =  call ccc  i8*  @GC_malloc(i64  2)  
  %3 = getelementptr  i8, i8* %2, i32 0 
  store  i8 97, i8* %3, align 8 
  %4 = getelementptr  i8, i8* %2, i32 1 
  store  i8 0, i8* %4, align 8 
  ret i8* %2 
}


define external ccc  {i8* (i8*, i8*)*, {i8*}} @mapVal_closure(i8*  %f_0)    {
entry_0:
  %0 = bitcast i8* %f_0 to {i8* (i8*)*, {}}* 
  %1 = load  {i8* (i8*)*, {}}, {i8* (i8*)*, {}}* %0, align 8 
  %2 = alloca {{i8* (i8*)*, {}}}, align 8 
  %3 = getelementptr  {{i8* (i8*)*, {}}}, {{i8* (i8*)*, {}}}* %2, i32 0, i32 0 
  store  {i8* (i8*)*, {}} %1, {i8* (i8*)*, {}}* %3, align 8 
  %4 = load  {{i8* (i8*)*, {}}}, {{i8* (i8*)*, {}}}* %2, align 8 
  %5 = alloca {i8* (i8*, i8*)*, {{i8* (i8*)*, {}}}}, align 8 
  %6 = getelementptr  {i8* (i8*, i8*)*, {{i8* (i8*)*, {}}}}, {i8* (i8*, i8*)*, {{i8* (i8*)*, {}}}}* %5, i32 0, i32 0 
  store  i8* (i8*, i8*)* @__closure__0, i8* (i8*, i8*)** %6, align 8 
  %7 = getelementptr  {i8* (i8*, i8*)*, {{i8* (i8*)*, {}}}}, {i8* (i8*, i8*)*, {{i8* (i8*)*, {}}}}* %5, i32 0, i32 1 
  store  {{i8* (i8*)*, {}}} %4, {{i8* (i8*)*, {}}}* %7, align 8 
  %8 = load  {i8* (i8*, i8*)*, {{i8* (i8*)*, {}}}}, {i8* (i8*, i8*)*, {{i8* (i8*)*, {}}}}* %5, align 8 
  ret {i8* (i8*, i8*)*, {{i8* (i8*)*, {}}}} %8 
}


@mapVal =    global {{i8* (i8*, i8*)*, {i8*}} (i8*)*, {}} { {i8* (i8*, i8*)*, {i8*}} (i8*)* @mapVal_closure, {} {  } }


define external ccc  i8* @m_closure(i8*  %a_0)    {
entry_0:
  %0 = bitcast i8* %a_0 to i8* 
  %1 =  call ccc  i8*  @GC_malloc(i64  2)  
  %2 = getelementptr  i8, i8* %1, i32 0 
  store  i8 97, i8* %2, align 8 
  %3 = getelementptr  i8, i8* %1, i32 1 
  store  i8 0, i8* %3, align 8 
  ret i8* %1 
}


@m =    global {i8* (i8*)*, {}} { i8* (i8*)* @m_closure, {} {  } }


declare external ccc  i32 @puts(i8*)    


declare external ccc  i8* @malloc(i64)    


declare external ccc  i8* @GC_malloc(i64)    


declare external ccc  i8* @calloc(i32, i32)    


declare external ccc  i1 @__streq__(i8*, i8*)    


define external ccc  void @main()    {
entry_0:
  %0 = load  {{i8* (i8*, i8*)*, {i8*}} (i8*)*, {}}, {{i8* (i8*, i8*)*, {i8*}} (i8*)*, {}}* @mapVal, align 8 
  %1 = alloca {}, align 8 
  %2 = load  {}, {}* %1, align 8 
  %3 = alloca {i8* (i8*)*, {}}, align 8 
  %4 = getelementptr  {i8* (i8*)*, {}}, {i8* (i8*)*, {}}* %3, i32 0, i32 0 
  store  i8* (i8*)* @__closure__1, i8* (i8*)** %4, align 8 
  %5 = getelementptr  {i8* (i8*)*, {}}, {i8* (i8*)*, {}}* %3, i32 0, i32 1 
  store  {} %2, {}* %5, align 8 
  %6 = load  {i8* (i8*)*, {}}, {i8* (i8*)*, {}}* %3, align 8 
  %7 = alloca {{i8* (i8*, i8*)*, {i8*}} (i8*)*, {}}, align 8 
  store  {{i8* (i8*, i8*)*, {i8*}} (i8*)*, {}} %0, {{i8* (i8*, i8*)*, {i8*}} (i8*)*, {}}* %7, align 8 
  %8 = getelementptr  {{i8* (i8*, i8*)*, {i8*}} (i8*)*, {}}, {{i8* (i8*, i8*)*, {i8*}} (i8*)*, {}}* %7, i32 0, i32 0 
  %9 = load  {i8* (i8*, i8*)*, {i8*}} (i8*)*, {i8* (i8*, i8*)*, {i8*}} (i8*)** %8, align 8 
  %10 = getelementptr  {{i8* (i8*, i8*)*, {i8*}} (i8*)*, {}}, {{i8* (i8*, i8*)*, {i8*}} (i8*)*, {}}* %7, i32 0, i32 1 
  %11 = load  {}, {}* %10, align 8 
  %12 =  call ccc  {i8* (i8*, i8*)*, {i8*}}  %9({i8* (i8*)*, {}}  %6)  
  %13 = alloca double, align 8 
  store  double 2.000000e0, double* %13, align 8 
  %14 = bitcast double* %13 to i8* 
  %15 = alloca {i8* (i8*, i8*)*, {i8*}}, align 8 
  store  {i8* (i8*, i8*)*, {i8*}} %12, {i8* (i8*, i8*)*, {i8*}}* %15, align 8 
  %16 = getelementptr  {i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* %15, i32 0, i32 0 
  %17 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %16, align 8 
  %18 = getelementptr  {i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* %15, i32 0, i32 1 
  %19 = load  {i8*}, {i8*}* %18, align 8 
  %20 = getelementptr  {i8*}, {i8*}* %18, i32 0, i32 0 
  %21 = load  i8*, i8** %20, align 8 
  %22 =  call ccc  i8*  %17(i8*  %21, i8*  %14)  
  %23 =  call ccc  i32  @puts(i8*  %22)  
  ret void 
}