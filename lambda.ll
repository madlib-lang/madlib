; ModuleID = 'main'


 


define external ccc  i8* @__closure__0(i8*  %f_0, i8*  %t_0)    {
entry_0:
  %0 = bitcast i8* %f_0 to {i8* (i8*)*, {}}* 
  %1 = load  {i8* (i8*)*, {}}, {i8* (i8*)*, {}}* %0, align 8 
  %2 = bitcast i8* %t_0 to {i8*, i8*}* 
  %3 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 0 
  %4 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 1 
  %5 = load  i8*, i8** %3, align 8 
  %6 = and i1 1, 1 
  %7 = load  i8*, i8** %4, align 8 
  %8 = and i1 %6, 1 
  br i1 %8, label %branchExpBlock_0, label %exitBlock_0 
branchExpBlock_0:
  %9 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 0 
  %10 = load  i8*, i8** %9, align 8 
  %11 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 1 
  %12 = load  i8*, i8** %11, align 8 
  %13 = alloca {i8* (i8*)*, {}}, align 8 
  store  {i8* (i8*)*, {}} %1, {i8* (i8*)*, {}}* %13, align 8 
  %14 = getelementptr  {i8* (i8*)*, {}}, {i8* (i8*)*, {}}* %13, i32 0, i32 0 
  %15 = load  i8* (i8*)*, i8* (i8*)** %14, align 8 
  %16 = getelementptr  {i8* (i8*)*, {}}, {i8* (i8*)*, {}}* %13, i32 0, i32 1 
  %17 = load  {}, {}* %16, align 8 
  %18 =  call ccc  i8*  %15(i8*  %10)  
  %19 = alloca {i8* (i8*)*, {}}, align 8 
  store  {i8* (i8*)*, {}} %1, {i8* (i8*)*, {}}* %19, align 8 
  %20 = getelementptr  {i8* (i8*)*, {}}, {i8* (i8*)*, {}}* %19, i32 0, i32 0 
  %21 = load  i8* (i8*)*, i8* (i8*)** %20, align 8 
  %22 = getelementptr  {i8* (i8*)*, {}}, {i8* (i8*)*, {}}* %19, i32 0, i32 1 
  %23 = load  {}, {}* %22, align 8 
  %24 =  call ccc  i8*  %21(i8*  %12)  
  %25 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %26 = bitcast i8* %25 to {i8*, i8*}* 
  %27 = getelementptr  {i8*, i8*}, {i8*, i8*}* %26, i32 0, i32 0 
  store  i8* %18, i8** %27, align 8 
  %28 = getelementptr  {i8*, i8*}, {i8*, i8*}* %26, i32 0, i32 1 
  store  i8* %24, i8** %28, align 8 
  br label %exitBlock_0 
exitBlock_0:
  %29 = phi {i8*, i8*}* [%26, %branchExpBlock_0], [zeroinitializer, %entry_0] 
  %30 = bitcast {i8*, i8*}* %29 to i8* 
  ret i8* %30 
}


define external ccc  {i8* (i8*, i8*)*, {i8*}} @mapTuple(i8*  %f_0)    {
entry_0:
  %0 = bitcast i8* %f_0 to {i8* (i8*)*, {}}* 
  %1 = load  {i8* (i8*)*, {}}, {i8* (i8*)*, {}}* %0, align 8 
  %2 = alloca {i8* (i8*)*, {}}, align 8 
  store  {i8* (i8*)*, {}} %1, {i8* (i8*)*, {}}* %2, align 8 
  %3 = bitcast {i8* (i8*)*, {}}* %2 to i8* 
  %4 = alloca {i8*}, align 8 
  %5 = getelementptr  {i8*}, {i8*}* %4, i32 0, i32 0 
  store  i8* %3, i8** %5, align 8 
  %6 = load  {i8*}, {i8*}* %4, align 8 
  %7 = alloca {i8* (i8*, i8*)*, {i8*}}, align 8 
  %8 = getelementptr  {i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* %7, i32 0, i32 0 
  store  i8* (i8*, i8*)* @__closure__0, i8* (i8*, i8*)** %8, align 8 
  %9 = getelementptr  {i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* %7, i32 0, i32 1 
  store  {i8*} %6, {i8*}* %9, align 8 
  %10 = load  {i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* %7, align 8 
  ret {i8* (i8*, i8*)*, {i8*}} %10 
}


define external ccc  i8* @m(i8*  %a_0)    {
entry_0:
  %0 = bitcast i8* %a_0 to i8* 
  %1 =  call ccc  i8*  @GC_malloc(i64  2)  
  %2 = getelementptr  i8, i8* %1, i32 0 
  store  i8 109, i8* %2, align 8 
  %3 = getelementptr  i8, i8* %1, i32 1 
  store  i8 0, i8* %3, align 8 
  ret i8* %1 
}


declare external ccc  i32 @puts(i8*)    


declare external ccc  i8* @malloc(i64)    


declare external ccc  i8* @GC_malloc(i64)    


declare external ccc  i8* @calloc(i32, i32)    


declare external ccc  i1 @__streq__(i8*, i8*)    


define external ccc  void @main()    {
entry_0:
  %0 = bitcast i8* (i8*)* @m to i8* 
  %1 =  call ccc  {i8* (i8*, i8*)*, {i8*}}  @mapTuple(i8*  %0)  
  %2 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({double, double}* getelementptr inbounds ({double, double}, {double, double}* inttoptr (i32 0 to {double, double}*), i32 1) to i64))  
  %3 = bitcast i8* %2 to {double, double}* 
  %4 = getelementptr  {double, double}, {double, double}* %3, i32 0, i32 0 
  store  double 1.000000e0, double* %4, align 8 
  %5 = getelementptr  {double, double}, {double, double}* %3, i32 0, i32 1 
  store  double 2.000000e0, double* %5, align 8 
  %6 = bitcast {double, double}* %3 to i8* 
  %7 = alloca {i8* (i8*, i8*)*, {i8*}}, align 8 
  store  {i8* (i8*, i8*)*, {i8*}} %1, {i8* (i8*, i8*)*, {i8*}}* %7, align 8 
  %8 = getelementptr  {i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* %7, i32 0, i32 0 
  %9 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %8, align 8 
  %10 = getelementptr  {i8* (i8*, i8*)*, {i8*}}, {i8* (i8*, i8*)*, {i8*}}* %7, i32 0, i32 1 
  %11 = load  {i8*}, {i8*}* %10, align 8 
  %12 = getelementptr  {i8*}, {i8*}* %10, i32 0, i32 0 
  %13 = load  i8*, i8** %12, align 8 
  %14 =  call ccc  i8*  %9(i8*  %13, i8*  %6)  
  ret void 
}