; ModuleID = 'main'


 


@$Number$Float = external   global {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}} 


declare external ccc  i8* @"$Number$Float$*"(i8*, i8*)    


declare external ccc  i8* @"$Number$Float$+"(i8*, i8*)    


declare external ccc  i8* @$Number$Float$-(i8*, i8*)    


@$Number$Integer = external   global {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}} 


declare external ccc  i8* @"$Number$Integer$*"(i8*, i8*)    


declare external ccc  i8* @"$Number$Integer$+"(i8*, i8*)    


declare external ccc  i8* @$Number$Integer$-(i8*, i8*)    


define external ccc  i8* @$Show$Integer$show(i8* )    {
  %2 = bitcast i8* (i8*)* @__4bd7afeabdc9f506813b36f5947fa76f__showInteger to i8* 
  %3 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %4 = bitcast i8* %3 to {i8*, i32, i32, i8*}* 
  %5 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %4, i32 0, i32 0 
  store  i8* %2, i8** %5, align 8 
  %6 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %4, i32 0, i32 1 
  store  i32 1, i32* %6, align 8 
  %7 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %4, i32 0, i32 2 
  store  i32 1, i32* %7, align 8 
  %8 = bitcast {i8*, i32, i32, i8*}* %4 to i8* 
  %9 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %8, i32  1, i8*  %0)  
  ret i8* %9 
}


@$Show$Integer =    global {{i8*, i32, i32, i8*}} { {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*)* @$Show$Integer$show to i8*), i32 1, i32 1, i8* undef } }


define external ccc  i8* @$lambda$lifted$0(i8*  %$Number$f5_0, i8*  %x_0)    {
  %1 = bitcast i8* %$Number$f5_0 to i8* 
  %2 = bitcast i8* %x_0 to i8* 
  %3 = bitcast i8* %1 to {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* 
  %4 = getelementptr  {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* %3, i32 0, i32 0 
  %5 = bitcast {i8*, i32, i32, i8*}* %4 to i8* 
  %6 = bitcast i8* %1 to {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* 
  %7 = getelementptr  {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* %6, i32 0, i32 0 
  %8 = bitcast {i8*, i32, i32, i8*}* %7 to i8* 
  %9 = bitcast i8* %1 to {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* 
  %10 = getelementptr  {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* %9, i32 0, i32 0 
  %11 = bitcast {i8*, i32, i32, i8*}* %10 to i8* 
  %12 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %13 = bitcast i8* %12 to double* 
  store  double 1.000000e0, double* %13, align 8 
  %14 = bitcast double* %13 to i8* 
  %15 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %16 = bitcast i8* %15 to double* 
  store  double 2.000000e0, double* %16, align 8 
  %17 = bitcast double* %16 to i8* 
  %18 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %11, i32  2, i8*  %14, i8*  %17)  
  %19 = bitcast i8* %18 to i8* 
  %20 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %8, i32  2, i8*  %2, i8*  %19)  
  %21 = bitcast i8* %20 to i8* 
  %22 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %23 = bitcast i8* %22 to double* 
  store  double 5.000000e0, double* %23, align 8 
  %24 = bitcast double* %23 to i8* 
  %25 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %5, i32  2, i8*  %21, i8*  %24)  
  %26 = bitcast i8* %25 to i8* 
  ret i8* %26 
}


declare external ccc  i8* @puts(i8 addrspace(1)*)    


define external ccc  i8* @__4bd7afeabdc9f506813b36f5947fa76f__log(i8* )    {
  %2 = bitcast i8* %0 to i8 addrspace(1)** 
  %3 = load  i8 addrspace(1)*, i8 addrspace(1)** %2, align 8 
  %4 =  call ccc  i8*  @puts(i8 addrspace(1)*  %3)  
  ret i8* %4 
}


declare external ccc  i8* @__integerToStr__(i8*)    


define external ccc  i8* @__4bd7afeabdc9f506813b36f5947fa76f__showInteger(i8* )    {
  %2 = bitcast i8* %0 to i64* 
  %3 = load  i64, i64* %2, align 8 
  %4 =  call ccc  i8*  @__integerToStr__(i64  %3)  
  ret i8* %4 
}


define external ccc  i8* @__4bd7afeabdc9f506813b36f5947fa76f__inc(i8*  %$Number$f5_0)    {
  %1 = bitcast i8* %$Number$f5_0 to i8* 
  %2 = bitcast i8* (i8*, i8*)* @$lambda$lifted$0 to i8* 
  %3 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %4 = bitcast i8* %3 to {i8*}* 
  %5 = getelementptr  {i8*}, {i8*}* %4, i32 0, i32 0 
  store  i8* %1, i8** %5, align 8 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {i8*, i32, i32, i8*}* 
  %8 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 0 
  store  i8* %2, i8** %8, align 8 
  %9 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 1 
  store  i32 2, i32* %9, align 8 
  %10 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 2 
  store  i32 1, i32* %10, align 8 
  %11 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 3 
  store  i8* %3, i8** %11, align 8 
  %12 = bitcast {i8*, i32, i32, i8*}* %7 to i8* 
  ret i8* %12 
}


declare external ccc  i8* @__applyPAP__(i8*, i32, ...)    


declare external ccc  {i32, i8*}* @__buildRecord__(i32, i8*, ...)    


declare external ccc  i8* @__selectField__(i8 addrspace(1)*, {i32, i8*}*)    


declare external ccc  i1 @__streq__(i8 addrspace(1)*, i8 addrspace(1)*)    


declare external ccc  i8 addrspace(1)* @__strConcat__(i8 addrspace(1)*, i8 addrspace(1)*)    


declare external ccc  i1 @MadList_hasMinLength(double, {i8*, i8*}*)    


declare external ccc  i1 @MadList_hasLength(double, {i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_singleton(i8*)    


declare external ccc  {i8*, i8*}* @__MadList_push__(i8*, {i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_concat({i8*, i8*}*, {i8*, i8*}*)    


declare external ccc  i8* @GC_malloc(i64)    


declare external ccc  i8* @malloc(i64)    


declare external ccc  i8* @calloc(i32, i32)    


declare external ccc  void @__initEventLoop__()    


declare external ccc  void @__startEventLoop__()    


@__4bd7afeabdc9f506813b36f5947fa76f__a =    global i64 undef


define external ccc  void @main()    {
entry_0:
   call ccc  void  @__initEventLoop__()  
  %0 = bitcast i8* (i8*)* @__4bd7afeabdc9f506813b36f5947fa76f__inc to i8* 
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {i8*, i32, i32, i8*}* 
  %3 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %2, i32 0, i32 0 
  store  i8* %0, i8** %3, align 8 
  %4 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %2, i32 0, i32 1 
  store  i32 1, i32* %4, align 8 
  %5 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %2, i32 0, i32 2 
  store  i32 1, i32* %5, align 8 
  %6 = bitcast {i8*, i32, i32, i8*}* %2 to i8* 
  %7 = bitcast {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* @$Number$Integer to i8* 
  %8 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %6, i32  1, i8*  %7)  
  %9 = bitcast i8* %8 to {i8*, i32, i32, i8*}* 
  %10 = bitcast {i8*, i32, i32, i8*}* %9 to i8* 
  %11 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %12 = bitcast i8* %11 to double* 
  store  double 1.000000e0, double* %12, align 8 
  %13 = bitcast double* %12 to i8* 
  %14 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %10, i32  1, i8*  %13)  
  %15 = bitcast i8* %14 to i64* 
  %16 = load  i64, i64* %15, align 8 
  store  i64 %16, i64* @__4bd7afeabdc9f506813b36f5947fa76f__a, align 8 
  %17 = load  i64, i64* @__4bd7afeabdc9f506813b36f5947fa76f__a, align 8 
  %18 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64))  
  %19 = bitcast i8* %18 to i64* 
  store  i64 %17, i64* %19, align 8 
  %20 = bitcast i64* %19 to i8* 
  %21 =  call ccc  i8*  @$Show$Integer$show(i8*  %20)  
  %22 = bitcast i8* %21 to i8 addrspace(1)** 
  %23 = load  i8 addrspace(1)*, i8 addrspace(1)** %22, align 8 
  %24 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i8 addrspace(1)** getelementptr inbounds (i8 addrspace(1)*, i8 addrspace(1)** inttoptr (i32 0 to i8 addrspace(1)**), i32 1) to i64))  
  %25 = bitcast i8* %24 to i8 addrspace(1)** 
  store  i8 addrspace(1)* %23, i8 addrspace(1)** %25, align 8 
  %26 = bitcast i8 addrspace(1)** %25 to i8* 
  %27 =  call ccc  i8*  @__4bd7afeabdc9f506813b36f5947fa76f__log(i8*  %26)  
  %28 = bitcast i8* %27 to i8* 
   call ccc  void  @__startEventLoop__()  
  ret void 
}