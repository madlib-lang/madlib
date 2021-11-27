; ModuleID = 'main'


 


@$Number$Float = external   global {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}} 


declare external ccc  i8* @"$Number$Float$*"(i8*, i8*)    


declare external ccc  i8* @"$Number$Float$+"(i8*, i8*)    


declare external ccc  i8* @$Number$Float$-(i8*, i8*)    


declare external ccc  i8* @$Number$Float$__coerceNumber__(i8*)    


@$Number$Integer = external   global {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}} 


declare external ccc  i8* @"$Number$Integer$*"(i8*, i8*)    


declare external ccc  i8* @"$Number$Integer$+"(i8*, i8*)    


declare external ccc  i8* @$Number$Integer$-(i8*, i8*)    


declare external ccc  i8* @$Number$Integer$__coerceNumber__(i8*)    


define external ccc  i8* @$Show$Integer$show(i8* )    {
  %2 = bitcast i8* (i8*)* @__9c8027a934e4e6c2e530f94b3a07e07a__showInteger to i8* 
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


define external ccc  i8* @$Show$Float$show(i8* )    {
  %2 = bitcast i8* (i8*)* @__9c8027a934e4e6c2e530f94b3a07e07a__showFloat to i8* 
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


@$Show$Float =    global {{i8*, i32, i32, i8*}} { {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*)* @$Show$Float$show to i8*), i32 1, i32 1, i8* undef } }


define external ccc  i8* @$Show$Byte$show(i8* )    {
  %2 = bitcast i8* (i8*)* @__9c8027a934e4e6c2e530f94b3a07e07a__showByte to i8* 
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


@$Show$Byte =    global {{i8*, i32, i32, i8*}} { {i8*, i32, i32, i8*} { i8* bitcast (i8* (i8*)* @$Show$Byte$show to i8*), i32 1, i32 1, i8* undef } }


define external ccc  i8* @$lambda$lifted$0(i8*  %$Number$i8_0, i8*  %a_0, i8*  %count1_0, i8*  %__0)    {
  %1 = bitcast i8* %$Number$i8_0 to i8* 
  %2 = bitcast i8* %a_0 to i8* 
  %3 = bitcast i8* %count1_0 to i8* 
  %4 = bitcast i8* %__0 to i8* 
  %5 = bitcast i8* %1 to {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* 
  %6 = getelementptr  {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* %5, i32 0, i32 1 
  %7 = bitcast {i8*, i32, i32, i8*}* %6 to i8* 
  %8 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %7, i32  2, i8*  %3, i8*  %2)  
  %9 = bitcast i8* %8 to i64* 
  %10 = load  i64, i64* %9, align 8 
  %11 = bitcast i8* %count1_0 to i64* 
  %12 = bitcast i64 %10 to i64 
  store  i64 %12, i64* %11, align 8 
  %13 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64))  
  %14 = bitcast i8* %13 to i64* 
  store  i64 %10, i64* %14, align 8 
  %15 = bitcast i64* %14 to i8* 
  ret i8* %15 
}


define external ccc  i8* @$lambda$lifted$1(i8*  %$Number$i8_0, i8*  %a_0)    {
  %1 = bitcast i8* %$Number$i8_0 to i8* 
  %2 = bitcast i8* %a_0 to i8* 
  %3 = bitcast i8* %1 to {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* 
  %4 = getelementptr  {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* %3, i32 0, i32 3 
  %5 = bitcast {i8*, i32, i32, i8*}* %4 to i8* 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64))  
  %7 = bitcast i8* %6 to i64* 
  store  i64 0, i64* %7, align 8 
  %8 = bitcast i64* %7 to i8* 
  %9 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %5, i32  1, i8*  %8)  
  %10 = bitcast i8* %9 to i64* 
  %11 = load  i64, i64* %10, align 8 
  %12 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64))  
  %13 = bitcast i8* %12 to i64* 
  store  i64 %11, i64* %13, align 8 
  %14 = bitcast i8* (i8*, i8*, i8*, i8*)* @$lambda$lifted$0 to i8* 
  %15 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64))  
  %16 = bitcast i8* %15 to i64* 
  store  i64 %11, i64* %16, align 8 
  %17 = bitcast i64* %16 to i8* 
  %18 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*, i8*}* getelementptr inbounds ({i8*, i8*, i8*}, {i8*, i8*, i8*}* inttoptr (i32 0 to {i8*, i8*, i8*}*), i32 1) to i64))  
  %19 = bitcast i8* %18 to {i8*, i8*, i8*}* 
  %20 = getelementptr  {i8*, i8*, i8*}, {i8*, i8*, i8*}* %19, i32 0, i32 0 
  store  i8* %1, i8** %20, align 8 
  %21 = getelementptr  {i8*, i8*, i8*}, {i8*, i8*, i8*}* %19, i32 0, i32 1 
  store  i8* %2, i8** %21, align 8 
  %22 = getelementptr  {i8*, i8*, i8*}, {i8*, i8*, i8*}* %19, i32 0, i32 2 
  store  i8* %17, i8** %22, align 8 
  %23 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %24 = bitcast i8* %23 to {i8*, i32, i32, i8*}* 
  %25 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %24, i32 0, i32 0 
  store  i8* %14, i8** %25, align 8 
  %26 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %24, i32 0, i32 1 
  store  i32 4, i32* %26, align 8 
  %27 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %24, i32 0, i32 2 
  store  i32 1, i32* %27, align 8 
  %28 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %24, i32 0, i32 3 
  store  i8* %18, i8** %28, align 8 
  %29 = bitcast {i8*, i32, i32, i8*}* %24 to i8* 
  ret i8* %29 
}


declare external ccc  i8* @puts(i8 addrspace(1)*)    


define external ccc  i8* @__9c8027a934e4e6c2e530f94b3a07e07a__log(i8* )    {
  %2 = bitcast i8* %0 to i8 addrspace(1)** 
  %3 = load  i8 addrspace(1)*, i8 addrspace(1)** %2, align 8 
  %4 =  call ccc  i8*  @puts(i8 addrspace(1)*  %3)  
  ret i8* %4 
}


declare external ccc  i8* @__integerToStr__(i64)    


define external ccc  i8* @__9c8027a934e4e6c2e530f94b3a07e07a__showInteger(i8* )    {
  %2 = bitcast i8* %0 to i64* 
  %3 = load  i64, i64* %2, align 8 
  %4 =  call ccc  i8*  @__integerToStr__(i64  %3)  
  ret i8* %4 
}


declare external ccc  i8* @__floatToStr__(double)    


define external ccc  i8* @__9c8027a934e4e6c2e530f94b3a07e07a__showFloat(i8* )    {
  %2 = bitcast i8* %0 to double* 
  %3 = load  double, double* %2, align 8 
  %4 =  call ccc  i8*  @__floatToStr__(double  %3)  
  ret i8* %4 
}


declare external ccc  i8* @__byteToStr__(i8*)    


define external ccc  i8* @__9c8027a934e4e6c2e530f94b3a07e07a__showByte(i8* )    {
  %2 = bitcast i8* %0 to i8* 
  %3 = load  i8, i8* %2, align 8 
  %4 =  call ccc  i8*  @__byteToStr__(i8  %3)  
  ret i8* %4 
}


define external ccc  i8* @__9c8027a934e4e6c2e530f94b3a07e07a__makeCounter(i8*  %$Number$i8_0)    {
  %1 = bitcast i8* %$Number$i8_0 to i8* 
  %2 = bitcast i8* (i8*, i8*)* @$lambda$lifted$1 to i8* 
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


@__9c8027a934e4e6c2e530f94b3a07e07a__counter =    global {i8*, i32, i32, i8*}* undef


define external ccc  void @main()    {
entry_0:
   call ccc  void  @__initEventLoop__()  
  %0 = bitcast i8* (i8*)* @__9c8027a934e4e6c2e530f94b3a07e07a__makeCounter to i8* 
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {i8*, i32, i32, i8*}* 
  %3 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %2, i32 0, i32 0 
  store  i8* %0, i8** %3, align 8 
  %4 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %2, i32 0, i32 1 
  store  i32 1, i32* %4, align 8 
  %5 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %2, i32 0, i32 2 
  store  i32 1, i32* %5, align 8 
  %6 = bitcast {i8*, i32, i32, i8*}* %2 to i8* 
  %7 = bitcast {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* @$Number$Integer to i8* 
  %8 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %6, i32  1, i8*  %7)  
  %9 = bitcast i8* %8 to {i8*, i32, i32, i8*}* 
  %10 = bitcast {i8*, i32, i32, i8*}* %9 to i8* 
  %11 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64))  
  %12 = bitcast i8* %11 to i64* 
  store  i64 17, i64* %12, align 8 
  %13 = bitcast i64* %12 to i8* 
  %14 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %10, i32  1, i8*  %13)  
  %15 = bitcast i8* %14 to {i8*, i32, i32, i8*}* 
  store  {i8*, i32, i32, i8*}* %15, {i8*, i32, i32, i8*}** @__9c8027a934e4e6c2e530f94b3a07e07a__counter, align 8 
  %16 = load  {i8*, i32, i32, i8*}*, {i8*, i32, i32, i8*}** @__9c8027a934e4e6c2e530f94b3a07e07a__counter, align 8 
  %17 = bitcast {i8*, i32, i32, i8*}* %16 to i8* 
  %18 = bitcast i1* zeroinitializer to i8* 
  %19 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %17, i32  1, i8*  %18)  
  %20 = bitcast i8* %19 to i64* 
  %21 = load  i64, i64* %20, align 8 
  %22 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64))  
  %23 = bitcast i8* %22 to i64* 
  store  i64 %21, i64* %23, align 8 
  %24 = bitcast i64* %23 to i8* 
  %25 =  call ccc  i8*  @$Show$Integer$show(i8*  %24)  
  %26 = bitcast i8* %25 to i8 addrspace(1)** 
  %27 = load  i8 addrspace(1)*, i8 addrspace(1)** %26, align 8 
  %28 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i8 addrspace(1)** getelementptr inbounds (i8 addrspace(1)*, i8 addrspace(1)** inttoptr (i32 0 to i8 addrspace(1)**), i32 1) to i64))  
  %29 = bitcast i8* %28 to i8 addrspace(1)** 
  store  i8 addrspace(1)* %27, i8 addrspace(1)** %29, align 8 
  %30 = bitcast i8 addrspace(1)** %29 to i8* 
  %31 =  call ccc  i8*  @__9c8027a934e4e6c2e530f94b3a07e07a__log(i8*  %30)  
  %32 = bitcast i8* %31 to i8* 
  %33 = load  {i8*, i32, i32, i8*}*, {i8*, i32, i32, i8*}** @__9c8027a934e4e6c2e530f94b3a07e07a__counter, align 8 
  %34 = bitcast {i8*, i32, i32, i8*}* %33 to i8* 
  %35 = bitcast i1* zeroinitializer to i8* 
  %36 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %34, i32  1, i8*  %35)  
  %37 = bitcast i8* %36 to i64* 
  %38 = load  i64, i64* %37, align 8 
  %39 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64))  
  %40 = bitcast i8* %39 to i64* 
  store  i64 %38, i64* %40, align 8 
  %41 = bitcast i64* %40 to i8* 
  %42 =  call ccc  i8*  @$Show$Integer$show(i8*  %41)  
  %43 = bitcast i8* %42 to i8 addrspace(1)** 
  %44 = load  i8 addrspace(1)*, i8 addrspace(1)** %43, align 8 
  %45 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i8 addrspace(1)** getelementptr inbounds (i8 addrspace(1)*, i8 addrspace(1)** inttoptr (i32 0 to i8 addrspace(1)**), i32 1) to i64))  
  %46 = bitcast i8* %45 to i8 addrspace(1)** 
  store  i8 addrspace(1)* %44, i8 addrspace(1)** %46, align 8 
  %47 = bitcast i8 addrspace(1)** %46 to i8* 
  %48 =  call ccc  i8*  @__9c8027a934e4e6c2e530f94b3a07e07a__log(i8*  %47)  
  %49 = bitcast i8* %48 to i8* 
  %50 = load  {i8*, i32, i32, i8*}*, {i8*, i32, i32, i8*}** @__9c8027a934e4e6c2e530f94b3a07e07a__counter, align 8 
  %51 = bitcast {i8*, i32, i32, i8*}* %50 to i8* 
  %52 = bitcast i1* zeroinitializer to i8* 
  %53 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %51, i32  1, i8*  %52)  
  %54 = bitcast i8* %53 to i64* 
  %55 = load  i64, i64* %54, align 8 
  %56 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64))  
  %57 = bitcast i8* %56 to i64* 
  store  i64 %55, i64* %57, align 8 
  %58 = bitcast i64* %57 to i8* 
  %59 =  call ccc  i8*  @$Show$Integer$show(i8*  %58)  
  %60 = bitcast i8* %59 to i8 addrspace(1)** 
  %61 = load  i8 addrspace(1)*, i8 addrspace(1)** %60, align 8 
  %62 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i8 addrspace(1)** getelementptr inbounds (i8 addrspace(1)*, i8 addrspace(1)** inttoptr (i32 0 to i8 addrspace(1)**), i32 1) to i64))  
  %63 = bitcast i8* %62 to i8 addrspace(1)** 
  store  i8 addrspace(1)* %61, i8 addrspace(1)** %63, align 8 
  %64 = bitcast i8 addrspace(1)** %63 to i8* 
  %65 =  call ccc  i8*  @__9c8027a934e4e6c2e530f94b3a07e07a__log(i8*  %64)  
  %66 = bitcast i8* %65 to i8* 
  %67 = load  {i8*, i32, i32, i8*}*, {i8*, i32, i32, i8*}** @__9c8027a934e4e6c2e530f94b3a07e07a__counter, align 8 
  %68 = bitcast {i8*, i32, i32, i8*}* %67 to i8* 
  %69 = bitcast i1* zeroinitializer to i8* 
  %70 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %68, i32  1, i8*  %69)  
  %71 = bitcast i8* %70 to i64* 
  %72 = load  i64, i64* %71, align 8 
  %73 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64))  
  %74 = bitcast i8* %73 to i64* 
  store  i64 %72, i64* %74, align 8 
  %75 = bitcast i64* %74 to i8* 
  %76 =  call ccc  i8*  @$Show$Integer$show(i8*  %75)  
  %77 = bitcast i8* %76 to i8 addrspace(1)** 
  %78 = load  i8 addrspace(1)*, i8 addrspace(1)** %77, align 8 
  %79 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i8 addrspace(1)** getelementptr inbounds (i8 addrspace(1)*, i8 addrspace(1)** inttoptr (i32 0 to i8 addrspace(1)**), i32 1) to i64))  
  %80 = bitcast i8* %79 to i8 addrspace(1)** 
  store  i8 addrspace(1)* %78, i8 addrspace(1)** %80, align 8 
  %81 = bitcast i8 addrspace(1)** %80 to i8* 
  %82 =  call ccc  i8*  @__9c8027a934e4e6c2e530f94b3a07e07a__log(i8*  %81)  
  %83 = bitcast i8* %82 to i8* 
  %84 = load  {i8*, i32, i32, i8*}*, {i8*, i32, i32, i8*}** @__9c8027a934e4e6c2e530f94b3a07e07a__counter, align 8 
  %85 = bitcast {i8*, i32, i32, i8*}* %84 to i8* 
  %86 = bitcast i1* zeroinitializer to i8* 
  %87 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %85, i32  1, i8*  %86)  
  %88 = bitcast i8* %87 to i64* 
  %89 = load  i64, i64* %88, align 8 
  %90 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64))  
  %91 = bitcast i8* %90 to i64* 
  store  i64 %89, i64* %91, align 8 
  %92 = bitcast i64* %91 to i8* 
  %93 =  call ccc  i8*  @$Show$Integer$show(i8*  %92)  
  %94 = bitcast i8* %93 to i8 addrspace(1)** 
  %95 = load  i8 addrspace(1)*, i8 addrspace(1)** %94, align 8 
  %96 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i8 addrspace(1)** getelementptr inbounds (i8 addrspace(1)*, i8 addrspace(1)** inttoptr (i32 0 to i8 addrspace(1)**), i32 1) to i64))  
  %97 = bitcast i8* %96 to i8 addrspace(1)** 
  store  i8 addrspace(1)* %95, i8 addrspace(1)** %97, align 8 
  %98 = bitcast i8 addrspace(1)** %97 to i8* 
  %99 =  call ccc  i8*  @__9c8027a934e4e6c2e530f94b3a07e07a__log(i8*  %98)  
  %100 = bitcast i8* %99 to i8* 
   call ccc  void  @__startEventLoop__()  
  ret void 
}