; ModuleID = 'main'


 


define external ccc  {i8* (i8*, i8*)*, {}*}* @$Show$Number$show()    {
  ret {i8* (i8*, i8*)*, {}*}* @showNumber 
}


@$Show$Number =    global {{i8* (i8*, i8*)*, {}*}* ()*} { {i8* (i8*, i8*)*, {}*}* ()* @$Show$Number$show }


define external ccc  {i8* (i8*, i8*)*, {}*}* @$Show$Boolean$show()    {
  ret {i8* (i8*, i8*)*, {}*}* @showBoolean 
}


@$Show$Boolean =    global {{i8* (i8*, i8*)*, {}*}* ()*} { {i8* (i8*, i8*)*, {}*}* ()* @$Show$Boolean$show }


define external ccc  i8* @$closureFn$0(i8*  %env_0, i8*  %_P__0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %_P__0 to double* 
  %3 = load  double, double* %2, align 8 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %5 = bitcast i8* %4 to double* 
  store  double 7.000000e0, double* %5, align 8 
  %6 = bitcast double* %5 to i8* 
  %7 = bitcast {i8* (i8*, i8*)*, {}*}* @append to {i8* (i8*, i8*)*, i8*}* 
  %8 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %7, i32 0, i32 0 
  %9 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %8, align 8 
  %10 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %7, i32 0, i32 1 
  %11 = load  i8*, i8** %10, align 8 
  %12 =  call ccc  i8*  %9(i8*  %11, i8*  %6)  
  %13 = bitcast i8* %12 to {i8*, i8*}* 
  %14 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %15 = bitcast i8* %14 to double* 
  store  double %3, double* %15, align 8 
  %16 = bitcast double* %15 to i8* 
  %17 = bitcast {i8* (i8*, i8*)*, {}*}* @singleton to {i8* (i8*, i8*)*, i8*}* 
  %18 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %17, i32 0, i32 0 
  %19 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %18, align 8 
  %20 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %17, i32 0, i32 1 
  %21 = load  i8*, i8** %20, align 8 
  %22 =  call ccc  i8*  %19(i8*  %21, i8*  %16)  
  %23 = bitcast i8* %22 to {i8*, i8*}* 
  %24 = bitcast {i8*, i8*}* %23 to i8* 
  %25 = bitcast {i8*, i8*}* %13 to {i8* (i8*, i8*)*, i8*}* 
  %26 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %25, i32 0, i32 0 
  %27 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %26, align 8 
  %28 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %25, i32 0, i32 1 
  %29 = load  i8*, i8** %28, align 8 
  %30 =  call ccc  i8*  %27(i8*  %29, i8*  %24)  
  %31 = bitcast i8* %30 to {i8*, i8*}* 
  %32 = bitcast {i8*, i8*}* %31 to i8* 
  ret i8* %32 
}


declare external ccc  i8* @puts(i8*)    


define external ccc  i8* @log$0(i8*  %env_0, i8*  %arg_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %arg_0 to i8* 
  %3 =  call ccc  i8*  @puts(i8*  %2)  
  ret i8* %3 
}


@log =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @log$0, {}* zeroinitializer }


declare external ccc  i8* @__doubleToStr__(double)    


define external ccc  i8* @showNumber$0(i8*  %env_0, i8*  %arg_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %arg_0 to double* 
  %3 = load  double, double* %2, align 8 
  %4 =  call ccc  i8*  @__doubleToStr__(double  %3)  
  ret i8* %4 
}


@showNumber =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @showNumber$0, {}* zeroinitializer }


declare external ccc  i8* @__booleanToStr__(i1)    


define external ccc  i8* @showBoolean$0(i8*  %env_0, i8*  %arg_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %arg_0 to i1* 
  %3 = load  i1, i1* %2, align 8 
  %4 =  call ccc  i8*  @__booleanToStr__(i1  %3)  
  ret i8* %4 
}


@showBoolean =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @showBoolean$0, {}* zeroinitializer }


declare external ccc  {i8*, i8*}* @MadList_singleton(i8*)    


define external ccc  i8* @singleton$0(i8*  %env_0, i8*  %arg_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %arg_0 to i8* 
  %3 =  call ccc  {i8*, i8*}*  @MadList_singleton(i8*  %2)  
  %4 = bitcast {i8*, i8*}* %3 to i8* 
  ret i8* %4 
}


@singleton =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @singleton$0, {}* zeroinitializer }


declare external ccc  {i8*, i8*}* @MadList_append(i8*, {i8*, i8*}*)    


define external ccc  i8* @append$0(i8*  %env_0, i8*  %arg_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %3 = bitcast i8* %2 to {i8*}* 
  %4 = getelementptr  {i8*}, {i8*}* %3, i32 0, i32 0 
  store  i8* %arg_0, i8** %4, align 8 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {i8*}*}* getelementptr inbounds ({i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {i8*}*}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {i8* (i8*, i8*)*, {i8*}*}* 
  %7 = getelementptr  {i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* %6, i32 0, i32 0 
  store  i8* (i8*, i8*)* @append$1, i8* (i8*, i8*)** %7, align 8 
  %8 = getelementptr  {i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* %6, i32 0, i32 1 
  store  {i8*}* %3, {i8*}** %8, align 8 
  %9 = bitcast {i8* (i8*, i8*)*, {i8*}*}* %6 to i8* 
  ret i8* %9 
}


define external ccc  i8* @append$1(i8*  %env_0, i8*  %arg_0)    {
  %1 = bitcast i8* %env_0 to {i8*}* 
  %2 = getelementptr  {i8*}, {i8*}* %1, i32 0, i32 0 
  %3 = load  i8*, i8** %2, align 8 
  %4 = bitcast i8* %3 to i8* 
  %5 = bitcast i8* %arg_0 to {i8*, i8*}* 
  %6 =  call ccc  {i8*, i8*}*  @MadList_append(i8*  %4, {i8*, i8*}*  %5)  
  %7 = bitcast {i8*, i8*}* %6 to i8* 
  ret i8* %7 
}


@append =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @append$0, {}* zeroinitializer }


declare external ccc  i8* @malloc(i64)    


declare external ccc  i8* @GC_malloc(i64)    


declare external ccc  i8* @calloc(i32, i32)    


declare external ccc  i1 @__streq__(i8*, i8*)    


declare external ccc  i8* @MadList_length(i8*)    


@$EMPTY_ENV =    global {} {  }


define external ccc  void @main()    {
entry_0:
  %0 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %1 = bitcast i8* %0 to {}* 
  %2 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {}*}* getelementptr inbounds ({i8* (i8*, i8*)*, {}*}, {i8* (i8*, i8*)*, {}*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {}*}*), i32 1) to i64))  
  %3 = bitcast i8* %2 to {i8* (i8*, i8*)*, {}*}* 
  %4 = getelementptr  {i8* (i8*, i8*)*, {}*}, {i8* (i8*, i8*)*, {}*}* %3, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$closureFn$0, i8* (i8*, i8*)** %4, align 8 
  %5 = getelementptr  {i8* (i8*, i8*)*, {}*}, {i8* (i8*, i8*)*, {}*}* %3, i32 0, i32 1 
  store  {}* %1, {}** %5, align 8 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %7 = bitcast i8* %6 to double* 
  store  double 3.000000e0, double* %7, align 8 
  %8 = bitcast double* %7 to i8* 
  %9 = bitcast {i8* (i8*, i8*)*, {}*}* %3 to {i8* (i8*, i8*)*, i8*}* 
  %10 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %9, i32 0, i32 0 
  %11 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %10, align 8 
  %12 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %9, i32 0, i32 1 
  %13 = load  i8*, i8** %12, align 8 
  %14 =  call ccc  i8*  %11(i8*  %13, i8*  %8)  
  %15 = bitcast i8* %14 to {i8*, i8*}* 
  %16 =  call ccc  i8*  @MadList_length({i8*, i8*}*  %15)  
  %17 = bitcast i8* %16 to double* 
  %18 = load  double, double* %17, align 8 
  %19 = fcmp oge double %18, 2.000000e0 
  br i1 %19, label %branchExpBlock_0, label %exitBlock_0 
branchExpBlock_0:
  %20 = getelementptr  {i8*, i8*}, {i8*, i8*}* %15, i32 0, i32 0 
  %21 = load  i8*, i8** %20, align 8 
  %22 = bitcast i8* %21 to {i8*, i8*}* 
  %23 = getelementptr  {i8*, i8*}, {i8*, i8*}* %15, i32 0, i32 1 
  %24 = load  i8*, i8** %23, align 8 
  %25 = bitcast i8* %24 to {i8*, i8*}* 
  %26 = getelementptr  {i8*, i8*}, {i8*, i8*}* %25, i32 0, i32 0 
  %27 = load  i8*, i8** %26, align 8 
  %28 = bitcast i8* %27 to {i8*, i8*}* 
  %29 = getelementptr  {i8*, i8*}, {i8*, i8*}* %25, i32 0, i32 1 
  %30 = load  i8*, i8** %29, align 8 
  %31 = bitcast i8* %30 to {i8*, i8*}* 
  %32 = getelementptr  {{i8* (i8*, i8*)*, {}*}* ()*}, {{i8* (i8*, i8*)*, {}*}* ()*}* @$Show$Number, i32 0, i32 0 
  %33 = load  {i8* (i8*, i8*)*, {}*}* ()*, {i8* (i8*, i8*)*, {}*}* ()** %32, align 8 
  %34 =  call ccc  {i8* (i8*, i8*)*, {}*}*  %33()  
  %35 = bitcast {i8*, i8*}* %28 to i8* 
  %36 = bitcast {i8* (i8*, i8*)*, {}*}* %34 to {i8* (i8*, i8*)*, i8*}* 
  %37 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %36, i32 0, i32 0 
  %38 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %37, align 8 
  %39 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %36, i32 0, i32 1 
  %40 = load  i8*, i8** %39, align 8 
  %41 =  call ccc  i8*  %38(i8*  %40, i8*  %35)  
  %42 = bitcast i8* %41 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %43 = phi i8* [%42, %branchExpBlock_0], [zeroinitializer, %entry_0] 
  %44 = bitcast {i8* (i8*, i8*)*, {}*}* @log to {i8* (i8*, i8*)*, i8*}* 
  %45 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %44, i32 0, i32 0 
  %46 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %45, align 8 
  %47 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %44, i32 0, i32 1 
  %48 = load  i8*, i8** %47, align 8 
  %49 =  call ccc  i8*  %46(i8*  %48, i8*  %43)  
  %50 = bitcast i8* %49 to i8* 
  %51 = getelementptr  {{i8* (i8*, i8*)*, {}*}* ()*}, {{i8* (i8*, i8*)*, {}*}* ()*}* @$Show$Number, i32 0, i32 0 
  %52 = load  {i8* (i8*, i8*)*, {}*}* ()*, {i8* (i8*, i8*)*, {}*}* ()** %51, align 8 
  %53 =  call ccc  {i8* (i8*, i8*)*, {}*}*  %52()  
  %54 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %55 = bitcast i8* %54 to double* 
  store  double 2.198325e0, double* %55, align 8 
  %56 = bitcast double* %55 to i8* 
  %57 = bitcast {i8* (i8*, i8*)*, {}*}* %53 to {i8* (i8*, i8*)*, i8*}* 
  %58 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %57, i32 0, i32 0 
  %59 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %58, align 8 
  %60 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %57, i32 0, i32 1 
  %61 = load  i8*, i8** %60, align 8 
  %62 =  call ccc  i8*  %59(i8*  %61, i8*  %56)  
  %63 = bitcast i8* %62 to i8* 
  %64 = bitcast {i8* (i8*, i8*)*, {}*}* @log to {i8* (i8*, i8*)*, i8*}* 
  %65 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %64, i32 0, i32 0 
  %66 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %65, align 8 
  %67 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %64, i32 0, i32 1 
  %68 = load  i8*, i8** %67, align 8 
  %69 =  call ccc  i8*  %66(i8*  %68, i8*  %63)  
  %70 = bitcast i8* %69 to i8* 
  %71 = getelementptr  {{i8* (i8*, i8*)*, {}*}* ()*}, {{i8* (i8*, i8*)*, {}*}* ()*}* @$Show$Boolean, i32 0, i32 0 
  %72 = load  {i8* (i8*, i8*)*, {}*}* ()*, {i8* (i8*, i8*)*, {}*}* ()** %71, align 8 
  %73 =  call ccc  {i8* (i8*, i8*)*, {}*}*  %72()  
  %74 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i1* getelementptr inbounds (i1, i1* inttoptr (i32 0 to i1*), i32 1) to i64))  
  %75 = bitcast i8* %74 to i1* 
  store  i1 0, i1* %75, align 8 
  %76 = bitcast i1* %75 to i8* 
  %77 = bitcast {i8* (i8*, i8*)*, {}*}* %73 to {i8* (i8*, i8*)*, i8*}* 
  %78 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %77, i32 0, i32 0 
  %79 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %78, align 8 
  %80 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %77, i32 0, i32 1 
  %81 = load  i8*, i8** %80, align 8 
  %82 =  call ccc  i8*  %79(i8*  %81, i8*  %76)  
  %83 = bitcast i8* %82 to i8* 
  %84 = bitcast {i8* (i8*, i8*)*, {}*}* @log to {i8* (i8*, i8*)*, i8*}* 
  %85 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %84, i32 0, i32 0 
  %86 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %85, align 8 
  %87 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %84, i32 0, i32 1 
  %88 = load  i8*, i8** %87, align 8 
  %89 =  call ccc  i8*  %86(i8*  %88, i8*  %83)  
  %90 = bitcast i8* %89 to i8* 
  %91 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %92 = bitcast i8* %91 to double* 
  store  double 1.980000e2, double* %92, align 8 
  %93 = bitcast double* %92 to i8* 
  %94 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %95 = bitcast i8* %94 to double* 
  store  double 2.000000e0, double* %95, align 8 
  %96 = bitcast double* %95 to i8* 
  %97 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %98 = bitcast i8* %97 to {i8*, i8*}* 
  %99 = getelementptr  {i8*, i8*}, {i8*, i8*}* %98, i32 0, i32 0 
  store  i8* %93, i8** %99, align 8 
  %100 = getelementptr  {i8*, i8*}, {i8*, i8*}* %98, i32 0, i32 1 
  store  i8* %96, i8** %100, align 8 
  %101 = bitcast {i8*, i8*}* %98 to i8* 
  %102 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %103 = bitcast i8* %102 to double* 
  store  double 1.000000e0, double* %103, align 8 
  %104 = bitcast double* %103 to i8* 
  %105 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %106 = bitcast i8* %105 to {i8*, i8*}* 
  %107 = getelementptr  {i8*, i8*}, {i8*, i8*}* %106, i32 0, i32 0 
  store  i8* %101, i8** %107, align 8 
  %108 = getelementptr  {i8*, i8*}, {i8*, i8*}* %106, i32 0, i32 1 
  store  i8* %104, i8** %108, align 8 
  %109 = getelementptr  {i8*, i8*}, {i8*, i8*}* %106, i32 0, i32 0 
  %110 = getelementptr  {i8*, i8*}, {i8*, i8*}* %106, i32 0, i32 1 
  %111 = load  i8*, i8** %109, align 8 
  %112 = bitcast i8* %111 to {i8*, i8*}* 
  %113 = getelementptr  {i8*, i8*}, {i8*, i8*}* %112, i32 0, i32 0 
  %114 = getelementptr  {i8*, i8*}, {i8*, i8*}* %112, i32 0, i32 1 
  %115 = load  i8*, i8** %113, align 8 
  %116 = bitcast i8* %115 to {i8*, i8*}* 
  %117 = and i1 1, 1 
  %118 = load  i8*, i8** %114, align 8 
  %119 = bitcast i8* %118 to double* 
  %120 = load  double, double* %119, align 8 
  %121 = fcmp oeq double 2.000000e0, %120 
  %122 = and i1 %117, %121 
  %123 = and i1 1, %122 
  %124 = load  i8*, i8** %110, align 8 
  %125 = bitcast i8* %124 to double* 
  %126 = load  double, double* %125, align 8 
  %127 = fcmp oeq double 1.000000e0, %126 
  %128 = and i1 %123, %127 
  br i1 %128, label %branchExpBlock_1, label %exitBlock_1 
branchExpBlock_1:
  %129 = getelementptr  {i8*, i8*}, {i8*, i8*}* %106, i32 0, i32 0 
  %130 = load  i8*, i8** %129, align 8 
  %131 = bitcast i8* %130 to {i8*, i8*}* 
  %132 = getelementptr  {i8*, i8*}, {i8*, i8*}* %131, i32 0, i32 0 
  %133 = load  i8*, i8** %132, align 8 
  %134 = bitcast i8* %133 to {i8*, i8*}* 
  %135 = getelementptr  {i8*, i8*}, {i8*, i8*}* %131, i32 0, i32 1 
  %136 = load  i8*, i8** %135, align 8 
  %137 = bitcast i8* %136 to double* 
  %138 = load  double, double* %137, align 8 
  %139 = getelementptr  {i8*, i8*}, {i8*, i8*}* %106, i32 0, i32 1 
  %140 = load  i8*, i8** %139, align 8 
  %141 = bitcast i8* %140 to double* 
  %142 = load  double, double* %141, align 8 
  %143 = getelementptr  {{i8* (i8*, i8*)*, {}*}* ()*}, {{i8* (i8*, i8*)*, {}*}* ()*}* @$Show$Number, i32 0, i32 0 
  %144 = load  {i8* (i8*, i8*)*, {}*}* ()*, {i8* (i8*, i8*)*, {}*}* ()** %143, align 8 
  %145 =  call ccc  {i8* (i8*, i8*)*, {}*}*  %144()  
  %146 = bitcast {i8*, i8*}* %134 to i8* 
  %147 = bitcast {i8* (i8*, i8*)*, {}*}* %145 to {i8* (i8*, i8*)*, i8*}* 
  %148 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %147, i32 0, i32 0 
  %149 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %148, align 8 
  %150 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %147, i32 0, i32 1 
  %151 = load  i8*, i8** %150, align 8 
  %152 =  call ccc  i8*  %149(i8*  %151, i8*  %146)  
  %153 = bitcast i8* %152 to i8* 
  br label %exitBlock_1 
exitBlock_1:
  %154 = phi i8* [%153, %branchExpBlock_1], [zeroinitializer, %exitBlock_0] 
  %155 = bitcast {i8* (i8*, i8*)*, {}*}* @log to {i8* (i8*, i8*)*, i8*}* 
  %156 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %155, i32 0, i32 0 
  %157 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %156, align 8 
  %158 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %155, i32 0, i32 1 
  %159 = load  i8*, i8** %158, align 8 
  %160 =  call ccc  i8*  %157(i8*  %159, i8*  %154)  
  %161 = bitcast i8* %160 to i8* 
  ret void 
}