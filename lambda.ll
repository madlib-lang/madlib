; ModuleID = 'main'


 


define external ccc  i8* @Just$0(i8*  %env_0, i8*  %arg_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %3 = bitcast i8* %2 to {i8*}* 
  %4 = getelementptr  {i8*}, {i8*}* %3, i32 0, i32 0 
  store  i8* %arg_0, i8** %4, align 8 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64, i8*}* getelementptr inbounds ({i64, i8*}, {i64, i8*}* inttoptr (i32 0 to {i64, i8*}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {i64, i8*}* 
  %7 = getelementptr  {i64, i8*}, {i64, i8*}* %6, i32 0, i32 1 
  store  i8* %arg_0, i8** %7, align 8 
  %8 = getelementptr  {i64, i8*}, {i64, i8*}* %6, i32 0, i32 0 
  store  i64 0, i64* %8, align 8 
  %9 = bitcast {i64, i8*}* %6 to i8* 
  ret i8* %9 
}


@Just =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @Just$0, {}* zeroinitializer }


@Nothing =    global {i64} { i64 1 }


declare external ccc  i8* @puts(i8*)    


define external ccc  i8* @log$0(i8*  %env_0, i8*  %arg_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 =  call ccc  i8*  @puts(i8*  %arg_0)  
  ret i8* %2 
}


@log =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @log$0, {}* zeroinitializer }


define external ccc  i8* @log$uncurried(i8* )    {
  %2 = bitcast i8* %0 to i8* 
  %3 =  call ccc  i8*  @puts(i8*  %0)  
  ret i8* %3 
}


declare external ccc  i8* @__doubleToStr__(i8*)    


define external ccc  i8* @showNumber$0(i8*  %env_0, i8*  %arg_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 =  call ccc  i8*  @__doubleToStr__(i8*  %arg_0)  
  ret i8* %2 
}


@showNumber =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @showNumber$0, {}* zeroinitializer }


define external ccc  i8* @showNumber$uncurried(i8* )    {
  %2 = bitcast i8* %0 to double* 
  %3 = load  double, double* %2, align 8 
  %4 =  call ccc  i8*  @__doubleToStr__(i8*  %0)  
  ret i8* %4 
}


declare external ccc  i8* @MadList_length(i8*)    


define external ccc  i8* @len$0(i8*  %env_0, i8*  %arg_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 =  call ccc  i8*  @MadList_length(i8*  %arg_0)  
  ret i8* %2 
}


@len =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @len$0, {}* zeroinitializer }


define external ccc  i8* @len$uncurried(i8* )    {
  %2 = bitcast i8* %0 to {i8*, i8*}* 
  %3 =  call ccc  i8*  @MadList_length(i8*  %0)  
  ret i8* %3 
}


declare external ccc  i8* @MadList_push(i8*, i8*)    


define external ccc  i8* @push$0(i8*  %env_0, i8*  %arg_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %3 = bitcast i8* %2 to {i8*}* 
  %4 = getelementptr  {i8*}, {i8*}* %3, i32 0, i32 0 
  store  i8* %arg_0, i8** %4, align 8 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {i8*}*}* getelementptr inbounds ({i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {i8*}*}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {i8* (i8*, i8*)*, {i8*}*}* 
  %7 = getelementptr  {i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* %6, i32 0, i32 0 
  store  i8* (i8*, i8*)* @push$1, i8* (i8*, i8*)** %7, align 8 
  %8 = getelementptr  {i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* %6, i32 0, i32 1 
  store  {i8*}* %3, {i8*}** %8, align 8 
  %9 = bitcast {i8* (i8*, i8*)*, {i8*}*}* %6 to i8* 
  ret i8* %9 
}


define external ccc  i8* @push$1(i8*  %env_0, i8*  %arg_0)    {
  %1 = bitcast i8* %env_0 to {i8*}* 
  %2 = getelementptr  {i8*}, {i8*}* %1, i32 0, i32 0 
  %3 = load  i8*, i8** %2, align 8 
  %4 =  call ccc  i8*  @MadList_push(i8*  %3, i8*  %arg_0)  
  ret i8* %4 
}


@push =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @push$0, {}* zeroinitializer }


define external ccc  i8* @push$uncurried(i8* , i8* )    {
  %3 = bitcast i8* %0 to i8* 
  %4 = bitcast i8* %1 to {i8*, i8*}* 
  %5 =  call ccc  i8*  @MadList_push(i8*  %0, i8*  %1)  
  ret i8* %5 
}


declare external ccc  i8* @MadList_map(i8*, i8*)    


define external ccc  i8* @map2$0(i8*  %env_0, i8*  %arg_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %3 = bitcast i8* %2 to {i8*}* 
  %4 = getelementptr  {i8*}, {i8*}* %3, i32 0, i32 0 
  store  i8* %arg_0, i8** %4, align 8 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {i8*}*}* getelementptr inbounds ({i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {i8*}*}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {i8* (i8*, i8*)*, {i8*}*}* 
  %7 = getelementptr  {i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* %6, i32 0, i32 0 
  store  i8* (i8*, i8*)* @map2$1, i8* (i8*, i8*)** %7, align 8 
  %8 = getelementptr  {i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* %6, i32 0, i32 1 
  store  {i8*}* %3, {i8*}** %8, align 8 
  %9 = bitcast {i8* (i8*, i8*)*, {i8*}*}* %6 to i8* 
  ret i8* %9 
}


define external ccc  i8* @map2$1(i8*  %env_0, i8*  %arg_0)    {
  %1 = bitcast i8* %env_0 to {i8*}* 
  %2 = getelementptr  {i8*}, {i8*}* %1, i32 0, i32 0 
  %3 = load  i8*, i8** %2, align 8 
  %4 =  call ccc  i8*  @MadList_map(i8*  %3, i8*  %arg_0)  
  ret i8* %4 
}


@map2 =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @map2$0, {}* zeroinitializer }


define external ccc  i8* @map2$uncurried(i8* , i8* )    {
  %3 = bitcast i8* %1 to {i8*, i8*}* 
  %4 =  call ccc  i8*  @MadList_map(i8*  %0, i8*  %1)  
  ret i8* %4 
}


@fn =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @fn$fn, {}* @$EMPTY_ENV }


define external ccc  i8* @fn$fn(i8*  %env_0, i8*  %__0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %__0 to i8* 
  %3 = load  {i8*, i8*}*, {i8*, i8*}** @l1, align 8 
  %4 = bitcast {i8*, i8*}* %3 to i8* 
  ret i8* %4 
}


define external ccc  i8* @fn$uncurried(i8*  %__0)    {
  %1 = bitcast i8* %__0 to i8* 
  %2 = load  {i8*, i8*}*, {i8*, i8*}** @l1, align 8 
  %3 = bitcast {i8*, i8*}* %2 to i8* 
  ret i8* %3 
}


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


@show =    global {i8* (i8*, i8*)*, {}*}* undef


@l1 =    global {i8*, i8*}* undef


@l2 =    global {i8*, i8*}* undef


@l3 =    global {i8*, i8*}* undef


@superList =    global {i8*, i8*}* undef


define external ccc  void @main()    {
entry_0:
  store  {i8* (i8*, i8*)*, {}*}* @showNumber, {i8* (i8*, i8*)*, {}*}** @show, align 8 
  %0 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %1 = bitcast i8* %0 to double* 
  store  double 3.000000e0, double* %1, align 8 
  %2 = bitcast double* %1 to i8* 
  %3 =  call ccc  {i8*, i8*}*  @MadList_singleton(i8*  %2)  
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %5 = bitcast i8* %4 to double* 
  store  double 2.000000e0, double* %5, align 8 
  %6 = bitcast double* %5 to i8* 
  %7 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %6, {i8*, i8*}*  %3)  
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %9 = bitcast i8* %8 to double* 
  store  double 1.000000e0, double* %9, align 8 
  %10 = bitcast double* %9 to i8* 
  %11 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %10, {i8*, i8*}*  %7)  
  store  {i8*, i8*}* %11, {i8*, i8*}** @l1, align 8 
  %12 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %13 = bitcast i8* %12 to double* 
  store  double 6.000000e0, double* %13, align 8 
  %14 = bitcast double* %13 to i8* 
  %15 =  call ccc  {i8*, i8*}*  @MadList_singleton(i8*  %14)  
  %16 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %17 = bitcast i8* %16 to double* 
  store  double 5.000000e0, double* %17, align 8 
  %18 = bitcast double* %17 to i8* 
  %19 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %18, {i8*, i8*}*  %15)  
  %20 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %21 = bitcast i8* %20 to double* 
  store  double 4.000000e0, double* %21, align 8 
  %22 = bitcast double* %21 to i8* 
  %23 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %22, {i8*, i8*}*  %19)  
  store  {i8*, i8*}* %23, {i8*, i8*}** @l2, align 8 
  %24 =  call ccc  {i8*, i8*}*  @MadList_concat({i8*, i8*}*  %11, {i8*, i8*}*  %23)  
  store  {i8*, i8*}* %24, {i8*, i8*}** @l3, align 8 
  %25 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %26 = bitcast i8* %25 to double* 
  store  double 2.000000e0, double* %26, align 8 
  %27 = bitcast double* %26 to i8* 
  %28 =  call ccc  i8*  @fn$uncurried(i8*  %27)  
  %29 = bitcast i8* %28 to {i8*, i8*}* 
  store  {i8*, i8*}* %29, {i8*, i8*}** @superList, align 8 
  %30 =  call ccc  i1  @MadList_hasMinLength(double  2.000000e0, {i8*, i8*}*  %11)  
  %31 = getelementptr  {i8*, i8*}, {i8*, i8*}* %11, i32 0, i32 0 
  %32 = load  i8*, i8** %31, align 8 
  %33 = bitcast i8* %32 to {i8*, i8*}* 
  %34 = getelementptr  {i8*, i8*}, {i8*, i8*}* %11, i32 0, i32 1 
  %35 = load  i8*, i8** %34, align 8 
  %36 = bitcast i8* %35 to {i8*, i8*}* 
  %37 = getelementptr  {i8*, i8*}, {i8*, i8*}* %36, i32 0, i32 0 
  %38 = load  i8*, i8** %37, align 8 
  %39 = bitcast i8* %38 to {i8*, i8*}* 
  %40 = getelementptr  {i8*, i8*}, {i8*, i8*}* %36, i32 0, i32 1 
  %41 = load  i8*, i8** %40, align 8 
  %42 = bitcast i8* %41 to {i8*, i8*}* 
  %43 = and i1 1, 1 
  %44 = and i1 1, %43 
  %45 = and i1 %30, %44 
  br i1 %45, label %branchExpBlock_0, label %exitBlock_0 
branchExpBlock_0:
  %46 = getelementptr  {i8*, i8*}, {i8*, i8*}* %11, i32 0, i32 0 
  %47 = load  i8*, i8** %46, align 8 
  %48 = bitcast i8* %47 to {i8*, i8*}* 
  %49 = getelementptr  {i8*, i8*}, {i8*, i8*}* %11, i32 0, i32 1 
  %50 = load  i8*, i8** %49, align 8 
  %51 = bitcast i8* %50 to {i8*, i8*}* 
  %52 = getelementptr  {i8*, i8*}, {i8*, i8*}* %51, i32 0, i32 0 
  %53 = load  i8*, i8** %52, align 8 
  %54 = bitcast i8* %53 to {i8*, i8*}* 
  %55 = getelementptr  {i8*, i8*}, {i8*, i8*}* %51, i32 0, i32 1 
  %56 = load  i8*, i8** %55, align 8 
  %57 = bitcast i8* %56 to {i8*, i8*}* 
  %58 = bitcast {i8*, i8*}* %54 to i8* 
  %59 =  call ccc  i8*  @showNumber$uncurried(i8*  %58)  
  %60 = bitcast i8* %59 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %61 = phi i8* [%60, %branchExpBlock_0], [zeroinitializer, %entry_0] 
  %62 = bitcast i8* %61 to i8* 
  %63 =  call ccc  i8*  @log$uncurried(i8*  %62)  
  %64 = bitcast i8* %63 to i8* 
  ret void 
}